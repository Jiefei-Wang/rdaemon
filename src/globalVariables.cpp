#define R_NO_REMAP
#include <string>
#include <map>
#include <Rinternals.h>

#ifdef _WIN32
#include <windows.h>
#define PKG_SPACE "Local\\rdaemon_"
#else
#include <sys/mman.h>
#include <sys/stat.h>        /* For mode constants */
#include <fcntl.h>           /* For O_* constants */
#include <unistd.h>          /* For close file descriptor */
#include <errno.h>
#include <string.h>
#ifdef __APPLE__
#define PKG_SPACE "/rd_"
#else
#define PKG_SPACE "/rdaemon_"
#endif
#endif


// [[Rcpp::export]]
unsigned int getNameMaxLen(){
    #ifdef __APPLE__
    #ifndef SHM_NAME_MAX
    #define SHM_NAME_MAX 32
    #endif
    return SHM_NAME_MAX - strlen(PKG_SPACE);
    #endif
    #ifdef unix
    return PATH_MAX - strlen(PKG_SPACE);
    #endif
    return UINT_MAX;
}


std::string getName(std::string name)
{
    return PKG_SPACE + name;
}


#ifdef _WIN32
std::map<std::string, HANDLE> handleMap;
std::map<std::string, int *> mappedMemoryMap;

// [[Rcpp::export]]
bool existsGlobalVariable(SEXP sharedMemoryName)
{
    std::string name = getName(CHAR(Rf_asChar(sharedMemoryName)));

    HANDLE hMapFile = CreateFileMappingA(
        INVALID_HANDLE_VALUE, // use paging file
        NULL,                 // default security
        PAGE_READONLY,        // read access
        0,                    // maximum object size (high-order DWORD)
        1,                    // maximum object size (low-order DWORD)
        name.c_str());
    bool exist = (GetLastError() == ERROR_ALREADY_EXISTS);
    CloseHandle(hMapFile);
    return exist;
}

bool createGlobalVariable(SEXP sharedMemoryName, int size)
{
    HANDLE hMapFile;
    int *pBuf;
    std::string name;

    name = getName(CHAR(Rf_asChar(sharedMemoryName)));
    if (handleMap.find(name) != handleMap.end())
    {
        return true;
    }

    hMapFile = CreateFileMappingA(
        INVALID_HANDLE_VALUE, // use paging file
        NULL,                 // default security
        PAGE_READWRITE,       // read/write access
        0,                    // maximum object size (high-order DWORD)
        size,                 // maximum object size (low-order DWORD)
        name.c_str());        // name of mapping object

    if (hMapFile == NULL)
    {
        return false;
    }
    pBuf = (int *)MapViewOfFile(hMapFile,            // handle to map object
                                      FILE_MAP_ALL_ACCESS, // read/write permission
                                      0,
                                      0,
                                      size);

    if (pBuf == NULL)
    {
        CloseHandle(hMapFile);
        return false;
    }

    handleMap.emplace(name, hMapFile);
    mappedMemoryMap.emplace(name, pBuf);
    return true;
}

// [[Rcpp::export]]
void setGlobalVariable(SEXP sharedMemoryName, int value)
{
    std::string name = getName(CHAR(Rf_asChar(sharedMemoryName)));
    if (handleMap.find(name) == handleMap.end())
    {
        createGlobalVariable(sharedMemoryName, sizeof(int));
    }
    int *ptr = mappedMemoryMap.at(name);
    *ptr = value;
}

// [[Rcpp::export]]
int getGlobalVariable(SEXP sharedMemoryName)
{
    std::string name = getName(CHAR(Rf_asChar(sharedMemoryName)));
    if (handleMap.find(name) == handleMap.end())
    {
        if(!existsGlobalVariable(sharedMemoryName))
            return NA_INTEGER;
        else
            createGlobalVariable(sharedMemoryName, sizeof(int));
    }
    return *mappedMemoryMap.at(name);
}

// [[Rcpp::export]]
void unsetGlobalVariable(SEXP sharedMemoryName)
{
    std::string name = getName(CHAR(Rf_asChar(sharedMemoryName)));
    if (handleMap.find(name) != handleMap.end())
    {
        UnmapViewOfFile(mappedMemoryMap.at(name));
        CloseHandle(handleMap.at(name));
    }
}

#else

bool existsGlobalVariable(SEXP sharedMemoryName)
{
    std::string name = getName(CHAR(Rf_asChar(sharedMemoryName)));
    int fd = shm_open(name.c_str(), O_RDONLY, S_IRUSR|S_IWUSR);

    bool exist = (fd != -1);
    if(exist)
        close(fd);
    return exist;
}

void setGlobalVariable(SEXP sharedMemoryName, int value)
{
    std::string name = getName(CHAR(Rf_asChar(sharedMemoryName)));
    size_t size = sizeof(int);
    bool exists = existsGlobalVariable(sharedMemoryName);
    int fd = shm_open(name.c_str(), O_CREAT|O_RDWR, S_IRUSR|S_IWUSR);
    if(fd == -1){
        Rf_error("Fail to create the shared memory file! Error: %s", strerror(errno));
    }
    if(!exists){
        int success = ftruncate(fd, size);
        if(success == -1){
            close(fd);
            Rf_error("Fail to truncate the shared memory file! Error: %s", strerror(errno));
        }
    }
    int *ptr = (int *)mmap(NULL, size,
                                          PROT_READ|PROT_WRITE,
                                          MAP_SHARED, fd, 0);
    close(fd);
    if(ptr == (void*) -1){
        Rf_error("Fail to perform the memory mapping!! Error: %s", strerror(errno));
    }
    *ptr = value;
    munmap(ptr, size);
}

int getGlobalVariable(SEXP sharedMemoryName)
{
    std::string name = getName(CHAR(Rf_asChar(sharedMemoryName)));
    size_t size = sizeof(int);
    int fd = shm_open(name.c_str(), O_RDONLY, S_IRUSR|S_IWUSR);
    if(fd == -1){
        return NA_INTEGER;
    }
    int *ptr = (int *)mmap(NULL, size,
                                          PROT_READ,
                                          MAP_SHARED, fd, 0);
    
    close(fd);
    if(ptr != (void*) -1){
        int value = *ptr;
        munmap(ptr, size);
        return value;
    }else{
        return NA_INTEGER;
    }
}

void unsetGlobalVariable(SEXP sharedMemoryName)
{
    std::string name = getName(CHAR(Rf_asChar(sharedMemoryName)));
    shm_unlink(name.c_str());
}

#endif