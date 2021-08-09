#include <string>
#include <map>
#include <windows.h>
#include <Rinternals.h>

#define DATA_TYPE int

#ifdef _WIN32
#define PKG_SPACE "Local\\rdaemon_"
#else
#define PKG_SPACE "rdaemon_"
#endif

std::map<std::string, HANDLE> handleMap;
std::map<std::string, DATA_TYPE *> mappedMemoryMap;


#ifdef _WIN32
std::string getName(std::string name)
{
    return PKG_SPACE + name;
}

// [[Rcpp::export]]
bool existsGlobalVariable(SEXP sharedMemoryName)
{
    std::string name = getName(CHAR(asChar(sharedMemoryName)));

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

// [[Rcpp::export]]
bool createGlobalVariable(SEXP sharedMemoryName, int size)
{
    HANDLE hMapFile;
    DATA_TYPE *pBuf;
    std::string name;

    name = getName(CHAR(asChar(sharedMemoryName)));
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
    pBuf = (DATA_TYPE *)MapViewOfFile(hMapFile,            // handle to map object
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
    std::string name = getName(CHAR(asChar(sharedMemoryName)));
    if (handleMap.find(name) == handleMap.end())
    {
        createGlobalVariable(sharedMemoryName, sizeof(DATA_TYPE));
    }
    DATA_TYPE *ptr = mappedMemoryMap.at(name);
    *ptr = value;
}

// [[Rcpp::export]]
int getGlobalVariable(SEXP sharedMemoryName)
{
    std::string name = getName(CHAR(asChar(sharedMemoryName)));
    if (handleMap.find(name) == handleMap.end())
    {
        if(!existsGlobalVariable(sharedMemoryName))
            return NA_INTEGER;
        else
            createGlobalVariable(sharedMemoryName, sizeof(DATA_TYPE));
    }
    return *mappedMemoryMap.at(name);
}

// [[Rcpp::export]]
void unsetGlobalVariable(SEXP sharedMemoryName)
{
    std::string name = getName(CHAR(asChar(sharedMemoryName)));
    if (handleMap.find(name) != handleMap.end())
    {
        UnmapViewOfFile(mappedMemoryMap.at(name));
        CloseHandle(handleMap.at(name));
    }
}


// [[Rcpp::export]]
int C_test(){
    return NA_INTEGER;
}

#endif