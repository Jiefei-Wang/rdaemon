#include <Rinternals.h>
#include <string>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sys/prctl.h>
#endif

// [[Rcpp::export]]
void detachConsole()
{
#ifdef _WIN32
    FreeConsole();
#else
     if ( getppid() == 1 ) {
         Rf_error("Cannot detach group leader!");
     }
    int sid = setsid();  
    if (sid < 0)    
    {  
        Rf_error("Fail to detach! Error: %s", strerror(errno));
    }  
#endif
}

// [[Rcpp::export]]
void setProcessName(std::string name){
#ifdef _WIN32
#else
    prctl(PR_SET_NAME, (unsigned long)"test", 0, 0, 0);
#endif
}