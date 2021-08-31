#include <Rinternals.h>
#include <string>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#include <string.h>
#include <errno.h>
#endif

// [[Rcpp::export]]
void detachConsole()
{
#ifdef _WIN32
    FreeConsole();
#else
     if ( getppid() == 1 ) {
<<<<<<< HEAD
         Rprintf("The current process is the group leader!");
=======
         Rprintf("The current process is the group leader!")
>>>>>>> 33d1528ab4bb53b63343af3d36a862627cff6cc9
         return;
         //Rf_error("Cannot detach group leader!");
     }
    int sid = setsid();  
    if (sid < 0)    
    {  
        Rf_error("Fail to detach! Error: %s", strerror(errno));
    }  
#endif
}

