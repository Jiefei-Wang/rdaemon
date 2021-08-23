
#ifdef _WIN32
#include <windows.h>
#else
#endif

// [[Rcpp::export]]
void detachConsole()
{
#ifdef _WIN32
    FreeConsole();
#else
    //int sid = setsid();
#endif
}