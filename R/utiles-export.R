#' Utility functions for the daemon
#' 
#' Utility functions for the daemon
#' 
#' @param pid integer(1), the process ID
#' 
#' @details 
#' `interruptProcess`: send SIGINT signal to the other process. 
#' The implementation for Windows is tricky and therefore 
#' it is only recommended to run this function in the daemon.
#' 
#' @returns 
#' `interruptProcess`: invisible()
#' @examples 
#' ## interrupt a process by the PID
#' \dontrun{
#' interruptProcess(pid = 1234L)
#' }
#' @rdname daemon-utils
#' @export
interruptProcess <- function(pid){
    if(Sys.info()[['sysname']]=="Windows"){
        send_SIGINT(pid)
    }else{
        tools::pskill(pid, tools::SIGINT)
    }
    invisible()
}


#' @details 
#' `isProcessAlive`: check whether a process is running
#' @returns 
#' `isProcessAlive`: logical(1)
#' @examples 
#' isProcessAlive(1234L)
#' 
#' @rdname daemon-utils
#' @export
isProcessAlive <- function(pid){
    isProcessRunning(pid)
}