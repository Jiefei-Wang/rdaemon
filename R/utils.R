#' @export
interruptProcess <- function(pid){
    if(Sys.info()[['sysname']]=="Windows"){
        send_SIGINT(pid)
    }else{
        tools::pskill(pid, tools::SIGINT)
    }
}

#' @export
isProcessAlive <- function(pid){
    tryCatch(
        if(Sys.info()[['sysname']]=="Windows"){
            out = system2("wmic",
                          paste0('process where "ProcessID = ',pid, '" get processid'),
                          stdout = TRUE)
            any(grepl(pid, out, fixed = TRUE))
        }else{
            system2("ps", c("-p", pid), stdout = NULL, stderr = NULL) == 0L
        },
        warning = function(e) TRUE,
        error = function(e) TRUE
    )
}

readTxt <- function(file){
    readChar(file, file.info(file)$size)
}




