interruptWorker <- function(pid){
    if(Sys.info()[['sysname']]=="Windows"){
        send_SIGINT(pid)
    }else{
        tools::pskill(pid, tools::SIGINT)
    }
}

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

log <- function(...){
    if(!is.null(deamonLogFile)){
        msg <- paste0(...)
        current <- Sys.time()
        msg <- paste0(current, "  ", msg, "\n")
        cat(msg, file = deamonLogFile, append = TRUE)
    }
}

readTxt <- function(file){
    readChar(file, file.info(file)$size)
}


daemonPortName <- function(name){
    paste0(name, "_port")
}

daemonPidName <- function(name){
    paste0(name, "_pid")
}

getDaemonPort <- function(name){
    getGlobalVariable(daemonPortName(name))
}

getDaemonPid <- function(name){
    getGlobalVariable(daemonPidName(name))
}

setDaemonPort <- function(name, port){
    setGlobalVariable(daemonPortName(name), port)
}

setDaemonPid <- function(name, pid){
    setGlobalVariable(daemonPidName(name), pid)
}

