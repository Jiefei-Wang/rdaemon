## TODO: use the temp file that is created by the deamon process
## Feature:
## 1. allow running (named) daemon, one daemon can handle multiple workers
## 2. daemon is only able to run the expression and set the data

clientData <- new.env(parent = emptyenv())
## element's name is daemon name
clientData$connections <- list()
clientData$ports <- list()
clientData$pids <- list()


daemonRegistrationValid <- function(name){
    port <- getDaemonPort(name)
    pid <- getDaemonPid(name)
    if(!is.null(clientData$ports[[name]])){
        if(clientData$ports[[name]] != port||
           clientData$pids[[name]] != pid||
           !isProcessAlive(pid)){
            return(FALSE)
        }
    }
    TRUE
}

## It is safe to call this function many times
## with the same name
loadDaemon <- function(name, pid = Sys.getpid()){
    daemonPort <- getDaemonPort(name)
    daemonPid <- getDaemonPid(name)
    
    ## Remove the incorrect daemon record
    if(!daemonRegistrationValid(name)){
        deregisterDaemon(name, pid = pid)
    }
    
    if(existsDaemon(name)&&
       is.null(clientData$connections[[name]])
    ){
        con <- socketConnection(port = daemonPort, open = "r+")
        writeData(con, pid)
        clientData$connections[[name]] <- con
        clientData$ports[[name]] <- daemonPort
        clientData$pids[[name]] <- daemonPid
    }
}

registerDaemon <- function(name, pid = pid, logPath = NULL){
    ## TODO: run daemon in the background
    if(!existsDaemon(name)){
        rscript <- R.home("bin/Rscript")
        script <- system.file(package="rdaemon", "script", "startDaemon.R")
        Sys.setenv(rdaemon_name = name)
        if(!is.null(logPath))
            Sys.setenv(rdaemon_logPath = logPath)
        system2(rscript, shQuote(script), stdout = FALSE, wait = FALSE)
    }
    loadDaemon(name)
}

deregisterDaemon <- function(name, pid = Sys.getpid()){
    if(!is.null(clientData$connections[[name]])){
        writeData(clientData$connections[[name]],
                  request.deregister(pid))
        close(clientData$connections[[name]])
        clientData$connections[[name]] <- NULL
        clientData$ports[[name]] <- NULL
        clientData$pids[[name]] <- NULL
    }
}

killDaemon <- function(name){
    pid <- getDaemonPid(name)
    if(is.na(pid))
        return()
    
    if(isProcessAlive(pid)){
        tools::pskill(pid, tools::SIGTERM)
    }
}

existsDaemon <- function(name){
    daemonPid <- getDaemonPid(name)
    daemonPort <- getDaemonPort(name)
    if(!is.na(daemonPid)&&
       !is.na(daemonPort)&&
       isProcessAlive(daemonPid)&&
       portOccupied(daemonPort)){
        TRUE
    }else{
        FALSE
    }
}

daemonSetTask <- function(name, expr = NULL, pid = Sys.getpid()){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    task <- request.setTask(substitute(expr), pid = pid)
    writeData(con, task)
}

daemonGetTask <- function(name, expr, pid = Sys.getpid()){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    task <- request.getTask(pid = pid)
    flushData(con)
    writeData(con, task)
    waitData(con)
}

daemonSetTaskScript <- function(name, script, pid = Sys.getpid()){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    expr <- parse(file = script)
    task <- request.setTask(expr, pid = pid)
    writeData(con, task)
}

daemonExport <- function(name, ..., pid = Sys.getpid()){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    x <- request.export(list(...), pid = pid)
    writeData(con, x)
}

daemonCopyTask <- function(name, sourcePid, targetPid = Sys.getpid()){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    task <- request.copyTask(sourcePid, targetPid)
    writeData(con, task)
}
