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
    
    if(existsDaemon(name)){
        if(is.null(clientData$connections[[name]])){
            con <- socketConnection(port = daemonPort, open = "r+")
            writeData(con, pid)
            clientData$connections[[name]] <- con
            clientData$ports[[name]] <- daemonPort
            clientData$pids[[name]] <- daemonPid
        }
        TRUE
    }else{
        FALSE
    }
}

#' @export
registerDaemon <- function(name, pid = pid, logFile = NULL){
    ## TODO: run daemon in the background
    if(!existsDaemon(name)){
        rscript <- R.home("bin/Rscript")
        script <- system.file(package="rdaemon", "scripts", "startDaemon.R")
        Sys.setenv(rdaemon_name = name)
        if(!is.null(logFile))
            Sys.setenv(rdaemon_logFile = logFile)
        system2(rscript, shQuote(script), stdout = FALSE, wait = FALSE)
    }
    Sys.sleep(1)
    while(!loadDaemon(name)){
        
    }
}

#' @export
deregisterDaemon <- function(name, pid = Sys.getpid()){
    if(!is.null(clientData$connections[[name]])){
        writeData(clientData$connections[[name]],
                  request.removeClient(pid))
        close(clientData$connections[[name]])
        clientData$connections[[name]] <- NULL
        clientData$ports[[name]] <- NULL
        clientData$pids[[name]] <- NULL
    }
}

#' @export
killDaemon <- function(name){
    pid <- getDaemonPid(name)
    if(is.na(pid))
        return()
    
    if(isProcessAlive(pid)){
        tools::pskill(pid, tools::SIGTERM)
    }
}

#' @export
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

#' @export
daemonSetTask <- function(name, expr = NULL, pid = Sys.getpid()){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    task <- request.setTask(substitute(expr), pid = pid)
    writeData(con, task)
}

#' @export
daemonGetTask <- function(name, expr, pid = Sys.getpid()){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    task <- request.getTask(pid = pid)
    flushData(con)
    writeData(con, task)
    waitData(con)
}

#' @export
daemonSetTaskScript <- function(name, script, pid = Sys.getpid()){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    expr <- parse(file = script)
    task <- request.setTask(expr, pid = pid)
    writeData(con, task)
}

#' @export
daemonExport <- function(name, ..., pid = Sys.getpid()){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    x <- request.export(list(...), pid = pid)
    writeData(con, x)
}

#' @export
daemonCopyTask <- function(name, sourcePid, targetPid = Sys.getpid()){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    task <- request.copyTask(sourcePid, targetPid)
    writeData(con, task)
}
