## TODO: use the temp file that is created by the deamon process
## Feature:
## 1. allow running (named) daemon, one daemon can handle multiple workers
## 2. daemon is only able to run the expression and set the data

clientData <- new.env(parent = emptyenv())
## element's name is daemon name
clientData$connections <- list()
clientData$ports <- list()
clientData$pids <- list()


validateDaemonRegistration <- function(name){
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
loadDaemon <- function(name){
    port <- getDaemonPort(name)
    pid <- getDaemonPid(name)
    
    ## Remove the incorrect daemon record
    if(validateDaemonRegistration(name)){
        deregisterDaemon(name)
    }
    
    if(existsDaemon(name)&&
       is.null(clientData$connections[[name]])
    ){
        con <- socketConnection(port = port, open = "r+")
        writeData(con, Sys.getpid())
        clientData$connections[[name]] <- con
        clientData$ports[[name]] <- port
        clientData$pids[[name]] <- pid
    }
}

registerDaemon <- function(name){
    ## TODO: run daemon in the background
    if(!existsDaemon(name)){
        
    }
    loadDaemon(name)
}

deregisterDaemon <- function(name){
    if(!is.null(clientData$connections[[name]])){
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
    pid <- getDaemonPid(name)
    if(is.na(pid)){
        FALSE
    }else{
        isProcessAlive(pid)
    }
}

daemonSetTask <- function(name, expr){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    task <- request.setTask(substitute(expr))
    writeData(con, task)
}

daemonSetTaskScript <- function(name, path){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    
}

daemonExport <- function(name, ...){
    con <- clientData$connections[[name]]
    stopifnot(!is.null(con))
    
    x <- request.export(list(...))
    writeData(con, x)
}





