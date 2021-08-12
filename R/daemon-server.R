serverData <- new.env(parent = emptyenv())
serverData$name <- NULL
serverData$serverConn <- NULL
serverData$port <- NULL
## element's name is pid
## connections: The server to client connection
## tasks: The client task
## taskData: The data used by the client task
serverData$connections <- list()
serverData$tasks <- list()
serverData$taskData <- list()
## timeout: Time to wait before quit if no task is running
## isServer: Whether this is a daemon server
## taskPid: The pid corresponds to the currently processed task
serverData$timeout <- 60
serverData$isServer <- FALSE
serverData$taskPid <- NULL

server.deregisterDaemon <- function(pid){
    pid <- as.character(pid)
    con <- serverData$connections[[pid]]
    if(!is.null(con)){
        close(con)
    }
    serverData$connections[[pid]] <- NULL
    serverData$tasks[[pid]] <- NULL
    serverData$taskData[[pid]] <- NULL
}


server.daemonSetTask <- function(expr, pid){
    pid <- as.character(pid)
    serverData$tasks[[pid]] <- expr
}

server.daemonGetTask <- function(pid){
    pid <- as.character(pid)
    serverData$tasks[[pid]]
}

server.daemonExport <- function(objects, pid){
    pid <- as.character(pid)
    for(i in names(objects)){
        serverData$taskData[[pid]][[i]] <- objects[[i]]
    }
}


server.daemonCopyTask <- function(sourcePid, targetPid){
    sourcePid <- as.character(sourcePid)
    targetPid <- as.character(targetPid)
    serverData$taskData[[targetPid]] <- serverData$taskData[[sourcePid]]
    serverData$tasks[[targetPid]] <- serverData$tasks[[sourcePid]]
}




