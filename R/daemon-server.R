serverData <- new.env(parent = emptyenv())
serverData$name <- NULL
serverData$serverConn <- NULL
serverData$port <- NULL
## element's name is the task taskId
## connections: The server to client connection
## tasks: The client task
## taskData: The data used by the client task
serverData$connections <- list()
serverData$tasks <- list()
serverData$taskData <- list()
serverData$clientPid <- list()
## timeout: Time to wait before quit if no task is running
## isServer: Whether this is a daemon server
## taskPid: The pid corresponds to the currently processed task
serverData$timeout <- 60
serverData$isServer <- FALSE
serverData$taskId <- NULL

server.deregisterDaemon <- function(taskId){
    taskId <- as.character(taskId)
    con <- serverData$connections[[taskId]]
    if(!is.null(con)){
        close(con)
    }
    serverData$connections[[taskId]] <- NULL
    serverData$tasks[[taskId]] <- NULL
    serverData$taskData[[taskId]] <- NULL
}


server.daemonSetTask <- function(taskId, expr){
    taskId <- as.character(taskId)
    serverData$tasks[[taskId]] <- expr
}

server.daemonGetTask <- function(taskId){
    taskId <- as.character(taskId)
    serverData$tasks[[taskId]]
}

server.daemonExport <- function(taskId, objects){
    taskId <- as.character(taskId)
    if(is.null(serverData$taskData[[taskId]])){
        serverData$taskData[[taskId]] <- list()
    }
    for(i in names(objects)){
        serverData$taskData[[taskId]][[i]] <- objects[[i]]
    }
}


server.daemonCopyTask <- function(sourceId, targetId){
    sourceId <- as.character(sourceId)
    targetId <- as.character(targetId)
    serverData$taskData[[targetId]] <- serverData$taskData[[sourceId]]
    serverData$tasks[[targetId]] <- serverData$tasks[[sourceId]]
}




