#' @export
lastRegisteredDaemon <- function(){
    if(serverData$isServer){
        serverData$daemonName
    }else{
        clientData$lastRegisteredDaemon
    }
}


#' @export
daemonTaskId <- function(){
    if(serverData$isServer){
        serverData$currentTaskId
    }else{
        Sys.getpid()
    }
}


#' @export
registerDaemon <- function(
    daemonName = lastRegisteredDaemon(), 
    logFile = NULL){
    stopifnot(!serverData$isServer)
    client.registerDaemon(
        daemonName = daemonName, 
        logFile = logFile)
}

#' @export
deregisterDaemon <- function(
    daemonName = lastRegisteredDaemon(), 
    taskId = daemonTaskId()){
    stopifnot(!serverData$isServer)
    client.deregisterDaemon(daemonName = daemonName, 
                            taskId = taskId)
}

#' @export
killDaemon <- function(daemonName = lastRegisteredDaemon()){
    if(serverData$isServer && identical(serverData$daemonName, daemonName)){
        quit(save = "no")
    }else{
        client.killDaemon(daemonName = daemonName)
    }
}

#' @export
existsDaemon <- function(daemonName = lastRegisteredDaemon()){
    if(serverData$isServer && identical(serverData$daemonName, daemonName))
        return(TRUE)
    
    daemonPid <- getDaemonPid(daemonName)
    daemonPort <- getDaemonPort(daemonName)
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
daemonSetTask <- function(expr = NULL, 
                          daemonName = lastRegisteredDaemon(),
                          taskId = daemonTaskId()){
    if(serverData$isServer){
        stopifnot(identical(serverData$daemonName, daemonName))
        server.setTask(expr = expr, taskId = taskId)
    }else{
        client.setTask(daemonName = daemonName, 
                       taskId = taskId,
                       expr = expr)
    }
}

#' @export
daemonEval <- function(expr,
                       daemonName = lastRegisteredDaemon(), 
                       taskId = daemonTaskId()){
    response <-if(serverData$isServer){
        stopifnot(identical(serverData$daemonName, daemonName))
        server.eval(expr = expr, taskId = taskId)
    }else{
        client.eval(daemonName = daemonName, 
                    taskId = taskId,
                    expr = expr)
    }
    if(inherits(response, "simpleError")){
        stop(response)
    }else{
        response
    }
    
}

#' @export
daemonGetTask <- function(daemonName = lastRegisteredDaemon(), 
                          taskId = daemonTaskId()){
    if(serverData$isServer){
        stopifnot(identical(serverData$daemonName, daemonName))
        server.getTask(taskId = taskId)
    }else{
        client.getTask(daemonName = daemonName, taskId = taskId)
    }
}

#' @export
daemonSetTaskScript <- function(script, 
                                daemonName = lastRegisteredDaemon(), 
                                taskId = daemonTaskId()){
    expr <- parse(file = script)
    daemonSetTask(daemonName = daemonName, expr = expr, taskId = taskId)
}

#' @export
daemonExport <- function(..., 
                         daemonName = lastRegisteredDaemon(), 
                         taskId = daemonTaskId()){
    objects <- list(...)
    if(serverData$isServer){
        stopifnot(identical(serverData$daemonName, daemonName))
        server.export(objects = objects, taskId = taskId)
    }else{
        client.export(daemonName = daemonName, 
                      taskId = taskId, 
                      objects = objects)
    }
}

#' @export
daemonCopyTask <- function(sourceId, 
                           targetId = daemonTaskId(), 
                           daemonName = lastRegisteredDaemon()){
    if(serverData$isServer){
        stopifnot(identical(serverData$daemonName, daemonName))
        server.copyTask(sourceId = sourceId, targetId = targetId)
    }else{
        client.copyTask(daemonName = daemonName, 
                        sourceId = sourceId, 
                        targetId = targetId)
    }
}


