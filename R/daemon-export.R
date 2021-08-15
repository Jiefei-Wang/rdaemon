#' @export
lastRegisteredDaemon <- function(){
    if(serverData$isServer){
        serverData$name
    }else{
        clientData$lastRegisteredDaemon
    }
}


#' @export
daemonTaskId <- function(){
    if(serverData$isServer){
        serverData$taskId
    }else{
        Sys.getpid()
    }
}


#' @export
registerDaemon <- function(name, taskId = daemonTaskId(), logFile = NULL){
    stopifnot(!serverData$isServer)
    client.registerDaemon(
        name = name, 
        taskId = taskId, 
        logFile = logFile)
}

#' @export
deregisterDaemon <- function(name = lastRegisteredDaemon(), taskId = daemonTaskId()){
    if(serverData$isServer){
        stopifnot(identical(serverData$name, name))
        server.deregisterDaemon(taskId = taskId)
    }else{
        client.deregisterDaemon(name = name, taskId = taskId)
    }
}

#' @export
killDaemon <- function(name = lastRegisteredDaemon()){
    if(serverData$isServer && identical(serverData$name, name)){
        quit(save = "no")
    }else{
        client.killDaemon(name = name)
    }
}

#' @export
existsDaemon <- function(name = lastRegisteredDaemon()){
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
daemonSetTask <- function(expr = NULL, taskId = daemonTaskId(), name = lastRegisteredDaemon()){
    if(serverData$isServer){
        stopifnot(identical(serverData$name, name))
        server.daemonSetTask(expr = expr, taskId = taskId)
    }else{
        client.daemonSetTask(name = name, expr = expr, taskId = taskId)
    }
}

#' @export
daemonGetTask <- function(taskId = daemonTaskId(), name = lastRegisteredDaemon()){
    if(serverData$isServer){
        stopifnot(identical(serverData$name, name))
        server.daemonGetTask(taskId = taskId)
    }else{
        client.daemonGetTask(name = name, taskId = taskId)
    }
}

#' @export
daemonSetTaskScript <- function(script, taskId = daemonTaskId(), name = lastRegisteredDaemon()){
    expr <- parse(file = script)
    daemonSetTask(name = name, expr = expr, taskId = taskId)
}

#' @export
daemonExport <- function(..., taskId = daemonTaskId(), name = lastRegisteredDaemon()){
    objects <- list(...)
    if(serverData$isServer){
        stopifnot(identical(serverData$name, name))
        server.daemonExport(objects = objects, taskId = taskId)
    }else{
        client.daemonExport(name = name, objects = objects, taskId = taskId)
    }
}

#' @export
daemonCopyTask <- function(sourceId, targetId = daemonTaskId(), name = lastRegisteredDaemon()){
   if(serverData$isServer){
        stopifnot(identical(serverData$name, name))
        if(missing(targetId)){
            targetId <- serverData$taskId
        }
        server.daemonCopyTask(sourceId = sourceId, targetId = targetId)
    }else{
        client.daemonCopyTask(name = name, sourceId = sourceId, targetId = targetId)
    }
}


