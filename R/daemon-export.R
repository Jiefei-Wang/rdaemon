#' @export
registerDaemon <- function(name, pid = pid, logFile = NULL){
    stopifnot(!serverData$isServer)
    client.registerDaemon(
        name = name, 
        pid = pid, 
        logFile = logFile)
}

#' @export
deregisterDaemon <- function(name, pid = Sys.getpid()){
    if(serverData$isServer){
        stopifnot(identical(serverData$name, name))
        if(missing(pid)){
            pid <- serverData$taskPid
        }
        server.deregisterDaemon(pid = pid)
    }else{
        client.deregisterDaemon(name = name, pid = pid)
    }
}

#' @export
killDaemon <- function(name){
    if(serverData$isServer && identical(serverData$name, name)){
        quit(save = "no")
    }else{
        client.killDaemon(name = name)
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
    if(serverData$isServer){
        stopifnot(identical(serverData$name, name))
        if(missing(pid)){
            pid <- serverData$taskPid
        }
        server.daemonSetTask(expr = expr, pid = pid)
    }else{
        client.daemonSetTask(name = name, expr = expr, pid = pid)
    }
}

#' @export
daemonGetTask <- function(name, pid = Sys.getpid()){
    if(serverData$isServer){
        stopifnot(identical(serverData$name, name))
        if(missing(pid)){
            pid <- serverData$taskPid
        }
        server.daemonGetTask(pid = pid)
    }else{
        client.daemonGetTask(name = name, pid = pid)
    }
}

#' @export
daemonSetTaskScript <- function(name, script, pid = Sys.getpid()){
    expr <- parse(file = script)
    if(missing(pid)){
            pid <- serverData$taskPid
    }
    daemonSetTask(name = name, expr = expr, pid = pid)
}

#' @export
daemonExport <- function(name, ..., pid = Sys.getpid()){
    objects <- list(...)
    if(serverData$isServer){
        stopifnot(identical(serverData$name, name))
        if(missing(pid)){
            pid <- serverData$taskPid
        }
        server.daemonExport(objects = objects, pid = pid)
    }else{
        client.daemonExport(name = name, objects = objects, pid = pid)
    }
}

#' @export
daemonCopyTask <- function(name, sourcePid, targetPid = Sys.getpid()){
   if(serverData$isServer){
        stopifnot(identical(serverData$name, name))
        if(missing(targetPid)){
            targetPid <- serverData$taskPid
        }
        server.daemonCopyTask(sourcePid = sourcePid, targetPid = targetPid)
    }else{
        client.daemonCopyTask(name = name, sourcePid = sourcePid, targetPid = targetPid)
    }
}



#' @export
serverDaemonName <- function(){
    serverData$name
}

#' @export
serverDaemonTaskPid <- function(){
    serverData$taskPid
}