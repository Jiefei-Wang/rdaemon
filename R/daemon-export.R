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
registerDaemon <- function(daemonName = lastRegisteredDaemon(),
                           logFile = NULL,
                           threshold = c("INFO", "WARN", "ERROR", "DEBUG", "TRACE")){
    threshold <- match.arg(threshold)
    stopifnot(!serverData$isServer)
    client.registerDaemon(
        daemonName = daemonName, 
        logFile = logFile,
        threshold = threshold)
}

#' @export
deregisterDaemon <- function(
    daemonName = lastRegisteredDaemon(), 
    deleteTask = TRUE){
    stopifnot(!serverData$isServer)
    client.deregisterDaemon(daemonName = daemonName, 
                            deleteTask = deleteTask)
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
daemonExists <- function(daemonName = lastRegisteredDaemon()){
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
                          taskId = daemonTaskId(),
                          expr.char = NULL,
                          interval = 1){
    stopifnot(xor(missing(expr), missing(expr.char)))
    if(missing(expr.char)){
        expr <- substitute(expr)
    }else{
        expr.char <- gsub("\r", "", expr.char)
        expr <- parse(text = expr.char)
    }
    
    if(serverData$isServer){
        stopifnot(identical(serverData$daemonName, daemonName))
        server.setTask(expr = expr, taskId = taskId)
    }else{
        client.setTask(daemonName = daemonName, 
                       taskId = taskId,
                       expr = expr)
    }
    # daemonSetTaskInterval(interval = interval, daemonName = daemonName, taskId = taskId)
}

#' @export
daemonSetTaskScript <- function(script, 
                                daemonName = lastRegisteredDaemon(), 
                                taskId = daemonTaskId()){
    stopifnot(!serverData$isServer)
    script <- readChar(script, file.info(script)$size)
    daemonSetTask(daemonName = daemonName, 
                  taskId = taskId, 
                  expr.char = script)
}

#' @export
daemonSetTaskInterval <- function(interval, 
                                daemonName = lastRegisteredDaemon(), 
                                taskId = daemonTaskId()){
    expr <- paste0("rdaemon:::server.setTaskInterval(",taskId,",", interval,")")
    daemonEval(expr.char = expr, daemonName = daemonName, taskId = taskId)
    invisible()
}

#' @export
daemonGetTaskInterval <- function(interval, 
                                daemonName = lastRegisteredDaemon(), 
                                taskId = daemonTaskId()){
    expr <- paste0("rdaemon:::serverData$taskIntervals[['",taskId,"']]")
    daemonEval(expr.char = expr, daemonName = daemonName, taskId = taskId)
}

#' @export
daemonEval <- function(expr,
                       daemonName = lastRegisteredDaemon(), 
                       taskId = daemonTaskId(),
                       expr.char = NULL){
     stopifnot(xor(missing(expr), missing(expr.char)))
    if(missing(expr.char)){
        expr <- substitute(expr)
    }else{
        expr.char <- gsub("\r", "", expr.char)
        expr <- parse(text = expr.char)
    }
    
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
daemonExport <- function(..., 
                         daemonName = lastRegisteredDaemon(), 
                         taskId = daemonTaskId()){
    objects <- list(...)
    stopifnot(length(names(objects))>0)
    stopifnot(all(nzchar(names(objects))))
    
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
daemonLogs <- function(daemonName = lastRegisteredDaemon()){
    logPath <- daemonEval(daemonName = daemonName, 
                          expr = rdaemon:::serverData$logFile)
    if(is.null(logPath))
        return(logPath)
    
    stopifnot(file.exists(logPath))
    readLines(logPath)
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

