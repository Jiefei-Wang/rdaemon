## Feature:
## 1. allow running (named) daemon, one daemon can handle multiple workers
## 2. daemon is only able to run the expression and set the data

clientData <- new.env(parent = emptyenv())
## element's name is the daemon name
clientData$daemonConnections <- list()
clientData$daemonPorts <- list()
clientData$daemonPids <- list()
clientData$daemonTasks <- list()
clientData$lastRegisteredDaemon <- paste0("DefaultDaemon_", Sys.getpid())
clientData$lastSetTaskId <- paste0("DefaultTask_", Sys.getpid())
clientData$waitTimeout <- 2*60

## accessors
.connection <- function(daemonName){
    clientData$daemonConnections[[daemonName]]
}

.port <- function(daemonName){
    clientData$daemonPorts[[daemonName]]
}

.pid <- function(daemonName){
    clientData$daemonPids[[daemonName]]
}

.taskIds <- function(daemonName){
    clientData$daemonTaskIds[[daemonName]]
}

.lastRegisteredDaemon <- function(){
    clientData$lastRegisteredDaemon
}

.lastSetTaskId <- function(){
    clientData$lastSetTaskId
}

.setConnection <- function(daemonName, con){
    clientData$daemonConnections[[daemonName]] <- con
}

.setPort <- function(daemonName, port){
    clientData$daemonPorts[[daemonName]] <- port
}

.setPid <- function(daemonName, pid){
    clientData$daemonPids[[daemonName]] <- pid
}

.setTaskIds <- function(daemonName, taskIds){
    clientData$daemonTaskIds[[daemonName]] <- taskIds
}

.addTaskId <- function(daemonName, taskId){
    if(is.null(clientData$daemonTaskIds[[daemonName]])){
        clientData$daemonTaskIds[[daemonName]] <- c()
    }
    clientData$daemonTaskIds[[daemonName]] <- unique(c(
        clientData$daemonTaskIds[[daemonName]], 
        taskId
    ))
}

.setLastRegisteredDaemon <- function(daemonName){
    clientData$lastRegisteredDaemon <- daemonName
}

.setLastSetTaskId <- function(taskId){
    clientData$lastSetTaskId <- taskId
}

## utils
.registered <- function(daemonName){
    !is.null(.connection(daemonName))
}

.disconnect <- function(daemonName){
    if(.registered(daemonName)){
        close(.connection(daemonName))
    }
    .setConnection(daemonName, NULL)
    .setPort(daemonName, NULL)
    .setPid(daemonName, NULL)
}

.writeToDaemon <- function(con, request, 
                           waitResponse = FALSE, 
                           timeout = clientData$waitTimeout){
    flushData(con)
    writeData(con, request)
    if(waitResponse)
        waitData(con, timeout = timeout)$data
    else
        NULL
}

.writeOneTimeRequest <- function(daemonName, 
                                 request, 
                                 waitResponse = FALSE, 
                                 timeout = clientData$waitTimeout){
    request <- request.oneTimeConnection(request)
    daemonPort <- getDaemonPort(daemonName)
    daemonPid <- getDaemonPid(daemonName)
    setDaemonConnection(daemonName, TRUE)
    con <- socketConnection(port = daemonPort, open = "r+")
    response <- .writeToDaemon(con = con, 
                               request = request, 
                               timeout = timeout,
                               waitResponse = waitResponse)
    close(con)
    return(response)
}

.sendRequest <- function(daemonName, request, 
                         waitResponse = FALSE, 
                         timeout = clientData$waitTimeout){
    if(!daemonExists(daemonName))
        stop("The daemon '",daemonName,"' does not exist!")
    
    if(.registered(daemonName)){
        con <- .connection(daemonName)
        .writeToDaemon(con, request, 
                       waitResponse = waitResponse,
                       timeout = timeout)
    }else{
        .writeOneTimeRequest(daemonName, request, 
                             waitResponse = waitResponse, 
                             timeout = timeout)
    }
}

daemonRegistrationValid <- function(daemonName){
    if(!.registered(daemonName)){
        return(FALSE)
    }
    daemonPort <- getDaemonPort(daemonName)
    daemonPid <- getDaemonPid(daemonName)
    if(.port(daemonName) == daemonPort &&
       .pid(daemonName) == daemonPid &&
       isProcessAlive(daemonPid)&&
       portOccupied(daemonPort)){
        TRUE
    }else{
        FALSE
    }
}

## It is safe to call this function many times
## with the same name
loadDaemon <- function(daemonName){
    ## Remove the incorrect daemon record
    if(!daemonRegistrationValid()){
        .disconnect(daemonName)
    }
    daemonPort <- getDaemonPort(daemonName)
    daemonPid <- getDaemonPid(daemonName)
    
    if(daemonExists(daemonName)){
        if(!.registered(daemonName)){
            setDaemonConnection(daemonName, TRUE)
            con <- socketConnection(port = daemonPort, open = "r+")
            handShake <- request.handshake()
            writeData(con, handShake)
            .setConnection(daemonName, con)
            .setPort(daemonName, daemonPort)
            .setPid(daemonName, daemonPid)
            .setLastRegisteredDaemon(daemonName)
        }
        TRUE
    }else{
        FALSE
    }
}


## The functions that will be executed in the client
client.registerDaemon <- 
    function(daemonName = lastRegisteredDaemon(),
             logFile = NULL, 
             threshold = c("INFO", "WARN", "ERROR", "DEBUG", "TRACE")){
        if(!daemonExists(daemonName)){
            rscript <- R.home("bin/Rscript")
            script <- system.file(package="rdaemon", "scripts", "startDaemon.R")
            ## TODO: unset the environment after use
            Sys.setenv(rdaemon_daemonName = daemonName)
            Sys.setenv(rdaemon_threshold = threshold)
            if(!is.null(logFile))
                Sys.setenv(rdaemon_logFile = logFile)
            
            system2(rscript, shQuote(script), stdout = FALSE, wait = FALSE)
        }
        Sys.sleep(1)
        while(!loadDaemon(daemonName)){
            
        }
        .setLastRegisteredDaemon(daemonName)
    }

client.deregisterDaemon <- 
    function(daemonName = lastRegisteredDaemon(), deleteTask = TRUE)
    {
        if(!daemonExists(daemonName))
            return(invisible())
        if(deleteTask){
            taskIds <- .taskIds(daemonName)
        }else{
            taskIds <- NULL
        }
        
        request <- request.close(taskIds = taskIds)
        .sendRequest(daemonName = daemonName, 
                     request = request,
                     waitResponse = FALSE)
        .disconnect(daemonName)
        if(deleteTask){
            .setTaskIds(daemonName, NULL)
        }
    }


client.killDaemon <- function(daemonName = lastRegisteredDaemon()){
    if(daemonExists(daemonName)){
        pid <- getDaemonPid(daemonName)
        tools::pskill(pid, tools::SIGTERM)
    }
}


client.setTask <- 
    function(daemonName = lastRegisteredDaemon(), 
             taskId = lastSetTaskId(), 
             expr = NULL,
             exports,
             interval){
        request <- request.setTask(taskId = taskId, 
                                   expr = expr,
                                   exports = exports,
                                   interval = interval)
        
        .sendRequest(daemonName = daemonName, 
                     request = request,
                     waitResponse = FALSE)
        .addTaskId(daemonName = daemonName,
                   taskId = taskId)
        .setLastSetTaskId(taskId)
        invisible()
    }


client.eval <- function(daemonName = lastRegisteredDaemon(), 
                        taskId = lastSetTaskId(), 
                        expr = NULL){
    request <- request.eval(taskId = taskId, expr = expr)
    
    .sendRequest(daemonName = daemonName, 
                             request = request,
                             waitResponse = TRUE)
}


client.getTask <- 
    function(daemonName = lastRegisteredDaemon(), 
             taskId = lastSetTaskId()){
        request <- request.getTask(taskId = taskId)
        .sendRequest(daemonName = daemonName, 
                     request = request,
                     waitResponse = TRUE)
    }


client.deleteTask <- 
    function(daemonName = lastRegisteredDaemon(), 
             taskId = lastSetTaskId()){
        request <- request.deleteTask(taskId = taskId)
        .sendRequest(daemonName = daemonName, 
                     request = request,
                     waitResponse = TRUE)
        invisible()
    }


client.export <- 
    function(daemonName = lastRegisteredDaemon(), 
             taskId = lastSetTaskId(),
             objects){
        request <- request.export(taskId = taskId, objects = objects)
        .sendRequest(daemonName = daemonName, 
                     request = request,
                     waitResponse = FALSE)
        invisible()
    }


client.copyTask <- 
    function(daemonName = lastRegisteredDaemon(), 
             sourceId, 
             targetId = lastSetTaskId()){
        request <- request.copyTask(sourceId, targetId)
        .sendRequest(daemonName = daemonName, 
                     request = request,
                     waitResponse = FALSE)
        invisible()
    }


