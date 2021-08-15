## TODO: use the temp file that is created by the deamon process
## Feature:
## 1. allow running (named) daemon, one daemon can handle multiple workers
## 2. daemon is only able to run the expression and set the data

clientData <- new.env(parent = emptyenv())
## element's name is the daemon name + task id
clientData$daemonConnections <- list()
clientData$daemonPorts <- list()
clientData$daemonPids <- list()
clientData$daemonNames <- list()
clientData$lastRegisteredDaemon <- NULL

getClientName <- function(name, taskId){
    paste0(name, taskId)
}


daemonRegistrationValid <- function(name, taskId){
    clientName <- getClientName(name, taskId)
    daemonName <- clientData$daemonNames[[clientName]]
    if(is.null(daemonName)){
        return(FALSE)
    }
    daemonPort <- getDaemonPort(daemonName)
    daemonPid <- getDaemonPid(daemonName)
    if(clientData$daemonPorts[[clientName]] == daemonPort &&
       clientData$daemonPids[[clientName]] == daemonPid &&
       isProcessAlive(daemonPid)){
        TRUE
    }else{
        FALSE
    }
}

## It is safe to call this function many times
## with the same name
loadDaemon <- function(name, taskId){
    ## Remove the incorrect daemon record
    if(!daemonRegistrationValid(taskId)){
        deregisterDaemon(name, taskId = taskId)
    }
    daemonPort <- getDaemonPort(name)
    daemonPid <- getDaemonPid(name)
    
    clientName <- getClientName(name, taskId)
    if(existsDaemon(name)){
        if(is.null(clientData$daemonConnections[[clientName]])){
            con <- socketConnection(port = daemonPort, open = "r+")
            handShake <- request.handshake(taskId)
            writeData(con, handShake)
            clientData$daemonConnections[[clientName]] <- con
            clientData$daemonPorts[[clientName]] <- daemonPort
            clientData$daemonPids[[clientName]] <- daemonPid
            clientData$daemonNames[[clientName]] <- name
        }
        TRUE
    }else{
        FALSE
    }
}

client.registerDaemon <- 
    function(name = lastRegisteredDaemon(), taskId = daemonTaskId(), logFile = NULL){
    if(!existsDaemon(name)){
        rscript <- R.home("bin/Rscript")
        script <- system.file(package="rdaemon", "scripts", "startDaemon.R")
        ## TODO: unset the environment after use
        Sys.setenv(rdaemon_name = name)
        if(!is.null(logFile))
            Sys.setenv(rdaemon_logFile = logFile)
        system2(rscript, shQuote(script), stdout = FALSE, wait = FALSE)
    }
    Sys.sleep(1)
    while(!loadDaemon(name)){
        
    }
    clientData$lastRegisteredDaemon <- name
}

client.deregisterDaemon <- 
    function(name = lastRegisteredDaemon(), taskId = daemonTaskId()){
    clientName <- getClientName(name, taskId)
    con <- clientData$daemonConnections[[clientName]]
    if(!is.null(con)){
        writeData(con, request.removeTask(taskId))
        close(clientData$daemonConnections[[clientName]])
        clientData$daemonConnections[[clientName]] <- NULL
        clientData$daemonPorts[[clientName]] <- NULL
        clientData$daemonPids[[clientName]] <- NULL
        clientData$daemonNames[[clientName]] <- NULL
    }
}

client.killDaemon <- function(name){
    if(client.existsDaemon(name)){
        pid <- getDaemonPid(name)
        tools::pskill(pid, tools::SIGTERM)
    }
}

client.existsDaemon <- function(name = lastRegisteredDaemon()){
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

client.daemonSetTask <- 
    function(name = lastRegisteredDaemon(), expr = NULL, taskId = daemonTaskId()){
    clientName <- getClientName(name, taskId)
    con <- clientData$daemonConnections[[clientName]]
    stopifnot(!is.null(con))
    
    task <- request.setTask(taskId = taskId, expr = substitute(expr))
    writeData(con, task)
}

client.daemonGetTask <- 
    function(name = lastRegisteredDaemon(), taskId = daemonTaskId()){
    clientName <- getClientName(name, taskId)
    con <- clientData$daemonConnections[[clientName]]
    stopifnot(!is.null(con))
    
    task <- request.getTask(taskId = taskId)
    flushData(con)
    writeData(con, task)
    waitData(con)
}


client.daemonExport <- 
    function(name = lastRegisteredDaemon(), objects, taskId = daemonTaskId()){
    clientName <- getClientName(name, taskId)
    con <- clientData$daemonConnections[[clientName]]
    stopifnot(!is.null(con))
    
    x <- request.export(taskId = taskId, objects = objects)
    writeData(con, x)
}

client.daemonCopyTask <- 
    function(name = lastRegisteredDaemon(), sourceId, targetId = daemonTaskId()){
    clientName1 <- getClientName(name, sourceId)
    clientName2 <- getClientName(name, targetId)
    con1 <- clientData$daemonConnections[[clientName1]]
    con2 <- clientData$daemonConnections[[clientName2]]
    stopifnot(!is.null(con1)||!is.null(con2))
    
    if(is.null(con1)){
        con <- con2
    }else{
        con <- con1
    }
    
    task <- request.copyTask(sourceId, targetId)
    writeData(con, task)
}
