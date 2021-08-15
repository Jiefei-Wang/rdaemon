runDaemon <- function(name, interruptable = TRUE, detach = FALSE, logFile = NULL){
    ## log system
    if(!is.null(logFile)&&nzchar(logFile)){
        con <- file(logFile, open = "wt", blocking = FALSE)
        sink(con, append=TRUE)
        sink(con, append=TRUE, type="message")
        message("Daemon PID: ", Sys.getpid())
        on.exit({
            sink() 
            sink(type="message")
            close(con)
        })
    }
    
    ## Try to start the daemon server
    serverData$port <- findPort()
    ## Run and check if this daemon gets the permission to continue
    serverData$serverConn <- serverSocket(serverData$port)
    setDaemonPort(name, serverData$port)
    setDaemonPid(name, Sys.getpid())
    Sys.sleep(1)
    if(getDaemonPort(name) != serverData$port){
        close(serverData$serverConn)
        serverData$serverConn <- NULL
        return()
    }
    serverData$name <- name
    serverData$isServer <- TRUE
    on.exit(quitDaemon(), add = TRUE)
    
    if(detach){
        detachConsole()
    }
    
    
    ## The daemon loop
    repeat{
        tryCatch(
            {
                ## Accept new connections
                acceptConnections()
                
                ## run the existing task
                runTasks()
                
                ## process incoming request
                processRequest()
                
                ## Check if the daemon is timeout
                timeout <- checkTimeout()
                if(timeout){
                    message("No task is running, quit daemon")
                    break
                }
                message("Client Number:", length(serverData$connections))
            },
            error = function(e) 
                message("Unclassified error: ", e$message),
            warning = function(e) 
                message("Unclassified warning: ", e$message),
            interrupt = function(e) if(interruptable) stop("interrupt")
        )
    }
}

quitDaemon <- function(){
    close(serverData$serverConn)
    serverData$serverConn <- NULL
    setDaemonPort(name, NA_integer_)
    setDaemonPid(name, NA_integer_)
    serverData$isServer <- FALSE
}

acceptConnections <- function(){
    success <- TRUE
    while(success){
        success <- tryCatch({
            con <- 
                suppressWarnings(
                    socketAccept(serverData$serverConn, open = "r+", timeout = 1)
                )
            msg <- waitData(con, timeout = 10)
            if(!isHandshake(msg)){
                stop("Handshake failed, cannot establish the new connection!")
            }
            pid <- as.character(msg$pid)
            taskId <- as.character(msg$taskId)
            oldCon <- serverData$connections[[taskId]]
            if(!is.null(oldCon)){
                close(oldCon)
            }
            serverData$connections[[taskId]] <- con
            serverData$task[[taskId]] <- NULL
            serverData$taskData[[taskId]] <- new.env(parent = .GlobalEnv)
            serverData$clientPid[[taskId]] <- pid
            TRUE
        },
        error = function(e) FALSE)
    }
}

runTasks <- function(){
    pids <- names(serverData$tasks)
    for(pid in pids){
        serverData$taskPid <- pid
        tryCatch(
            {
                eval(
                    expr = serverData$tasks[[pid]], 
                    envir  = serverData$taskData[[pid]])
            },
            error = function(e) 
                message("Error in evaluating the task for the pid ",
                        pid, ": ", e$message)
        )
    }
}


processRequest <- function(){
    pids <- names(serverData$connections)
    for(pid in pids){
        con <- serverData$connections[[as.character(pid)]]
        requests <- readData(con)
        for(request in requests){
            tryCatch(
                processIndividualRequest(pid, request),
                error = function(e) 
                    message("Error in processing the request from the pid ",
                            pid, ": ", e$message)
            )
        }
    }
}

processIndividualRequest <- function(requestPid, request){
    targetPid <- as.character(request$pid)
    data <- request$data
    if(isSetTaskRequest(request)){
        server.daemonSetTask(expr = data, pid = targetPid)
        return()
    }
    
    if(isGetTaskRequest(request)){
        con <- serverData$connections[[requestPid]]
        if(is.null(con)){
            stop("Cannot send the task back, the connection is closed")
        }
        writeData(con, server.daemonGetTask(pid = targetPid))
        return()
    }
    
    if(isExportRequest(request)){
        server.daemonExport(objects = data, pid= targetPid)
        return()
    }
    
    if(isRemoveTaskRequest(request)){
        server.deregisterDaemon(targetPid)
        return(TRUE)
    }
    
    if(isCopyTask(request)){
        sourcePid <- data
        server.daemonCopyTask(sourcePid = sourcePid, targetPid = targetPid)
        return()
    }
    
    stop("Unknown task type: ", request$type)
}


checkTimeout <- function(){
    if(length(serverData$tasks)==0){
        if(is.null(serverData$startTime)){
            serverData$startTime <- Sys.time()
        }
        timeDiff <- difftime(Sys.time(), serverData$startTime, units = "secs")
        if(timeDiff > serverData$timeout){
            return(TRUE)
        }
    }else{
        serverData$startTime <- NULL
    }
    FALSE
}
