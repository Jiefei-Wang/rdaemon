## TODO: remove the closed connection
runDaemon <- function(daemonName, 
                      interruptable = TRUE, 
                      detach = FALSE, 
                      logFile = NULL,
                      threshold= c("INFO", "WARN", "ERROR", "DEBUG"),
                      debug = FALSE){
    threshold <- match.arg(threshold)
    futile.logger::flog.threshold(get(threshold))
    
    ## log system
    if(!is.null(logFile)&&nzchar(logFile)){
        con <- file(logFile, open = "wt", blocking = FALSE)
        sink(con, append=TRUE)
        sink(con, append=TRUE, type="message")
        flog.info("Daemon PID: %d", Sys.getpid())
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
    stopifnot(!is.null(serverData$serverConn))
    
    setDaemonPort(daemonName, serverData$port)
    setDaemonPid(daemonName, Sys.getpid())
    Sys.sleep(1)
    if(getDaemonPort(daemonName) != serverData$port){
        close(serverData$serverConn)
        serverData$serverConn <- NULL
        return()
    }
    serverData$daemonName <- daemonName
    serverData$isServer <- TRUE
    if(!debug)
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
                flog.debug("Client Number: %d", length(serverData$connections))
            },
            error = function(e) 
                flog.error("Unclassified error: %s", e$message),
            warning = function(e) 
                flog.warn("Unclassified warning: %s", e$message),
            interrupt = function(e) if(interruptable) stop("interrupt", call. = FALSE)
        )
    }
}

quitDaemon <- function(){
    close(serverData$serverConn)
    serverData$serverConn <- NULL
    daemonName <- serverData$daemonName
    setDaemonPort(daemonName, NA_integer_)
    setDaemonPid(daemonName, NA_integer_)
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
            request <- waitData(con, timeout = 10)
            pid <- as.character(request$pid)
            if(isOneTimeConnection(request)){
                flog.debug("Receive an one-time request from pid %s", pid)
                request <- request$data
                processIndividualRequest(request = request, pid = pid, con = con)
                close(con)
                next
            }
            if(!isHandshake(request)){
                close(con)
                next
            }
            
            oldCon <- serverData$connections[[pid]]
            if(!is.null(oldCon)){
                close(oldCon)
            }
            serverData$connections[[pid]] <- con
            TRUE
        },
        error = function(e) {
            FALSE
        })
    }
}

runTasks <- function(){
    taskIds <- names(serverData$tasks)
    for(taskId in taskIds){
        serverData$currentTaskId <- taskId
        tryCatch(
            {
                eval(
                    expr = serverData$tasks[[taskId]], 
                    envir  = serverData$taskData[[taskId]])
            },
            error = function(e) 
                flog.error(
                    "Error in evaluating the task with the id %s: %s",
                    taskId, e$message
                ),
            warning = function(e)
                flog.warn(
                    "Warning in evaluating the task with the id %s: %s",
                    taskId, e$message
                )
        )
    }
}


processRequest <- function(){
    pids <- names(serverData$connections)
    for(pid in pids){
        con <- serverData$connections[[pid]]
        requests <- readData(con)
        for(request in requests){
            tryCatch(
                processIndividualRequest(request),
                error = function(e) 
                    flog.error(
                        "Error in processing the request from the pid %s: %s",
                        pid, e$message),
                warning = function(e)
                    flog.warn(
                        "Warning in processing the request from the pid %s: %s",
                        taskId, e$message
                    )
            )
        }
    }
}

processIndividualRequest <- function(request, pid = NULL, con = NULL){
    taskId <- as.character(request$taskId)
    data <- request$data
    if(is.null(pid))
        pid <- request$pid
    pid <- as.character(pid)
    if(is.null(con) && length(pid)!=0)
        con <- serverData$connections[[pid]]
    
    if(isSetTaskRequest(request)){
        server.setTask(expr = data, taskId = taskId)
        return()
    }
    
    if(isEval(request)){
        if(is.null(con)){
            stop("The connection to the pid `",pid,"` does not exist")
        }
        result <- server.eval(expr = data, taskId = taskId)
        server.response(con, result)
        return()
    }
    
    
    if(isGetTaskRequest(request)){
        if(is.null(con)){
            stop("The connection to the pid `",pid,"` does not exist")
        }
        
        result <- server.getTask(taskId = taskId)
        server.response(con, result)
        return()
    }
    
    if(isExportRequest(request)){
        server.export(taskId = taskId, objects = data)
        return()
    }
    
    if(isDeleteTaskRequest(request)){
        server.deleteTask(taskId)
        return(TRUE)
    }
    
    if(isCopyTask(request)){
        sourceId <- data
        server.copyTask(sourceId = sourceId, targetId = taskId)
        return()
    }
    
    if(isClose(request)){
        if(!is.null(con)){
            close(con)
        }
        serverData$connections[[pid]] <- NULL
        for(taskId in as.character(data)){
            serverData$tasks[[taskId]] <- NULL
            serverData$taskData[[taskId]] <- NULL
            flog.debug("Delete the task %s", taskId)
        }
        flog.info("The connection to pid %s has been closed", pid)
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
