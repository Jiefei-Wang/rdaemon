serverData <- new.env(parent = emptyenv())
serverData$serverConn <- NULL
serverData$port <- NULL
## element's name is pid
serverData$connections <- list()
serverData$tasks <- list()
serverData$taskData <- list()


runDaemon <- function(name, interruptable = TRUE, detach = FALSE, logPath = NULL){
    if(!is.null(logPath)&&nzchar(logPath)){
        con <- file(logPath, open = "wt", blocking = FALSE)
        sink(con, append=TRUE)
        sink(con, append=TRUE, type="message")
        on.exit({
            sink() 
            sink(type="message")
            close(con)
        })
    }
    
    if(is.null(serverData$serverConn)){
        serverData$port <- findPort()
        
        ## Run and check if this daemon gets the permission to continue
        serverData$serverConn <- serverSocket(serverData$port)
        setDaemonPort(name, serverData$port)
        Sys.sleep(1)
        if(getDaemonPort(name) != serverData$port){
            close(serverData$serverConn)
            serverData$serverConn <- NULL
            return()
        }
        setDaemonPid(name, Sys.getpid())
    }
    if(detach){
        detachConsole()
    }
    
    repeat{
        tryCatch(
            {
                ## Accept new connections
                acceptConnections()
                
                ## run the existing task
                runTasks()
                
                ## process incoming request
                processRequest()
                
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


acceptConnections <- function(){
    success <- TRUE
    while(success){
        success <- tryCatch({
            con <- 
                suppressWarnings(
                    socketAccept(serverData$serverConn, open = "r+", timeout = 1)
                )
            pid <- waitData(con, timeout = 10)
            if(is.null(pid)){
                stop("Handshake failed, cannot establish the new connection!")
            }
            pid <- as.character(pid)
            oldCon <- serverData$connections[[pid]]
            if(!is.null(oldCon)){
                close(oldCon)
            }
            serverData$connections[[pid]] <- con
            serverData$task[[pid]] <- NULL
            serverData$taskData[[pid]] <- new.env(parent = .GlobalEnv)
            TRUE
        },
        error = function(e) FALSE)
    }
}

runTasks <- function(){
    pids <- names(serverData$tasks)
    for(pid in pids){
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
        serverData$tasks[[targetPid]] <- data
        return()
    }
    if(isGetTaskRequest(request)){
        con <- serverData$connections[[requestPid]]
        if(is.null(con)){
            stop("Cannot send the task back, the connection is closed")
        }
        writeData(con, serverData$tasks[[targetPid]])
        return()
    }
    if(isExportRequest(request)){
        for(i in names(data)){
            serverData$taskData[[targetPid]][[i]] <- data[[i]]
        }
        return()
    }
    if(isRemoveClientRequest(request)){
        if(!is.null(serverData$connections[[targetPid]]))
            close(serverData$connections[[targetPid]])
        serverData$connections[[targetPid]] <- NULL
        serverData$taskData[[targetPid]] <- NULL
        serverData$tasks[[targetPid]] <- NULL
        return(TRUE)
    }
    
    if(isCopyTask(request)){
        destPid <- as.character(data)
        serverData$taskData[[destPid]] <- serverData$taskData[[targetPid]]
        serverData$tasks[[destPid]] <- serverData$tasks[[targetPid]]
        return()
    }
    
    stop("Unknown task type: ", request$type)
}
