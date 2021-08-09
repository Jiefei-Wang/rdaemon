serverData <- new.env(parent = emptyenv())
serverData$serverConn <- NULL
serverData$port <- NULL
## element's name is pid
serverData$connections <- list()
serverData$tasks <- list()
serverData$taskData <- list()


runDaemon <- function(name){
    serverData$port <- findPort()
    
    ## Run and check if this daemon gets the permission to continue
    setDaemonPort(name, serverData$port)
    serverData$serverConn <- serverSocket(serverData$port)
    Sys.sleep(1)
    if(getDaemonPort(name) != serverData$port){
        return()
    }
    setDaemonPid(name, Sys.getpid())
    
    repeat{
        tryCatch(
            {
                ## Accept new connections
                acceptConnections()
                
                ## run the existing task
                runTasks()
                
                ## process incoming request
                processRequest()
                
                message("Waiting for the next command")
            }
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
            startTime <- Sys.time()
            pid <- NULL
            while(difftime(Sys.time(), startTime) < 10){
                pid <- readData(con, n = 1)[[1]]
                if(!is.null(pid))
                    break
            }
            if(is.null(pid)){
                stop("Handshake failed, cannot establish the new connection!")
            }
            serverData$connections[[pid]] <- c(serverData$connections, con)
            serverData$taskData[[pid]] <- new.env(parent = .GlobalEnv)
            TRUE
        },
        error = function(e) FALSE)
    }
}

runTasks <- function(){
    pids <- names(serverData$tasks)
    for(i in pids){
        tryCatch(
            {
                eval(
                    expr = serverData$tasks[[i]], 
                    envir  = serverData$taskData[[i]])
            },
            error = function(e) 
                message("Error in evaluating the task for the pid ",
                        i, ": ", e$message)
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
                processIndividualRequest(pid, request),
                error = function(e) 
                    message("Error in processing the request for the pid ",
                            pid, ": ", e$message)
            )
        }
    }
}

processIndividualRequest <- function(pid, request){
    if(is.null(serverData$connections[[pid]]))
        return()
    
    data <- request$data
    if(isSetTaskRequest(request)){
        serverData$tasks[[pid]] <- data
        return()
    }
    if(isGetTaskRequest(request)){
        writeData(con, serverData$tasks[[pid]])
        return()
    }
    if(isExportRequest(request)){
        for(i in names(data)){
            serverData$taskData[[pid]][[i]] <- data[[i]]
        }
        return()
    }
    if(isRemoveClientRequest(request)){
        close(serverData$connections[[pid]])
        serverData$connections[[pid]] <- NULL
        serverData$taskData[[pid]] <- NULL
        serverData$tasks[[pid]] <- NULL
        return(TRUE)
    }
}
