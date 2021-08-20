timerData <- new.env(parent = emptyenv())

getTimerName <- function(class, name){
    paste0(class, name)
}

## The first execution always return TRUE 
isTimeOut <- function(class, name, timeout){
    timerName <- getTimerName(class, name)
    lastTime <- timerData[[timerName]]
    if(is.null(lastTime)||
       .difftime(Sys.time(), lastTime) > timeout){
        resetTimer(class, name)
        TRUE
    }else{
        FALSE
    }
}

resetTimer <- function(class, name){
    timerName <- getTimerName(class, name)
    timerData[[timerName]] <- Sys.time()
}