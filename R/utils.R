

readTxt <- function(file){
    readChar(file, file.info(file)$size)
}

.difftime <- function(t1, t2){
    difftime(t1, t2, units = "secs")
}

.warning <- function(prefix){
    function(e){
        flog.warn(paste0(prefix, ": %s"), conditionMessage(e))
        tryInvokeRestart("muffleWarning")
    }
}

.error <- function(prefix){
    function(e){
        flog.error(paste0(prefix, ": %s"), conditionMessage(e))
    }
}

.suspendInterruptsIfRequired <- function(expr, interruptable){
    if(interruptable){
        expr
    }else{
        suspendInterrupts(expr)
    }
}

handleExceptions <- function(expr, warningPrefix, errorPrefix){
    tryCatch(
        {
            withCallingHandlers(
                expr,
                warning = .warning(warningPrefix)
            )
        },
        error = .error(errorPrefix)
    )
}

isScalerChar <- function(x){
    length(x) == 1 && is.character(x)
}

checkDaemonArgs <- function(daemonName = NULL, taskId = NULL){
    if(!is.null(daemonName)){
        stopifnot(isScalerChar(daemonName))
    }
    if(!is.null(taskId)){
        stopifnot(isScalerChar(taskId))
    }
}


getOS <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
