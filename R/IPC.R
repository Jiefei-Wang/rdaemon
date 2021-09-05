truncateLongName <- function(name){
    if(nchar(name) > getNameMaxLen()){
            name <- substr(name, 0, getNameMaxLen() - 5)
            warning("The daemon name exceeds the name length limit ", 
                    "and will be truncated to '", name, "'")
    }
    name
}

daemonPortName <- function(name){
    paste0(name, "_port")
}

daemonPidName <- function(name){
    paste0(name, "_pid")
}

daemonConnectionName <- function(name){
    paste0(name, "_con")
}

getDaemonPort <- function(name){
    getGlobalVariable(daemonPortName(name))
}

getDaemonPid <- function(name){
    getGlobalVariable(daemonPidName(name))
}

getDaemonConnection <- function(name){
    con <- getGlobalVariable(daemonConnectionName(name))
    if(is.na(con))
        FALSE
    else
        as.logical(con)
}

setDaemonPort <- function(name, port){
    setGlobalVariable(daemonPortName(name), port)
}

setDaemonPid <- function(name, pid){
    setGlobalVariable(daemonPidName(name), pid)
}

setDaemonConnection <- function(name, hasConnection){
    setGlobalVariable(daemonConnectionName(name), as.integer(hasConnection))
}