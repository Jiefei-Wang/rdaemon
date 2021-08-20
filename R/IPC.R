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
    as.logical(getGlobalVariable(daemonConnectionName(name)))
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