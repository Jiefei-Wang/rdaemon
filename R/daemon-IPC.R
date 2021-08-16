daemonPortName <- function(name){
    paste0(name, "_port")
}

daemonPidName <- function(name){
    paste0(name, "_pid")
}

getDaemonPort <- function(name){
    getGlobalVariable(daemonPortName(name))
}

getDaemonPid <- function(name){
    getGlobalVariable(daemonPidName(name))
}

setDaemonPort <- function(name, port){
    setGlobalVariable(daemonPortName(name), port)
}

setDaemonPid <- function(name, pid){
    setGlobalVariable(daemonPidName(name), pid)
}