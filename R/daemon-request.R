request.setTask <- function(expr, pid){
    list(
        pid = pid,
        type = "setTask",
        data = expr
    )
}

request.getTask <- function(pid){
    list(
        pid = pid,
        type = "getTask"
    )
}

request.export <- function(x, pid){
    stopifnot(all(nzchar(x)))
    list(
        pid = pid,
        type = "export",
        data = x
    )
}

request.removeClient <- function(pid){
    list(
        pid = pid,
        type = "removeClient"
    )
}

request.copyTask <- function(sourcePid, destPid){
    list(
        pid = sourcePid,
        type = "copyTask",
        data = destPid
    )
}


isSetTaskRequest <- function(msg){
    msg$type == "setTask"
}

isGetTaskRequest <- function(msg){
    msg$type == "getTask"
}

isExportRequest <- function(msg){
    msg$type == "export"
}

isRemoveClientRequest <- function(msg){
    msg$type == "removeClient"
}

isCopyTask <- function(msg){
    msg$type == "copyTask"
}
