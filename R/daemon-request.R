request.setTask <- function(taskId, expr){
    list(
        taskId = taskId,
        type = "setTask",
        data = expr
    )
}

request.getTask <- function(taskId){
    list(
        taskId = taskId,
        type = "getTask"
    )
}

request.export <- function(taskId, objects){
    stopifnot(all(nzchar(objects)))
    list(
        taskId = taskId,
        type = "export",
        data = objects
    )
}

request.removeTask <- function(taskId){
    list(
        taskId = taskId,
        type = "removeTask"
    )
}

request.copyTask <- function(sourcePid, targetPid){
    list(
        taskId = targetPid,
        type = "copyTask",
        data = sourcePid
    )
}

request.handshake <- function(taskId){
    list(
        pid = Sys.getpid(),
        type = "handshake",
        taskId = taskId
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

isRemoveTaskRequest <- function(msg){
    msg$type == "removeTask"
}

isCopyTask <- function(msg){
    msg$type == "copyTask"
}

isHandshake <- function(msg){
    msg$type == "handshake"
}
