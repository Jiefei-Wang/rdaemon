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


request.setTask <- function(expr){
    list(
        pid = Sys.getpid(),
        type = "setTask",
        data = expr
    )
}

request.getTask <- function(){
    list(
        pid = Sys.getpid(),
        type = "getTask"
    )
}

request.export <- function(expr){
    list(
        pid = Sys.getpid(),
        type = "export",
        data = expr
    )
}

request.removeClient <- function(){
    list(
        pid = Sys.getpid(),
        type = "removeClient"
    )
}