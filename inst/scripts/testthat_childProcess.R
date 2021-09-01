doDetach <- Sys.getenv("testthat_do_detach") == "true"
file <- Sys.getenv("testthat_pid_file")
message("child:", file)
parent <- readLines(file)
writeLines(c(parent, as.character(Sys.getpid())), con = file)

if(doDetach){
    rdaemon:::detachConsole()
}

Sys.sleep(30)