name <- Sys.getenv("rdaemon_name")
logFile <- Sys.getenv("rdaemon_logFile")

stopifnot(nzchar(name))
library(rdaemon)
rdaemon:::runDaemon(
    name = name, 
    interruptable = FALSE, 
    detach = TRUE, 
    logFile = logFile
)
