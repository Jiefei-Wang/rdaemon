daemonName <- Sys.getenv("rdaemon_daemonName")
logFile <- Sys.getenv("rdaemon_logFile")
threshold <- Sys.getenv("rdaemon_threshold")

stopifnot(nzchar(daemonName))
library(rdaemon)
rdaemon:::runDaemon(
    daemonName = daemonName, 
    interruptable = FALSE, 
    detach = TRUE, 
    logFile = logFile,
    threshold = threshold
)
