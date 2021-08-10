name <- Sys.getenv("rdaemon_name")
logPath <- Sys.getenv("rdaemon_logPath")

stopifnot(nzchar(name))
library(rdaemon)
runDaemon(name = name, interruptable = FALSE, detach = TRUE, logPath = logPath)
