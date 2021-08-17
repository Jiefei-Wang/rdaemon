logFile <- tempfile()

containPattern <- function(pattern, waitTime = 4){
    startTime <- Sys.time()
    while(difftime(Sys.time(), startTime, units = "secs")< 4){
        logs <- readLines(logFile)
        result <- grepl(pattern, logs)
        if(any(result)){
            return(TRUE)
        }
    }
    FALSE
}


test_that("Run the daemon in the backgroud",{
    daemonName <- "myTestDaemon"
    registerDaemon(daemonName = daemonName, 
                   logFile = logFile, 
                   threshold = "DEBUG")
    Sys.sleep(1)
    expect_true(daemonExists(daemonName = daemonName))
    expect_true(file.exists(logFile))
    expect_true(containPattern("Daemon PID"))
    expect_equal(daemonName, lastRegisteredDaemon())
})


test_that("daemon export and set task",{
    daemonExport(i = 10)
    daemonSetTask(message("The value of i is ", i))
    expect_true(containPattern("The value of i is 10"))
})

test_that("daemon eval",{
    daemonEval({i <- 11})
    expect_true(containPattern("The value of i is 11"))
})


test_that("daemon copy",{
    daemonCopyTask(sourceId = daemonTaskId(), targetId = 100)
    daemonEval({i <- 12})
    daemonEval({i <- 13}, taskId = 100)
    expect_true(containPattern("The value of i is 12"))
    expect_true(containPattern("The value of i is 13"))
})

test_that("daemon task script",{
    script <- tempfile()
    cat("message('test script')", file = script)
    daemonSetTaskScript(script)
    expect_true(containPattern("test script"))
})

test_that("daemon get task",{
    response <- daemonGetTask()
    attributes(response) <- NULL
    expect_equal(response, expression(message('test script')))
})

test_that("daemon deregister",{
    deregisterDaemon()
    expect_true(containPattern("Delete the task"))
    expect_true(containPattern("The connection to pid"))
    expect_false(.registered(lastRegisteredDaemon()))
})

test_that("daemon one-time request",{
    response <- daemonEval(i, taskId = 100)
    expect_equal(response, 13)
    expect_false(.registered(lastRegisteredDaemon()))
})

test_that("daemon kill",{
    killDaemon()
    expect_false(daemonExists(daemonName = "myTestDaemon"))
})

