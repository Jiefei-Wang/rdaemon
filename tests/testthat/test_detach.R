test_that("kill parent with non-detached child",{
    skip_if_not(Sys.info()[['sysname']] != "Windows")
   tmpfile <- tempfile() 
   
   rscript <- R.home("bin/Rscript")
   parentScript <- system.file(package="rdaemon", "scripts", "testthat_parentProcess.R")
   childScript <- system.file(package="rdaemon", "scripts", "testthat_childProcess.R")
   
   Sys.setenv(testthat_pid_file = tmpfile)
   Sys.setenv(testthat_do_detach = "false")
   Sys.setenv(testthat_child_script = childScript)
   
   system2(rscript, shQuote(parentScript), stdout = FALSE, wait = FALSE)
   Sys.sleep(1)
   pids <- as.integer(readLines(tmpfile))
   expect_true(isProcessAlive(pids[1]))
   expect_true(isProcessAlive(pids[2]))
   tools::pskill(pids[1], tools::SIGTERM)
   Sys.sleep(1)
   expect_false(isProcessAlive(pids[1]))
   expect_false(isProcessAlive(pids[2]))
}
)

test_that("kill parent with detached child",{
   skip_if_not(Sys.info()[['sysname']] != "Windows")
   tmpfile <- tempfile()
   Sys.setenv(testthat_pid_file = tmpfile)
   Sys.setenv(testthat_do_detach = "true")
   
   rscript <- R.home("bin/Rscript")
   script <- system.file(package="rdaemon", "scripts", "testthat_parentProcess.R")
   system2(rscript, shQuote(script), stdout = FALSE, wait = FALSE)
   Sys.sleep(2)
   pids <- as.integer(readLines(file))
   expect_false(isProcessAlive(pids[1]))
   expect_false(isProcessAlive(pids[2]))
}
)