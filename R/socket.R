## Borrowed from both parallel:::initDefaultClusterOptions and 
## https://rdrr.io/github/quarto-dev/quarto-r/src/R/daemon.R
findPort <- function() {
  for (i in 1:20) {
    # determine the port (exclude those considered unsafe by Chrome)
    while(TRUE) {
      port <- 3000 + sample(5000, 1)
      if (!port %in% c(3659, 4045, 6000, 6665:6669,6697))
        break
    }
    # see if it's active
    if (!portActive(port)) {
      return(port)
    }
  }
  NULL
}

portActive <- function(port) {
  tryCatch({
    suppressWarnings(con <- socketConnection("127.0.0.1", port, timeout = 1))
    close(con)
    TRUE
  }, error = function(e) FALSE)
}


writeData <- function(con, data){
    writeLines(
        base64enc::base64encode(serialize(data, NULL)), 
        con)
}

readData <- function(con, n = -1){
    data <- readLines(con, n = n)
    if(length(data)){
        lapply(data, function(x)unserialize(base64enc::base64decode(x)))
    }else{
        NULL
    }
}