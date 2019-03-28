complete <- function(directory, id = 1:332) {
      nobs <- numeric()
      for (i in id) {
            
            ReadFile <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                                     ".csv", sep = ""))
            nobs <- c(nobs, sum(complete.cases(ReadFile)))
      }
      return(data.frame(id, nobs))
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)