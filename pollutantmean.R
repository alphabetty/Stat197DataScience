pollutantmean <- function(directory, pollutant, id = 1:332) {
      data <- numeric()
      for (i in id) {
            
            ReadFile <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                                     ".csv", sep = ""))
            
            data <- c(data, ReadFile[[pollutant]])
      }
      return(mean(data, na.rm = TRUE))
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)