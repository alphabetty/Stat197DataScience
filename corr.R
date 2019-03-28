corr <- function(directory, threshold = 0) {
      df <-  complete(directory) 
      ids <-  df[df["nobs"] > threshold, ]$id
      corr_new <- numeric()
      for (i in ids) {
            
            ReadFile <-  read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                                       ".csv", sep = ""))
            df_new <-  ReadFile[complete.cases(ReadFile), ]
            corr_new <-  c(corr_new, cor(df_new$sulfate, df_new$nitrate))
      }
      return(corr_new)
}

cr <- corr("specdata", 150)
head(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
cr <- corr("specdata")
summary(cr)
length(cr)