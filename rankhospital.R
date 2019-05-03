# Ranking Hospital by Outcome in a State

rankhospital <- function(state, outcome, num = "best") {
      
      ## Read outcome data
      
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Check that state and outcome are valid
      
      if (!(state %in% data$State)) {
            result <- "Error: Invalid State"
      }
      else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
            result <- "Error: Invalid Outcome"
      }
      else {
            keys <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
            outcomeKey <- keys[outcome]
            
            
            ## Return hospital name in that state with the given rank
            ## 30-day death rate
            
            dataPerState <- split(data, data$State)
            dataOurState <- dataPerState[[state]]
            dataOutcome <- suppressWarnings(as.numeric(dataOurState[, outcomeKey]))
            good <- complete.cases(dataOutcome)
            dataOutcome <- dataOutcome[good]
            dataOurState <- dataOurState[good,]
            dataOurState <- dataOurState[order(dataOutcome, dataOurState["Hospital.Name"]),]
            if (grepl("^[0-9]+$", num)) {
                  if (as.numeric(num) > length(dataOutcome)) {
                        result <- NA
                  }
                  else {
                        result <- dataOurState[as.numeric(num), "Hospital.Name"]
                  }
            }    
            else if (num == "best") {
                  result <- dataOurState[1, "Hospital.Name"]
            }
            else if (num == "worst") {
                  result <- dataOurState[length(dataOutcome), "Hospital.Name"]
            }
            else result <- NA
      }
      result
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("AG", "heart attack", 2)
rankhospital("AL", "heart burn", 2)
rankhospital("AG", "heart burn", 2)