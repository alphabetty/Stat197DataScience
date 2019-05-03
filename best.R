# Finding the Best Hospital in a State

best <- function(state, outcome) {
      ## Read outcome data
      
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Check that state and outcome are valid
      
      if (!(state %in% data$State)) {
            result <- "Error: Invalid State"
      }
      else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
            result <- "Error: Invalid Outcome"
      }
      else{
            keys <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
            outcomeKey <- keys[outcome]
            
            ## Return hospital name in that state with lowest 30-day death rate
            
            dataPerState <- split(data, data$State)
            dataOurState <- dataPerState[[state]]
            dataOurState <- dataOurState[ order(dataOurState["Hospital.Name"]), ]
            dataOutcome <- suppressWarnings(as.numeric(dataOurState[, outcomeKey]))
            good <- complete.cases(dataOutcome)
            dataOutcome <- dataOutcome[good]
            dataOurState <- dataOurState[good,]
            minimum <- min(dataOutcome)
            index <- match(minimum, dataOutcome)
            result <- dataOurState[index, 2]
      }
      result
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("AG", "heart attack")
best("AL", "heart burn")
best("AG", "heart burn")