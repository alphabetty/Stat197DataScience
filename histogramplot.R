# Plot the 30-day mortality rates for Heart Attack

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
hist(outcome[, 11], breaks = 50)
hist(outcome[, 11], breaks = 100)