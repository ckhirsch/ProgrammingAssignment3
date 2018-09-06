## Function "best" takes two arguments, name of a "state" and one of three "outcome",
## returns name of hospital with lowest mortality of "outcome" in "state";
## For invalid state, throw error via stop with message "invalid state"
## For invalid outcome, throw error via stop with message "invalid outcome"

best <- function(state, outcome) {
        #read outcome data
        outcome_df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
                               stringsAsFactors = FALSE)
        rel_data <- outcome_df[c(2,7,11,17,23)]
        names(rel_data) <- c("Hospital","State","heart attack","heart failure","pneumonia")
        if (!is.element(state, rel_data$State)) {
                stop("invalid state")
        }
        if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        subset_data <- subset(rel_data, !is.na(rel_data[ ,outcome]))
        ranked_data <- subset_data[order(subset_data[outcome],subset_data$Hospital),]
        state_data <- ranked_data[ranked_data$State == state, ]
        #Return hospital name in that state with lowest 30-day death rate
        state_data[1,1]
        
}