##Function "rankhospital" takes three arguments, a "state", a health "outcome", 
##and a ranking "num".  It returns the hospital in the specified state that has the 
##rank on the specified outcome.

rankhospital <- function(state, outcome, num = "best") { 
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
        if (is.numeric(num)) {
                state_data[num,1]
        } else if (num == "best") {
                state_data[1,1]
        } else if (num == "worst") {
                state_data[length(state_data$State), 1]
        } else {
                NA
        }
}