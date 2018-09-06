##The "rankall" function takes two arguments, a medical "outcome" and a rank "num",
##returns a hospital for each state that matches the given rank; if no such hospital exists
##returns NA.  Error for invalid outcome.

rankall <- function(outcome, num = "best") {
        outcome_df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
                               stringsAsFactors = FALSE)
        rel_data <- outcome_df[c(2,7,11,17,23)]
        names(rel_data) <- c("Hospital","State","heart attack","heart failure","pneumonia")
        if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        subset_data <- subset(rel_data, !is.na(rel_data[ ,outcome]))
        ranked_data <- subset_data[order(subset_data[outcome],subset_data$Hospital),]
        split_data <- split(ranked_data, ranked_data$State)
        select_data <- lapply(split_data, rank_select, num)
        df <- data.frame(stringsAsFactors = FALSE)
        for (i in 1:length(select_data)) {
                df <- rbind(df, data.frame(hospital=select_data[[i]][[1]], 
                                           state=names(select_data[i])))
        }
        df
}

##Helper function that actually selects the ranked hospitals by state; passed to lapply

rank_select <- function(collection, num) {
        if (is.numeric(num)) {
                data.frame(hospital=collection[num,1], state=collection[num,2], stringsAsFactors = FALSE)
        } else if (num == "best") {
                data.frame(hospital=collection[1,1], state=collection[1,2], stringsAsFactors = FALSE)
        } else if (num == "worst") {
                data.frame(hospital=collection[length(collection$State), 1], 
                           state=collection[length(collection$State), 2], stringsAsFactors = FALSE)
        } else {
                NA
        }
}