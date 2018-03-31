## Programming Assignment 3
## Author:  William Lewis
## Date:  31 March 2018
# --------------------------

# The best.R function returns the name of the hospital with the lowest 30-day mortality
# rate for a specified outcome in that state.  The function takes 2 arguments: the state 
# and a specified outcome.

best <- function(state, outcome) {
      
      my_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = F)
      
      #a named numeric vector to index the columns for the 3 specified outcomes in the larger dataframe
      outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      
      #confirm the input state is a valid value
      if(!length(my_data$State[my_data$State == state] > 0)) stop("invalid state")
      #confirm the outcome is a valid value
      if(!outcome %in% names(outcomes)) stop("invalid outcome")
      
      #subset the data by state and outcome without NA values
      df <- my_data[, c(2, 7, outcomes[outcome])]
      df <- na.omit(df)
      names(df) <- c("hospital", "state", "outcome")
      df <- df[df$state == state,]
      
      #return the hospital name with the lowest mortality outcome value 
      df$hospital[df$outcome == min(df$outcome)]
}

# Experimental Results
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")