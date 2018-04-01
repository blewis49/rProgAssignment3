## Programming Assignment 3
## Author:  William Lewis
## Date:  31 March 2018
# --------------------------

library(dplyr)

# This function uses the outcome-of-care-measures.csv file included in the repo on github.com
# The rankhospital.R function returns a character vector containing the hospital name from a specified
# state that has the rank specified in the num argument and a specified outcome.

rankhospital <- function(state, outcome, num = "best") {
      
      my_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = F)
      
      #a named numeric vector to index the columns for the 3 specified outcomes in the larger dataframe
      outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      #confirm the input state and outcome have a valid value
      if(!length(my_data$State[my_data$State == state] > 0)) stop("invalid state")
      if(!outcome %in% names(outcomes)) stop("invalid outcome")
      
      df <- my_data[, c(2, 7, outcomes[outcome])]
      df <- na.omit(df)
      names(df) <- c("Hospital", "State", "Outcome")
      #sort the dataframe by state, outcome and hospital and filter by the specified state argument
      df <- df %>% 
            arrange(State, Outcome, Hospital) %>% 
            filter(State == state)
      
      #select the hospital according to the rank specified
      if (num == "best") {
            col_index <- 1
      } else if (num == "worst") {
            col_index <- nrow(df)
      } else {
            col_index <- num
      }
      df$Hospital[col_index]
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
