## Programming Assignment 3
## Author:  William Lewis
## Date:  31 March 2018
# --------------------------

library(dplyr)

# The rankall.R function returns a dataframe with 2 columns, the name of the hospital 
# and is state for a specified outcome and a rank for the 30-day mortality rate. 
# The function takes 2 arguments, the specified outcome and the desired rank 

rankall <- function(outcome, num = "best") {
      
      my_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = F)
      
      outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      if(!outcome %in% names(outcomes)) stop("invalid outcome")
      
      df <- my_data[, c(2, 7, outcomes[outcome])]
      df <- na.omit(df)
      names(df) <- c("Hospital", "State", "Outcome")
      df <- df %>% arrange(State, Outcome, Hospital) 
      
      #split the dataframe into separate lists by state
      my_list <- split(df, df$State)
      
      if (num == "best") {
            col_index <- 1
      } else if (num == "worst") {
            #sort the dataframe in descending order and split by state so the worst 
            #ranked hospital has an index value of 1
            df <- df %>% arrange(desc(State), desc(Outcome), desc(Hospital))
            my_list <- split(df, df$State)
            col_index <- 1
      } else {
            col_index <- num
      }
      
      #sapply function uses an anonymous function that gets the hospital name by rank
      hosp_list <- sapply(my_list, function(x) x[[1]][col_index])
      #build a list of state names that coincide with the hospital names
      state_names <- names(hosp_list)
      
      #return a dataframe with the hospital name by state for the specified rank
      final_df <- data.frame(hospital = hosp_list, state = state_names, row.names = state_names)
}

# Experimental results:
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

#Quiz ---------------
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)