tryCatch({
  library(tidyverse)
  library(lubridate)
  library(randomForest)
  },
  error = function(e){
    print(e)
    stop('FileNotFoundException')
})

# Read in the Data --------------------------------------------------------
filepath <- '~/data.RData' # change the filepath to wherever you put the data
df <- readRDS(file = filepath)


# Look at the Data --------------------------------------------------------

  # what does the data look like, what information do we have?  
  # What is the format of that information?


# Step 1: Get rid of things that we wouldn't have known ---------------------------

df2 <- df  
    # If the reservation was cancelled after the call started, we won't know about it yet
    # Modify the reservation_cancelled column to represent what we would have known when the call happened
    
    
    # If the ticket wasn't already created before the call started, then I wouldn't have known the created time
    # Modify the ticket_created column to represent what we would have known when the call happened
    

# Step 2: Make the checkin time column more useful by converting to AMS --------
df3 <- df2
  # use timezone hotel
  

  # Get rid of the rowwise structure of the table (rowwise tables are much slower to operate on, normally)
  class(df3) <- c('tbl_df', 'data.frame')

  
# Step 3: Turn absolute times into relative times ---------------------------------
    # Because all the timestamps are now in the same timezone, we can convert them to unixtimes -- 

df4 <- df3 %>%
    # indicator of which day of week the call came in (use wday)
  
    # turn the timestamps into unixtime
    
  
    # Relative times for age of ticket, time since reservation was cancelled, age of reservation,
    #   distance to checkin

    # indicator of whether or not the reservation was cancelled when the call came in

    # indicator of whether there was already a core ticket open

  
# Step 4: Make sure my factors (0/1s) are treated as factors---------------------------

    df5 <- df4 %>% 
    
    # I don't want the day of week to be ordered
    df5$call_start_day <- factor(df5$call_start_day, ordered = FALSE)
# Step 5: Split the dataset -------------------------------------------------------


# Don't want to use the call_id for predictions


# Step 6: Logistic Regression -----------------------------------------------------

model_glm <- glm(formula = ,#add formula here#
                 family = 'binomial', data = # add your data frame
                   )

predict_glm <- predict(model_glm, type = 'response', newdata = # add your data frame
                         )

    # Prediction function
    ROCRpred = prediction(predict_glm, # add the actual result you were trying to predict
                          )
    # Performance function
    ROCRperf = performance(ROCRpred, "tpr", "fpr")
    # Plot ROC curve -- this is written for, you won't have to change anything
    plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
    abline(a = 0, b = 1)


# Step 7: Random Forest -----------------------------------------------------------

model_rf <- randomForest(formula = ,# add your formula
             data = , # add your data
             trees = 1000)

predict_rf <- predict(model_rf, type = 'response', newdata = #add your data
                        )


# Step 8: compare predictions -----------------------------------------------------

glm_predictions <- ifelse(predict_glm1 > .6, 1, 0)
rf_predicitons <- as.numeric(predict_rf0 == 1)
actual_results <- as.numeric(df6$core_topic == 1)

results <- cbind(actual_results, glm_predictions, rf_predicitons)

data.frame(results) %>% group_by(actual_results) %>% summarise(n = n(),
                                                glm_predictions = sum(glm_predictions),
                                                rf_predicitons = sum(rf_predicitons))
