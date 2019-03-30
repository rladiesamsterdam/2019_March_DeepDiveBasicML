tryCatch({
  library(tidyverse)
  library(lubridate)
  library(randomForest)
  library(ROCR)
  },
  error = function(e){
    print(e)
    stop('FileNotFoundException')
})

# Read in the Data --------------------------------------------------------
# change this to be appropriate for your use case
filepath <- '~/ml_data.RData'
df <- readRDS(file = filepath)

# Look at the Data --------------------------------------------------------

  # what does the data look like, what information do we have?  
  # What is the format of that information?


# Step 1: Get rid of things that we wouldn't have known ---------------------------

df2 <- df %>% 
    # If the reservation was cancelled after the call started, we won't know about it yet
    # Modify the reservation_cancelled column to represent what we would have known when the call happened
    mutate(reservation_cancelled = ifelse(reservation_cancelled > call_start_time, NA, reservation_cancelled)) %>%
    
    # If the ticket wasn't already created before the call started, then I wouldn't have known the created time
    # Modify the ticket_created column to represent what we would have known when the call happened
    mutate(ticket_created = ifelse(ticket_created > call_start_time, NA, ticket_created)) %>%
    
    # I cannot possibly know how many touches it took to resolve the issue until after the call is answered, 
    # so I need to remove this information!
    select(-touches_to_resolve)
    

# Step 2: Make the checkin time column more useful by converting to AMS --------
df3 <- df2 %>%
  # the as.POSIXct function expects timezone to be a string, and it won't accept an entire column
  # of strings UNLESS, I use the rowwise function
  rowwise() %>%
  mutate(checkin = as.POSIXct(checkin, tz = timezone_hotel)) %>%
  select(-timezone_hotel)

# Get rid of the rowwise structure of the table (rowwise tables are much slower to operate on, normally)
class(df3) <- c('tbl_df', 'data.frame')

  
# Step 3: Turn absolute times into relative times ---------------------------------
# Because all the timestamps are now in the same timezone, I can convert them to unixtimes -- 
#   I do this because they are faster/easier to work with
df4 <- df3 %>%
    mutate(ticket_created = as.numeric(as.POSIXct(ticket_created, tz = 'Europe/Amsterdam')),
           reservation_cancelled = as.numeric(as.POSIXct(reservation_cancelled, tz = 'Europe/Amsterdam')),
           reservation_created = as.numeric(as.POSIXct(reservation_created, tz = 'Europe/Amsterdam')),
           checkin = as.numeric(checkin, origin = '1970-01-01 00:00:00 CET'),
           call_start_time = as.numeric(as.POSIXct(call_start_time, tz = 'Europe/Amsterdam'))
    ) %>%
    # Relateive times for age of ticket, time since reservation was cancelled, age of reservation,
    #   distance to checkin
    mutate(age_of_ticket = call_start_time - ticket_created,
           time_since_reservation_was_cancelled = call_start_time - reservation_cancelled,
           age_of_reservation = call_start_time - reservation_created,
           distance_to_checkin = checkin - call_start_time) %>%
    # indicator of whether or not the reservation was cancelled when the call came in
    mutate(is_cancelled = as.numeric(!is.na(reservation_cancelled)))%>%
    mutate(core_ticket_already_open = as.numeric(core_topic == 1 & !is.na(age_of_ticket))) %>%
    select(-ticket_created, -reservation_cancelled, -reservation_created, -checkin) %>%
    mutate(call_start_time = as.POSIXct(call_start_time, origin = '1970-01-01 00:00:00 CET')) %>%
    mutate(call_start_day = wday(call_start_time, label = TRUE, abbr = TRUE)) %>%
    select(-call_start_time) %>% 
    select(-ends_with('complete'))
  
  
# Step 4: Make sure my factors (0/1s) are treated as factors ---------------------------

df5 <- df4 %>%  
  mutate(core_topic = factor(core_topic),
         cancel_start = factor(cancel_start),
         status_request_start = factor(status_request_start),
         core_ticket_already_open = factor(core_ticket_already_open),
         is_cancelled = factor(is_cancelled),
         call_source = factor(call_source)) %>%
  select(-time_since_reservation_was_cancelled, -age_of_ticket) 
    # I don't want the day of week to be an ordered factor, so I can make sure it won't be here:
    df5$call_start_day <- factor(df5$call_start_day, ordered = FALSE)
# Step 5: Split the dataset -------------------------------------------------------
# There are other ways to split a dataset, and other R functions designed for this.
#     Here I am just using sample frac, but you may prefer caTools::sample.split or one of the 
#     other functions out there.
train <- sample_frac(df5, .60)
test <- sample_frac(df5[!df5$call_id %in% train$call_id,], .75)
validate <- df5[!df5$call_id %in% train$call_id & 
                  !df5$call_id %in% test$call_id, ]
# I don't want to use the call_id for predictions, I am just going to take it out now!
train <- train %>% select(-call_id)
test <- test %>% select(-call_id)
validate <- validate %>% select(-call_id)

# Step 6: Logistic Regression -----------------------------------------------------

model_glm <- glm(formula = core_topic ~ ., family = 'binomial', data = train)


predict_glm <- predict(model_glm, type = 'response', newdata = test)


# We don't have time to talk about the ROCR package, but I suggest you check it out.  The code
#   I used to generate the ROC curve is given here.
# Prediction function
ROCRpred = prediction(predict_glm, test$core_topic)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(a = 0, b = 1)


# Step 7: Random Forest -----------------------------------------------------------

model_rf <- randomForest(formula = core_topic ~ .,
             data = train, trees = 1000)

predict_rf <- predict(model_rf, type = 'response', newdata = test)


# Step 8: compare predictions -----------------------------------------------------
# depnding on what threshold we use, the model will perform differently (look at the ROC curve to understand more)
#   play around with this threshold to see if you can find a better one for our business constraints!
glm_predictions <- ifelse(predict_glm > .6, 1, 0)  
#  If I want to change the threshold for the random forest model, I can make the following change to the 
#     argument of randomForest() in Step 7:
#       randomForest(formula = core_topic ~ ., data = train, trees = 1000, cutoff = c(0.70,0.30))
#       these values I give in the cutoff set the threshold for how certain I have to be.  
#       Can you think of adjustments you would make to help with our business constraints?
rf_predicitons <- as.numeric(predict_rf == 1)
# Here I just grab what the actual outcome was so I can compare my predicted to actual results
actual_results <- as.numeric(test$core_topic == 1)

# Stick everything together so I can look at how my models did
results <- cbind(actual_results, glm_predictions, rf_predicitons)

# The table here includes one row for the cases that were actually core topics (1) and that weren't (0)
#  The value n shows how many calls were in each group
#  The other 2 columns show how many calls each of the models would have classified as core topics
#  Depending on the settings you chose, you can see how well your model did. 
#  Keep in mind our business constraints!  We may want something different than you.
data.frame(results) %>% group_by(actual_results) %>% summarise(n = n(),
                                                glm_predictions = sum(glm_predictions),
                                                rf_predicitons = sum(rf_predicitons))

#  You want to see low numbers in the first row and higher numbers in the second.
#  If everything were perfect (which would be super suspicious) you'd see:
# A tibble: 2 x 4
# actual_results     n          glm_predictions   rf_predicitons
# <dbl>             <int>              <dbl>            <dbl>
#   0               7233                  0                0
#   1               7767               7767             7767

# In general, you may have constraints on your model that make you prefer false positives to missing 
#   true positives or vice versa.  Everything we did today was about working through a very straightforward
#   solution to my business case, so take this as an example, but not the rules for how to do things.  
#   I don't believe there is always one right way or one specific way, I am just showing you how I think 
#   about things in case it is helpful for you.  
