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
    mutate(ticket_created = ifelse(ticket_created > call_start_time, NA, ticket_created))
    

# Step 2: Make the checkin time column more useful by converting to AMS --------
df3 <- df2 %>%
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

    # I don't want the day of week to be ordered
    df5$call_start_day <- factor(df5$call_start_day, ordered = FALSE)
# Step 5: Split the dataset -------------------------------------------------------

train <- sample_frac(df5, .60)
test <- sample_frac(df5[!df5$call_id %in% train$call_id,], .75)
validate <- df5[!df5$call_id %in% train$call_id & 
                  !df5$call_id %in% test$call_id, ]
# I don't want to use the call_id for predictions
train <- train %>% select(-call_id)
test <- test %>% select(-call_id)
validate <- validate %>% select(-call_id)

# Step 6: Logistic Regression -----------------------------------------------------

model_glm <- glm(formula = core_topic ~ ., family = 'binomial', data = train)


predict_glm <- predict(model_glm, type = 'response', newdata = test)

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

glm_predictions <- ifelse(predict_glm > .6, 1, 0)
rf_predicitons <- as.numeric(predict_rf == 1)
actual_results <- as.numeric(test$core_topic == 1)

results <- cbind(actual_results, glm_predictions, rf_predicitons)

data.frame(results) %>% group_by(actual_results) %>% summarise(n = n(),
                                                glm_predictions = sum(glm_predictions),
                                                rf_predicitons = sum(rf_predicitons))
