##########################################################
#  R Ladies Workshop -- Machine Learning 28 March 2019   #
##########################################################
library(tidyverse)
library(bkrdata)
library(sparklyr)
library(caret)

# remove later & replace with a csv call to sanitized table----------------------------------
sc <- connect_to('spark')
my_tbl <- tbl(sc, 'kpisane.calls_and_ticketing_outcomes_v1') %>%
    filter(called_line == 'CS_en') %>%
    mutate(tmp_called_line_type = substr(called_line, 1, 2)) %>%
    filter(tmp_called_line_type == 'CS') %>%
    select(-starts_with('tmp')) %>%
    filter(!is.na(reservation_id)) %>%
    mutate(call_start_time = from_unixtime(acs_call_start_epoch)) %>%
    select(-reservation_id, -called_line, -starts_with('offered'), -answered,
         -ticket_id, -ends_with('epoch')) %>%
    mutate(core_topic = ifelse(final_topic_parent_display_name %in% 
                                              c('Review',
                                                'Information',
                                                'Cancellation',
                                                'Refund',
                                                'Modification',
                                                'Special Request',
                                                'Confirmation',
                                                'Booker Details',
                                                'Feedback'), 1, 0))

mt <- my_tbl %>% select(acs_callid, call_source, final_topic_name, final_topic_parent_display_name,
                        ticket_created, cancel_start, cancel_complete, modify_reservation_start,
                        modify_reservation_complete, waive_fee_start, waive_fee_complete,
                        status_request_start, status_request_complete, self_service, 
                        checkin, reservation_cancelled, reservation_created, policy, core_topic, call_start_time,
                        touches_to_resolve, timezone_hotel) %>%
    collect_monitoring()


mx <- sample_n(mt, size = 100000)
# ppl %>% mutate(simp = case_when(
# workday_job_key_id == 'CS0563' ~ 'CSG Exec',
# workday_job_key_id == 'CS0564' ~ 'CSP Exec',

mt$acs_callid <-  seq_along(mt$acs_callid)
mt <- mt %>% rename(mt, call_id = acs_callid) %>%
    select(-final_topic_parent_display_name, -final_topic_name,
           -waive_fee_complete, -waive_fee_start, -self_service,
           -policy)


mt_0 <- mt %>% filter(core_topic == 0)
mt_1 <- mt %>% filter(core_topic == 1)

mt_0$tester <- rnorm(mean = 0, sd = 1, n = length(mt_0$call_id))
mt_0 <- mt_0 %>% mutate(call_source = ifelse(tester <= 0, 'hotel', call_source))  %>%
    select(-tester)

mt_1 <- mt_1 %>% sample_n(size = 90000)
mt_alt <- rbind(mt_0, mt_1)
saveRDS(mt_alt, file = '~/ml_tmp.RData', compress = TRUE)





# titanic.survival.train = glm(survived ~ pclass + sex + pclass:sex + age + sibsp,
#                              family = binomial(logit), data = titanic.train)
#binomial.train <- glm(core_topic ~ , family = binomial(logit), data = mt)

mt2 <- mt %>%
  mutate(reservation_is_cancelled = reservation_cancelled < call_start_time) %>%
  mutate(reservation_cancelled = as.POSIXct(reservation_cancelled),
         reservation_created = as.POSIXct(reservation_created),
         call_start_time = as.POSIXct(call_start_time),
         ticket_created = as.POSIXct(ticket_created),
         checkin = as.POSIXct(checkin))

mt3 <- mt2 %>% select(-final_topic_name, -final_topic_parent_display_name) %>%
    mutate(ticket_already_open = ticket_created < call_start_time) %>%
    select(-ticket_created) %>%
    mutate(distance_to_checkin = 14*3600 + as.numeric(duration(interval(call_start_time, checkin)))) %>%
    mutate(distance_from_reservation_created = as.numeric(duration(interval(call_start_time, reservation_created))))

train <- sample_n(mt3, size = 50000)
model_glm <- glm(data = train, core_topic ~ call_source +
                   distance_to_checkin, family = binomial(logit))

# logistic regression -----------------------------------------------------



lr0 <- glm(core_topic ~ call_source + reservation_is_cancelled +
            distance_to_checkin + distance_from_reservation_created +
            cancel_start + cancel_start + #modify_reservation_start +
            status_request_start, 
           data = train, family = "binomial")
predict0 <- predict(lr0, type = 'response', newdata = train)

summary(predict0) # Check that the predictions are centered around 0.5 since I made the training data balanced
# z <- table(train$worked_by_test_agent, predict0 > 0.5)
# (z[1,1]+z[2,2])/length(train$worked_by_test_agent)
##################################################################################
#               ROCR curve for the model                                         #
##################################################################################
#     From discussion with Akarsh, see notes on lecture 'Modeling the Expert'
ROCRpred = prediction(predict0, train$core_topic)
as.numeric(performance(ROCRpred, "auc")@y.values)
# Prediction function
ROCRpred = prediction(predict0, train$core_topic)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(a = 0, b = 1)


# balance data set --------------------------------------------------------

x <- sample_n(mt3[mt3$core_topic == 0,], size = 5000)
y <- sample_n(mt3[mt3$core_topic == 1,], size = 5000)
train_balance <- rbind(x,y)

lr_bal <- glm(core_topic ~ call_source + reservation_is_cancelled +
             distance_to_checkin + distance_from_reservation_created +
             cancel_start + cancel_start + #modify_reservation_start +
             status_request_start, 
           data = train_balance, family = "binomial")
predict_bal <- predict(lr_bal, type = 'response', newdata = train_balance)

summary(predict_bal) # Check that the predictions are centered around 0.5 since I made the training data balanced
# z <- table(train$worked_by_test_agent, predict0 > 0.5)
# (z[1,1]+z[2,2])/length(train$worked_by_test_agent)
##################################################################################
#               ROCR curve for the model                                         #
##################################################################################
#     From discussion with Akarsh, see notes on lecture 'Modeling the Expert'
ROCRpred = prediction(predict_bal, train_balance$core_topic)
as.numeric(performance(ROCRpred, "auc")@y.values)
# Prediction function
ROCRpred = prediction(predict_bal, train_balance$core_topic)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(a = 0, b = 1)

predict_bal



randomForest(data = train, core_topic ~ call_source + reservation_is_cancelled +
               distance_to_checkin + distance_from_reservation_created +
               cancel_start + cancel_start + #modify_reservation_start +
               status_request_start)

# clean data --------------------------------------------------------------

mt4 <- mt3 %>% select(acs_callid, core_topic, call_source, reservation_is_cancelled, distance_to_checkin,
                      distance_from_reservation_created, cancel_start, status_request_start, 
                      modify_reservation_start) %>%
    mutate(reservation_is_cancelled = ifelse(is.na(reservation_is_cancelled), FALSE, reservation_is_cancelled))


# training/test sets ------------------------------------------------------
train <- sample_frac(mt4, .60)
test <- sample_frac(mt4[!mt4$acs_callid %in% train$acs_callid,], .75)
validate <- mt4[!mt4$acs_callid %in% train$acs_callid & !mt4$acs_callid %in% test$acs_callid, ]


# logistic regression -----------------------------------------------------

l0 <- glm(core_topic ~., data = train, family = binomial(logit))
rf0 <- randomForest(core_topic ~., data = train)

