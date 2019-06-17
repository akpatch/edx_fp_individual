#######################################################################################################################
# Create edx set, validation set, and submission file##################################################################
#######################################################################################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")


library(readr)
library(knitr)
library(caret)
library(tidyverse)

##The data used for this project can be found in my github through the following link

# https://github.com/akpatch/edx_fp_individual.git


##If you plan to run the code, please be sure to update the path for the data as it is not linked directly

# Note I used code from a kaggle kernal when cleaning the data 
acled_af_asia<-read_csv("C:/Users/APATCH/OneDrive - UMUC/Data Science Course/African Conflict/asia_africa_conflicts.csv", col_types = cols(
  .default = col_character(),
  FATALITIES = col_integer(),
  GEO_PRECISION = col_integer(),
  GWNO = col_integer(),
  INTER1 = col_integer(),
  INTER2 = col_integer(),
  INTERACTION = col_integer(),
  LATITUDE = col_character(),
  LONGITUDE = col_character(),
  TIME_PRECISION = col_integer(),
  YEAR = col_integer()
))



acled_af_asia$LATITUDE[!grepl("^[0-9.]+$", acled_af_asia$LATITUDE)] <- NA
acled_af_asia$LONGITUDE[!grepl("^[0-9.]+$", acled_af_asia$LONGITUDE)] <- NA

acled_af_asia$LATITUDE = as.numeric(as.character(acled_af_asia$LATITUDE))
acled_af_asia$LONGITUDE = as.numeric(as.character(acled_af_asia$LONGITUDE))


acled_af_asia$EVENT_TYPE = trimws(acled_af_asia$EVENT_TYPE)
acled_af_asia$EVENT_TYPE = gsub("Strategic development","Strategic Development",acled_af_asia$EVENT_TYPE)
acled_af_asia$EVENT_TYPE = gsub("Violence against civilians","Violence Against Civilians",acled_af_asia$EVENT_TYPE)
acled_af_asia$EVENT_TYPE = gsub("Battle-no change of territory","Battle-No change of territory",acled_af_asia$EVENT_TYPE)
acled_af_asia$EVENT_TYPE = gsub("Remote violence","Remote Violence",acled_af_asia$EVENT_TYPE)
acled_af_asia$EVENT_TYPE = gsub("Strategic development","Strategic Development",acled_af_asia$EVENT_TYPE)
acled_af_asia$EVENT_TYPE = gsub("Violence against civilians","Violence Against Civilians",acled_af_asia$EVENT_TYPE)
acled_af_asia$EVENT_TYPE = gsub("Battle-no change of territory","Battle-No change of territory",acled_af_asia$EVENT_TYPE)
acled_af_asia$EVENT_TYPE = gsub("Remote violence","Remote Violence",acled_af_asia$EVENT_TYPE)


acled_af_asia2<-select(acled_af_asia, ACTOR1, ACTOR2, ADMIN1, ADMIN2, COUNTRY, EVENT_DATE, EVENT_TYPE, FATALITIES, LOCATION, YEAR)
acled_af_asia2<-na.omit(acled_af_asia2)


#Data Exploration

table(acled_af_asia2$EVENT_TYPE)

hist(acled_af_asia2$FATALITIES)

acled_af_asia2<- subset(acled_af_asia2, acled_af_asia2$FATALITIES<40)

hist(acled_af_asia2$FATALITIES)



set.seed(1)
test_index <- createDataPartition(y = acled_af_asia2$FATALITIES, times = 1, p = 0.1, list = FALSE)
train<- acled_af_asia2[-test_index,]
test <- acled_af_asia2[test_index,]







##Create a function to calculate Root Mean Squared Errors (RMSE)
RMSE <- function(true_fatalities, predicted_fatalities){
  sqrt(mean((true_fatalities - predicted_fatalities)^2))
}


##Calculate the average fatality rate
mu_hat <- mean(train$FATALITIES)
mu_hat

###calculate RMSE for the overall average
overall_average_rmse <- RMSE(test$FATALITIES, mu_hat)
overall_average_rmse


###Here I am creating a tibble to hold the results of the models to compare RMSE
rmse_results <- tibble(method = "Overall average", RMSE = overall_average_rmse)

#View the table
rmse_results %>% knitr::kable()


## b_hat_c is the average of the diffence between the fatality rate within a specific country minus the overall average fatality rate
mu_hat <- mean(train$FATALITIES) 
country_avgs <- train %>% 
  group_by(COUNTRY) %>% 
  summarize(b_hat_c = mean(FATALITIES - mu_hat))


##Here we visualize the distribution of b_hat_c
country_avgs %>% qplot(b_hat_c, geom ="histogram", bins = 10, data = ., color = I("black"))

##Now we use the validation data to check the RSME for the mode

#first we have to calculate b_hat_c for the validation set and calculate the predicted ratings for the test model
predicted_fatalities <- mu_hat + test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  .$b_hat_c

#next we test to see the RMSE 
model_country_rmse <- RMSE(predicted_fatalities, test$FATALITIES)

#finally we add this model to our RSME table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Country Effect Model",
                                     RMSE = model_country_rmse ))
#View the table
rmse_results %>% knitr::kable()



## b_hat_et is the average fatality observed for each type of event recorded in the data
train %>% 
  group_by(EVENT_TYPE) %>% 
  summarize(b_hat_et= mean(FATALITIES)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_hat_et)) + 
  geom_histogram(bins = 30, color = "black")

##Now we use the test data to check the RSME for the model
#first we have to create user averages for the test set so that we can make our predictions
event_avgs <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  group_by(EVENT_TYPE) %>%
  summarize(b_hat_et= mean(FATALITIES - mu_hat - b_hat_c))

## Then we have to calculate the predicted ratings for the test model
predicted_fatalities <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  left_join(event_avgs, by='EVENT_TYPE') %>%
  mutate(pred = mu_hat + b_hat_c + b_hat_et) %>%
  .$pred

#next we test to see the RMSE 
model_eventtype_rmse <- RMSE(predicted_fatalities, test$FATALITIES)

#finally we add this model to our RSME table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Country + Event Type Effects Model",  
                                     RMSE = model_eventtype_rmse ))

#View the table
rmse_results %>% knitr::kable()


## b_hat_l is the average factality rate for each specific location observed in the dataset
train %>% 
  group_by(LOCATION) %>% 
  summarize(b_hat_l = mean(FATALITIES)) %>% 
  ggplot(aes(b_hat_l)) + 
  geom_histogram(bins = 30, color = "black")

##Now we use the test data to check the RSME for the model
#first we have to create user averages for the test set so that we can make our predictions
location_avgs <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  left_join(event_avgs, by='EVENT_TYPE') %>%
  group_by(LOCATION) %>%
  summarize(b_hat_l = mean(FATALITIES - mu_hat - b_hat_c- b_hat_et))

## Then we have to calculate the predicted ratings for the test model
predicted_fatalities <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  left_join(event_avgs, by='EVENT_TYPE') %>%
  left_join(location_avgs, by='LOCATION') %>%
  mutate(pred = mu_hat + b_hat_c + b_hat_et+b_hat_l) %>%
  .$pred

#next we test to see the RMSE 
model_location_rmse <- RMSE(predicted_fatalities, test$FATALITIES)

#finally we add this model to our RSME table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Country + Event + Location Effects Model",  
                                     RMSE = model_location_rmse ))

#View the table
rmse_results %>% knitr::kable()



## b_hat_a is the average factality rate for each specific actor1 observed in the dataset
train %>% 
  group_by(ACTOR1) %>% 
  summarize(b_hat_a = mean(FATALITIES)) %>% 
  ggplot(aes(b_hat_a)) + 
  geom_histogram(bins = 30, color = "black")

##Now we use the test data to check the RSME for the model
#first we have to create user averages for the test set so that we can make our predictions
actor_avgs <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  left_join(event_avgs, by='EVENT_TYPE') %>%
  left_join(location_avgs, by='LOCATION') %>%
  group_by(ACTOR1) %>%
  summarize(b_hat_a = mean(FATALITIES - mu_hat - b_hat_c- b_hat_et - b_hat_l))

## Then we have to calculate the predicted ratings for the test model
predicted_fatalities <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  left_join(event_avgs, by='EVENT_TYPE') %>%
  left_join(location_avgs, by='LOCATION') %>%
  left_join(actor_avgs, by='ACTOR1') %>%
  mutate(pred = mu_hat + b_hat_c + b_hat_et+b_hat_l+ b_hat_a) %>%
  .$pred

#next we test to see the RMSE 
model_actor_rmse <- RMSE(predicted_fatalities, test$FATALITIES)

#finally we add this model to our RSME table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Country + Event + Location + Actor Effects Model",  
                                     RMSE = model_actor_rmse ))

#View the table
rmse_results %>% knitr::kable()


## b_hat_a2 is the average factality rate for each specific actor2 observed in the dataset
train %>% 
  group_by(ACTOR2) %>% 
  summarize(b_hat_a2 = mean(FATALITIES)) %>% 
  ggplot(aes(b_hat_a2)) + 
  geom_histogram(bins = 30, color = "black")

#first we have to create user averages for the test set so that we can make our predictions
actor2_avgs <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  left_join(event_avgs, by='EVENT_TYPE') %>%
  left_join(location_avgs, by='LOCATION') %>%
  left_join(actor_avgs, by='ACTOR1') %>%
  group_by(ACTOR2) %>%
  summarize(b_hat_a2 = mean(FATALITIES - mu_hat - b_hat_c- b_hat_et - b_hat_l-b_hat_a))

## Then we have to calculate the predicted ratings for the test model
predicted_fatalities <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  left_join(event_avgs, by='EVENT_TYPE') %>%
  left_join(location_avgs, by='LOCATION') %>%
  left_join(actor_avgs, by='ACTOR1') %>%
  left_join(actor2_avgs, by='ACTOR2') %>%
  mutate(pred = mu_hat + b_hat_c + b_hat_et+b_hat_l+ b_hat_a+ b_hat_a2) %>%
  .$pred

#next we test to see the RMSE 
model_actor2_rmse <- RMSE(predicted_fatalities, test$FATALITIES)

#finally we add this model to our RSME table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Country  + Event + Location + Actor Effects Model",  
                                     RMSE = model_actor2_rmse ))

#View the table
rmse_results %>% knitr::kable()





################################REGULARIZATION#####################################################################################
##In oder to ensure that states with different number of events are not causing interference in our model we can use regularization

##Here we use cross-validation to identify the lambda (regularization penalty term) that will be most useful in tuning our model

lambdas <- seq(0, 20, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train$FATALITIES)
  
  b_c <- train %>% 
    group_by(COUNTRY) %>%
    summarize(b_c = sum(FATALITIES - mu)/(n()+l))
  
  
  
  predicted_fatalities <- 
    test%>% 
    left_join(b_c, by = 'COUNTRY') %>%
    mutate(pred = mu + b_c) %>%
    pull(pred)
  
  return(RMSE(predicted_fatalities , test$FATALITIES))
})

##Next we plot the rmses across lambdas to help identify which will be the most useful for our model
qplot(lambdas, rmses)  


##Now we want to identify the optimal lambda for our model
lambda <- lambdas[which.min(rmses)]
lambda





## b_hat_c is the average of the diffence between the fatality rate within a specific country minus the overall average fatality rate
country_avgs <- train %>% 
  group_by(COUNTRY) %>% 
  summarize(b_hat_c = sum(FATALITIES - mu_hat)/(n()+lambda), n_i = n()) 


##Here we visualize the distribution of b_hat_c
country_avgs %>% qplot(b_hat_c, geom ="histogram", bins = 10, data = ., color = I("black"))

##Now we use the validation data to check the RSME for the mode

#first we have to calculate b_hat_c for the validation set and calculate the predicted ratings for the test model
predicted_fatalities <- mu_hat + test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  .$b_hat_c

#next we test to see the RMSE 
model_country_rmse <- RMSE(predicted_fatalities, test$FATALITIES)

#finally we add this model to our RSME table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Country Effect Model",
                                     RMSE = model_country_rmse ))
#View the table
rmse_results %>% knitr::kable()




## b_hat_l is the average factality rate for each specific location observed in the dataset
train %>% 
  group_by(LOCATION) %>% 
  summarize(b_hat_l = mean(FATALITIES)) %>% 
  ggplot(aes(b_hat_l)) + 
  geom_histogram(bins = 30, color = "black")

##Now we use the test data to check the RSME for the model
#first we have to create user averages for the test set so that we can make our predictions
location_avgs <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  group_by(LOCATION) %>%
  summarize(b_hat_l = mean(FATALITIES - mu_hat - b_hat_c))

## Then we have to calculate the predicted ratings for the test model
predicted_fatalities <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  left_join(location_avgs, by='LOCATION') %>%
  mutate(pred = mu_hat + b_hat_c +b_hat_l) %>%
  .$pred

#next we test to see the RMSE 
model_location_rmse <- RMSE(predicted_fatalities, test$FATALITIES)

#finally we add this model to our RSME table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Country + Location Effects Model",  
                                     RMSE = model_location_rmse ))

#View the table
rmse_results %>% knitr::kable()

## b_hat_l is the average factality rate for each specific location observed in the dataset
train %>% 
  group_by(ACTOR1) %>% 
  summarize(b_hat_a = mean(FATALITIES)) %>% 
  ggplot(aes(b_hat_a)) + 
  geom_histogram(bins = 30, color = "black")

##Now we use the test data to check the RSME for the model
#first we have to create user averages for the test set so that we can make our predictions
actor_avgs <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  left_join(location_avgs, by='LOCATION') %>%
  group_by(ACTOR1) %>%
  summarize(b_hat_a = mean(FATALITIES - mu_hat - b_hat_c- - b_hat_l))

## Then we have to calculate the predicted ratings for the test model
predicted_fatalities <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  left_join(location_avgs, by='LOCATION') %>%
  left_join(actor_avgs, by='ACTOR1') %>%
  mutate(pred = mu_hat + b_hat_c +b_hat_l+ b_hat_a) %>%
  .$pred

#next we test to see the RMSE 
model_actor_rmse <- RMSE(predicted_fatalities, test$FATALITIES)

#finally we add this model to our RSME table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Country  Location + Actor Effects Model",  
                                     RMSE = model_actor_rmse ))

#View the table
rmse_results %>% knitr::kable()

## b_hat_l is the average factality rate for each specific location observed in the dataset
train %>% 
  group_by(ACTOR2) %>% 
  summarize(b_hat_a2 = mean(FATALITIES)) %>% 
  ggplot(aes(b_hat_a2)) + 
  geom_histogram(bins = 30, color = "black")

##Now we use the test data to check the RSME for the model
#first we have to create user averages for the test set so that we can make our predictions
actor2_avgs <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  left_join(location_avgs, by='LOCATION') %>%
  left_join(actor_avgs, by='ACTOR1') %>%
  group_by(ACTOR2) %>%
  summarize(b_hat_a2 = mean(FATALITIES - mu_hat - b_hat_c- b_hat_l-b_hat_a))

## Then we have to calculate the predicted ratings for the test model
predicted_fatalities <- test %>% 
  left_join(country_avgs, by='COUNTRY') %>%
  left_join(location_avgs, by='LOCATION') %>%
  left_join(actor_avgs, by='ACTOR1') %>%
  left_join(actor2_avgs, by='ACTOR2') %>%
  mutate(pred = mu_hat + b_hat_c + b_hat_l+ b_hat_a+ b_hat_a2) %>%
  .$pred

#next we test to see the RMSE 
model_actor_rmse <- RMSE(predicted_fatalities, test$FATALITIES)

#finally we add this model to our RSME table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Country + Location + Actor 1 & 2 Effects Model",  
                                     RMSE = model_actor_rmse ))

#View the table
rmse_results %>% knitr::kable()





