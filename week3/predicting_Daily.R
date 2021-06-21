library(tidyverse)
library(scales)
library(modelr)
library(lubridate)
theme_set(theme_bw())
options(repr.plot.width=4, repr.plot.height=3)
trips_per_day <- read_tsv('trips_per_day.tsv')


#Split the data into randomly selected training, validation, and test sets, with 80% of the data 
#for training the model, 10% for validation, and 10% for a final test set 
#(to be used once and only once towards the end of this exercise). You can adapt the code from 
#last week's complexity control notebook to do this. You can use a single validation fold, don't 
#worry about k-fold cross-validation here.

# split data 80-10-10
num_days <- nrow(trips_per_day)
num_train <- floor(num_days * 0.8)
# randomly sample rows for the training set
training_data <- sample(1:num_days, num_train, replace=F)
# used to fit the model
trips_per_day_train <- trips_per_day[training_data, ]
# used to evaluate the fit and test
trips_per_day_valid_and_test <- trips_per_day[-training_data, ]
num_days_test_and_validate <-nrow(trips_per_day_valid_and_test)
num_validate <- floor(num_days_test_and_validate * 0.5)
# randomly sample rows for the validation set
validation <- sample(1:num_days_test_and_validate, num_validate, replace=F) 
# used to validate the model
trips_per_day_validate <- trips_per_day_valid_and_test[validation, ]
# used to test the model
trips_per_day_test <- trips_per_day_valid_and_test[-validation, ]
# r model for each polynomial degree with min temp
K <- 1:8
train_err <- c()
validate_err <- c()
for (k in K) {
  
  # fit on the training data
  model_tmin <- lm(num_trips ~ poly(tmin, k, raw=T), data=trips_per_day_train)
  
  # evaluate on the training data
  train_err[k] <- sqrt(mean((predict(model_tmin, trips_per_day_train) - trips_per_day_train$num_trips)^2))
  
  # evaluate on the validate data
  validate_err[k] <- sqrt(mean((predict(model_tmin, trips_per_day_validate) - trips_per_day_validate$num_trips)^2))
}
train_err
validate_err
# plot the training and validation error as a function of polynomial degree
plot_data <- data.frame(K, train_err, validate_err) %>%
  gather("split", "error", -K)
ggplot(plot_data, aes(x=K, y=error, color=split)) +
  geom_line() +
  scale_x_continuous(breaks=K) +
  xlab('Polynomial Degree') +
  ylab('RMSE')


# we see that a fifth degree polynomial is a reasonable choice
# model for degree 5 polynomial
model_tmin5 <- lm(num_trips ~ poly(tmin, 5, raw=T), data=trips_per_day_train)
# train model
trips_per_day_train <- trips_per_day_train %>%
  add_predictions(model_tmin5) %>%
  mutate(split="train")
# validate model
trips_per_day_validate <- trips_per_day_validate %>%
  add_predictions(model_tmin5) %>%
  mutate(split="validate")
# plotting
plot_data <- bind_rows(trips_per_day_train, trips_per_day_validate)
ggplot(plot_data, aes(x=tmin, y=num_trips)) +
  geom_point(aes(color=split)) +
  geom_line(aes(y=pred)) +
  xlab("Minimum temperature") +
  ylab('Daily trips') +
  scale_y_continuous()


  
# Model with precipitation
# plotting and finding the mean, median, and standard deviation of the precipitation
trips_per_day %>%
  summarize(prcp_mean = mean(prcp), prcp_median = median(prcp), prcp_sd = sd(prcp))
trips_per_day %>%
  ggplot(aes(x = prcp, y = num_trips)) +
  geom_point() +
  geom_vline(xintercept = mean(trips_per_day$prcp))
trips_per_day_with_rain <- trips_per_day %>% 
  mutate(noisy = prcp > mean(prcp))

# split data with rain 80-10-10
num_days <- nrow(trips_per_day_with_rain)
num_train <- floor(num_days * 0.8)
# randomly sample rows for the training set
training_data <- sample(1:num_days, num_train, replace=F)
# used to fit the model
trips_per_day_rain_train <- trips_per_day_with_rain[training_data, ]
# used to evaluate the fit and test
trips_per_day_rain_valid_and_test <- trips_per_day_with_rain[-training_data, ]
num_days_test_and_validate <-nrow(trips_per_day_rain_valid_and_test)
num_validate <- floor(num_days_test_and_validate * 0.5)
# randomly sample rows for the validation set
validation <- sample(1:num_days_test_and_validate, num_validate, replace=F) 
# used to validate the model
trips_per_day_rain_valid <- trips_per_day_rain_valid_and_test[validation, ]
# used to test the model
trips_per_day_with_rain_test <- trips_per_day_rain_valid_and_test[-validation, ]
# model for each polynomial degree with min temp and rain
K <- 1:8
train_err <- c()
validate_err <- c()
for (k in K) {
  
  # fit on the training data
  model_prcp <- lm(num_trips ~ poly(tmin, k, raw=T) + noisy, data=trips_per_day_rain_train)
  
  # evaluate on the training data
  train_err[k] <- sqrt(mean((predict(model_prcp, trips_per_day_rain_train) - trips_per_day_rain_train$num_trips)^2))
  
  # evaluate on the validate data
  validate_err[k] <- sqrt(mean((predict(model_prcp, trips_per_day_rain_valid) - trips_per_day_rain_valid$num_trips)^2))
}
train_err
validate_err
# plot the training and validation error as a function of polynomial degree
plot_data <- data.frame(K, train_err, validate_err) %>%
  gather("split", "error", -K)
ggplot(plot_data, aes(x=K, y=error, color=split)) +
  geom_line() +
  scale_x_continuous(breaks=K) +
  xlab('Polynomial Degree') +
  ylab('RMSE')


# model for degree 5 polynomial
model_prcp5 <- lm(num_trips ~ poly(tmin, 5, raw=T) + noisy, data=trips_per_day_rain_train)
# train model
trips_per_day_rain_train <- trips_per_day_rain_train %>%
  add_predictions(model_prcp5) %>%
  mutate(split="train")
# validate model
trips_per_day_rain_valid <- trips_per_day_rain_valid %>%
  add_predictions(model_prcp5) %>%
  mutate(split="validate")
# plot the training data and validation data with the model
plot_data <- bind_rows(trips_per_day_rain_train, trips_per_day_rain_valid)
ggplot(plot_data, aes(x=ymd, y=num_trips, color=noisy)) +
  geom_point() +
  geom_line(aes(y=pred)) +
  xlab("YMD") +
  ylab('Daily trips') +
  scale_y_continuous()
# actual vs. predicted
ggplot(plot_data, aes(x=pred, y=num_trips, color=split)) +
  geom_point() +
  geom_abline() +
  scale_x_continuous("Predicted Number of Trips", label=comma) +
  scale_y_continuous("Actual Number of Trips", label=comma)

# test the model
test_err <- sqrt(mean((predict(model_prcp5, trips_per_day_with_rain_test) - trips_per_day_with_rain_test$num_trips)^2))
test_err

# model with weekend

# plot the number of trips as a function of whether or not a day is a weekend
trips_per_day %>%
  ggplot(aes(x = wday(ymd), y = num_trips)) +
  geom_point()
# add a column to the dataframe to indicate if a day is a weekend
trips_per_day_for_weekends <- trips_per_day %>% 
  mutate(aWeekend = wday(ymd) %in% c(1,7))

# split data with weekends 80-10-10
num_days <- nrow(trips_per_day_for_weekends)
num_train <- floor(num_days * 0.8)
# randomly sample rows for the training set
training_data <- sample(1:num_days, num_train, replace=F)
# used to fit the model
trips_per_day_for_weekends_train <- trips_per_day_for_weekends[training_data, ]
# used to evaluate the fit and test
trips_per_day_for_weekends_validate_and_test <- trips_per_day_for_weekends[-training_data, ]
num_days_test_and_validate <-nrow(trips_per_day_for_weekends_validate_and_test)

num_validate <- floor(num_days_test_and_validate *0.5)
# randomly sample rows for the validation set
validation <- sample(1:num_days_test_and_validate, num_validate, replace=F) 
# used to validate the model
trips_per_day_for_weekends_validate <- trips_per_day_for_weekends_validate_and_test[validation, ]
# used to test the model
trips_per_day_for_weekends_test <- trips_per_day_for_weekends_validate_and_test[-validation, ]
# model for each polynomial degree with min temp and weekends

K <- 1:8
train_err <- c()
validate_err <- c()
for (k in K) {
  
  # fit on the training data
  model_wknd <- lm(num_trips ~ poly(tmin, k, raw=T) + aWeekend, data=trips_per_day_for_weekends_train)
  
  # evaluate on the training data
  train_err[k] <- sqrt(mean((predict(model_wknd, trips_per_day_for_weekends_train) - trips_per_day_for_weekends_train$num_trips)^2))
  
  # evaluate on the validate data
  validate_err[k] <- sqrt(mean((predict(model_wknd, trips_per_day_for_weekends_validate) - trips_per_day_for_weekends_validate$num_trips)^2))
}
train_err
validate_err
# plot the training and validation error as a function of polynomial degree
plot_data <- data.frame(K, train_err, validate_err) %>%
  gather("split", "error", -K)
ggplot(plot_data, aes(x=K, y=error, color=split)) +
  geom_line() +
  scale_x_continuous(breaks=K) +
  xlab('Polynomial Degree') +
  ylab('RMSE')


# model for degree 5 polynomial
model_wknd5 <- lm(num_trips ~ poly(tmin, 5, raw=T) + aWeekend, data=trips_per_day_for_weekends_train)
# train model
trips_per_day_for_weekends_train <- trips_per_day_for_weekends_train %>%
  add_predictions(model_wknd5) %>%
  mutate(split="train")
# validate model
trips_per_day_for_weekends_validate <- trips_per_day_for_weekends_validate %>%
  add_predictions(model_wknd5) %>%
  mutate(split="validate")
# plot the training data and validation data with the model
plot_data <- bind_rows(trips_per_day_for_weekends_train, trips_per_day_for_weekends_validate)
ggplot(plot_data, aes(x=ymd, y=num_trips, color=aWeekend)) +
  geom_point() +
  geom_line(aes(y=pred)) +
  xlab("YMD") +
  ylab('Daily trips') +
  scale_y_continuous()
# actual vs. predicted
ggplot(plot_data, aes(x=pred, y=num_trips, color=split)) +
  geom_point() +
  geom_abline() +
  scale_x_continuous("Predicted Number of Trips", label=comma) +
  scale_y_continuous("Actual Number of Trips", label=comma)
# test the model
test_err <- sqrt(mean((predict(model_wknd5, trips_per_day_for_weekends_test) - trips_per_day_for_weekends_test$num_trips)^2))
test_err

# Max temp
# plot the number of trips as a function of the precipitation
trips_per_day %>%
  ggplot(aes(x = tmax, y = num_trips)) +
  geom_point()

# split data for max temp 80-10-10
num_days <- nrow(trips_per_day)

num_train <- floor(num_days *0.8)
# randomly sample rows for the training set
training_data <- sample(1:num_days, num_train, replace=F)
# used to fit the model
trips_per_day_train <- trips_per_day[training_data, ]
# used to evaluate the fit and test
trips_per_day_valid_and_test <- trips_per_day[-training_data, ]
num_days_test_and_validate <-nrow(trips_per_day_valid_and_test)

num_validate <- floor(num_days_test_and_validate * 0.5)
# randomly sample rows for the validation set
validation <- sample(1:num_days_test_and_validate, num_validate, replace=F) 
# used to validate the model
trips_per_day_validate <- trips_per_day_valid_and_test[validation, ]
# used to test the model
trips_per_day_test <- trips_per_day_valid_and_test[-validation, ]
# model for each polynomial degree with max temp
K <- 1:8
train_err <- c()
validate_err <- c()
for (k in K) {
  
  # fit on the training data
  model_tmax <- lm(num_trips ~ poly(tmax, k, raw=T), data=trips_per_day_train)
  
  # evaluate on the training data
  train_err[k] <- sqrt(mean((predict(model_tmax, trips_per_day_train) - trips_per_day_train$num_trips)^2))
  
  # evaluate on the validate data
  validate_err[k] <- sqrt(mean((predict(model_tmax, trips_per_day_validate) - trips_per_day_validate$num_trips)^2))
}
train_err
validate_err
# plot the training and validation error as a function of polynomial degree
plot_data <- data.frame(K, train_err, validate_err) %>%
  gather("split", "error", -K)
ggplot(plot_data, aes(x=K, y=error, color=split)) +
  geom_line() +
  scale_x_continuous(breaks=K) +
  xlab('Polynomial Degree') +
  ylab('RMSE')


# model for degree 5 polynomial
model_tmax5 <- lm(num_trips ~ poly(tmax, 5, raw=T), data=trips_per_day_train)
# train model
trips_per_day_train <- trips_per_day_train %>%
  add_predictions(model_tmax5) %>%
  mutate(split="train")
# validate model
trips_per_day_validate <- trips_per_day_validate %>%
  add_predictions(model_tmax5) %>%
  mutate(split="validate")
# plot the training data and validation data with the model
plot_data <- bind_rows(trips_per_day_train, trips_per_day_validate)
ggplot(plot_data, aes(x=tmax, y=num_trips)) +
  geom_point(aes(color=split)) +
  geom_line(aes(y=pred)) +
  xlab("Maximum temperature") +
  ylab('Daily trips') +
  scale_y_continuous()


ggplot(plot_data, aes(x=pred, y=num_trips, color=split)) +
  geom_point() +
  geom_abline() +
  scale_x_continuous("Predicted Number of Trips", label=comma) +
  scale_y_continuous("Actual Number of Trips", label=comma)
# test the model
test_err <- sqrt(mean((predict(model_tmax5, trips_per_day_test) - trips_per_day_test$num_trips)^2))
test_err


# min temp and precipitation and weekends and max temp

# create data frame with all necessary info 
trips_per_day_with_all_info <- trips_per_day %>% 
  mutate(noisy = prcp > mean(prcp)) %>% 
  mutate(aWeekend = wday(ymd) %in% c(1,7))
# View(trips_per_day_with_all_info)
# split data with all info 80-10-10}
num_days <- nrow(trips_per_day_with_all_info)

num_train <- floor(num_days * 0.8)
# randomly sample rows for the training set
training_data <- sample(1:num_days, num_train, replace=F)
# used to fit the model
trips_per_day_with_all_info_train <- trips_per_day_with_all_info[training_data, ]
# used to evaluate the fit and test
trips_per_day_with_all_info_validate_and_test <- trips_per_day_with_all_info[-training_data, ]
num_days_test_and_validate <-nrow(trips_per_day_with_all_info_validate_and_test)

num_validate <- floor(num_days_test_and_validate *0.5)
# randomly sample rows for the validation set
validation <- sample(1:num_days_test_and_validate, num_validate, replace=F) 
# used to validate the model
trips_per_day_with_all_info_validate <- trips_per_day_with_all_info_validate_and_test[validation, ]
# used to test the model
trips_per_day_with_all_info_test <- trips_per_day_with_all_info_validate_and_test[-validation, ]
# model for each polynomial degree with all info}
K <- 1:8
train_err <- c()
validate_err <- c()
for (k in K) {
  
  # fit on the training data
  model_all <- lm(num_trips ~ poly(tmin, k, raw=T) + noisy + aWeekend + poly(tmax, k, raw=T), data=trips_per_day_with_all_info_train)
  
  # evaluate on the training data
  train_err[k] <- sqrt(mean((predict(model_all, trips_per_day_with_all_info_train) - trips_per_day_with_all_info_train$num_trips)^2))
  
  # evaluate on the validate data
  validate_err[k] <- sqrt(mean((predict(model_all, trips_per_day_with_all_info_validate) - trips_per_day_with_all_info_validate$num_trips)^2))
}
train_err
validate_err

# plot the training and validation error as a function of polynomial degree
plot_data <- data.frame(K, train_err, validate_err) %>%
  gather("split", "error", -K)
ggplot(plot_data, aes(x=K, y=error, color=split)) +
  geom_line() +
  scale_x_continuous(breaks=K) +
  xlab('Polynomial Degree') +
  ylab('RMSE')
# model for a degree 5 polynomial}
# model for degree 5 polynomial
model_all5 <- lm(num_trips ~ poly(tmin, 5, raw=T) + noisy + aWeekend + tmax, data=trips_per_day_with_all_info_train)

#training
trips_per_day_with_all_info_train <- trips_per_day_with_all_info_train %>%
  add_predictions(model_all5) %>%
  mutate(split="train")
# validate model
trips_per_day_with_all_info_validate <- trips_per_day_with_all_info_validate %>%
  add_predictions(model_all5) %>%
  mutate(split="validate")

# plot the training data and validation data with the model
plot_data <- bind_rows(trips_per_day_with_all_info_train, trips_per_day_with_all_info_validate)
ggplot(plot_data, aes(x=ymd, y=num_trips)) +
  geom_point(aes(color=split)) +
  geom_line(aes(y=pred)) +
  xlab("YMD") +
  ylab('Daily trips') +
  scale_y_continuous()
#predicted vs actual
ggplot(plot_data, aes(x=pred, y=num_trips, color=split)) +
  geom_point() +
  geom_abline() +
  scale_x_continuous("Predicted Number of Trips", label=comma) +
  scale_y_continuous("Actual Number of Trips", label=comma)
