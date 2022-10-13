## Code for NN

# Read the needed data

library(nnet)
library(gamlss.add)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(caret)
library(tidyverse)
library(neuralnet)

airbnbNN <- read.csv("MPM_Prop.csv")

str(airbnbNN)

airbnbNN$property_type <- as.numeric(airbnbNN$property_type)
airbnbNN$room_type <- as.numeric(airbnbNN$room_type)
airbnbNN$bed_type <- as.numeric(airbnbNN$bed_type)
airbnbNN$cancellation_policy <- as.numeric(airbnbNN$cancellation_policy)
airbnbNN$cleaning_fee <- as.numeric(airbnbNN$cleaning_fee)
airbnbNN$city <- as.numeric(airbnbNN$city)
airbnbNN$city <- as.numeric(airbnbNN$city)
airbnbNN$host_has_profile_pic <- as.numeric(airbnbNN$host_has_profile_pic)
airbnbNN$host_identity_verified <- as.numeric(airbnbNN$host_identity_verified)
airbnbNN$instant_bookable <- as.numeric(airbnbNN$instant_bookable)
airbnbNN$amenities_Breakfast <- as.numeric(airbnbNN$amenities_Breakfast)
airbnbNN$amenities_Gym <- as.numeric(airbnbNN$amenities_Gym)
airbnbNN$amenities_Pets <- as.numeric(airbnbNN$amenities_Pets)
airbnbNN$amenities_WiFi <- as.numeric(airbnbNN$amenities_WiFi)
airbnbNN$accommodates <- as.numeric(airbnbNN$accommodates)
airbnbNN$bathrooms <- as.numeric(airbnbNN$bathrooms)
airbnbNN$number_of_reviews <- as.numeric(airbnbNN$number_of_reviews)
airbnbNN$review_scores_rating <- as.numeric(airbnbNN$review_scores_rating)
airbnbNN$bedrooms <- as.numeric(airbnbNN$bedrooms)
airbnbNN$beds <- as.numeric(airbnbNN$beds)

airbnbNN$X <- NULL
airbnbNN$Unnamed..0 <- NULL

str(airbnbNN)

## These functions should be used for categorical / count variables
# airbnbNN %>%
#   ggplot(aes(x = log_price, fill = room_type)) +
#   geom_histogram(position = "stack")
# 
# airbnbNN %>%
#   ggplot(aes(x = log_price, fill = bed_type)) +
#   geom_histogram(position = "stack")
# 
# airbnbNN %>%
#   ggplot(aes(x = number_of_reviews, fill = log_price)) +
#   geom_histogram(poisiton = "stack")

## Plots for continuous variable
airbnbNN %>%
  ggplot(aes(x = log_price, y = number_of_reviews, color = room_type)) +
  geom_point()

airbnbNN %>%
  ggplot(aes(x=log_price, y=review_scores_rating, color = room_type)) +
  geom_point()


## -------------------- From Lab 1 ---------------------------------------------
set.seed(123)
is_train <- runif(nrow(airbnbNN)) < 0.8
mean(is_train)

train <- airbnbNN[is_train, ]
test <- airbnbNN[!is_train, ]

airbnb_net <- nnet(log_price ~ ., data = train, size=15, maxit=100, range=0.1, decay=5e-4)

plot(airbnb_net)
airbnb_net

pred <- predict(airbnb_net, test, type="class")
a_nn <- table(pred=pre)

?predict

## ------------------------ Lab 2 ----------------------------------------------

set.seed(1234)
indices <- createDataPartition(airbnbNN$log_price, p=.85, list = F)

airbnbNN %>%
  mutate(train = row_number() %in% indices) %>%
  select(log_price, train) %>%
  table()



## --------------------- Lab 3 -------------------------------------------------

indices_subset <- createDataPartition(airbnbNN$log_price, p=0.1, list = FALSE)
subset_airbnbNN <- airbnbNN %>% slice(indices_subset)
str(subset_airbnbNN)

set.seed(12345)
indices <- createDataPartition(subset_airbnbNN$log_price, p=0.8, list = FALSE)
train <- airbnbNN %>% slice(indices)
test <- airbnbNN %>% slice(-indices)
boxplot(train$log_price, test$log_price, subset_airbnbNN %>% sample_frac(0.2) %>% pull(log_price))

max <- apply(subset_airbnbNN, 2, max)
min <- apply(subset_airbnbNN, 2, min)
subset_airbnbNN_scaled <-  as.data.frame(scale(subset_airbnbNN, center = min, scale = max - min))
train_scaled <- subset_airbnbNN_scaled %>% slice(indices)
test_scaled <- subset_airbnbNN_scaled %>% slice(-indices)

# Fit the network
str(subset_airbnbNN)
set.seed(20)
airbnb_net_lab3 <- neuralnet(log_price ~ review_scores_rating + number_of_reviews + property_type + room_type +
                               accommodates + room_type + bathrooms + bed_type + cancellation_policy + 
                               cleaning_fee + city + host_has_profile_pic + host_identity_verified + 
                               instant_bookable + bedrooms + beds + amenities_Gym + amenities_WiFi + 
                               amenities_Pets + amenities_Breakfast, 
                             data = train_scaled, hidden=3, linear.output = TRUE)

?neuralnet

pred_scaled <- compute(airbnb_net_lab3, test %>% select(-log_price))
pred <- pred_scaled$net.result * (max(subset_airbnbNN$log_price) - min(subset_airbnbNN$log_price)) + 
  min(subset_airbnbNN$log_price)
pred

plot(test$log_price, pred, col='blue', pch=16, ylab="predict log_price NN", xlab = "real log_price")
abline(0,1)

sqrt(mean((test$log_price - pred)^2))

set.seed(21)
tuGrid <- expand.grid(.layer1=c(1:4), .layer2=c(0,2), .layer3=c(0))

trCtrl <- trainControl(
  method = 'repeatedcv',
  number = 2,
  repeats = 3,
  returnResamp = 'final'
)

model <- train(
  x = subset_airbnbNN %>% select(-log_price),
  y = subset_airbnbNN_scaled %>% pull(log_price),
  method = 'neuralnet', metric = 'RMSE',
  linear.output = TRUE,
  preProcess = c('center', 'scale'),
  tuneGrid = tuGrid,
  trControl = trCtrl
)

plot(model)

plot(model$finalModel)

# with best model

set.seed(22)
airbnb_net_lab3 <- neuralnet(log_price ~ review_scores_rating + number_of_reviews + property_type + room_type +
                               accommodates + room_type + bathrooms + bed_type + cancellation_policy + 
                               cleaning_fee + city + host_has_profile_pic + host_identity_verified + 
                               instant_bookable + bedrooms + beds + amenities_Gym + amenities_WiFi + 
                               amenities_Pets + amenities_Breakfast, 
                             data = train_scaled, hidden=2, linear.output = TRUE)
plot(airbnb_net_lab3)

pred_scaled <- compute(airbnb_net_lab3, test %>% select(-log_price))
pred <- pred_scaled$net.result * (max(subset_airbnbNN$log_price) - min(subset_airbnbNN$log_price)) + 
  min(subset_airbnbNN$log_price)
pred

plot(test$log_price, pred, col='blue', pch=16, ylab="predict log_price NN", xlab = "real log_price")
abline(0,1)

sqrt(mean((test$log_price - pred)^2))





