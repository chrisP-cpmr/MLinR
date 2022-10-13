# Clean code

# MPM Group Project
# Students: Giedo, Chris, Azher, Micha

# ---------- Import / Install packages -----------------------------------------

library(ggplot2)
library(plyr)
library(multcomp)
library(splines)
library(faraway)
library(dplyr)
library(caret)
library(tidyverse)
library(neuralnet)
library(nnet)
library(gamlss.add)
library(ggplot2)
library(confidence)

# ---------- Read / inspect data -----------------------------------------------

df.airbnb <- read.csv("MPM_Prop.csv")
head(df.airbnb)
str(df.airbnb)
summary(df.airbnb)

attach(df.airbnb)

# ----------- Set as factor ----------------------------------------------------

df.airbnb$property_type <- as.factor(df.airbnb$property_type)
df.airbnb$room_type <- as.factor(df.airbnb$room_type)
df.airbnb$bed_type <- as.factor(df.airbnb$bed_type)
df.airbnb$cancellation_policy <- as.factor(df.airbnb$cancellation_policy)
df.airbnb$cleaning_fee <- as.factor(df.airbnb$cleaning_fee)
df.airbnb$city <- as.factor(df.airbnb$city)
df.airbnb$city <- as.factor(df.airbnb$city)
df.airbnb$host_has_profile_pic <- as.factor(df.airbnb$host_has_profile_pic)
df.airbnb$host_identity_verified <- as.factor(df.airbnb$host_identity_verified)
df.airbnb$instant_bookable <- as.factor(df.airbnb$instant_bookable)
df.airbnb$amenities_Breakfast <- as.factor(df.airbnb$amenities_Breakfast)
df.airbnb$amenities_Gym <- as.factor(df.airbnb$amenities_Gym)
df.airbnb$amenities_Pets <- as.factor(df.airbnb$amenities_Pets)
df.airbnb$amenities_WiFi <- as.factor(df.airbnb$amenities_WiFi)

# ------------ Poisson Model----------------------------------------------------

# Which predictor can be classified as count data in our data set?
# accommodates
# bathrooms
# number of reviews -> not sure about this one
# bedrooms
# beds

## glm Possion

colnames(df.airbnb)

lm.accommodates.1 <- lm(accommodates ~ city, data = df.airbnb)
coef(lm.accommodates.1)

set.seed(511)
sim.data.accommodates <- simulate(lm.accommodates.1)

ggplot(mapping = aes(y = sim.data.accommodates$sim_1,
                     x = df.airbnb$city)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  ylab("accomodates") +
  xlab("city")

# We can see, the lm model createds values below 0 which should not be possible


# simple model
glm.accommodates.3 <- glm(accommodates ~ city, 
                          family = "poisson",
                          data = df.airbnb)

coef(glm.accommodates.3)
summary(glm.accommodates.3)

set.seed(522)
sim.data.accommodates.Poisson <- simulate(glm.accommodates.3)

ggplot(mapping = aes(y = sim.data.accommodates.Poisson$sim_1,
                     x = df.airbnb$city)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  ylab("accomodates") +
  xlab("city")

# compared to the lm model, the poisson distribution does not fit values below 0

# complex model
glm.accommodates.4 <- glm(accommodates ~ city + property_type + log_price + 
                            property_type * city,
                          family = "poisson",
                          data = df.airbnb)

coef(glm.accommodates.4)
summary(glm.accommodates.4)

set.seed(533)
sim.data.accommodates.Poisson.1 <- simulate(glm.accommodates.4)

ggplot(mapping = aes(y = sim.data.accommodates.Poisson.1$sim_1,
                     x = df.airbnb$city)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  ylab("accomodates") +
  xlab("city")

# cross validation
# 10-fold cross validation
set.seed(544)
r.squared.simple <- c()
r.squared.complex <- c()
# shuffle data
df.airbnb <- df.airbnb[sample(nrow(df.airbnb)),]
folds <- cut(seq(1,nrow(df.airbnb)), breaks = 10, labels = FALSE)
for(i in 1:10){
  testIndexes <- which(folds==i, arr.ind = TRUE)
  df.airbnb.test <- df.airbnb[testIndexes, ]
  df.airbnb.train <- df.airbnb[-testIndexes, ]
  ## insert your models - simple
  # fit the model with test data
  model.1.train <- glm(formula = formula(glm.accommodates.3),
                       data = df.airbnb.train)
  # predict the model
  predicted.model.1.test <- predict(model.1.train,
                                    newdata = df.airbnb.test)
  # compute R^2
  r.squared.simple[i] <- cor(predicted.model.1.test, 
                             df.airbnb.test$accommodates)^2
  ## insert you model - complex
  # fit the model with test data
  model.2.train <- glm(formula = formula(glm.accommodates.4),
                       data = df.airbnb.train)
  # predict the model
  predicted.model.2.test <- predict(model.2.train,
                                    newdata = df.airbnb.test)
  # compute R^2
  r.squared.complex <- cor(predicted.model.2.test, 
                           df.airbnb.test$accommodates)^2
}

mean(r.squared.simple)
mean(r.squared.complex)

# the simple model is better at predicting the number of how many people a object
# can accommodate.


# ------------------------- binary model ---------------------------------------


# create two groups split on the median
summary(df.airbnb["log_price"])
price_median <- 4.718

df.airbnb$group <- ifelse(df.airbnb$log_price >= price_median, 1, 0)
df.airbnb$group <- as.factor(df.airbnb$group)
str(df.airbnb)






