# MPM Group Project
# Students: Giedo, Chris, Azher, Micha

# ---------- Import / Install packages -----------------------------------------

install.packages("tcltk")
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

df.airbnb$price <- backtransform(df.airbnb$log_price, type="log")


# ----------- Graphical Analysis -----------------------------------------------

plot(log_price ~ bedrooms, 
     main = "Relationship of numb. of bedrooms on price",
     pch = 19,
     col = amenities_WiFi)

boxplot(log_price ~ amenities_WiFi, 
        main = 'Types of properties',
        ylab = 'log_price')

boxplot(log_price ~ amenities_Gym, 
        main = 'Types of properties',
        ylab = 'log_price')

boxplot(log_price ~ amenities_Breakfast, 
        main = 'Types of properties',
        ylab = 'log_price')

qplot(y = log_price, x = bedrooms,
      data = df.airbnb,
      facets = ~ amenities_Breakfast)


# ----------- Testing categorical variable 'property_type' ----------------------

count(df.airbnb, 'property_type')
# some categories have only very few entries, maybe eliminate certain categories?

df.airbnb$property_type <- as.factor(df.airbnb$property_type)

boxplot(log_price ~ property_type,
        main = 'Property type', 
        ylab = 'log_price')



lm.aribnb.prop_type <- lm(log_price ~ property_type)
coef(lm.aribnb.prop_type)

summary(df.airbnb['property_type'])
str(df.airbnb['property_type'])
summary(lm.aribnb.prop_type)


# ----------- Analyzing predictor city -----------------------------------------

df.airbnb$city <- as.factor(df.airbnb$city)
summary(df.airbnb['city'])
str(df.airbnb['city'])
count(df.airbnb$city)

ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = city)) +
  geom_boxplot()

ggplot(data = df.airbnb, 
       mapping = aes(y = log_price, 
                     x = review_scores_rating,
                     col = city)) + 
  geom_point() +
  geom_smooth()


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

# ---------------------------
# simple model
glm.accommodates.1 <- glm(accommodates ~ log_price + property_type + amenities_Gym +
                            amenities_Breakfast + bed_type + city, 
                          family = "poisson",
                          data = df.airbnb)

summary(glm.accommodates.1)
summary(glm.accommodates.1)$r.squared

# complex model
# removing 
glm.accomodates.2 <- glm(accommodates ~ property_type[17] + review_scores_rating + 
                           bedrooms + amenities_Pets + city + (property_type + bedrooms) * 
                           city, 
                         family="poisson",
                         data = df.airbnb)

summary(glm.accomodates.2)
summary(glm.accomodates.2)$r.squared

# define the test train split
set.seed(123)
indices <- createDataPartition(df.airbnb$log_price, p=.85, list = F)

# set train and test data
df.airbnb.train <- df.airbnb %>% 
  slice(indices)
df.airbnb.test <- df.airbnb %>%
  slice(-indices)

# fit the model with train data
glm.accommodates.1.train <- glm(formula = formula(glm.accommodates.1),
                                data = df.airbnb.train)

# predict with test data
predicted.accomodates.1.test <- predict(glm.accommodates.1.train, 
                                        newdata = df.airbnb.test)

# compute R^2
cor(predicted.accomodates.1.test, df.airbnb.test$accommodates)^2

# the same for the complex model
# fit the model with train data
glm.accommodates.2.train <- glm(formula = formula(glm.accomodates.2),
                                data = df.airbnb.train)

# predict with test data
predicted.accomodates.2.test <- predict(glm.accommodates.2.train, 
                                        newdata = df.airbnb.test)

# compute R^2
cor(predicted.accomodates.2.test, df.airbnb.test$accommodates)^2

# result, the complex model models the data far better

# lets run several cross validation checks
set.seed(234)
r.squared.simple <- c()
r.squared.complex <- c()

for(i in 1:10^2){
  ## prepare data
  indices <- createDataPartition(df.airbnb$log_price, p=.85, list = F)
  df.airbnb.train <- df.airbnb %>% 
    slice(indices)
  df.airbnb.test <- df.airbnb %>%
    slice(-indices)
  ##create the simple model
  glm.accommodates.1.train <- glm(formula = formula(glm.accommodates.1),
                                  data = df.airbnb.train)
  # predict with test data
  predicted.accomodates.1.test <- predict(glm.accommodates.1.train, 
                                          newdata = df.airbnb.test)
  # compute R^2
  r.squared.simple[i] <- cor(predicted.accomodates.1.test, 
                             df.airbnb.test$accommodates)^2
  ##create complex model
  # fit the model with train data
  glm.accommodates.2.train <- glm(formula = formula(glm.accomodates.2),
                                  data = df.airbnb.train)
  # predict with test data
  predicted.accomodates.2.test <- predict(glm.accommodates.2.train, 
                                          newdata = df.airbnb.test)
  # compute R^2
  r.squared.complex <- cor(predicted.accomodates.2.test, 
                           df.airbnb.test$accommodates)^2
}

mean(r.squared.simple)
mean(r.squared.complex)

# the mean of the more complex model is consistently better

boxplot(r.squared.simple, r.squared.complex)



# ------------ Fitting first model ---------------------------------------------


lm.airbnb.1 <- lm(log_price ~ property_type + room_type + accommodates + bathrooms +
                    bed_type + cancellation_policy + cleaning_fee + city +
                    host_has_profile_pic + host_identity_verified + host_since +
                    instant_bookable + number_of_reviews + review_scores_rating +
                    bedrooms + beds + amenities_Breakfast + amenities_Gym +
                    amenities_Pets + amenities_WiFi)

summary(lm.airbnb.1)

lm.airbnb.2 <- lm(log_price ~ property_type + room_type + accommodates + bathrooms +
                    bed_type + cancellation_policy + cleaning_fee + city +
                    host_has_profile_pic + host_identity_verified + host_since +
                    instant_bookable + number_of_reviews + review_scores_rating +
                    bedrooms + beds + amenities_Breakfast + amenities_Gym +
                    amenities_Pets + amenities_WiFi + (property_type + room_type + 
                                                         accommodates + bathrooms + bed_type + cancellation_policy + 
                                                         cleaning_fee + host_has_profile_pic + host_identity_verified +
                                                         host_since + instant_bookable + number_of_reviews + 
                                                         review_scores_rating + bedrooms + beds + amenities_Breakfast + 
                                                         amenities_Gym + amenities_Pets + amenities_WiFi) * city)

summary(lm.airbnb.2)

anova(lm.airbnb.1, lm.airbnb.2)

# ------------------- Creating a ANN -----------------------------

boxplot(log_price ~ property_type)
boxplot(log_price ~ city)
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = review_scores_rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(. ~ city)

testIndexes.NN <- which(folds==1, arr.ind = TRUE)
testData.NN <- df.airbnb[testIndexes.NN, ]
trainData.NN <- df.airbnb[-testIndexes.NN, ]

price.NN <- nnet(log_price ~ ., data = trainData.NN, size=10, maxit=100, range=5, decay=5e-4)
plot(price.NN)
price.NN

## make a prediction
pred.NN <- predict(price.NN, testData.NN, type = "raw")
cm.NN <- table(pred=pred.NN, true=testData.NN$log_price)
cm.NN


## 2nd try
max <- apply(df.airbnb, 2, max)
min <- apply(df.airbnb, 2, min)
airbnb_scaled <- as.data.frame(scale(df.airbnb, center = min, scale = max - min))
airbnb_net = neuralnet(log_price ~ . , trainData.NN, hidden = 3 , linear.output = TRUE)
plot(cereal_net)

# ------------------- Creating a 10-fold cross validation template ------------------

# 10-fold cross validation
set.seed(111)
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
  model.1.train <- glm(formula = formula(glm.accommodates.1),
                       data = df.airbnb.train)
  # predict the model
  predicted.model.1.test <- predict(model.1.train,
                                    newdata = df.airbnb.test)
  # compute R^2
  r.squared.simple[i] <- cor(predicted.model.1.test, 
                             df.airbnb.test$accommodates)^2
  ## insert you model - complex
  # fit the model with test data
  model.2.train <- glm(formula = formula(glm.accomodates.2),
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

#--------------------Smoother to Characterise the relationship between Log_Price and Predictors------

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = property_type)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)


library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = room_type)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = accommodates)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)


library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = bathrooms)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = bed_type)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = cancellation_policy)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = cleaning_fee)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = host_has_profile_pic)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = host_identity_verified)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = instant_bookable)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = number_of_reviews)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = review_scores_rating)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = bedrooms)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = beds)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = amenities_Breakfast)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = amenities_Gym)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = amenities_Pets)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)

library(ggplot2) 
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = amenities_WiFi)) +
  geom_point() + geom_smooth() + facet_wrap(. ~ city)


# No smoothing : amenities_WiFi, amenities_Pets, amenities_Gym, amenities_Breakfast, instand_bookable, host_identitiy_verified, host_has_profile_pic, cleaning_fee, cancellation_policy, bed_type, bathrooms, room_type, property_type 

# Yes smoothing : beds, bedrooms, review_scores_rating, number_of_reviews, accomodates

#----------workinprogress--------------------------------------

lm.airbnb.3 <- lm(log_price ~ accommodates * city, data = df.airbnb)
qqnorm(resid(lm.airbnb.3)) 
qqline(resid(lm.airbnb.3))
summary(lm.airbnb.3)

#------------I can not install below package though some codes are running, so that is in progress---------------------
install.packages("plgraphics",
                 repos = "http://R-forge.R-project.org")
library(plgraphics)
plregr(lm.airbnb.3,
       plotselect = c(default = 0, 
                      qq = 1), 
       xvar = FALSE)

#
ggplot(mapping = aes (y=resid(lm.airbnb.3),
                      x= fitted(lm.airbnb.3)))+
  geom_abline(intercept = 0, slope = 0)+
  geom_point()+
  geom_smooth()

#--------------Homoscedasticity----------------------------------

ggplot(mapping = aes(y = abs(resid(lm.airbnb.3)), x = fitted(lm.airbnb.3))) +
  geom_abline(intercept = 0, slope = 0) + geom_point() +
  geom_smooth()

plregr(lm.airbnb.3,
       plotselect = c(default = 0,
                      absresfit = 2),
       xvar = FALSE)




#----------Influential observations-------------------
ggplot(data = df.airbnb,
       mapping = aes(y = log_price,
                     x = accommodates)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(. ~ city)


## 1) compute Cook’s distance 
cooks.dist.lm.airbnb.3 <- cooks.distance(lm.airbnb.3)
## 2) plot it
ggplot(data = lm.airbnb.3,
       mapping = aes(y = log_price, x = accommodates,
                     colour = cooks.dist.lm.airbnb.3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(. ~ city)


plot(lm.airbnb.3)


resid.lm.airbnb.3 <- resid(lm.airbnb.3)
ggplot(data = df.airbnb,
       mapping = aes(y = resid.lm.airbnb.3,
                     x = city))+
  geom_boxplot()


