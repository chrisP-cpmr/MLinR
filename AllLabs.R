# All Labs

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
library(mgcv)

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


# ------------------------ Lab 1 -----------------------------------------------

plot(log_price ~ number_of_reviews,
     ylab = "log_price",
     xlab = "number_of_reviews",
     col = city)
plot(log_price ~ review_scores_rating,
     ylab = "log_price",
     xlab = "rating",
     pch = 19,
     col = city)
legend("topleft",
       pch = 19,
       legend = c(0,1,2),
       col = c("white", "black", "red"))

qplot(y=log_price, x=review_scores_rating,
      data = df.airbnb,
      facets = ~ room_type)

qplot(y=log_price, x=review_scores_rating,
      data = df.airbnb,
      facets = ~ city)

qplot(y=log_price, x=number_of_reviews,
      data = df.airbnb,
      facets = ~ room_type)

lm.df.airbnb.1 <- lm(log_price ~ review_scores_rating + factor(room_type) +
                       factor(city) + factor(amenities_Breakfast))
summary(lm.df.airbnb.1)
coef(lm.df.airbnb.1)

lm.df.airbnb.2 <- lm(log_price ~ factor(room_type) * factor(city))
summary(lm.df.airbnb.2)

# room type ha s an interaction with city

lm.df.airbnb.3 <- lm(log_price ~ review_scores_rating * factor(city))
summary(lm.df.airbnb.3)

# the rating has no interaction with city

lm.df.airbnb.4 <- lm(log_price ~ number_of_reviews * factor(city))
summary(lm.df.airbnb.4)
summary(lm.df.airbnb.4)$adj.r.squared

# the number of reviews have an interaction with the city

lm.df.airbnb.5 <- lm(log_price ~ review_scores_rating)

fitted.df.airbnb.5 <- fitted(lm.df.airbnb.5)
str(fitted.df.airbnb.3)

plot(log_price ~ review_scores_rating, 
     data = df.airbnb,
     ylab = "log_price",
     xlab = "rating",
     col = "darkgray")
##
points(fitted.df.airbnb.5 ~ review_scores_rating, 
       col = "purple",
       pch = 19, 
       data = df.airbnb)
##
abline(lm.df.airbnb.5, col = "black")

resid.lm.df.airbnb.5 <- resid(lm.df.airbnb.5)
length(resid.lm.df.airbnb.5)

# -------------------- Lab 2 ---------------------------------------------------

boxplot(log_price ~ city,
        ylab = "log_price",
        xlab = "city")
boxplot(log_price ~ property_type,
        ylab = "log_price",
        xlab = "property_type")
boxplot(log_price ~ bed_type,
        ylab = "log_price",
        xlab = "bed_type")
boxplot(log_price ~ room_type,
        ylab = "log_price",
        xlab = "room_type")

# clear differences in groups, most applicable for the predictor room type

lm.df.airbnb.6 <- lm(log_price ~ factor(city), data = df.airbnb)
summary(lm.df.airbnb.6)
# all levels differ significantly

lm.df.airbnb.7 <- lm(log_price ~ factor(property_type), data = df.airbnb)
summary(lm.df.airbnb.7)
# most of the levels have a significant difference

lm.df.airbnb.8 <- lm(log_price ~ factor(bed_type), data = df.airbnb)
summary(lm.df.airbnb.8)
# only half of the categories are significant

lm.df.airbnb.9 <- lm(log_price ~ factor(room_type), data = df.airbnb)
summary(lm.df.airbnb.9)
# all significant, was obvious form box plot

lm.df.airbnb.10 <- lm(log_price ~ 1, data = df.airbnb)
summary(lm.df.airbnb.10)

# compare models
anova(lm.df.airbnb.8, lm.df.airbnb.10)
# results show that the predictors are important

lm.df.airbnb.11 <- update(lm.df.airbnb.6, . ~ . + factor(property_type) +
                        factor(bed_type) + factor(room_type))
drop1(lm.df.airbnb.11, test = "F")
# the drop1 statement indicates, that all the categorical predictors are important

# model with all categorical predictors
lm.df.airbnb.12 <- update(lm.df.airbnb.11, . ~ . + factor(cancellation_policy) +
                            factor(cleaning_fee) + factor(host_has_profile_pic) +
                            factor(host_identity_verified) + factor(instant_bookable) +
                            factor(amenities_Breakfast) + factor(amenities_Gym) +
                            factor(amenities_Pets) + factor(amenities_WiFi))
drop1(lm.df.airbnb.12, test="F")
# drop1 indicates that all predictors are significant expect "amenitites_WiFi"

# create model with only relevant categorical values
lm.df.airbnb.13 <- update(lm.df.airbnb.11, . ~ . + factor(cancellation_policy) +
                            factor(cleaning_fee) + factor(host_has_profile_pic) +
                            factor(host_identity_verified) + factor(instant_bookable) +
                            factor(amenities_Breakfast) + factor(amenities_Gym) +
                            factor(amenities_Pets))
drop1(lm.df.airbnb.13, test = "F")
str(df.airbnb)
# lets now add all the continuous variables
lm.df.airbnb.14 <- update(lm.df.airbnb.13, . ~ . + accommodates + bathrooms + 
                            number_of_reviews + review_scores_rating + bedrooms +
                            beds)
drop1(lm.df.airbnb.14, test = "F")

# all values have a significant influence on the price, "amenities_Pets" is 
# least significant

anova(lm.df.airbnb.10, lm.df.airbnb.14)
# the complex model is a much better model and explains much more of the
# variance than the simple model, the F value is high since many observations
# have been added


# ---------------------- Lab 3 -------------------------------------------------

# plot with linear function
ggplot(mapping = aes(y=log_price, 
                     x=review_scores_rating),
       data = df.airbnb) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(mapping = aes(y=log_price, 
                     x=number_of_reviews),
       data=df.airbnb) +
  geom_point() +
  geom_smooth(method = lm)

# plot with non-linear function

ggplot(mapping = aes(y=log_price, 
                     x=review_scores_rating,
                     colour = room_type),
       data = df.airbnb) +
  geom_point() +
  geom_smooth()
# indication that the relationship is not linear

ggplot(mapping = aes(y=log_price, 
                     x=number_of_reviews,
                     colour = city),
       data=df.airbnb) +
  geom_point() +
  geom_smooth()
# indication that the relationship is not linear

ggplot(mapping = aes(y=log_price, 
                     x=accommodates), 
       data = df.airbnb) +
  geom_point() +
  geom_smooth()
# indicates quadratic relationship
str(df.airbnb)
ggplot(mapping = aes(y=log_price, 
                     x=bedrooms), 
       data = df.airbnb) +
  geom_point() +
  geom_smooth()
# indicates quadratic relationship

ggplot(mapping = aes(y=log_price, 
                     x=number_of_reviews),
       data=df.airbnb) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  facet_wrap(. ~ city )

ggplot(mapping = aes(y=log_price, 
                     x=number_of_reviews),
       data=df.airbnb) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  facet_wrap(. ~ room_type)
# clear indication that room types differ statistically
str(df.airbnb)
# fitting lm with interactions
lm.df.airbnb.20 <- lm(log_price ~ factor(property_type) + factor(room_type) + 
                        accommodates + bathrooms + factor(bed_type) + 
                        factor(cancellation_policy) + factor(cleaning_fee) +
                        factor(city) + factor(host_has_profile_pic) + 
                        factor(host_identity_verified) + factor(instant_bookable) +
                        number_of_reviews + review_scores_rating + bedrooms +
                        beds + factor(amenities_Breakfast) + factor(amenities_Gym) +
                        factor(amenities_Pets) + review_scores_rating:factor(room_type))
drop1(lm.df.airbnb.20, test ="F")

lm.df.airbnb.21 <- update(lm.df.airbnb.20, . ~ . - review_scores_rating:factor(room_type) +
                            review_scores_rating:factor(city))
drop1(lm.df.airbnb.21, test = "F")
# all predictors are significant and we should regard this model 

lm.df.airbnb.22 <- update(lm.df.airbnb.21, . ~ . + review_scores_rating:factor(room_type))
drop1(lm.df.airbnb.22, test = "F")
# not all predictors are significant

# -------------------------------- Lab 4 ---------------------------------------

# model with linear effect on number of review
lm.df.airbnb.40 <- lm(log_price ~ number_of_reviews + city + room_type,
                      data = df.airbnb)
# model with quadratic effect on number of reviews
lm.df.airbnb.41 <- update(lm.df.airbnb.40, . ~ . + I(number_of_reviews^2))

anova(lm.df.airbnb.40, lm.df.airbnb.41)
# the anova comparison indicates a strong need for the quadratic relationship

# fit a cubid effect with the ploy function to reduce risk of collinearity
lm.df.airbnb.42 <- lm(log_price ~ number_of_reviews + city + room_type +
                        poly(number_of_reviews, degree = 3),
                      data = df.airbnb)
anova(lm.df.airbnb.41, lm.df.airbnb.42)
# same effect
summary(lm.df.airbnb.42)

lm.df.airbnb.43 <- lm(log_price ~ number_of_reviews + city + room_type +
                        poly(number_of_reviews, degree = 3) + number_of_reviews +
                        poly(review_scores_rating, degree = 3) + review_scores_rating,
                      data = df.airbnb)

gam.df.airbnb.40 <- gam(log_price ~ s(number_of_reviews) + s(review_scores_rating) +
                          city + room_type,
                        data = df.airbnb)
summary(gam.df.airbnb.40)
# df of 8 and 9 

plot(gam.df.airbnb.40, residuals = TRUE, cex = 2)

anova(lm.df.airbnb.43, gam.df.airbnb.40)


# ------------------------------ Lab 5 -----------------------------------------

# split data into 4 groups
df.airbnb$group <- cut(seq(1,nrow(df.airbnb)), breaks = 4, labels = FALSE)
df.airbnb$group <- as.factor(df.airbnb$group)
str(df.airbnb)
plot(log_price ~ review_scores_rating,
     ylab = "log_price",
     xlab = "review_scores_rating",
     col = group,
     data = df.airbnb)

qplot(y=log_price, x=review_scores_rating,
      data = df.airbnb,
      facets = ~ group)

summary(df.airbnb$log_price)
firstQ <- 4.317
median(df.airbnb$log_price)
thridQ <- 5.220
df.airbnb$group <- 1
for(i in 1:73470 ) {
  if (df.airbnb$log_price[as.numeric(i)] > 5.220) {
    df.airbnb$group[i] <- "expensive"
  } else if (df.airbnb$log_price[as.numeric(i)] >= 4.718499 && 
             df.airbnb$log_price[as.numeric(i)] < 5.220) {
    df.airbnb$group[i] <- "pricy"
  } else if (df.airbnb$log_price[as.numeric(i)] >= 4.317 && 
             df.airbnb$log_price[as.numeric(i)] < 4.718499) {
    df.airbnb$group[i] <- "affordable"
  } else {
    df.airbnb$group[i] <- "cheap"
  }
}
df.airbnb$group <- as.factor(df.airbnb$group)

plot(log_price ~ review_scores_rating,
     ylab = "log_price",
     xlab = "review_scores_rating",
     col = group,
     data = df.airbnb)
str(df.airbnb)
df.air
boxplot(log_price ~ group, data = df.airbnb)
aggregate(cbind(count = log_price) ~ group, 
          data = df.airbnb, 
          FUN = function(x){NROW(x)})

str(df.airbnb)

# apply a count model to another response variable - Number of reviews

boxplot(number_of_reviews ~ city,
        ylab = "number_of_reviews",
        xlab = "city")

boxplot(number_of_reviews ~ room_type)

boxplot(number_of_reviews ~ property_type)

plot(number_of_reviews ~ review_scores_rating)
plot(number_of_reviews ~ log_price)

glm.df.airbnb.50 <- glm(number_of_reviews ~ city + room_type + 
                          review_scores_rating + log_price + property_type,
                        family = "quasipoisson",
                        data = df.airbnb)
summary(glm.df.airbnb.50)

coef(glm.df.airbnb.50)["city1"]
exp(coef(glm.df.airbnb.50)["city1"] %>% round(digits = 2))

#
drop1(glm.df.airbnb.50, test = "F")

str(df.airbnb)
glm.df.airbnb.51 <- glm(number_of_reviews ~ log_price + factor(property_type) +
                          factor(room_type) + accommodates + bathrooms + factor(bed_type) +
                          factor(cancellation_policy) + factor(cleaning_fee) +
                          factor(city) + factor(host_has_profile_pic) + 
                          factor(host_identity_verified) + factor(instant_bookable) +
                          review_scores_rating + bedrooms + beds + factor(amenities_Breakfast) +
                          factor(amenities_Gym) + factor(amenities_Pets) +
                          factor(amenities_WiFi),
                        family = "quasipoisson",
                        data = df.airbnb)
summary(glm.df.airbnb.51)
drop1(glm.df.airbnb.51, test = "F")
# except amenities_Pets all predictors are significant

glm.df.airbnb.52 <- update(glm.df.airbnb.51, . ~ . - factor(amenities_Pets))

gam.df.airbnb.53 <- gam(number_of_reviews ~ s(log_price) + property_type + room_type +
                          accommodates + bathrooms + bed_type + cancellation_policy +
                          cleaning_fee + city + host_has_profile_pic +
                          host_identity_verified + instant_bookable +
                          s(review_scores_rating) + bedrooms + beds + amenities_Breakfast+
                          amenities_Gym + amenities_WiFi,
                        family = "quasipoisson",
                        data = df.airbnb)

summary(gam.df.airbnb.53)

anova(gam.df.airbnb.53, glm.df.airbnb.52)

# 10-fold cross validation
set.seed(555)
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
  model.1.train <- glm(formula = formula(glm.df.airbnb.52),
                       data = df.airbnb.train)
  # predict the model
  predicted.model.1.test <- predict(model.1.train,
                                    newdata = df.airbnb.test)
  # compute R^2
  r.squared.simple[i] <- cor(predicted.model.1.test, 
                             df.airbnb.test$number_of_reviews)^2
  ## insert you model - complex
  # fit the model with test data
  model.2.train <- gam(formula = formula(gam.df.airbnb.53),
                       data = df.airbnb.train)
  # predict the model
  predicted.model.2.test <- predict(model.2.train,
                                    newdata = df.airbnb.test)
  # compute R^2
  r.squared.complex[i] <- cor(predicted.model.2.test, 
                           df.airbnb.test$number_of_reviews)^2
}

str(r.squared.complex)
mean(r.squared.simple)*100
mean(r.squared.complex)*100

boxplot(r.squared.complex, r.squared.simple)

# apply a binomial model on a 0.75 data split of log_price
firstQ <- 4.317
median(df.airbnb$log_price)
thridQ <- 5.220
df.airbnb$isExpensive <- 0
for(i in 1:73470 ) {
  if (df.airbnb$log_price[as.numeric(i)] > 4.718499) {
    df.airbnb$isExpensive[i] <- 1
  } else {
    df.airbnb$isExpensive[i] <- 0
  }
}
df.airbnb$isExpensive <- as.factor(df.airbnb$isExpensive)

glm.df.airbnb.55 <- glm(isExpensive ~ city + review_scores_rating,
                        family = "binomial",
                        data = df.airbnb)
summary(glm.df.airbnb.55)

fitted.glm <- ifelse(fitted(glm.df.airbnb.55) < 0.5,
                     yes = 0, no = 1)

d.obs.fit.airbnb <- data.frame(obs = df.airbnb$isExpensive,
                               fitted = fitted.glm)
head(d.obs.fit.airbnb)
table(d.obs.fit.airbnb$obs)
table(obs = d.obs.fit.airbnb$obs,
      fit = d.obs.fit.airbnb$fitted)

glm.df.airbnb.56 <- glm(isExpensive ~ property_type + room_type +
                          accommodates + bathrooms + bed_type + cancellation_policy +
                          cleaning_fee + city + host_has_profile_pic +
                          host_identity_verified + instant_bookable + number_of_reviews +
                          review_scores_rating + bedrooms + beds + amenities_Breakfast+
                          amenities_Gym + amenities_WiFi + amenities_Pets,
                        family = "binomial",
                        data = df.airbnb)
drop1(glm.df.airbnb.56, test = "F")

fitted.glm.2 <- ifelse(fitted(glm.df.airbnb.56) < 0.5,
                       yes = 0, no = 1)
d.obs.fit.airbnb.2 <- data.frame(obs = df.airbnb$isExpensive,
                               fitted = fitted.glm.2)
head(d.obs.fit.airbnb.2)
table(d.obs.fit.airbnb.2$obs)
table(obs=d.obs.fit.airbnb.2$obs,
      fit=d.obs.fit.airbnb.2$fitted)

glm.df.airbnb.57 <- glm(isExpensive ~ room_type +
                          accommodates + amenities_Breakfast +
                          amenities_Gym + amenities_WiFi,
                        family = "binomial",
                        data = df.airbnb)
summary(glm.df.airbnb.57)

fitted.glm.3 <- ifelse(fitted(glm.df.airbnb.57) < 0.5,
                       yes = 0, no = 1)
d.obs.fit.airbnb.3 <- data.frame(obs = df.airbnb$isExpensive,
                                 fitted = fitted.glm.3)
head(d.obs.fit.airbnb.3)
table(d.obs.fit.airbnb.3$obs)
table(obs=d.obs.fit.airbnb.3$obs,
      fit=d.obs.fit.airbnb.3$fitted)

glm.df.airbnb.58 <- glm(isExpensive ~ room_type +
                          accommodates + amenities_Gym,
                        family = "quasibinomial",
                        data = df.airbnb)
summary(glm.df.airbnb.58)

fitted.glm.4 <- ifelse(fitted(glm.df.airbnb.58) < 0.5,
                       yes = 0, no = 1)
d.obs.fit.airbnb.4 <- data.frame(obs = df.airbnb$isExpensive,
                                 fitted = fitted.glm.4)
head(d.obs.fit.airbnb.4)
table(d.obs.fit.airbnb.4$obs)
table(obs=d.obs.fit.airbnb.4$obs,
      fit=d.obs.fit.airbnb.4$fitted)


## ------------------------- SVM -----------------------------------------------

library(tidyverse)
library(e1071)
library(caret)

df.airbnb.svm <- read.csv("MPM_Last_.csv")

df.airbnb.svm$property_type <- as.factor(df.airbnb.svm$property_type)
df.airbnb.svm$room_type <- as.factor(df.airbnb.svm$room_type)
df.airbnb.svm$bed_type <- as.factor(df.airbnb.svm$bed_type)
df.airbnb.svm$cancellation_policy <- as.factor(df.airbnb.svm$cancellation_policy)
df.airbnb.svm$cleaning_fee <- as.factor(df.airbnb.svm$cleaning_fee)
df.airbnb.svm$city <- as.factor(df.airbnb.svm$city)
df.airbnb.svm$city <- as.factor(df.airbnb.svm$city)
df.airbnb.svm$host_has_profile_pic <- as.factor(df.airbnb.svm$host_has_profile_pic)
df.airbnb.svm$host_identity_verified <- as.factor(df.airbnb.svm$host_identity_verified)
df.airbnb.svm$instant_bookable <- as.factor(df.airbnb.svm$instant_bookable)
df.airbnb.svm$amenities_Breakfast <- as.factor(df.airbnb.svm$amenities_Breakfast)
df.airbnb.svm$amenities_Gym <- as.factor(df.airbnb.svm$amenities_Gym)
df.airbnb.svm$amenities_Pets <- as.factor(df.airbnb.svm$amenities_Pets)
df.airbnb.svm$amenities_WiFi <- as.factor(df.airbnb.svm$amenities_WiFi)

df.airbnb.svm$price <- 3^df.airbnb.svm$log_price

str(df.airbnb.svm)

df.airbnb.svm$isExpensive <- 0
for(i in 1:73470 ) {
  if (df.airbnb.svm$log_price[as.numeric(i)] > 4.718499) {
    df.airbnb.svm$isExpensive[i] <- 1
  } else {
    df.airbnb.svm$isExpensive[i] <- 0
  }
}


set.seed(111)
indices.svm <- createDataPartition(df.airbnb.svm$isExpensive, p=0.85, list = F)

train.svm <- df.airbnb.svm %>% slice(indices)
test_in.svm <- df.airbnb.svm %>% slice(-indices) %>% select(-isExpensive)
test_truth.svm <- df.airbnb.svm %>% slice(-indices) %>% pull(isExpensive)

str(train.svm)
str(df.airbnb.svm)
set.seed(111)  
airbnb_svm <- svm(df.airbnb.svm$isExpensive ~ ., train.svm, kernel = "linear", scale = T, cost = 10)




