# MPM Group Project
# Students: Giedo, Chris, Azher, Micha

# ---------- Import / Install packages -----------------------------------------

library(ggplot2)
library(plyr)
library(multcomp)
library(splines)
library(faraway)
library(dplyr)

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


# ----------- Testing categorical variable 'property_type ----------------------

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


# ------------ Count data ------------------------------------------------------

# Which predictor can be classified as count data in our data set?
# accommodates
# bathrooms
# number of reviews -> not sure about this one
# bedrooms
# beds

str(df.airbnb)

glm.accommodates <- glm(log_price ~ accommodates, 
                        family = "poisson")
summary(glm.accommodates)

glm.bathrooms <- glm(bathrooms ~ city,
                family = "poisson")
summary(glm.bathrooms)



plot(log_price ~ bathrooms)
abline(lm(log_price ~ bathrooms),
       col = "yellow")



lm.city <- lm(log_price ~ city)
summary(lm.city)

ggplot(mapping = aes(y = bathrooms,
                     x = factor(city))) +
  geom_violin() +
  ylab("bathrooms") +
  xlab("city")

plot(log_price ~ city,
     main = "log_price - city",
     pch = 19,
     col = "blue")
abline(lm.city)

str(df.airbnb$city)



# ------------ Fitting first model ---------------------------------------------


lm.airbnb.1 <- lm(log_price ~ property_type + room_type + accommodates + bathrooms +
                    bed_type + cancellation_policy + cleaning_fee + city +
                    host_has_profile_pic + host_identity_verified + # host_since +
                    instant_bookable + number_of_reviews + review_scores_rating +
                    bedrooms + beds + amenities_Breakfast + amenities_Gym +
                    amenities_Pets + amenities_WiFi)

summary(lm.airbnb.1)

lm.airbnb.2 <- lm(log_price ~ property_type + room_type + accommodates + bathrooms +
                    bed_type + cancellation_policy + cleaning_fee + city +
                    host_has_profile_pic + host_identity_verified + #host_since +
                    instant_bookable + number_of_reviews + review_scores_rating +
                    bedrooms + beds + amenities_Breakfast + amenities_Gym +
                    amenities_Pets + amenities_WiFi + (property_type + room_type + 
                    accommodates + bathrooms + bed_type + cancellation_policy + 
                    cleaning_fee + host_has_profile_pic + host_identity_verified 
                    #+host_since
                    + instant_bookable + number_of_reviews + 
                    review_scores_rating + bedrooms + beds + amenities_Breakfast + 
                    amenities_Gym + amenities_Pets + amenities_WiFi) * city)

summary(lm.airbnb.2)

anova(lm.airbnb.1, lm.airbnb.2)

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
