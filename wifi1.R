

#### WiFi Locationing Project

# Goal of the project:



#### 0. Environment

### 0.1 Working directory

setwd("C:/Users/User/Desktop/DataAnalytics/4.2. Wifi Locationing")

### 0.2. Packages required

install.packages(mvabund)
library(caret)
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(doParallel)

options(scipen=999)       ## to avoid scientific numeration

var.p = function(x){var(x)*(length(x)-1)/length(x)}


#### 1. Data exploration

### 1.1. Downloading data

## 1.1.1. Downloading training data
training <- read_csv("trainingData.csv")
training <- as.data.frame(training)
training[1:529] <- lapply(training[1:529], as.numeric)
training[523:528] <- lapply(training[523:528], as.factor)

## 1.1.2. Downloading validation data
validation <- read_csv("validationData.csv")
validation <- as.data.frame(validation)
validation[1:529] <- lapply(validation[1:529], as.numeric)
validation[523:528] <- lapply(validation[523:528], as.factor)

### 1.2. Data observation
head(training)
str(training)
summary(training)
sum(is.na(training))

head(validation)
sum(is.na(validation))

training_clean <- training
validation_clean <- validation

training_clean[training_clean == "100"] <- -105
validation_clean[validation_clean == "100"] <- -105


# unique values on the supposed-to-be label variables
unique(training$SPACEID)
unique(training$RELATIVEPOSITION)


## 1.2.1. Basic statistics
summary(training_clean$FLOOR)
summary(training_clean$BUILDINGID)
summary(training_clean$SPACEID)
summary(training_clean$USERID)


## 1.2.2. Zero variance / Near zero variance

nearZeroVar(training_clean, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE,
            names = TRUE)

## Data cleaning

# how many WAP's with zero variance? Thus, how many useless WAP's?
names(which(apply(training_clean,2,var) == 0))

# remove every useless WAP
which(apply(training_clean,2,max) == -105)
which(apply(training_clean,2,var) == 0 )
training_clean <- training_clean[,-which(apply(training_clean,2,var) == 0)]

which(apply(validation_clean,2,max) == -105)
which(apply(validation_clean,2,var) == 0 )
validation_clean <- validation_clean[,-which(apply(validation_clean[1:520],2,var) == 0)]

common_col <- intersect(names(training_clean[,1:465]), names(validation_clean[,1:367]))

training_clean <- cbind(training_clean[,common_col],training_clean[466:475])
validation_clean <- cbind(validation_clean[,common_col], validation_clean[368:377])

# merge building and floor
training$BUILDFLOOR <- paste0(training$BUILDINGID, training$FLOOR)
training_clean$BUILDFLOOR <- paste0(training_clean$BUILDINGID, training_clean$FLOOR)
validation$BUILDFLOOR <- paste0(validation$BUILDINGID, validation$FLOOR)
validation_clean$BUILDFLOOR <- paste0(validation_clean$BUILDINGID, validation_clean$FLOOR)
training$BUILDFLOOR <- as.factor(training$BUILDFLOOR)
validation$BUILDFLOOR <- as.factor(validation$BUILDFLOOR)
training_clean$BUILDFLOOR <- as.factor(validation_clean$BUILDFLOOR)
validation_clean$BUILDFLOOR <- as.factor(validation_clean$BUILDFLOOR)

# remove user 6 observations (half of it are wrong)
training_clean <- training_clean %>% filter(!(USERID == 6))
validation_clean <- validation_clean %>% filter(1(USERID == 6))

# how many WAP's with low standard deviation?
training_highsd1 <- cbind(training_clean[,which(apply(training_clean[,1:464],2,sd) > 1)], training_clean[,466:475])

training_highsd2 <- cbind(training_clean[,which(apply(training_clean[,1:464],2,sd) > 2)], training_clean[,466:475])

training_highsd5 <- cbind(training_clean[,which(apply(training_clean[,1:464],2,sd) > 5)], training_clean[,466:475])


## 1.2.3. Subsetting data
## 1.2.3.1. By user
user01 <- training_clean %>% filter(USERID == 1)
user02 <- training_clean %>% filter(USERID == 2)
user03 <- training_clean %>% filter(USERID == 3)
user04 <- training_clean %>% filter(USERID == 4)
user05 <- training_clean %>% filter(USERID == 5)
user06 <- training_clean %>% filter(USERID == 6)
user07 <- training_clean %>% filter(USERID == 7)
user08 <- training_clean %>% filter(USERID == 8)
user09 <- training_clean %>% filter(USERID == 9)
user10 <- training_clean %>% filter(USERID == 10)
user11 <- training_clean %>% filter(USERID == 11)
user12 <- training_clean %>% filter(USERID == 12)
user13 <- training_clean %>% filter(USERID == 13)
user14 <- training_clean %>% filter(USERID == 14)
user15 <- training_clean %>% filter(USERID == 15)
user16 <- training_clean %>% filter(USERID == 16)
user17 <- training_clean %>% filter(USERID == 17)
user18 <- training_clean %>% filter(USERID == 18)

summary(user01$BUILDINGID)
summary(user02$BUILDINGID)
summary(user03$BUILDINGID)
summary(user04$BUILDINGID)
summary(user05$BUILDINGID)
summary(user06$BUILDINGID)
summary(user07$BUILDINGID)
summary(user08$BUILDINGID)
summary(user09$BUILDINGID)
summary(user10$BUILDINGID)
summary(user11$BUILDINGID)
summary(user12$BUILDINGID)
summary(user13$BUILDINGID)
summary(user14$BUILDINGID)
summary(user15$BUILDINGID)
summary(user16$BUILDINGID)
summary(user17$BUILDINGID)
summary(user18$BUILDINGID)

summary(user01$PHONEID)
summary(user02$PHONEID)
summary(user03$PHONEID)
summary(user04$PHONEID)
summary(user05$PHONEID)
summary(user06$PHONEID)
summary(user07$PHONEID)
summary(user08$PHONEID)
summary(user09$PHONEID)
summary(user10$PHONEID)
summary(user11$PHONEID)
summary(user12$PHONEID)
summary(user13$PHONEID)
summary(user14$PHONEID)
summary(user15$PHONEID)
summary(user16$PHONEID)
summary(user17$PHONEID)
summary(user18$PHONEID)


## 1.2.3.2. By phone ID
phone01 <- training_clean %>% filter(PHONEID == 1)
phone03 <- training_clean %>% filter(PHONEID == 3)
phone06 <- training_clean %>% filter(PHONEID == 6)
phone07 <- training_clean %>% filter(PHONEID == 7)
phone08 <- training_clean %>% filter(PHONEID == 8)
phone10 <- training_clean %>% filter(PHONEID == 10)
phone11 <- training_clean %>% filter(PHONEID == 11)
phone13 <- training_clean %>% filter(PHONEID == 13)
phone14 <- training_clean %>% filter(PHONEID == 14)
phone15 <- training_clean %>% filter(PHONEID == 15)
phone16 <- training_clean %>% filter(PHONEID == 16)
phone17 <- training_clean %>% filter(PHONEID == 17)
phone18 <- training_clean %>% filter(PHONEID == 18)
phone19 <- training_clean %>% filter(PHONEID == 19)
phone20 <- training_clean %>% filter(PHONEID == 20)
phone21 <- training_clean %>% filter(PHONEID == 21)
phone22 <- training_clean %>% filter(PHONEID == 22)
phone23 <- training_clean %>% filter(PHONEID == 23)
phone24 <- training_clean %>% filter(PHONEID == 24)


## 1.2.3.3. By building
building0 <- training_clean %>% filter(BUILDINGID == 0)
building1 <- training_clean %>% filter(BUILDINGID == 1)
building2 <- training_clean %>% filter(BUILDINGID == 2)

## 1.2.3.3.1. By building and floor
build0_floor0 <- building0 %>% filter(FLOOR == 0)
build0_floor1 <- building0 %>% filter(FLOOR == 1)
build0_floor2 <- building0 %>% filter(FLOOR == 2)
build0_floor3 <- building0 %>% filter(FLOOR == 3)
build1_floor0 <- building1 %>% filter(FLOOR == 0)
build1_floor1 <- building1 %>% filter(FLOOR == 1)
build1_floor2 <- building1 %>% filter(FLOOR == 2)
build1_floor3 <- building1 %>% filter(FLOOR == 3)
build2_floor0 <- building2 %>% filter(FLOOR == 0)
build2_floor1 <- building2 %>% filter(FLOOR == 1)
build2_floor2 <- building2 %>% filter(FLOOR == 2)
build2_floor3 <- building2 %>% filter(FLOOR == 3)
build2_floor4 <- building2 %>% filter(FLOOR == 4)

apply(building0[,c(1:465)],2,function(x) which(mean(x) == -105))
apply(building1[,c(1:465)],2,function(x) which(mean(x) == -105))
apply(building2[,c(1:465)],2,function(x) which(mean(x) == -105))

sum(apply(build0_floor0[,c(1:465)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build0_floor1[,c(1:465)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build0_floor2[,c(1:465)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build1_floor0[,c(1:465)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build1_floor1[,c(1:465)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build1_floor2[,c(1:465)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build2_floor0[,c(1:465)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build2_floor1[,c(1:465)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build2_floor2[,c(1:465)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build2_floor3[,c(1:465)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build2_floor4[,c(1:465)],1,function(x) length(which(x > -30 & x <= 0))))

apply(training_clean[,c(1:465)], 1, function(x) which(x > -67 & x <= -30))
apply(training_clean[,c(1:465)], 2, function(x) which(x > -67 & x <= -30))
apply(training_clean[,c(1:465)], 1, function(x) which(x > -70 & x <= -30)) 
apply(training_clean[,c(1:465)], 1, function(x) length(which(x > -30 & x <= 0)))
training_clean[apply(training_clean[,c(1:465)], 1, function(x) length(which(x > -30 & x <= 0)))]




training_clean[apply(training_clean[,c(1:520)], 1, function(x) length(which(x > -30 & x <= 0)))]
weird_obs <- training_clean[apply(training_clean[,c(1:520)], 1, function(x) length(which(x > -30 & x <= 0)))]
summary(weird_obs)

## 1.3.2. Basic plots

plot(training_clean$LONGITUDE, training_clean$LATITUDE)

plot(phone01$LONGITUDE, phone01$LATITUDE)
plot(phone03$LONGITUDE, phone03$LATITUDE)
plot(phone06$LONGITUDE, phone06$LATITUDE)
plot(phone07$LONGITUDE, phone07$LATITUDE)
plot(phone08$LONGITUDE, phone08$LATITUDE)
plot(phone10$LONGITUDE, phone10$LATITUDE)
plot(phone11$LONGITUDE, phone11$LATITUDE)
plot(phone13$LONGITUDE, phone13$LATITUDE)
plot(phone14$LONGITUDE, phone14$LATITUDE)
plot(phone16$LONGITUDE, phone16$LATITUDE)
plot(phone17$LONGITUDE, phone17$LATITUDE)
plot(phone18$LONGITUDE, phone18$LATITUDE)
plot(phone19$LONGITUDE, phone19$LATITUDE)
plot(phone22$LONGITUDE, phone22$LATITUDE)
plot(phone23$LONGITUDE, phone23$LATITUDE)
plot(phone24$LONGITUDE, phone24$LATITUDE)


plot(build0_floor0$LONGITUDE, build0_floor0$LATITUDE)
plot(build0_floor1$LONGITUDE, build0_floor1$LATITUDE)
plot(build0_floor2$LONGITUDE, build0_floor2$LATITUDE)
plot(build1_floor0$LONGITUDE, build1_floor0$LATITUDE)
plot(build1_floor1$LONGITUDE, build1_floor1$LATITUDE)
plot(build1_floor2$LONGITUDE, build1_floor2$LATITUDE)
plot(build2_floor0$LONGITUDE, build2_floor0$LATITUDE)
plot(build2_floor1$LONGITUDE, build2_floor1$LATITUDE)
plot(build2_floor2$LONGITUDE, build2_floor2$LATITUDE)
plot(build2_floor3$LONGITUDE, build2_floor3$LATITUDE)

plot(user01$LONGITUDE, user01$LATITUDE)
plot(user02$LONGITUDE, user02$LATITUDE)
plot(user03$LONGITUDE, user03$LATITUDE)
plot(user04$LONGITUDE, user04$LATITUDE)
plot(user05$LONGITUDE, user05$LATITUDE)
plot(user06$LONGITUDE, user06$LATITUDE)
plot(user07$LONGITUDE, user07$LATITUDE)
plot(user08$LONGITUDE, user08$LATITUDE)
plot(user09$LONGITUDE, user09$LATITUDE)
plot(user10$LONGITUDE, user10$LATITUDE)
plot(user11$LONGITUDE, user11$LATITUDE)
plot(user12$LONGITUDE, user12$LATITUDE)
plot(user13$LONGITUDE, user13$LATITUDE)
plot(user14$LONGITUDE, user14$LATITUDE)
plot(user15$LONGITUDE, user15$LATITUDE)
plot(user16$LONGITUDE, user16$LATITUDE)
plot(user17$LONGITUDE, user17$LATITUDE)
plot(user18$LONGITUDE, user18$LATITUDE)



#### 1.4. WAP's localization
## those on the training DF with means equal to -105 are useless
sum(apply(training_clean[,c(1:465)],2, function(x) length(which(mean(x) == -105))))

## those with means equal to -105 can't be located on that building
sum(apply(building0[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(building1[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(building2[,c(1:465)],2, function(x) length(which(mean(x) > -105))))


## same by floor
sum(apply(build0_floor0[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(build0_floor1[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(build0_floor2[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(build0_floor3[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(build1_floor0[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(build1_floor1[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(build1_floor2[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(build1_floor3[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(build2_floor0[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(build2_floor1[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(build2_floor2[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(build2_floor3[,c(1:465)],2, function(x) length(which(mean(x) > -105))))
sum(apply(build2_floor4[,c(1:465)],2, function(x) length(which(mean(x) > -105))))


## can we filter it?
# building

building0_clean <- building0[,-which(apply(building0[,c(1:465)],2, function(x) mean(x) == -105))]
building1_clean <- building1[,-which(apply(building1[,c(1:465)],2, function(x) mean(x) == -105))]
building2_clean <- building2[,-which(apply(building2[,c(1:465)],2, function(x) mean(x) == -105))]

# same by floor
b0_f0_clean <- build0_floor0[,-which(apply(build0_floor0[,c(1:465)],2, function(x) mean(x) == -105))]
b0_f1_clean <- build0_floor1[,-which(apply(build0_floor1[,c(1:465)],2, function(x) mean(x) == -105))]
b0_f2_clean <- build0_floor2[,-which(apply(build0_floor2[,c(1:465)],2, function(x) mean(x) == -105))]
b0_f3_clean <- build0_floor3[,-which(apply(build0_floor3[,c(1:465)],2, function(x) mean(x) == -105))]
b1_f0_clean <- build1_floor0[,-which(apply(build1_floor0[,c(1:465)],2, function(x) mean(x) == -105))]
b1_f1_clean <- build1_floor1[,-which(apply(build1_floor1[,c(1:465)],2, function(x) mean(x) == -105))]
b1_f2_clean <- build1_floor2[,-which(apply(build1_floor2[,c(1:465)],2, function(x) mean(x) == -105))]
b1_f3_clean <- build1_floor3[,-which(apply(build1_floor3[,c(1:465)],2, function(x) mean(x) == -105))]
b2_f0_clean <- build2_floor0[,-which(apply(build2_floor0[,c(1:465)],2, function(x) mean(x) == -105))]
b2_f1_clean <- build2_floor1[,-which(apply(build2_floor1[,c(1:465)],2, function(x) mean(x) == -105))]
b2_f2_clean <- build2_floor2[,-which(apply(build2_floor2[,c(1:465)],2, function(x) mean(x) == -105))]
b2_f3_clean <- build2_floor3[,-which(apply(build2_floor3[,c(1:465)],2, function(x) mean(x) == -105))]
b2_f4_clean <- build2_floor4[,-which(apply(build2_floor4[,c(1:465)],2, function(x) mean(x) == -105))]


# only good observations

apply(b0_f0_clean[,c(1:119)],2, function(x) length(which(x> -70 & x< -30)))
apply(b0_f1_clean[,c(1:133)],2, function(x) length(which(x> -70 & x< -30)))
apply(b0_f2_clean[,c(1:168)],2, function(x) length(which(x> -70 & x< -30)))
apply(b0_f3_clean[,c(1:162)],2, function(x) length(which(x> -70 & x< -30)))
apply(b1_f0_clean[,c(1:121)],2, function(x) length(which(x> -70 & x< -30)))
apply(b1_f1_clean[,c(1:168)],2, function(x) length(which(x> -70 & x< -30)))
apply(b1_f2_clean[,c(1:190)],2, function(x) length(which(x> -70 & x< -30)))
apply(b1_f3_clean[,c(1:146)],2, function(x) length(which(x> -70 & x< -30)))
apply(b2_f0_clean[,c(1:88)],2, function(x) length(which(x> -70 & x< -30)))
apply(b2_f1_clean[,c(1:161)],2, function(x) length(which(x> -70 & x< -30)))
apply(b2_f2_clean[,c(1:149)],2, function(x) length(which(x> -70 & x< -30)))
apply(b2_f3_clean[,c(1:172)],2, function(x) length(which(x> -70 & x< -30)))
apply(b2_f4_clean[,c(1:90)],2, function(x) length(which(x> -70 & x< -30)))

## Variance and Standard Deviation to check where are WAP's located
which(apply(b0_f0_clean,2,var) > 60)
which(apply(b0_f1_clean,2,var) > 60)
which(apply(b0_f2_clean,2,var) > 60)
which(apply(b0_f3_clean,2,var) > 60)
which(apply(b1_f0_clean,2,var) == 0)
which(apply(b1_f1_clean,2,var) == 0)
which(apply(b1_f2_clean,2,var) == 0)
which(apply(b1_f3_clean,2,var) == 0)
which(apply(b2_f0_clean,2,var) == 0)
which(apply(b2_f1_clean,2,var) == 0)
which(apply(b2_f2_clean,2,var) == 0)
which(apply(b2_f3_clean,2,var) == 0)
which(apply(b2_f4_clean,2,var) == 0)



## 
apply(m, 2, function(x) length(x[x<0]))

apply(building0_clean[,c(1:200)],2, function(x) length(x[x>-70]))
sum(apply(building0_clean[,c(1:200)],2,function(x) length(which(x > -70 & x < -30))))
sum(apply(building0_clean[,c(1:200)],2, function(x) mean(x) > -104))

building0_true <- building0_clean[,which(apply(building0_clean[,c(1:200)],2, function(x) mean(x) > -104))]
building1_true <- building1_clean[,which(apply(building1_clean[,c(1:207)],2, function(x) mean(x) > -104))] 
building2_true <- building2_clean[,which(apply(building2_clean[,c(1:203)],2, function(x) mean(x) > -104))]


boxplot(building0_true)
boxplot(building1_true)
boxplot(building2_true)


b0_f0_true <- b0_f0_clean[,which(apply(b0_f0_clean[,c(1:128)],2, function(x) mean(x) > -104))]
b0_f1_true <- b0_f1_clean[,which(apply(b0_f1_clean[,c(1:142)],2, function(x) mean(x) > -104))]
b0_f2_true <- b0_f2_clean[,which(apply(b0_f2_clean[,c(1:177)],2, function(x) mean(x) > -104))]
b0_f3_true <- b0_f3_clean[,which(apply(b0_f3_clean[,c(1:171)],2, function(x) mean(x) > -104))]
b1_f0_true <- b1_f0_clean[,which(apply(b1_f0_clean[,c(1:130)],2, function(x) mean(x) > -104))]
b1_f1_true <- b1_f1_clean[,which(apply(b1_f1_clean[,c(1:177)],2, function(x) mean(x) > -104))]
b1_f2_true <- b1_f2_clean[,which(apply(b1_f2_clean[,c(1:199)],2, function(x) mean(x) > -104))]
b1_f3_true <- b1_f3_clean[,which(apply(b1_f3_clean[,c(1:155)],2, function(x) mean(x) > -104))]
b2_f0_true <- b2_f0_clean[,which(apply(b2_f0_clean[,c(1:97)],2, function(x) mean(x) > -104))]
b2_f1_true <- b2_f1_clean[,which(apply(b2_f1_clean[,c(1:170)],2, function(x) mean(x) > -104))]
b2_f2_true <- b2_f2_clean[,which(apply(b2_f2_clean[,c(1:158)],2, function(x) mean(x) > -104))]
b2_f3_true <- b2_f3_clean[,which(apply(b2_f3_clean[,c(1:187)],2, function(x) mean(x) > -104))]
b2_f4_true <- b2_f4_clean[,which(apply(b2_f4_clean[,c(1:128)],2, function(x) mean(x) > -104))]

b0_f0_true <- cbind(b0_f0_true, b0_f0_clean$LONGITUDE)
b0_f1_true <- cbind(b0_f1_true, b0_f1_clean$LONGITUDE)
b0_f2_true <- cbind(b0_f2_true, b0_f2_clean$LONGITUDE)
b0_f3_true <- cbind(b0_f3_true, b0_f3_clean$LONGITUDE)
b1_f0_true <- cbind(b1_f0_true, b1_f0_clean$LONGITUDE)
b1_f1_true <- cbind(b1_f1_true, b1_f1_clean$LONGITUDE)
b1_f2_true <- cbind(b1_f2_true, b1_f2_clean$LONGITUDE)
b1_f3_true <- cbind(b1_f3_true, b1_f3_clean$LONGITUDE)
b2_f0_true <- cbind(b2_f0_true, b2_f0_clean$LONGITUDE)
b2_f1_true <- cbind(b2_f1_true, b2_f1_clean$LONGITUDE)
b2_f2_true <- cbind(b2_f2_true, b2_f2_clean$LONGITUDE)
b2_f3_true <- cbind(b2_f3_true, b2_f3_clean$LONGITUDE)
b2_f4_true <- cbind(b2_f4_true, b2_f4_clean$LONGITUDE)

boxplot(b0_f0_true[,c(1:49)])
boxplot(b0_f1_true[,c(1:67)])
boxplot(b0_f2_true[,c(1:73)])
boxplot(b0_f3_true[,c(1:60)])
boxplot(b1_f0_true[,c(1:91)])
boxplot(b1_f1_true[,c(1:111)])
boxplot(b1_f2_true[,c(1:99)])
boxplot(b1_f3_true[,c(1:46)])
boxplot(b2_f0_true[,c(1:44)])
boxplot(b2_f1_true[,c(1:66)])
boxplot(b2_f2_true[,c(1:60)])
boxplot(b2_f3_true[,c(1:65)])
boxplot(b2_f4_true[,c(1:58)])

apply(b0_f0_clean,2,var)
apply(b0_f0_true,2,var)

apply(b0_f1_clean,2,var)
apply(b0_f1_true,2,var)

apply(b0_f2_clean,2,var)
apply(b0_f2_true,2,var)

apply(b0_f3_clean,2,var)
apply(b0_f3_true,2,var)

apply(b1_f0_clean,2,var)
apply(b1_f0_true,2,var)

apply(b1_f1_clean,2,var)
apply(b1_f1_true,2,var)

apply(b1_f2_clean,2,var)
apply(b1_f2_true,2,var)

apply(b1_f3_clean,2,var)
apply(b1_f3_true,2,var)

apply(b2_f0_clean,2,var)
apply(b2_f0_true,2,var)

apply(b2_f1_clean,2,var)
apply(b2_f1_true,2,var)

apply(b2_f2_clean,2,var)
apply(b2_f2_true,2,var)

apply(b2_f3_clean,2,var)
apply(b2_f3_true,2,var)

apply(b2_f4_clean,2,var)
apply(b2_f4_true,2,var)

sum(apply(b0_f0_true[,c(1:49)],2, function(x) max(x) > -30))
sum(apply(b0_f1_true[,c(1:67)],2, function(x) max(x) > -30))
sum(apply(b0_f2_true[,c(1:73)],2, function(x) max(x) > -30))
sum(apply(b0_f3_true[,c(1:60)],2, function(x) max(x) > -30))
sum(apply(b1_f0_true[,c(1:91)],2, function(x) max(x) > -30))
sum(apply(b1_f1_true[,c(1:111)],2, function(x) max(x) > -30))
sum(apply(b1_f2_true[,c(1:99)],2, function(x) max(x) > -30))
sum(apply(b1_f3_true[,c(1:46)],2, function(x) max(x) > -30))
sum(apply(b2_f0_true[,c(1:44)],2, function(x) max(x) > -30))
sum(apply(b2_f1_true[,c(1:66)],2, function(x) max(x) > -30))
sum(apply(b2_f2_true[,c(1:60)],2, function(x) max(x) > -30))
sum(apply(b2_f3_true[,c(1:65)],2, function(x) max(x) > -30))
sum(apply(b2_f4_true[,c(1:58)],2, function(x) max(x) > -30))

sum(apply(b2_f3_true[,c(1:56)],1, function(x) max(x) > -30))
sum(apply(b2_f4_true[,c(1:49)],1, function(x) max(x) > -30))

apply(b2_f3_true[,c(1:56)],1, function(x) max(x) > -30)

b2_f3_true.1 <- b2_f3_true[which(apply(b2_f3_true[,c(1:65)],1, function(x) max(x) <= -30)),]
b2_f4_true.1 <- b2_f4_true[which(apply(b2_f4_true[,c(1:58)],1, function(x) max(x) <= -30)),]

b2_f3_wrong <- b2_f3_true[which(apply(b2_f3_true[,c(1:65)],1, function(x) max(x) > -30)),]
b2_f4_wrong <- b2_f4_true[which(apply(b2_f4_true[,c(1:58)],1, function(x) max(x) > -30)),]


summary(b2_f3_wrong)
summary(b2_f4_wrong)
View(b2_f3_wrong)

unique

b2_f3_true.2 <- b2_f3_clean %>% filter(!(USERID == 6)) %>% filter(!(USERID == 14))
b2_f4_true.2 <- b2_f4_clean %>% filter(!(USERID == 3)) %>% filter(!(USERID == 6))

boxplot(b2_f3_true.2[,c(1:178)])
boxplot(b2_f4_true.2[,c(1:119)])

sum(apply(b2_f0_true[,c(1:44)],1, function(x) max(x) > -30))
sum(apply(b2_f1_true[,c(1:66)],1, function(x) max(x) > -30))
sum(apply(b2_f2_true[,c(1:60)],1, function(x) max(x) > -30))
sum(apply(b2_f3_true[,c(1:65)],1, function(x) max(x) > -30))
sum(apply(b2_f4_true[,c(1:58)],1, function(x) max(x) > -30))
sum(apply(b2_f3_true.1[,c(1:65)],1, function(x) max(x) > -30))
sum(apply(b2_f4_true.1[,c(1:58)],1, function(x) max(x) > -30))
sum(apply(b2_f3_true.2[,c(1:178)],1, function(x) max(x) > -30))
sum(apply(b2_f4_true.2[,c(1:119)],1, function(x) max(x) > -30))

sum(apply(user03[,c(1:520)],1, function(x) max(x) > -30))
sum(apply(user06[,c(1:520)],1, function(x) max(x) > -30))
sum(apply(user14[,c(1:520)],1, function(x) max(x) > -30))

unique(b2_f3_wrong$USERID)
unique(b2_f4_wrong$USERID)
unique(b2_f3_wrong$PHONEID)
unique(b2_f4_wrong$PHONEID)




#### Training models

parallel::detectCores()
cl<- makeCluster(2)
registerDoParallel(cl)
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 3, allowParallel = TRUE)

sample_building <- createDataPartition(training_clean$BUILDINGID, p = 0.05, list = FALSE)
sample_floor <- createDataPartition(training_clean$BUILDFLOOR, p = 0.05, list = FALSE)
sample_longitude <- createDataPartition(training_clean$LONGITUDE, p = 0.05, list = FALSE)
sample_latitude <- createDataPartition(training_clean$LATITUDE, p = 0.05, list = FALSE)

training_floor <- training_clean[sample_floor,]
training_longitude <- training_clean[sample_longitude,]
training_latitude <- training_clean[sample_latitude,]

### Decision tree before starting to model 
BuildTree <- rpart(BUILDINGID~., data=training_highsd5, method = "class", control = list(maxdepth = 15))
summary(BuildTree)
rpart.plot(BuildTree, cex = .5)

### Models to predict BUILDING ID

## Random Forest
system.time()
train_RF1_building <- randomForest(BUILDINGID~.-LONGITUDE -LATITUDE -SPACEID 
                                   -FLOOR -USERID 
                                   -RELATIVEPOSITION -PHONEID -TIMESTAMP, 
                                   data = training_highsd5, ntrees=5, 
                                   trControl = fitControl)
train_RF1.1_building <- randomForest(BUILDINGID~.-LONGITUDE -LATITUDE -SPACEID 
                                     -FLOOR -USERID -RELATIVEPOSITION -PHONEID 
                                     -TIMESTAMP, data = training_clean, 
                                     ntrees=5, trControl = fitControl)


## KNN

# with only high SD deviation observations dataset
train_KNN1_building <- train(BUILDINGID~.-LONGITUDE -LATITUDE -SPACEID -FLOOR -USERID -RELATIVEPOSITION -PHONEID -TIMESTAMP, method = "knn",data = training_highsd5,
                             trControl = fitControl)

# with whole dataset

train_KNN1.1_building <- train(y = training_clean$BUILDINGID, x = training_clean[1:312],
                               method = "knn", trControl = fitControl)
Pred_KNN1_building <- predict(train_KNN1.1_building, validation_clean)
confusionMatrix(Pred_KNN1_building, validation_clean$BUILDINGID)
save(train_KNN1.1_building, file = "KNNBuilding.rda")

### Models to predict FLOOR ID
system.time(expr = TRUE)

## Model comparison by predicting samples
## KNN
# performance with sample: acc: 0.91 kappa: 0.90 k=5
train_KNN1_floor_beta <- train(BUILDFLOOR~. -LONGITUDE -LATITUDE -SPACEID -FLOOR -USERID
                               -RELATIVEPOSITION -PHONEID -TIMESTAMP, method = "knn",
                               data = training_floor, trControl = fitControl)

# SVM Linear    acc: 0.97 kappa: 0.97
train_SVML1_1_floor_beta <- train(BUILDFLOOR~. -BUILDINGID -LONGITUDE -LATITUDE -SPACEID 
                                  -FLOOR -USERID-RELATIVEPOSITION -PHONEID -TIMESTAMP, 
                                  method = "svmLinear", data = training_floor, 
                                  preProcess=c("center","scale"),trControl = fitControl)

# Random Forest    OOB estimate of  error rate: 5.14% (5t)/ 4.61% (20t) / 4.72% (50t)
train_RF1_floor_beta <- randomForest(BUILDFLOOR~. -LONGITUDE -LATITUDE -SPACEID 
                                     -FLOOR -USERID -RELATIVEPOSITION -PHONEID 
                                     -TIMESTAMP -BUILDINGID, data = training_floor, 
                                     ntrees=20, trControl = fitControl)

# Gradient Boosted  TOO SLOW
train_GBM1_1_floor_beta <- train(BUILDFLOOR~. -BUILDINGID -LONGITUDE -LATITUDE -SPACEID 
                                  -FLOOR -USERID-RELATIVEPOSITION -PHONEID -TIMESTAMP, 
                                  method = "gbm", data = training_floor, 
                                  trControl = fitControl)

# SVM Polynomial TOO SLOW
train_SVMPol_1_floor_beta <- train(BUILDFLOOR~. -BUILDINGID -LONGITUDE -LATITUDE -SPACEID 
                                   -FLOOR -USERID-RELATIVEPOSITION -PHONEID -TIMESTAMP, 
                                   method = "svmPoly", data = training_floor)



#### Chosen model 1: SVM Linear
## now with whole training dataset
system.time()
train_SVML1_1_floor <- train(y = training_clean$BUILDFLOOR, x = training_clean[1:312], 
                              method = "svmLinear", preProcess = c("center","scale"),
                             trControl = fitControl)
train_SVML1_1_floor
Pred_SVML_floor <- predict(train_SVML1_1_floor, newdata = validation_clean)
Pred_SVML_floor
confusionMatrix(Pred_SVML_floor, validation_clean$BUILDFLOOR)

#### Chosen model 2: Random Forest
## 20 trees: acc:0.91 kappa: 0.90
train_RF1_floor <- randomForest(y = training_clean$BUILDFLOOR, x = training_clean[1:312], 
                                     ntrees=20, trControl = fitControl)

Pred_RF1_20_floor <- predict(train_RF1_floor, validation_clean)
confusionMatrix(Pred_RF1_20_floor, validation_clean$BUILDFLOOR)
save(train_RF1_floor, file = "RF1_floor.rda")

## 50 trees: acc: 0.89 kappa: 0.87
train_RF1_50_floor <- randomForest(y = training_clean$BUILDFLOOR, x = training_clean[,1:312], 
                                ntrees=50, trControl = fitControl)

Pred_RF1_50_floor <- predict(train_RF1_50_floor, validation_clean)
confusionMatrix(Pred_RF1_50_floor, validation_clean$BUILDFLOOR)



#### Models to predict LONGITUDE
### Sampling
### KNN    RMSE: 21.5  Rsq: 0.97 MAE: 9.78 (training)
###        RMSE: 26.8  Rsq: 0.95 MAE: 12.54 (validated)
train_KNN1_long_beta <- train(y = training_longitude$LONGITUDE, x = training_longitude[,1:312],
                              method = "knn", preProcess = c("center","scale"))
Pred_KNN_long_beta <- predict(train_KNN1_long_beta, validation_clean)
postResample(Pred_KNN_long_beta, validation_clean$LONGITUDE)

### SVM Linear   RMSE: 39.99 Rsq: 0.90  MAE: 27.47 (training)
###              RMSE: NA    Rsq: NA    MAE: Na (validation)
train_SVML_long_beta <- train(y = training_longitude$LONGITUDE, x = training_longitude[,1:312],
                              method = "svmLinear", preProcess = c("center","scale"))
Pred_SVML_long_beta <- predict(train_SVML_long_beta, validation_clean)
postResample(Pred_SVML_long_beta, validation_clean$LONGITUDE)

#### Chosen model: KNN
## Train performance:       RMSE: 8.39 Rsq: 0.99 MAE: 2.77
## Validated performance:   RMSE: 20.18 Rsq: 0.97 MAE: 7.94
train_KNN1_longitude <- train(y = training_clean$LONGITUDE, x = training_clean[,1:312],
                              method = "knn", preProcess = c("center","scale"),
                              trControl = fitControl)
Pred_KNN_longitude <-  predict(train_KNN1_longitude, validation_clean)
postResample(Pred_KNN_longitude, validation_clean$LONGITUDE)
save(train_KNN1_longitude, file = "KNNLongitude.rda")

#### Models to predict LATITUDE
### Sampling
### KNN   RMSE: 19.96 Rsq: 0.97 MAE: 9.4 (training)
###       RMSE: 17.65 Rsq: 0.937 MAE: 9.74 (validation)
train_KNN1_lat_beta <- train(y = training_latitude$LATITUDE, x = training_latitude[,1:312],
                              method = "knn", preProcess = c("center","scale"))
Pred_KNN_lat_beta <- predict(train_KNN1_lat_beta, validation_clean)
postResample(Pred_KNN_lat_beta, validation_clean$LATITUDE)

### SVM Linear   RMSE: 24.71 Rsq: 0.87 MAE: 17.27 (train)
train_SVML_lat_beta <- train(y = training_latitude$LATITUDE, x = training_latitude[,1:312],
                              method = "svmLinear")
Pred_SVML_lat_beta <- predict(train_SVML_lat_beta, validation_clean)
postResample(Pred_SVML_lat_beta, validation_clean$LATITUDE)

### Chosen model: KNN
### RMSE: 5.91 RSq: 0.99 MAE: 2.29 (training)
### RMSE: 15.79 RSq: 0.95 MAE: 7.57 (validation)
train_KNN1_latitude <- train(y = training_clean$LATITUDE, x = training_clean[,1:312],
                             method = "knn", preProcess = c("center","scale"),
                             trControl = fitControl)
Pred_KNN_latitude <- predict(train_KNN1_latitude, validation_clean)
postResample(Pred_KNN_latitude, validation_clean$LATITUDE)
save(train_KNN1_latitude, file = "KNNLatitude.rda")
