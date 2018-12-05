

#### WiFi Locationing Project

# Goal of the project:



quim <- -7
ignacio <- 7

pepito <- function(quim, ignacio){
  Joana<- quim + ignacio
  returnValue(Joana)
}

nuñez<- 3
llambordins<- 0.2

joangaspart <- function(nuñez, llambordins){
  nuñez*llambordins
  
}


apply(user11[1:520], 1, function(joangaspart) which ((x) >= joangaspart))

#### 0. Environment

### 0.1 Working directory

setwd("C:/Users/User/Desktop/DataAnalytics/4.2. Wifi Locationing")

### 0.2. Packages required

library(caret)
library(readr)
library(dplyr)

#### 1. Data exploration

### 1.1. Downloading data

## 1.1.1. Downloading training data
training <- read_csv("trainingData.csv")
training <- as.data.frame(training)
training[1:529] <- lapply(training[1:529], as.numeric)

dades[2:13] <- lapply(dades[2:13], as.numeric)


## 1.1.2. Downloading validation data
validation <- read_csv("validationData.csv")
validation <- as.data.frame(validation)
validation[1:529] <- lapply(validation[1:529], as.numeric)

### 1.2. Data observation

head(training)
str(training$WAP099)
summary(training)
sum(is.na(training))

head(validation)
sum(is.na(validation))

training_clean <- training
validation_clean <- validation

training_clean[training_clean == "100"] <- -150
validation_clean[validation_clean == "100"] <- -150


# unique values on the supposed-to-be label variables
unique(training$SPACEID)
unique(training$RELATIVEPOSITION)


## 1.2.1. Basic statistics
summary(training_clean$FLOOR)
summary(training_clean$BUILDINGID)
summary(training_clean$SPACEID)
summary(training_clean$USERID)

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

summary(user00)

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

building0 <- training_clean %>% filter(BUILDINGID == 0)
building1 <- training_clean %>% filter(BUILDINGID == 1)
building2 <- training_clean %>% filter(BUILDINGID == 2)

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

apply(building0[,c(1:520)],2,function(x) which(mean(x) == -150))
apply(building1[,c(1:520)],2,function(x) which(mean(x) == -150))
apply(building2[,c(1:520)],2,function(x) which(mean(x) == -150))

sum(apply(build0_floor0[,c(1:520)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build0_floor1[,c(1:520)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build0_floor2[,c(1:520)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build1_floor0[,c(1:520)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build1_floor1[,c(1:520)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build1_floor2[,c(1:520)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build2_floor0[,c(1:520)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build2_floor1[,c(1:520)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build2_floor2[,c(1:520)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build2_floor3[,c(1:520)],1,function(x) length(which(x > -30 & x <= 0))))
sum(apply(build2_floor4[,c(1:520)],1,function(x) length(which(x > -30 & x <= 0))))

apply(training_clean[,c(1:520)], 1, function(x) which(x > -67 & x <= -30))
apply(training_clean[,c(1:520)], 2, function(x) which(x > -67 & x <= -30))
apply(training_clean[,c(1:520)], 1, function(x) which(x > -70 & x <= -30)) 
apply(training_clean[,c(1:520)], 1, function(x) length(which(x > -30 & x <= 0)))
training_clean[apply(training_clean[,c(1:520)], 1, function(x) length(which(x > -30 & x <= 0)))]




training_clean[apply(training_clean[,c(1:520)], 1, function(x) length(which(x > -30 & x <= 0)))]
weird_obs <- training_clean[apply(training_clean[,c(1:520)], 1, function(x) length(which(x > -30 & x <= 0)))]
summary(weird_obs)

## 1.2.2. Basic plots

training_clean %>% filter(PHONEID == 1) %>% select(1:520) %>% as.matrix() %>% heatmap()
training_clean %>% filter(PHONEID == 11) %>% select(1:520) %>% plot()

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
