

#### WiFi Locationing Project

# Goal of the project:



#### 0. Environment ----
#### 0.1 Working directory ----

setwd("C:/Users/User/Desktop/DataAnalytics/4.2. Wifi Locationing")

#### 0.2. Packages required ----

install.packages(mvabund)
library(caret)
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(class)
library(e1071)
library(doParallel)


options(scipen=999)       ## to avoid scientific numeration

var.p = function(x){var(x)*(length(x)-1)/length(x)}


#### 1. Data exploration ----

#### 1.1. Downloading data ----

#### 1.1.1. Downloading training data ----
training <- read_csv("trainingData.csv")
training <- as.data.frame(training)
training[1:529] <- lapply(training[1:529], as.numeric)
training[523:528] <- lapply(training[523:528], as.factor)

#### 1.1.2. Downloading validation data ----
validation <- read_csv("validationData.csv")
validation <- as.data.frame(validation)
validation[1:529] <- lapply(validation[1:529], as.numeric)
validation[523:528] <- lapply(validation[523:528], as.factor)

#### 1.2. Data observation ----
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


#### 1.2.1. Basic statistics ----
summary(training_clean$FLOOR)
summary(training_clean$BUILDINGID)
summary(training_clean$SPACEID)
summary(training_clean$USERID)


#### 1.2.2. Zero variance / Near zero variance ----

nearZeroVar(training_clean, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE,
            names = TRUE)

#### Data cleaning ----

#### how many WAP's with zero variance? Thus, how many useless WAP's? ----
names(which(apply(training_clean,2,var) == 0))

#### remove every useless WAP ----
which(apply(training_clean,2,max) == -105)
which(apply(training_clean,2,var) == 0 )
training_clean <- training_clean[,-which(apply(training_clean,2,var) == 0)]

which(apply(validation_clean,2,max) == -105)
which(apply(validation_clean,2,var) == 0 )
validation_clean <- validation_clean[,-which(apply(validation_clean[1:520],2,var) == 0)]

common_col <- intersect(names(training_clean[,1:465]), names(validation_clean[,1:367]))

training_clean <- cbind(training_clean[,common_col],training_clean[466:475])
validation_clean <- cbind(validation_clean[,common_col], validation_clean[368:377])

#### paste building and floor ----
training$BUILDFLOOR <- paste0(training$BUILDINGID, training$FLOOR)
training_clean$BUILDFLOOR <- paste0(training_clean$BUILDINGID, training_clean$FLOOR)
validation$BUILDFLOOR <- paste0(validation$BUILDINGID, validation$FLOOR)
validation_clean$BUILDFLOOR <- paste0(validation_clean$BUILDINGID, validation_clean$FLOOR)
training$BUILDFLOOR <- as.factor(training$BUILDFLOOR)
validation$BUILDFLOOR <- as.factor(validation$BUILDFLOOR)
training_clean$BUILDFLOOR <- as.factor(validation_clean$BUILDFLOOR)
validation_clean$BUILDFLOOR <- as.factor(validation_clean$BUILDFLOOR)

#### remove user 6 observations (almost half of them are wrong) ----
training_clean <- training_clean %>% filter(!(USERID == 6))
validation_clean <- validation_clean %>% filter(1(USERID == 6))

#### remove all wrong observations ----
training_clean <- training_clean[which(apply(building2_clean[,c(1:102)],1, function(x) max(x) <= -30)),]
which(apply(training_clean[,c(1:102)],1, function(x) max(x) <= -30))

wrong_obs <- training_clean[which(apply(training_clean[,c(1:312)], 1, function(x) max(x) > -30)),]

plot(wrong_obs$LONGITUDE, wrong_obs$LATITUDE,
     xlab = "LONGITUDE",
     ylab = "LATITUDE",
     main = "Wrong Observations")

ggplot(wrong_obs, aes(LONGITUDE, LATITUDE))
       + geom_point()


ggplot(wrong_obs, aes(USERID)) + geom_bar()

#### how many WAP's with low standard deviation? ----
training_highsd1 <- cbind(training_clean[,which(apply(training_clean[,1:464],2,sd) > 1)], training_clean[,466:475])

training_highsd2 <- cbind(training_clean[,which(apply(training_clean[,1:464],2,sd) > 2)], training_clean[,466:475])

training_highsd5 <- cbind(training_clean[,which(apply(training_clean[,1:312],2,sd) > 5)], training_clean[,313:322])
validation_highsd5 <- cbind(validation_clean[,which(apply(validation_clean[,1:312],2,sd) > 5)], validation_clean[,313:322])

common_sd5 <- intersect(names(training_highsd5[,1:174]), names(validation_highsd5[,1:166]))

training_highsd5 <- cbind(training_highsd5[,common_sd5],training_highsd5[175:184])
validation_highsd5 <- cbind(validation_highsd5[,common_sd5], validation_highsd5[167:176])


building0_highsd5 <- training_highsd5 %>% filter(BUILDINGID == 0)
building1_highsd5 <- training_highsd5 %>% filter(BUILDINGID == 1) 
building2_highsd5 <- training_highsd5 %>% filter(BUILDINGID == 2)

valid_b0_highsd5 <- validation_highsd5 %>% filter(BUILDINGID == 0)
valid_b1_highsd5 <- validation_highsd5 %>% filter(BUILDINGID == 1)
valid_b2_highsd5 <- validation_highsd5 %>% filter(BUILDINGID == 2)


#### 1.2.3. Subsetting data ----
#### 1.2.3.1. By user ----

users <- cbind.data.frame(
            c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),
            c((sum((training$USERID) == 1)),
           (sum((training$USERID) == 2)),
           (sum((training$USERID) == 3)),
           (sum((training$USERID) == 4)),
           (sum((training$USERID) == 5)),
           (sum((training$USERID) == 6)),
           (sum((training$USERID) == 7)),
           (sum((training$USERID) == 8)),
           (sum((training$USERID) == 9)),
           (sum((training$USERID) == 10)),
           (sum((training$USERID) == 11)),
           (sum((training$USERID) == 12)),
           (sum((training$USERID) == 13)),
           (sum((training$USERID) == 14)),
           (sum((training$USERID) == 15)),
           (sum((training$USERID) == 16)),
           (sum((training$USERID) == 17)),
           (sum((training$USERID) == 18))))

names(users)[1] <- "UserID"
names(users)[2] <- "Observations"

ggplot(data = users, aes(x=UserID, y=Observations)) +
  geom_bar(stat = "identity")


phone <- cbind.data.frame(
          c(1,3,6,7,8,10,11,13,14,16,17,18,19,22,23,24),
          c((sum((training$PHONEID) == 1)),
            (sum((training$PHONEID) == 3)),
            (sum((training$PHONEID) == 6)),
            (sum((training$PHONEID) == 7)),
            (sum((training$PHONEID) == 8)),
            (sum((training$PHONEID) == 10)),
            (sum((training$PHONEID) == 11)),
            (sum((training$PHONEID) == 13)),
            (sum((training$PHONEID) == 14)),
            (sum((training$PHONEID) == 16)),
            (sum((training$PHONEID) == 17)),
            (sum((training$PHONEID) == 18)),
            (sum((training$PHONEID) == 19)),
            (sum((training$PHONEID) == 22)),
            (sum((training$PHONEID) == 23)),
            (sum((training$PHONEID) == 24)))
)

names(phone)[1] <- "PhoneID"
names(phone)[2] <- "Observations"


ggplot(data = phone, aes(x=PhoneID, y=Observations)) +
  geom_bar(stat = "identity")

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


#### 1.2.3.2. By phone ID ----
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


#### 1.2.3.3. By building ----
building0 <- training %>% filter(BUILDINGID == 0)
building1 <- training %>% filter(BUILDINGID == 1)
building2 <- training %>% filter(BUILDINGID == 2)

#### 1.2.3.3.1. By building and floor ----
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

validation_building0 <- validation_clean %>% filter(BUILDINGID == 0)
validation_building1 <- validation_clean %>% filter(BUILDINGID == 1)
validation_building2 <- validation_clean %>% filter(BUILDINGID == 2)

val_b0_f0 <- validation_building0 %>% filter(FLOOR == 0)
val_b0_f1 <- validation_building0 %>% filter(FLOOR == 1)
val_b0_f2 <- validation_building0 %>% filter(FLOOR == 2)
val_b0_f3 <- validation_building0 %>% filter(FLOOR == 3)
val_b1_f0 <- validation_building1 %>% filter(FLOOR == 0)
val_b1_f1 <- validation_building1 %>% filter(FLOOR == 1)
val_b1_f2 <- validation_building1 %>% filter(FLOOR == 2)
val_b1_f3 <- validation_building1 %>% filter(FLOOR == 3)
val_b2_f0 <- validation_building2 %>% filter(FLOOR == 0)
val_b2_f1 <- validation_building2 %>% filter(FLOOR == 1)
val_b2_f2 <- validation_building2 %>% filter(FLOOR == 2)
val_b2_f3 <- validation_building2 %>% filter(FLOOR == 3)
val_b2_f4 <- validation_building2 %>% filter(FLOOR == 4)


plot(build0_floor0$LONGITUDE, build0_floor0$LATITUDE)
plot(build0_floor1$LONGITUDE, build0_floor1$LATITUDE)
plot(build0_floor2$LONGITUDE, build0_floor2$LATITUDE)
plot(build0_floor3$LONGITUDE, build0_floor3$LATITUDE)
plot(build1_floor0$LONGITUDE, build1_floor0$LATITUDE)
plot(build1_floor1$LONGITUDE, build1_floor1$LATITUDE)
plot(build1_floor2$LONGITUDE, build1_floor2$LATITUDE)
plot(build1_floor3$LONGITUDE, build1_floor3$LATITUDE)
plot(build2_floor0$LONGITUDE, build2_floor0$LATITUDE)
plot(build2_floor1$LONGITUDE, build2_floor1$LATITUDE)
plot(build2_floor2$LONGITUDE, build2_floor2$LATITUDE)
plot(build2_floor3$LONGITUDE, build2_floor3$LATITUDE)
plot(build2_floor4$LONGITUDE, build2_floor4$LATITUDE)

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

#### 1.3.2. Basic plots ----

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



#### 1.4. WAP's localization ----
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




#### Training models ----

parallel::detectCores()
cl<- makeCluster(2)
registerDoParallel(cl)
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 3, allowParallel = TRUE)

sample_building <- createDataPartition(training_clean$BUILDINGID, p = 0.05, list = FALSE)
sample_floor <- createDataPartition(training_clean$BUILDFLOOR, p = 0.05, list = FALSE)
sample_longitude <- createDataPartition(training_clean$LONGITUDE, p = 0.05, list = FALSE)
sample_latitude <- createDataPartition(training_clean$LATITUDE, p = 0.05, list = FALSE)

training_building <- training_clean[sample_building,]
training_floor <- training_clean[sample_floor,]
training_longitude <- training_clean[sample_longitude,]
training_latitude <- training_clean[sample_latitude,]

#### Decision tree before starting to model ----
BuildTree <- rpart(BUILDINGID~., data=training_highsd5, method = "class", control = list(maxdepth = 15))
summary(BuildTree)
rpart.plot(BuildTree, cex = .5)

#### Models to predict BUILDING ID ----

## Random Forest
system.time()

train_RF1_building_beta <- randomForest(y = training_building$BUILDINGID,
                                        x = training_building[,1:312],
                                        ntrees = 20)
Pred_RF1_building_beta <- predict(train_RF1_building_beta, training_building)

train_RF1_building <- randomForest(BUILDINGID~.-LONGITUDE -LATITUDE -SPACEID 
                                   -FLOOR -USERID 
                                   -RELATIVEPOSITION -PHONEID -TIMESTAMP, 
                                   data = training_highsd5, ntrees=5, 
                                   trControl = fitControl)
train_RF1.1_building <- randomForest(y = training_clean$BUILDINGID,
                                     x = training_clean[,1:312],
                                     ntrees = 20
)
Pred_RF1.1_building <- predict(train_RF1.1_building, validation_clean)
cm_RF1.1_building <- confusionMatrix(Pred_RF1.1_building, validation_clean$BUILDINGID)

plot(cm_RF1.1_building$table)


train_RF2_building <- randomForest(y = training_highsd5$BUILDINGID,
                                   x = training_highsd5[,1:136],
                                   ntrees = 20
  
)
Pred_RF2_building <- predict(train_RF2_building, validation_clean)
cm_RF2_building <- confusionMatrix(Pred_RF2_building, validation_clean$BUILDINGID)

plot(cm_RF2_building$table)

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

plot(Pred_KNN1_building)
plot(validation_clean$BUILDINGID)

train_KNNsd5_building <- train(y = training_highsd5$BUILDINGID, x = training_highsd5[1:136],
                               method = "knn", trControl = fitControl)
Pred_KNNsd5_building <- predict(train_KNNsd5_building, validation_highsd5)
confusionMatrix(Pred_KNNsd5_building, validation_highsd5$BUILDINGID)
save(train_KNNsd5_building, file = "KNNBuilding2.rda")


#### Models to predict FLOOR ID ----
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

validation_clean_floor <- validation_clean
validation_clean_floor$PredictedFLOOR <- cbind(validation_clean_floor, Pred_RF1_20_floor)

par(mfrow=c(1,2))
plot(validation_clean$BUILDFLOOR,
     xlab="Building-Floor",
     main="Real observations ")
plot(Pred_RF1_20_floor,
     xlab="Building-Flor",
     main="Predicted observations")

par(mfrow=c(1,1))

## 50 trees: acc: 0.89 kappa: 0.87
train_RF1_50_floor <- randomForest(y = training_clean$BUILDFLOOR, x = training_clean[,1:312], 
                                ntrees=50, trControl = fitControl)

Pred_RF1_50_floor <- predict(train_RF1_50_floor, validation_clean)
confusionMatrix(Pred_RF1_50_floor, validation_clean$BUILDFLOOR)


## Chosen model 3: KNN

train_KNN1_floor <- train(y = training_clean$BUILDFLOOR,
                          x = training_clean[,1:312]
                          , method = "knn",
                          trControl = fitControl)
Pred_KNN1_floor <- predict(train_KNN1_floor, validation_clean)
confusionMatrix(Pred_KNN1_floor, validation_clean$BUILDFLOOR)

#### Models to predict LONGITUDE ----
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


#### Models to predict LATITUDE ----
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

plot(Pred_KNN_longitude, Pred_KNN_latitude,
     xlab="Longitude",
     ylab="Latitude",
     main="Predictions")
plot(validation_clean$LONGITUDE, validation_clean$LATITUDE,
     xlab="Longitude",
     ylab="Latitude",
     main="Real")



#### Filtering Training dataset by BUILDING ----

building0_clean <- training_clean %>% filter(BUILDINGID == 0)
building1_clean <- training_clean %>% filter(BUILDINGID == 1) 
building2_clean <- training_clean %>% filter(BUILDINGID == 2)
validation_building0 <- validation_clean %>% filter(BUILDINGID == 0)
validation_building1 <- validation_clean %>% filter(BUILDINGID == 1)
validation_building2 <- validation_clean %>% filter(BUILDINGID == 2)

building0_clean <- building0_clean[,-which(apply(building0_clean,2,var) == 0)]
building1_clean <- building1_clean[,-which(apply(building1_clean,2,var) == 0)]
building2_clean <- building2_clean[,-which(apply(building2_clean,2,var) == 0)]
validation_building0 <- validation_building0[,-which(apply(validation_building0,2,var) == 0)]
validation_building1 <- validation_building1[,-which(apply(validation_building1,2,var) == 0)]
validation_building2 <- validation_building2[,-which(apply(validation_building2,2,var) == 0)]


common_b0clean <- intersect(names(building0_clean[,1:145]),
                            names(validation_building0[,1:138]))
common_b1clean <- intersect(names(building1_clean[,1:167]),
                            names(validation_building1[,1:153]))
common_b2clean <- intersect(names(building2_clean[,1:121]),
                            names(validation_building2[,1:110]))
building0_clean <- cbind(building0_clean[,common_b0clean],building0_clean[146:155])
validation_building0 <- cbind(validation_building0[,common_b0clean], validation_building0[139:148])
building1_clean <- cbind(building1_clean[,common_b1clean],building1_clean[168:177])
validation_building1 <- cbind(validation_building1[,common_b1clean], validation_building1[154:163])
building2_clean <- cbind(building2_clean[,common_b2clean],building2_clean[122:131])
validation_building2 <- cbind(validation_building2[,common_b2clean], validation_building2[111:120])


levels(building0_clean$BUILDFLOOR)
building0_clean$BUILDFLOOR <- factor(building0_clean$BUILDFLOOR)
levels(building0_clean$BUILDFLOOR)
levels(building1_clean$BUILDFLOOR)
building1_clean$BUILDFLOOR <- factor(building1_clean$BUILDFLOOR)
levels(building1_clean$BUILDFLOOR)
levels(building2_clean$BUILDFLOOR)
building2_clean$BUILDFLOOR <- factor(building2_clean$BUILDFLOOR)
levels(building2_clean$BUILDFLOOR)

levels(validation_building0$BUILDFLOOR)
validation_building0$BUILDFLOOR <- factor(validation_building0$BUILDFLOOR)
levels(validation_building0$BUILDFLOOR)

levels(validation_building1$BUILDFLOOR)
validation_building1$BUILDFLOOR <- factor(validation_building1$BUILDFLOOR)
levels(validation_building1$BUILDFLOOR)

levels(validation_building2$BUILDFLOOR)
validation_building2$BUILDFLOOR <- factor(validation_building2$BUILDFLOOR)
levels(validation_building2$BUILDFLOOR)


#### Iterating models to predict FLOOR ----
train_RF_bfloor0 <- randomForest(y=building0_clean$BUILDFLOOR, x = building0_clean[1:312],
                                 ntrees=20, trControl = fitControl)
Pred_RF_bfloor0 <- predict(train_RF_bfloor0, validation_building0)
confusionMatrix(Pred_RF_bfloor0, validation_building0$BUILDFLOOR)

train_RF_bfloor1 <- randomForest(y=building1_clean$BUILDFLOOR, x = building1_clean[1:312],
                                 ntrees=20, trControl = fitControl, mtry = TRUE)
Pred_RF_bfloor1 <- predict(train_RF_bfloor1, validation_building1)
confusionMatrix(Pred_RF_bfloor1, validation_building1$BUILDFLOOR)
plot(Pred_RF_bfloor1)
plot(validation_building1$BUILDFLOOR)

train_RF_bfloor2 <- randomForest(y=building2_clean$BUILDFLOOR, x = building2_clean[1:312],
                                 ntrees=20, trControl = fitControl)
Pred_RF_bfloor2 <- predict(train_RF_bfloor2, validation_building2)
confusionMatrix(Pred_RF_bfloor2, validation_building2$BUILDFLOOR)
plot(Pred_RF_bfloor2)
plot(validation_building2$BUILDFLOOR)

Pred_RF1_20_floor <- predict(train_RF1_floor, validation_clean)
confusionMatrix(Pred_RF1_20_floor, validation_clean$BUILDFLOOR)


### Error metrics reduction attempt with high SD WAP's datasets

### FLOOR with high SD dataset
train_RFsd5_floor <- randomForest(y = training_highsd5$BUILDFLOOR, x = training_highsd5[1:136], 
                                ntrees=20, trControl = fitControl)

Pred_RFsd5_floor <- predict(train_RFsd5_floor, validation_highsd5)
confusionMatrix(Pred_RFsd5_floor, validation_highsd5$BUILDFLOOR)


train_RFwhole_floor <- randomForest(y = training$BUILDFLOOR, x = training[1:520], 
                                  ntrees=20, trControl = fitControl)

Pred_RFwhole_floor <- predict(train_RFwhole_floor, validation)
confusionMatrix(Pred_RFwhole_floor, validation$BUILDFLOOR)

#### Iterating models to predict LONGITUDE / LATITUDE ----
### LONGITUDE / LATITUDE with only high SD WAP's
train_KNNsd5_long <- train(y = training_highsd5$LONGITUDE, x = training_highsd5[,1:136],
                          method = "knn", preProcess = c("center","scale"),
                          trControl = fitControl)
Pred_KNNsd5_long <- predict(train_KNNsd5_long, validation_highsd5)
postResample(Pred_KNNsd5_long, validation_highsd5$LONGITUDE)


train_KNNsd5_lat <- train(y = training_highsd5$LATITUDE, x = training_highsd5[,1:136],
                             method = "knn", preProcess = c("center","scale"),
                             trControl = fitControl)
Pred_KNNsd5_lat <- predict(train_KNNsd5_lat, validation_highsd5)
postResample(Pred_KNNsd5_lat, validation_highsd5$LATITUDE)

validation_sd5_predicted <- validation_highsd5
validation_sd5_predicted$PredLATITUDE <- Pred_KNNsd5_lat
validation_sd5_predicted$PredLONGITUDE <- Pred_KNNsd5_long

train_KNNsd5_b0_long <- train(y = building0_highsd5$LONGITUDE, x = building0_highsd5[,1:136],
                              method = "knn", preProcess = c("center","scale"),
                              trControl = fitControl)
Pred_KNNsd5_b0_long <- predict(train_KNNsd5_b0_long, valid_b0_highsd5)
postResample(Pred_KNNsd5_b0_long, valid_b0_highsd5$LONGITUDE)


train_KNNsd5_b1_long <- train(y = building1_highsd5$LONGITUDE, x = building1_highsd5[,1:136],
                              method = "knn", preProcess = c("center","scale"),
                              trControl = fitControl)
Pred_KNNsd5_b1_long <- predict(train_KNNsd5_b1_long, valid_b1_highsd5)
postResample(Pred_KNNsd5_b1_long, valid_b1_highsd5$LONGITUDE)

train_KNNsd5_b2_long <- train(y = building2_highsd5$LONGITUDE, x = building2_highsd5[,1:136],
                              method = "knn", preProcess = c("center","scale"),
                              trControl = fitControl)
Pred_KNNsd5_b2_long <- predict(train_KNNsd5_b2_long, valid_b2_highsd5)
postResample(Pred_KNNsd5_b2_long, valid_b2_highsd5$LONGITUDE)

train_KNNsd5_b0_lat <- train(y = building0_highsd5$LATITUDE, x = building0_highsd5[,1:136],
                             method = "knn", preProcess = c("center","scale"),
                             trControl = fitControl)
Pred_KNNsd5_b0_lat <- predict(train_KNNsd5_b0_lat, valid_b0_highsd5)
postResample(Pred_KNNsd5_b0_lat, valid_b0_highsd5$LATITUDE)

train_KNNsd5_b1_lat <- train(y = building1_highsd5$LATITUDE, x = building1_highsd5[,1:136],
                              method = "knn", preProcess = c("center","scale"),
                              trControl = fitControl)
Pred_KNNsd5_b1_lat <- predict(train_KNNsd5_b1_lat, valid_b1_highsd5)
postResample(Pred_KNNsd5_b1_lat, valid_b1_highsd5$LATITUDE)

train_KNNsd5_b2_lat <- train(y = building2_highsd5$LATITUDE, x = building2_highsd5[,1:136],
                             method = "knn", preProcess = c("center","scale"),
                             trControl = fitControl)
Pred_KNNsd5_b2_lat <- predict(train_KNNsd5_b2_lat, valid_b2_highsd5)
postResample(Pred_KNNsd5_b2_lat, valid_b2_highsd5$LATITUDE)

plot(building2_clean$LONGITUDE, building2_clean$LATITUDE,
     xlab = "Longitude", ylab = "Latitude", main = "Real Values")
plot(Pred_KNNsd5_long, Pred_KNNsd5_lat,
     xlab = "Longitude", ylab = "Latitude", main = "Predicted Values")



plot(validation_clean$LONGITUDE, validation_clean$LATITUDE,
     xlab = "Longitude", ylab = "Latitude", main = "Real Values")
plot(Pred_KNNsd5_long, Pred_KNNsd5_lat,
     xlab = "Longitude", ylab = "Latitude", main = "Predicted Values")




#### LONGITUDE / LATITUDE only with BUILDING datasets ----
train_KNN_b1_long <- train(y = building1_clean$LONGITUDE, x = building1_clean[,1:143],
                              method = "knn", preProcess = c("center","scale"),
                              trControl = fitControl)
Pred_KNN_b1_long <- predict(train_KNN_b1_long, validation_building1)
postResample(Pred_KNN_b1_long, validation_building1$LONGITUDE)

train_KNN_b2_long <- train(y = building2_clean$LONGITUDE, x = building2_clean[,1:102],
                              method = "knn", preProcess = c("center","scale"),
                              trControl = fitControl)
Pred_KNN_b2_long <- predict(train_KNN_b2_long, validation_building2)
postResample(Pred_KNN_b2_long, validation_building2$LONGITUDE)

train_KNN_b1_lat <- train(y = building1_clean$LATITUDE, x = building1_clean[,1:143],
                           method = "knn", preProcess = c("center","scale"),
                           trControl = fitControl)
Pred_KNN_b1_lat <- predict(train_KNN_b1_lat, validation_building1)
postResample(Pred_KNN_b1_lat, validation_building1$LATITUDE)

train_KNN_b2_lat <- train(y = building2_clean$LATITUDE, x = building2_clean[,1:102],
                           method = "knn", preProcess = c("center","scale"),
                           trControl = fitControl)
Pred_KNN_b2_lat <- predict(train_KNN_b2_lat, validation_building2)
postResample(Pred_KNN_b2_lat, validation_building2$LATITUDE)

plot(validation_building2$LONGITUDE, validation_building2$LATITUDE,
     xlab = "Longitude", ylab = "Latitude", main = "Real Values")
plot(Pred_KNN_b2_long, Pred_KNN_b2_lat,
     xlab = "Longitude", ylab = "Latitude", main = "Predicted Values")


plot(building1_clean$LONGITUDE, building1_clean$LATITUDE,
     xlab = "Longitude", ylab = "Latitude", main = "Real Values")
plot(Pred_KNN_b1_long, Pred_KNN_b1_lat,
     xlab = "Longitude", ylab = "Latitude", main = "Predicted Values")


building2_clean <- building2_clean[which(apply(building2_clean[,c(1:102)],1, function(x) max(x) <= -30)),]


####






train_SVM1_long <- svm(y = validation_clean$LONGITUDE, x = validation_clean[1:312],
                       scale = TRUE)
Pred_SVM1_long <- predict(train_SVM1_long, validation_clean[1:312])
postResample(Pred_SVM1_long, validation_clean$LONGITUDE)

train_SVM1_lat <- svm(y = validation_clean$LATITUDE, x = validation_clean[1:312],
                       scale = TRUE)
Pred_SVM1_lat <- predict(train_SVM1_lat, validation_clean[1:312])
postResample(Pred_SVM1_lat, validation_clean$LATITUDE)


train_SVMsd5_long <- svm(y = validation_highsd5$LONGITUDE, x = validation_highsd5[1:136],
    scale = TRUE)
Pred_SVMsd5_long <- predict(train_SVMsd5_long, validation_highsd5[1:136])
postResample(Pred_SVMsd5_long, validation_highsd5$LONGITUDE)

train_SVMsd5_lat <- svm(y = validation_highsd5$LATITUDE, x = validation_highsd5[1:136],
                         scale = TRUE)
Pred_SVMsd5_lat <- predict(train_SVMsd5_lat, validation_highsd5[1:136])
postResample(Pred_SVMsd5_lat, validation_highsd5$LATITUDE)



par(mfrow=c(1,4))

##  ----
plot(validation_clean$LONGITUDE, validation_clean$LATITUDE,
xlab = "Longitude", ylab = "Latitude", main = "Real Values")
plot(Pred_SVMsd5_long, Pred_SVMsd5_lat,
     xlab = "Longitude", ylab = "Latitude", main = "Predicted Values") 


## ----
sdfadsf


  




ggplot(build2_floor4, aes(LONGITUDE)) +
  geom_point(aes(y=LATITUDE))

ggplot(val_b2_f4, aes(LONGITUDE)) +
  geom_point(aes(y=LATITUDE))


ggplot(build1_floor2, aes(LONGITUDE)) +
  geom_point(aes(y=LATITUDE))

ggplot(val_b1_f2, aes(LONGITUDE)) +
  geom_point(aes(y=LATITUDE))

ggplot(build1_floor1, aes(LONGITUDE)) +
  geom_point(aes(y=LATITUDE))

ggplot(val_b1_f1, aes(LONGITUDE)) +
  geom_point(aes(y=LATITUDE))


ggplot(validation_highsd5, aes(LONGITUDE)) +
  geom_point(aes(y=LATITUDE))

ggplot(validation_sd5_predicted, aes(PredLONGITUDE)) +
  geom_point(aes(y=PredLATITUDE))
