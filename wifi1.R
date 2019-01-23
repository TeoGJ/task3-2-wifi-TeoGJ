#### WiFi Locationing Project ----

# Goal of the project:
# There are two business questions to answer:
# a) Can we rely on Wi-Fi fingerprinting for indoor locationing 
# as we already do on GPS outdoors?
# b) Can we develop a ML-based system to do so?



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

# Values overview
head(training)
str(training)
summary(training)
sum(is.na(training))

head(validation)
str(validation)
summary(validation)
sum(is.na(validation))

# I replicate both datasets:
#  a) Keep the original ones
#  b) Replications to engineer and preprocess

training_clean <- training
validation_clean <- validation


# Switch "100" values by "-105", since "100" value is a symbolic
# representation of signal absence. "-105" is the chosen value
# because it will make easier to understand statistical indicators
# such as mean, median, etc. and information given by each WAP will
# be a lot more interpretable.
training_clean[training_clean == "100"] <- -105
validation_clean[validation_clean == "100"] <- -105

##  Values distribution by features ----
summary(training_clean$BUILDINGID)
summary(training_clean$FLOOR)
summary(training_clean$USERID)
summary(training_clean$PHONEID)

summary(validation_clean$BUILDINGID)
summary(validation_clean$FLOOR)
summary(validation_clean$USERID)
summary(validation_clean$PHONEID)

# Since we will need to build models to predict FLOOR, it will be
# necessary to create a new feature pasting BUILDINGID and FLOOR
# in order distinguish floors with the same level but on different
# buildings

training$BUILDFLOOR <- paste0(training$BUILDINGID, training$FLOOR)
training_clean$BUILDFLOOR <- paste0(training_clean$BUILDINGID, training_clean$FLOOR)
validation$BUILDFLOOR <- paste0(validation$BUILDINGID, validation$FLOOR)
validation_clean$BUILDFLOOR <- paste0(validation_clean$BUILDINGID, validation_clean$FLOOR)
training$BUILDFLOOR <- as.factor(training$BUILDFLOOR)
validation$BUILDFLOOR <- as.factor(validation$BUILDFLOOR)
training_clean$BUILDFLOOR <- as.factor(training_clean$BUILDFLOOR)
validation_clean$BUILDFLOOR <- as.factor(validation_clean$BUILDFLOOR)


## Observations location
# Doing so we'll be able to check how the sample is representative
plot(training_clean$LONGITUDE,
     training_clean$LATITUDE,
     main = "Observations distribution",
     xlab = "LONGITUDE",
     ylab = "LATITUDE")

plot(validation_clean$LONGITUDE,
     validation_clean$LATITUDE,
     main = "Observations distribution",
     xlab = "LONGITUDE",
     ylab = "LATITUDE")


#### 1.2.2. Zero variance / Near zero variance ----

nearZeroVar(training_clean, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE,
            names = TRUE)

#### Data cleaning ----

#### Zero variance WAP's locationing & removal ----
# All zero variance WAP's are useless, since they give us no
# information about observations location. Thus, I will locate them
# and remove them. Then, I will intersect both datasets.

names(which(apply(training_clean,2,var) == 0))

which(apply(training_clean,2,var) == 0 )
training_clean <- training_clean[,-which(apply(training_clean,2,var) == 0)]

which(apply(validation_clean,2,var) == 0 )
validation_clean <- validation_clean[,-which(apply(validation_clean[1:520],2,var) == 0)]

common_col <- intersect(names(training_clean[,1:465]), names(validation_clean[,1:367]))

training_clean <- cbind(training_clean[,common_col],training_clean[466:475])
validation_clean <- cbind(validation_clean[,common_col], validation_clean[368:377])

#### Wrong observations removal ----
## All observations with higher value than -30 are supposed to be
## unreal, so it's necessary to locate them and probably remove.

# Find out if there are wrong observations

which(apply(training_clean[,(1:312)], 1, function(x) max(x) > -30 )) 
# 459 wrong observations on training set

which(apply(validation_clean[,(1:312)], 1, function(x) max(x) > -30 )) 
# All observations on validation set are acceptable

# Isolate wrong ones on a DF to know more about its location, device used,
# and users who kept them.
training_wrong <- training_clean[which(apply(training_clean[,(1:312)], 1, function(x) max(x) > -30 )),]
summary(training_wrong[,313:322])
summary(training_wrong$USERID)
summary(training_wrong$PHONEID)
summary(training_wrong$BUILDFLOOR)
summary(training_wrong$BUILDINGID)

# Wrong observations location
plot(training_wrong$LONGITUDE, training_wrong$LATITUDE,
     xlab = "LONGITUDE",
     ylab = "LATITUDE",
     main = "Wrong Observations")

# Wrong observations distribution by user
ggplot(training_wrong, aes(USERID)) + geom_bar()

# Isolate right observations to check if the sample is good enough
# to train models, mainly on the most affected floors
training_right <- training_clean[which(apply(training_clean[,(1:312)], 1, function(x) max(x) <= -30 )),]
summary(training_right$BUILDFLOOR)

t_right_b2f3 <- training_right %>% filter(BUILDFLOOR == 23)
t_right_b2f4 <- training_right %>% filter(BUILDFLOOR == 24)
t_clean_b2f4 <- training_clean %>% filter(BUILDFLOOR == 24)

plot(t_right_b2f4$LONGITUDE, t_right_b2f4$LATITUDE,
     xlab = "LONGITUDE",
     ylab = "LATITUDE",
     main = "Wrong Observations")

plot(t_clean_b2f4$LONGITUDE, t_clean_b2f4$LATITUDE,
     xlab = "LONGITUDE",
     ylab = "LATITUDE",
     main = "Wrong Observations")

training_clean <- training_right
training_right = NULL

# Almost 50% of user 6 observations are wrong, so all his observations
# will be removed
training_right <- training_right %>% filter(!(USERID == 6))



#### Subsetting DF's by standard deviation ----
training_highsd1 <- cbind(training_clean[,which(apply(training_clean[,1:312],2,sd) > 1)], training_clean[,313:322])

training_highsd2 <- cbind(training_clean[,which(apply(training_clean[,1:312],2,sd) > 2)], training_clean[,313:322])

training_highsd5 <- cbind(training_clean[,which(apply(training_clean[,1:312],2,sd) > 5)], training_clean[,313:322])
validation_highsd5 <- cbind(validation_clean[,which(apply(validation_clean[,1:312],2,sd) > 5)], validation_clean[,313:322])

common_sd5 <- intersect(names(training_highsd5[,1:172]), names(validation_highsd5[,1:166]))

training_highsd5 <- cbind(training_highsd5[,common_sd5],training_highsd5[173:182])
validation_highsd5 <- cbind(validation_highsd5[,common_sd5], validation_highsd5[167:176])

summary(training_highsd5$BUILDFLOOR)
summary(validation_highsd5$BUILDFLOOR)



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

