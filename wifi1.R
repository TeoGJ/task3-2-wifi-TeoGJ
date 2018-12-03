

#### WiFi Locationing Project

# Goal of the project:



#### 0. Environment

### 0.1 Working directory

setwd("C:/Users/User/Desktop/DataAnalytics/4.2. Wifi Locationing")

### 0.2. Packages required

library(caret)
library(readr)

#### 1. Data exploration

### 1.1. Downloading data

## 1.1.1. Downloading training data
training <- read_csv("trainingData.csv", 
                       +     na = "NA")

training <- as.data.frame(training)

## 1.1.2. Downloading validation data

validation <- read_csv("validationData.csv", 
                         +     na = "NA")

validation <- as.data.frame(validation)


### 1.2. Data observation

head(training)
sum(is.na(training))

head(validation)
sum(is.na(validation))
