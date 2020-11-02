# Clear Variables in the Global Environment -----
rm(list =ls())

# Note, some of these libraries are not needed for this code. However, the libraries below are helpful for machine learning applicaitons.
# To learn a little more about each of the packages, please visit their respective CRAN sites. A few of these packages will also be explored in
# future PD sessions.

# If using CProbe - you may need to install a few of these packages:
#install.packages(c('InformationValue', 'aod', 'tree', 'ISLR'))
library(readr)
library(dplyr)
library(zoo)
library(psych)
library(ROCR)
library(corrplot)
library(car)
library(InformationValue)
library(pbkrtest)
library(leaps)
library(glm2)
library(aod)
library(ggplot2)
library(caret)
library(e1071)
library(caTools)
# library(tree) - Not Supported on Cprobe
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)

# Data Import and Variable Type Changes 
# setwd("") # Set the working directory to the file location
data <- read.csv("insuranceTraining.csv")

# Initial Data Exploration - Refer to the Data Dictionary for a description of each variable
averageTarget = mean(as.numeric(data$TARGET_FLAG)) # Not used for the initial PD, however can be used to explore classification
averageTarget

averageClaim = mean(as.numeric(data$TARGET_AMT))
averageClaim # Here is the target prediction variable
nrow(data)
summary(data)
### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical ----
data$INDEX <- as.factor(data$INDEX)
data$TARGET_FLAG <- as.factor(data$TARGET_FLAG)
data$SEX <- as.factor(data$SEX)
data$EDUCATION <- as.factor(data$EDUCATION)
data$PARENT1 <- as.factor(data$PARENT1)
data$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$INCOME)))

data$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$HOME_VAL)))
data$MSTATUS <- as.factor(data$MSTATUS)
data$REVOKED <- as.factor(data$REVOKED)
data$RED_CAR <- as.factor(ifelse(data$RED_CAR=="yes", 1, 0))
data$URBANICITY <- ifelse(data$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
data$URBANICITY <- as.factor(data$URBANICITY)
data$JOB <- as.factor(data$JOB)
data$CAR_USE <- as.factor(data$CAR_USE)
data$CAR_TYPE <- as.factor(data$CAR_TYPE)
data$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$OLDCLAIM)))
data$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$BLUEBOOK)))
data$DO_KIDS_DRIVE <- as.factor(ifelse(data$KIDSDRIV > 0, 1, 0 ))
summary(data) # Not

#################### Part 1: Data Exploration ##############################################
#pairs(data[,2:10])

# Below are some examples of visualizations to aid with exploring the data.

# Histograms for Numeric Variables
par(mfrow=c(2,2))
hist(data$AGE, col = "green", xlab = "Age", main = "AGE Hist")
data0<- subset(data, TARGET_FLAG == 1 )
boxplot(data$AGE, col = "green", main = "AGE BoxPlot")
hist(data0$AGE, col = "red", xlab = "Age", main = "AGE Hist - Claim")
boxplot(data0$AGE, col = "red", main = "AGE BoxPlot - Claim")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(sqrt(data$TRAVTIME), col = "green", xlab = "SQRT TRAVTIME", main = "SQRT TRAVTIME Hist")
hist(data$YOJ, col = "blue", xlab = "YOJ", main = "YOJ Hist")
boxplot(sqrt(data$TRAVTIME), col = "green", main = "SQRT TRAVTIME BoxPlot")
boxplot(data$YOJ, col = "blue", main = "YOJ BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(sqrt(data0$TRAVTIME), col = "green", xlab = "SQRT TRAVTIME", main = "SQRT TRAVTIME Hist")
hist(data0$YOJ, col = "blue", xlab = "YOJ", main = "YOJ Hist")
boxplot(sqrt(data0$TRAVTIME), col = "green", main = "SQRT TRAVTIME BoxPlot")
boxplot(data0$YOJ, col = "blue", main = "YOJ BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(sqrt(data$BLUEBOOK), col = "green", xlab = "SQRT BLUEBOOK", main = "SQRT BLUEBOOK Hist")
hist((data$TIF), col = "blue", xlab = "TIF", main = "TIF Hist")
boxplot(sqrt(data$BLUEBOOK), col = "green", main = "SQRT BLUEBOOK BoxPlot")
boxplot(data$TIF, col = "blue", main = "TIF BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(data$MVR_PTS, col = "red", xlab = "MVR_PTS", main = "MVR_PTS Hist")
hist(data$CAR_AGE, col = "blue", xlab = "CAR_AGE", main = "CAR_AGE Hist")
boxplot(data$MVR_PTS, col = "red", main = "MVR_PTS BoxPlot")
boxplot(data$CAR_AGE, col = "blue", xlab = "CAR_AGE", main = "CAR_AGE BoxPlot")
par(mfrow=c(1,1))

hist(data$HOME_VAL, col = 'green', xlab = 'Home Value', main = "Home Value")

data %>%
  ggplot(aes(x = TARGET_FLAG)) + geom_bar() + ggtitle("Was Car in a Crash 1=Yes, 0 = No")

data %>%
  ggplot(aes(x = CAR_TYPE)) + geom_bar() + ggtitle("Car Type")

data %>%
  ggplot(aes(x = RED_CAR)) + geom_bar() + ggtitle("Red Car")

data %>%
  ggplot(aes(x = EDUCATION)) + geom_bar() + ggtitle("Education Level")

data %>%
  ggplot(aes(x = INCOME, y = BLUEBOOK, colour = TARGET_FLAG)) +
  geom_point() +
  facet_wrap(~JOB)

data %>%
  ggplot(aes(x = INCOME, y = BLUEBOOK, colour = TARGET_FLAG)) +
  geom_point() +
  facet_wrap(~JOB)

data %>%
  ggplot(aes(x = INCOME, y = CAR_AGE, colour = TARGET_FLAG)) +
  geom_point() +
  facet_wrap(~MSTATUS)

data %>%
  ggplot(aes(x = BLUEBOOK, y = TRAVTIME, colour = TARGET_FLAG)) +
  geom_point()

########### Part 2: Data Transformation ##################
# Fix NA's, note car age
# Keep Averages for Test Data
avg.AGE <- mean(data$AGE, na.rm = "TRUE") 
jobMeans <- data %>%
  group_by(JOB) %>%
  summarise(AVG_YOJ = mean(YOJ, na.rm = T), AVG_INCOME = mean(INCOME, na.rm = T), AVG_HOMEVAL = mean(HOME_VAL, na.rm = T))
jobMeans

carageMeans <- data %>%
  group_by(CAR_TYPE) %>%
  summarise(AVG_CARAGE = mean(CAR_AGE, na.rm = T), AVG_OLDCLAIM = mean(OLDCLAIM, na.rm = T))
carageMeans

carageOldClaimMeans <-  data %>%
  group_by(CAR_AGE) %>%
  summarise(OLDCLAIM = mean(OLDCLAIM, na.rm = T))

carageOldClaimMeans

data$M_AGE <- as.factor(ifelse(is.na(data$AGE), 1, 0))
data$IMP_AGE <- ifelse(is.na(data$AGE), avg.AGE, data$AGE)
# Years on Job
data$M_YOJ <- as.factor(ifelse(is.na(data$YOJ), 1, 0))
data$IMP_YOJ <- na.aggregate(data$YOJ, data$JOB, mean, na.rm = TRUE)
# Income - Use the Average income based on the Job
data$M_INCOME <- as.factor(ifelse(is.na(data$INCOME), 1, 0))
data$IMP_INCOME <- na.aggregate(data$INCOME, data$JOB, mean, na.rm = TRUE)
# Home Value - Potential Renters - Home value of 0, 
data$HOME_OWNER <- as.factor(ifelse(data$HOME_VAL == 0, 0, 1))
data$M_HOME_VAL <- as.factor(ifelse(is.na(data$HOME_VAL), 1, 0))
# Taking the averages of the zero values offsets the averages of the others - so need a different table for thier income
homeOwnerdata <- data %>%
  filter(HOME_OWNER == 1) %>%
  group_by(JOB) %>%
  summarise((AVG_HOME_VAL = mean(HOME_VAL, na.rm = T)))
homeOwnerdata
data$IMP_HOME_VAL <- ifelse(is.na(data$HOME_VAL), 
                            ifelse(data$JOB == "", 322081,
                                   ifelse(data$JOB == "Clerical", 155130,
                                          ifelse(data$JOB == "Doctor", 358864,
                                                 ifelse(data$JOB == "Home Maker", 114863,
                                                        ifelse(data$JOB == "Lawyer", 267072,
                                                               ifelse(data$JOB == "Manager", 264791,
                                                                      ifelse(data$JOB == "Professional", 245849,
                                                                             ifelse(data$JOB == "Student", 102624, 207757))))))))
                            ,data$HOME_VAL)


data$SQRT_HOME_VAL <- sqrt(data$IMP_HOME_VAL)
data$HOME_OWNER <- as.factor(ifelse(data$IMP_HOME_VAL == 0, 0, 1))
# Car Age
data$M_CAR_AGE <- as.factor(ifelse(is.na(data$CAR_AGE) | data$CAR_AGE < 0, 1, 0))
data$IMP_CAR_AGE <- na.aggregate(data$CAR_AGE, data$CAR_TYPE, mean, na.rm = TRUE)
data$IMP_CAR_AGE[data$IMP_CAR_AGE < 0 ] <- 0 
# Old Claim - If the 
data$M_OLDCLAIM <- as.factor(ifelse(is.na(data$OLDCLAIM) | 
                                      (data$IMP_CAR_AGE < 5 & !is.na(data$IMP_CAR_AGE)), 1, 0))
data$IMP_OLDCLAIM <- ifelse(data$IMP_CAR_AGE < 5 & !is.na(data$IMP_CAR_AGE),0,data$OLDCLAIM)
data$IMP_OLDCLAIM <- na.aggregate(data$IMP_OLDCLAIM, data$IMP_CAR_AGE, mean, na.rm = TRUE )

data$SQRT_TRAVTIME <- sqrt(data$TRAVTIME)
data$SQRT_BLUEBOOK <- sqrt(data$BLUEBOOK)

# Bin Income
data$INCOME_bin[is.na(data$INCOME)] <- "NA"
data$INCOME_bin[data$INCOME == 0] <- "Zero"
data$INCOME_bin[data$INCOME >= 1 & data$INCOME < 30000] <- "Low"
data$INCOME_bin[data$INCOME >= 30000 & data$INCOME < 80000] <- "Medium"
data$INCOME_bin[data$INCOME >= 80000] <- "High"
data$INCOME_bin <- factor(data$INCOME_bin)
data$INCOME_bin <- factor(data$INCOME_bin, levels=c("NA","Zero","Low","Medium","High"))

# Bin Age
data$AGE_bin[is.na(data$AGE)] <- 'NA'
data$AGE_bin[data$AGE < 21] <- 'Young Adult'
data$AGE_bin[data$AGE <= 40 & data$AGE >= 21] <- 'Adult'
data$AGE_bin[data$AGE <= 65 & data$AGE > 40] <- 'MiddleAge'
data$AGE_bin[data$AGE > 65] <- 'Ederly'
data$AGE_bin <- factor(data$AGE_bin)
data$AGE_bin <- factor(data$AGE_bin, levels = c('NA', 'Young Adult', 'Adult', 'MiddleAge', 'Ederly'))

# Bin Car Age - Newer variable - Used 
data$CAR_AGE_bin[is.na(data$CAR_AGE)] <- 'NA'
data$CAR_AGE_bin[data$CAR_AGE < 2] <- 'Brand New'
data$CAR_AGE_bin[data$CAR_AGE <= 6 & data$CAR_AGE >= 2] <- 'New'
data$CAR_AGE_bin[data$CAR_AGE <= 10 & data$CAR_AGE > 6] <- 'Used'
data$CAR_AGE_bin[data$CAR_AGE <= 15 & data$CAR_AGE > 10] <- 'Older'
data$CAR_AGE_bin[data$CAR_AGE > 15] <- 'Vintage'
data$CAR_AGE_bin <- factor(data$CAR_AGE_bin)
data$CAR_AGE_bin <- factor(data$CAR_AGE_bin, levels=c("NA","Brand New","New","Used","Older",'Vintage'))

summary(data)

# Select a subset of the data for training the model
vars <- subset(data, select = c(TARGET_AMT, IMP_AGE, IMP_YOJ, IMP_INCOME, PARENT1, IMP_HOME_VAL, SQRT_HOME_VAL, MSTATUS, SEX, 
                                EDUCATION, JOB, TRAVTIME, SQRT_TRAVTIME, CAR_USE, BLUEBOOK, SQRT_BLUEBOOK, TIF, CAR_TYPE, RED_CAR,
                                IMP_OLDCLAIM, CLM_FREQ, REVOKED, MVR_PTS, IMP_CAR_AGE, URBANICITY, DO_KIDS_DRIVE, INCOME_bin, HOME_OWNER, AGE_bin, CAR_AGE_bin))
summary(vars)
# Select a subset to view the correlation of the numeric variables and the Target Value
numeric <- subset(data, select = c(TARGET_AMT, IMP_AGE, HOMEKIDS, IMP_YOJ, IMP_INCOME, IMP_HOME_VAL, TRAVTIME, BLUEBOOK, TIF,
                                   CLM_FREQ, MVR_PTS, IMP_CAR_AGE), na.rm = TRUE)
c <- cor(numeric)
corrplot(c, method = "square")

############# Part 3: Model Development ######################

# Develop a function to calculate mean squared error. Can also use mltools::mse to calculate the mse from a prediction
mse <- function(sm) 
{mean(sm$residuals^2)}

## Part 3a - Split into a Train and Test Set
set.seed(1)
trainInd=sample(1: nrow(data), round(nrow(data)*.75))
train = vars[trainInd,]
data.test=data[-trainInd,]

# Model 1 - Example Basic Linear Regression Function
linearModel <- lm(TARGET_AMT ~ PARENT1 + SQRT_HOME_VAL + IMP_CAR_AGE + MVR_PTS +
                    CLM_FREQ + URBANICITY + DO_KIDS_DRIVE, data = train)
linearModel
summary(linearModel) # Poor Adjusted R2
linearPred <- predict(linearModel, newdata = data.test)
mse.linearModel <- mltools::mse(linearPred, data.test$TARGET_AMT)
mse.linearModel

# Model 2 - Example Stepwise for Linear Regression
step <- lm(TARGET_AMT ~. ,
              data = train)
stepAICmodel <- stepAIC(step, direction = 'backward')
summary(stepAICmodel)
stepAICPred <- predict(stepAICmodel, newdata = data.test)
mse.stepAICmodel <- mltools::mse(stepAICPred, data.test$TARGET_AMT)
mse.stepAICmodel # Lower MSE, but let's try a few different techniques

# Model 3 - Example Decision Tree
# Decision Tree - Regression - Tree package not available for Cprobe instance of RStudio
# tree.dataR=tree(TARGET_AMT~., vars, subset = train)
# summary(tree.dataR)
# plot(tree.dataR)
# text(tree.dataR, pretty=0)
# 
# cv.dataR <-  cv.tree(tree.dataR)
# plot(cv.dataR$size, cv.dataR$dev, type='b')
# cv.dataR  # Size 3
# 
# yhat=predict(tree.dataR, newdata = data.test)
# truval=data.test$TARGET_AMT
# plot(yhat ,truval)
# abline (0,1)
# mse.tree <- mean((yhat -truval)^2) # MSE calculation - an alternative

# Model 4 - Example Random Forest
rFmodel =randomForest(TARGET_AMT~., data=train,  importance =TRUE) #mtry - number of variables randomly sampled. default for regression regression (p/3)
rFmodel
importance(rFmodel) # Importance of Variables
varImpPlot(rFmodel)
# Predict and calculate MSE
rFmodelPred <- predict(rFmodel, newdata = data.test)
mse.rFmodel <- mltools::mse(rFmodelPred, data.test$TARGET_AMT)
mse.rFmodel # Lower MSE, but let's try a few different techniques

# Model 5 - Boosting
boostmodel = gbm(TARGET_AMT~., data=train, 
                 distribution="gaussian", n.trees =5000 , interaction.depth =1, shrinkage =0.001)
summary(boostmodel)

yhat.boost=predict(boostmodel, newdata = data.test,
                   n.trees =5000)

truval=data.test$TARGET_AMT
plot(yhat.boost ,truval)
abline(0,1)
mse.boost <- mltools::mse(yhat.boost, truval)
mse.boost

# Model 6 - XGBoost - Next Class

### Part 4: Model selection Evaluate Models ------------
mse.linearModel
mse.stepAICmodel # Winner! - If you are just using mean squared error.
mse.rFmodel
mse.boost


#### Classification Preperation --------------
# Select a subset of the data for training the model
varsC <- subset(data, select = c(TARGET_FLAG, IMP_AGE, IMP_YOJ, IMP_INCOME, PARENT1, IMP_HOME_VAL, SQRT_HOME_VAL, MSTATUS, SEX, 
                                EDUCATION, JOB, TRAVTIME, SQRT_TRAVTIME, CAR_USE, BLUEBOOK, SQRT_BLUEBOOK, TIF, CAR_TYPE, RED_CAR,
                                IMP_OLDCLAIM, CLM_FREQ, REVOKED, MVR_PTS, IMP_CAR_AGE, URBANICITY, DO_KIDS_DRIVE, INCOME_bin, HOME_OWNER, AGE_bin, CAR_AGE_bin))
summary(varsC)
# Select a subset to view the correlation of the numeric variables and the Target Value
numeric <- subset(data, select = c(TARGET_AMT, IMP_AGE, HOMEKIDS, IMP_YOJ, IMP_INCOME, IMP_HOME_VAL, TRAVTIME, BLUEBOOK, TIF,
                                   CLM_FREQ, MVR_PTS, IMP_CAR_AGE), na.rm = TRUE)
c <- cor(numeric)
corrplot(c, method = "square")
### Part 3 Model Development #############

## Part 3a - Split into a Train and Test Set
set.seed(1)
trainInd=sample(1: nrow(data), round(nrow(data)*.75))
train = varsC[trainInd,]
data.test=data[-trainInd,]


# Model 1 ---- Generalized Logistic Regression (Classification)
glmModel <- glm(TARGET_FLAG ~ PARENT1 + SQRT_HOME_VAL + IMP_CAR_AGE + MVR_PTS +
                    CLM_FREQ + URBANICITY + DO_KIDS_DRIVE, data = train, family = binomial())
glmModel
summary(glmModel) # Can use the AIC Value to evaluate performance
logRedPred <- predict(glmModel, newdata = data.test)
logRedPred ## See the result - Get the Logistic Prediction! - DON'T USE THIS WITHOUT TRANSFORMATION
logRedPred <- predict(glmModel, newdata = data.test, type = "response")
logRedPred # Much better - the result is the probability a claim will be submited
logRedPred  <- ifelse(logRedPred  > 0.5,1,0) 
table(logRedPred, data.test$TARGET_FLAG) # One way to create the confusion matrix
glmModel.acc <- mean(logRedPred == data.test$TARGET_FLAG)
glmModel.acc # 76% Accuracy

# Model 2 ---- Stepwise
step.glmModel <- glm(TARGET_FLAG ~., data = train, family = binomial())
stepModel <- stepAIC(step.glmModel, direction = 'backward')
summary(stepModel) # Can use the AIC Value to evaluate performance
stepPred <- predict(stepModel, newdata = data.test)
stepPred  ## See the result - Get the Logistic Prediction! - DON'T USE THIS WITHOUT TRANSFORMATION
stepPred  <- predict(stepModel, newdata = data.test, type = "response")
stepPred  # Much better - the result is the probability a claim will be submited
stepPred   <- ifelse(stepPred   > 0.5,1,0) 
table(stepPred , data.test$TARGET_FLAG) # One way to create the confusion matrix
confusionMatrix(stepPred , data.test$TARGET_FLAG) # Error
confusionMatrix(as.factor(stepPred), data.test$TARGET_FLAG)
stepModel.acc <- mean(stepPred == data.test$TARGET_FLAG)
stepModel.acc # 78% Accuracy

# Model 4 - Random Forest
set.seed(1)
rf5.data = randomForest(TARGET_FLAG~., data=train,
                        mtry=5, importance =TRUE)
rf5.data # Get the confusion matrix, Out of Bag Error Rate, and the classification errors
importance(rf5.data)
varImpPlot (rf5.data)
rf5.pred=predict(rf5.data, data.test,type="response") # Know your outputs!!!
rf5.pred
rf5.pred=predict(rf5.data, data.test,type="class") # Try different types, can use
confusionMatrix(rf5.pred, data.test$TARGET_FLAG)
rf.acc <- mean(rf5.pred == data.test$TARGET_FLAG)
rf.acc # 79% Accuracy

# Model 5 - Boost
# Classification requires the target flag is not a factor!!!
varsC$TARGET_FLAG2 <- as.numeric(varsC$TARGET_FLAG)-1
varsC$TARGET_FLAG2
set.seed(1)
boost.data =gbm(TARGET_FLAG2~.-TARGET_FLAG, data=varsC[trainInd,], 
                n.trees =1000 , interaction.depth =4, shrinkage = .1) # Will create a bernoulli distro when not specified

summary(boost.data)
boost.data

boost.pred=predict(boost.data, newdata = varsC[-trainInd,], type='response', n.trees = 1000)
boost.pred # View the output of the model, 
boost.pred <- ifelse(boost.pred > 0.5,1,0)
boost.pred <- as.factor(boost.pred)
confusionMatrix(boost.pred, data.test$TARGET_FLAG)
acc.boost <- (1359+254)/2040 # Accuracy
### Evaluate Model Performance
glmModel.acc
stepModel.acc
rf.acc
acc.boost # Champion!

#### 

acc.boost 

## Evaluate the loss of accuracy over t
boost.pred=predict(boost.data, newdata = varsC[-trainInd,], type='response', n.trees = 1000)
boost.pred # View the output of the model, 
boost.pred <- ifelse(boost.pred > 0.5,1,0)
boost.pred <- as.factor(boost.pred)
confusionMatrix(boost.pred, data.test$TARGET_FLAG)
