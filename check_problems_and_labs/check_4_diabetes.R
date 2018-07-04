# Check Your Understanding Chapter 4
# Pima Indians data from mlbench

# 4.1
library(mlbench)
data(PimaIndiansDiabetes2)
df <- PimaIndiansDiabetes2[]
str(df)
head(df)
attach(df)

# check if there are missing values
sum(is.na(df))
# which columns?
sapply(df, function(x) sum(is.na(x)))

# train-test split
set.seed(1234)
i <- sample(1:nrow(df), .8*nrow(df), replace=FALSE)
train <- df[i,]
test <- df[-i,]

# model 1
glm1 <- glm(diabetes~glucose, data=train, family=binomial)
summary(glm1)
probs <- predict(glm1, newdata=test, type="response")
pred <- ifelse(probs>0.5, 2, 1)
results <- table(pred, test$diabetes)
results
acc1 <- mean(pred==as.integer(test$diabetes), na.rm=TRUE)
sum(is.na(pred))

# 4.2
# more metrics
TP <- results[2, 2]
TN <- results[1, 1]
FP <- results[1, 2]
FN <- results[2, 1]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
error_rate <- 1 - accuracy
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)

library(caret)
confusionMatrix(as.integer(test$diabetes), pred)

# roc and auc
library(ROCR)
pr <- prediction(pred, as.integer(test$diabetes))
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)
auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc


# 4.3
glm1$coefficients
probs_formula = 1 / (1 + exp(-(glm1$coefficients[1]+glm1$coefficients[2]*test$glucose)))
head(probs_formula)
head(probs)
# they look the same
b1 <- glm1$coefficients[2]
plot(test$glucose, probs_formula)
# the S shape is observed
x_vals <- c(60, 100, 140, 180, 220)
y_vals <- 1 / (1 + exp(-(glm1$coefficients[1]+glm1$coefficients[2]*x_vals)))
y_vals
# yes, what I expect from the graph

# 4.4
# glm2 uses all predictors
glm2 <- glm(diabetes~., data=train, family=binomial)
summary(glm2)
probs <- predict(glm2, newdata=test, type="response")
pred <- ifelse(probs>0.5, 2, 1)
results2 <- table(pred, test$diabetes)
results2
acc2 <- mean(pred==as.integer(test$diabetes), na.rm=TRUE)
sum(is.na(pred))  # 67
# This model has a lower accuracy that just using glucose (less than 1% lower)
# can we blame the NAs?
# how many NAs?
sapply(df, function(x) sum(is.na(x)==TRUE))
# replace triceps and insulin NAs with mean
# first train 
train$triceps[is.na(train$triceps)] <- mean(train$triceps, na.rm=TRUE)
train$insulin[is.na(train$insulin)] <- mean(train$insulin, na.rm=TRUE)
train <- train[complete.cases(train),]
# first train 
test$triceps[is.na(test$triceps)] <- mean(test$triceps, na.rm=TRUE)
test$insulin[is.na(test$insulin)] <- mean(test$insulin, na.rm=TRUE)
test <- test[complete.cases(test),]
# create another model on cleaned data
glm3 <- glm(diabetes~., data=train, family=binomial)
summary(glm3)
probs <- predict(glm3, newdata=test, type="response")
pred <- ifelse(probs>0.5, 2, 1)
results3 <- table(pred, test$diabetes)
results3
acc3 <- mean(pred==as.integer(test$diabetes), na.rm=TRUE)
sum(is.na(pred))  # 0

