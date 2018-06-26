# Check Your Understanding
# kNN
# Using a combined red and white wine data from UCI ML Repository
# https://archive.ics.uci.edu/ml/datasets/Wine+Quality
# red and white wine csv files were combined into one
# with new column type


#############################################
# 8.1
# load wine data  6497 x 12
wine <- read.csv("wine_all.csv", header=TRUE)
str(wine)

# train and test
set.seed(1234)
i <- sample(1:nrow(wine), .8*nrow(wine), replace=FALSE)
train <- wine[i,]
test <- wine[-i,]
# check distrib of white/red 
summary(train$type)  # red 1284  white 3913
summary(test$type)   # red 315   white 985

# try logistic regression to pick type based on all other columns
glm1 <- glm(type~., data=train, family=binomial)
summary(glm1)
probs <- predict(glm1, newdata=test, type="response")
pred <- ifelse(probs>0.5, 2, 1)
table(pred, test$type)
#pred red white
#   1 311     3
#   2   4   982
acc_glm <- mean(pred==as.integer(test$type))  # .994

# try knn
library(class)
knn_pred <- knn(train=train[.1:12], test=test[.1:12], cl=train$type, k=2)
table(knn_pred, test$type)
#knn_pred red white
#   red   275    44
#   white  40   941
acc_knn_unscaled <- mean(knn_pred==test$type)  # .935

#############################################
# 8.2
# scale the data with method 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_norm <- as.data.frame(lapply(train[,1:12], normalize))
test_norm <- as.data.frame(lapply(test[,1:12], normalize))
knn_pred2 <- knn(train=train_norm, test=test_norm, cl=train$type, k=2)
table(knn_pred2, test$type)
#knn_pred2 red white
#    red   309    25
#    white   6   960
acc_knn_scaled <- mean(knn_pred2==test$type)  # 0.976

# scale the data with method 2
means <- sapply(train[,-13], mean)
stdvs <- sapply(train[,-13], sd)
train_scaled <- scale(train[,-13], center=means, scale=stdvs)
test_scaled <- scale(test[,-13], center=means, scale=stdvs)
knn_pred3 <- knn(train=train_scaled, test=test_scaled, cl=train$type, k=2)
acc_knn_scale2 <- mean(knn_pred3==test$type)

# try with fewer predictors
train_reduced <- train_scaled[,c(2:8,11)]
test_reduced <- test_scaled[,c(2:8,11)]
knn_pred4 <- knn(train=train_reduced, test=test_reduced, cl=train$type, k=2)
table(knn_pred4, test$type)
#knn_pred3 red white
#    red   305   115
#    white  10   870
acc_knn4 <- mean(knn_pred4==test$type)  # 0.903

#############################################
# 8.3 kNN regression
train_reg <- train_scaled[,1:11]
test_reg <- test_scaled[,1:11]
library(caret)
fit <- knnreg(train_reg, train$quality, k=2)
predictions <- predict(fit, test_reg)
cor_knn <- cor(predictions, test$quality) # 0.3988
cor_mse <- mse <- mean((predictions - test$quality)^2) # 0.792

# compare to linear regression
lm1 <- lm(train$quality~.-type, data=train)
summary(lm1)
predictions2 <- predict(lm1, newdata=test)
cor_lm <- cor(predictions2, test$quality)  # 0.56
mse_lm <- mean((predictions2 - test$quality)^2)   # 0.53

# discuss
# test data mean and median of quality are around 6, range is 3-9
# the two algorithms performed similarly


#############################################
# 8.4
# finding k
test_mse <- rep(0, 40)
test_cor <- rep(0, 40)
for (i in 1:40){
  fit <- knnreg(train_reg, train$quality, k=i)
  pred <- predict(fit, newdata=test_reg)
  test_cor[i] <- cor(pred, test$quality)
  test_mse[i] <- mean((pred - test$quality)^2)
}
which.min(test_mse)
which.max(test_cor)
# 22 is the best
test_mse[22]  # 0.54
test_cor[22]  # 0.54
# this outperformed the linear regression model



