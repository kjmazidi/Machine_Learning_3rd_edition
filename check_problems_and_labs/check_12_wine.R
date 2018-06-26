# Check Your Understanding
# Neural Network
# Using a combined red and white wine data from UCI ML Repository
# https://archive.ics.uci.edu/ml/datasets/Wine+Quality
# red and white wine csv files were combined into one
# with new column type


#############################################
# 12.1
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

# scale the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_norm <- as.data.frame(lapply(train[,1:12], normalize))
test_norm <- as.data.frame(lapply(test[,1:12], normalize))


#############################################
# Try kNN regression
train_reg <- train_norm[,1:11]
test_reg <- test_norm[,1:11]
library(caret)
fit <- knnreg(train_reg, train$quality, k=2)
predictions <- predict(fit, test_reg)
cor_knn <- cor(predictions, test$quality) # 0.3988
mse_knn <- mean((predictions - test$quality)^2) # 0.792

# compare to linear regression
train_reg2 <- cbind(train_reg, train$quality)
test_reg2 <- cbind(test_reg, test$quality)
lm1 <- lm(train$quality~., data=train_reg)
summary(lm1)
predictions2 <- predict(lm1, newdata=test_reg2)
cor_lm <- cor(predictions2, test$quality)  # 0.49
mse_lm <- mean((predictions2 - test$quality)^2)   # 0.787

# discuss
# test data mean and median of quality are around 6, range is 3-9
# the two algorithms performed similarly

# try neural network
library(neuralnet)
set.seed(1234)
n <- names(train_norm)
f <- as.formula(paste("quality ~ ", paste(n[!n %in% "quality"], collapse = " + ")))
set.seed(1234)
nn1 <- neuralnet(f, data=train_norm, hidden=c(9,6,3), linear.output = TRUE, threshold = 0.02, lifesign="full")
plot(nn1)
# evaluate
pred <- compute(nn1, test_norm[,-12])
pred <- pred$net.result
pred_unscale <- pred * (max(test$quality) - min(test$quality)) + min(test$quality)
cor_nn <- cor(pred_unscale, test$quality)
mse_nn <- mean((pred_unscale - test$quality)^2)

# it seems that the simpler the model the more it overfit the training data
# neuralnet() performed significantly worse than lm or knn
# hidden (9,6,2)  threshold: 0.04  52K steps mse: 1.25
# hidden (7,3)              0.03  29K steps  mse: 1.33
# hidden (7)                0.03  53K steps  mse: 1.34
# hidden (5)                0.01  10K steps  mse: 2.17