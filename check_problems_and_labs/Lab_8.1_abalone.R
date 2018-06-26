# Lab Chapter 8
# Using Abalone data from UCI ML Repository
# http://archive.ics.uci.edu/ml/datasets/Abalone

abalone <- read.csv("abalone.csv", header=FALSE)
names(abalone) <- c("sex", "length", "diameter", "height", "weight.whole",
                 "weight.shucked", "weight.viscera", "weight.shell", "rings")
abalone$sex <- as.integer(abalone$s)
str(abalone)
head(abalone)
attach(abalone)

# check if there are missing values
sum(is.na(abalone))

# train-test split
set.seed(1234)
i <- sample(1:nrow(abalone), .8*nrow(abalone), replace=FALSE)
train <- abalone[i,]
test <- abalone[-i,]

# try linear regression 
lm1 <- lm(rings~., data=train)
summary(lm1)
# evaluate
pred <- predict(lm1, newdata=test)
cor1 <- cor(pred, test$rings)  # 0.72
mse1 <- mean((pred-test$rings)^2) # 4.59
print(paste("rmse=", sqrt(mse1))) # 2.14

# try knn
# scale the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_norm <- as.data.frame(lapply(train[,1:9], normalize))
test_norm <- as.data.frame(lapply(test[,1:9], normalize))
library(caret)
knn1 <- knnreg(train_norm, train$rings, k=3)
pred1 <- predict(knn1, test_norm)
cor(pred1, test$rings)  # 0.9495877
mse <- mean((pred1 - test$rings)^2) # 2.486
rmse <- sqrt(mse) # 1.58

# try different values of k
# finding k
test_mse <- rep(0, 40)
test_cor <- rep(0, 40)
for (i in 1:40){
  fit <- knnreg(train_norm, train$rings, k=i)
  pred <- predict(fit, newdata=test_norm)
  test_cor[i] <- cor(pred, test$rings)
  test_mse[i] <- mean((pred - test$rings)^2)
}
which.min(test_mse) # 30
which.max(test_cor) # 11
test_mse[30] # 1.129
test_cor[30] # 0.968
# mse is a more important measure so I chose #30
# this outperformed k=3
