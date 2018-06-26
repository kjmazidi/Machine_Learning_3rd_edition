# Lab Chapter 10
# Using Abalone data from UCI ML Repository
# http://archive.ics.uci.edu/ml/datasets/Abalone

abalone <- read.csv("abalone.csv", header=FALSE)
names(abalone) <- c("sex", "length", "diameter", "height", "weight.whole",
                 "weight.shucked", "weight.viscera", "weight.shell", "rings")
str(abalone)
head(abalone)
attach(abalone)

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
cor_lm <- cor(pred, test$rings)  # 0.73
mse_lm <- mean((pred-test$rings)^2) # 4.45
print(paste("rmse=", sqrt(mse_lm))) # 2.11

# try knn
# scale the data
train_norm <- train[]
test_norm <- test[]
train_norm$sex <- as.integer(train$sex)
test_norm$sex <- as.integer(test$sex)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_norm <- as.data.frame(lapply(train_norm[,1:9], normalize))
test_norm <- as.data.frame(lapply(test_norm[,1:9], normalize))
library(caret)
knn1 <- knnreg(train_norm, train$rings, k=30)
pred1 <- predict(knn1, test_norm)
cor_knn <- cor(pred1, test$rings)  # 0.962
mse_knn <- mean((pred1 - test$rings)^2) # 1.13
rmse_knn <- sqrt(mse_knn) # 1.06

# try decision tree
library(tree)
tree_ab <- tree(rings~., data=train)
plot(tree_ab)
text(tree_ab, cex=0.5, pretty=0)
# evaluate
pred <- predict(tree_ab, newdata=test)
cor_tree <- cor(pred, test$rings)  # 0.64
mse_tree <- mean((pred - test$rings)^2)  # 5.6
# worse than knn and much worse than linear regression
# try bagging
library(randomForest)
set.seed(1234)
tree_bagged <- randomForest(rings~., data=train, importance=TRUE)
tree_bagged
pred <- predict(tree_bagged, newdata=test)
cor_bagged <- cor(pred, test$rings)
mse_bagged <- mean((pred - test$rings)^2)
# results
# kNN got the best cor and the best mse
# why? data not linear enough for linear regression
#      DT have square boundaries but kNN is more flexible
