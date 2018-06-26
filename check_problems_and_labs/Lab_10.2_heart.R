# Lab Chapter 10
# Using Heart data from UCI ML Repository
# https://archive.ics.uci.edu/ml/datasets/Heart+Disease


heart <- read.csv("processed.hungarian.data.csv", header=FALSE, na.strings = '?')
names(heart) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach",
                  "exang","oldpeak","slope","ca","thal","class")
str(heart)
heart$class <- factor(heart$class)
# check if there are missing values
sum(is.na(heart))
# which columns?
sapply(heart, function(x) sum(is.na(x)))
# get rid of slope, ca and thal, mostly NAs
heart <- heart[,-c(11:13)]
# get rid of all remaining NAs
heart <- heart[complete.cases(heart),]
head(heart)
attach(heart)
levels(class)
summary(class)

# train-test split
set.seed(1234)
i <- sample(1:nrow(heart), .8*nrow(heart), replace=FALSE)
train <- heart[i,]
test <- heart[-i,]

# logistic regression (repeating lab in Chapter 4)
glm1 <- glm(class~., data=train, family=binomial)
probs <- predict(glm1, newdata=test, type="response")
pred <- ifelse(probs>.5, 1, 0)
table(pred, test$class)
acc_glm <- mean(pred==test$class)

# try knn with k = 10
# an earlier lab showed k=10 was best
# scale the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_norm <- as.data.frame(lapply(train[,1:10], normalize))
test_norm <- as.data.frame(lapply(test[,1:10], normalize))
library(class)
pred <- knn(train=train_norm, test=test_norm, cl=train$class, k=10)
table(pred, test$class)
#pred  0  1
#    0 26 8
#    1  3 16
acc_knn <- mean(pred==test$class) # 0.79

# Decision Tree
library(tree)
tree1 <- tree(class~., data=train)
plot(tree1)
text(tree1, cex=0.5, pretty=0)
# evaluate
pred <- predict(tree1, newdata=test, type="class")
table(pred, test$class)
acc_tree <- mean(pred==test$class)

# random Forest
library(randomForest)
set.seed(1234)
tree_bagged <- randomForest(class~., data=train)
pred <- predict(tree_bagged, newdata=test)
table(pred, test$class)
acc_forest <- mean(pred==test$class)

# results
# forest, knn did slightly worse than logistic regression
# decision tree was the lowest performing
