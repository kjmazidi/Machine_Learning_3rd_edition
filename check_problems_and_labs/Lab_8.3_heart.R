# Lab Chapter 8
# Using Heart data from UCI ML Repository
# https://archive.ics.uci.edu/ml/datasets/Heart+Disease
# kNN regression
# an earlier lab with logistic regression got 0.7678 accuracy

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

# try knn with k = 3
# scale the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_norm <- as.data.frame(lapply(train[,1:10], normalize))
test_norm <- as.data.frame(lapply(test[,1:10], normalize))
library(class)
pred <- knn(train=train_norm, test=test_norm, cl=train$class, k=3)
table(pred, test$class)
#pred  0  1
#    0 26 10
#    1  3 14
mean(pred==test$class) # 0.754

# try different values of k
test_acc <- rep(0, 40)
for (i in 1:40){
  pred <- knn(train=train_norm, test=test_norm, cl=train$class, k=i)
  test_acc[i] <- mean(pred==test$class)
}
which.max(test_acc)
test_acc[10]  # 0.83  
# this was higher than knn with k=3 and higher than logistic regression in an earlier lab