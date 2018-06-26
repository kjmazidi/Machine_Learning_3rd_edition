# Lab Chapter 8
# Using Abalone data from UCI ML Repository
# http://archive.ics.uci.edu/ml/datasets/Abalone
# in an earlier lab we did logistic regression and got 79% accuracy

abalone <- read.csv("abalone.csv", header=FALSE)
names(abalone) <- c("sex", "length", "diameter", "height", "weight.whole",
                 "weight.shucked", "weight.viscera", "weight.shell", "rings")
abalone$sex <- as.integer(abalone$sex)
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

# examine rings data
range(rings)
median(rings)
hist(rings)

# make a large/small factor, cutoff=10
abalone$size <- factor(ifelse(rings>9,1,0))
summary(abalone$size)

# train-test split
set.seed(1234)
i <- sample(1:nrow(abalone), .8*nrow(abalone), replace=FALSE)
train <- abalone[i,]
test <- abalone[-i,]

# scale the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_norm <- as.data.frame(lapply(train[,1:8], normalize))
test_norm <- as.data.frame(lapply(test[,1:8], normalize))

# try k=3
library(class)
pred <- knn(train=train_norm, test=test_norm, cl=train$size, k=3)
table(pred, test$size)
#pred   0   1
#    0 237  36
#    1 189 374
mean(pred==test$size)  # 0.73

# try a range of k
# try different values of k
test_acc <- rep(0, 40)
for (i in 1:40){
  pred <- knn(train=train_norm, test=test_norm, cl=train$size, k=i)
  test_acc[i] <- mean(pred==test$size)
}
which.max(test_acc)  # turns out that k=3 is the best
# this is lower than the logistic regression in an earlier lab
