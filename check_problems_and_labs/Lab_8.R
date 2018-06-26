# Check Your Understanding Chapter 8
# Using Abalone data from UCI ML Repository
# http://archive.ics.uci.edu/ml/datasets/Abalone

abalone <- read.csv("abalone.csv", header=FALSE)
names(abalone) <- c("sex", "length", "diameter", "height", "weight.whole",
                 "weight.shucked", "weight.viscera", "weight.shell", "rings")

str(abalone)
head(abalone)
attach(abalone)

# check if there are missing values
sum(is.na(abalone))

# examine rings data
range(rings)
median(rings)
hist(rings)

# 8.1 knn Classification
# make a large/small factor, cutoff=10
abalone$size <- factor(ifelse(rings>9,1,0))
summary(abalone$size)
# train-test split
set.seed(1234)
i <- sample(1:nrow(abalone), .8*nrow(abalone), replace=FALSE)
train <- abalone[i,2:8]
train_labels <- abalone[i,10]
test <- abalone[-i,2:8]
test_labels <- abalone[-i,10]
### knn classification
library(class)
pred <- knn(train=train, test=test, cl=train_labels, k=2)
table(pred, test_labels)
mean(pred==test_labels)
# accuracy = 72%




# try knn regression
# train-test split
set.seed(1234)
i <- sample(1:nrow(abalone), .8*nrow(abalone), replace=FALSE)
train <- abalone[i,]
test <- abalone[-i,]
lm1 <- lm(rings~., data=train)
summary(lm1)
pred <- predict(lm1, newdata=test)
cor(pred, test$rings)
rmse <- sqrt(mean((pred=test$rings)^2)) 