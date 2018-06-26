# Lab Chapter 5
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

# make a large/small factor, cutoff=10
abalone$size <- factor(ifelse(rings>9,1,0))
summary(abalone$size)

# train-test split
set.seed(1234)
i <- sample(1:nrow(abalone), .8*nrow(abalone), replace=FALSE)
train <- abalone[i,]
test <- abalone[-i,]

# try logistic regression
glm1 <- glm(size~.-rings, data=train, family=binomial)
summary(glm1)

probs <- predict(glm1, newdata=test, type="response")
pred <- ifelse(probs>0.5, 1, 0)
table_glm <- table(pred, test$size)
#pred   0   1
#0 337  85
#1  89 325
acc_glm <- mean(pred==test$size)
# accuracy = 79%

# try naive Bayes
library(e1071)
nb1 <- naiveBayes(size~.-rings, data=train)
nb1
pred <- predict(nb1, newdata=test, type="class")
table_nb <- table(pred, test$size)
acc_nb <- mean(pred==test$size)

