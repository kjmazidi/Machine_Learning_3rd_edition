# Lab Chapter 5
# Using Heart data from UCI ML Repository
# https://archive.ics.uci.edu/ml/datasets/Heart+Disease

heart <- read.csv("processed.hungarian.data.csv", header=FALSE, na.strings = '?')
names(heart) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach",
                  "exang","oldpeak","slope","ca","thal","class")
heart$class <- factor(heart$class)
heart$sex <- factor(heart$sex)
heart$cp <- factor(heart$cp)
heart$trestbps <- as.integer(heart$trestbps)
heart$chol <- as.integer(heart$chol)
heart$thalach <- as.integer(heart$thalach)
heart <- heart[,-12]
str(heart)
head(heart)
attach(heart)
levels(class)
summary(class)

# check if there are missing values
sum(is.na(heart))
# which columns?
sapply(heart, function(x) sum(is.na(x)))


# train-test split
set.seed(1234)
i <- sample(1:nrow(heart), .8*nrow(heart), replace=FALSE)
train <- heart[i,]
test <- heart[-i,]

# model 1
glm1 <- glm(class~age+sex+cp+fbs+restecg+exang, data=train, family=binomial)
summary(glm1)
probs <- predict(glm1, newdata=test, type="response")
pred <- ifelse(probs>0.5, 1, 0)
table_glm <- table(pred, test$class)
table_glm
# 28 8
# 5 15
acc_glm <- mean(pred==test$class, na.rm=TRUE)
sum(is.na(pred))

# naive Bayes
library(e1071)
nb1 <- naiveBayes(class~age+sex+cp+fbs+restecg+exang, data=train)
nb1
pred <- predict(nb1, newdata=test, type="class")
table_nb <- table(pred, test$class)
table_nb
# 29 7
# 6 17
acc_nb <- mean(pred==test$class)
acc_nb
sum(is.na(pred))
# naive Bayes did slightly better
# nB had no NA in predictions whereas logistic regression had 3