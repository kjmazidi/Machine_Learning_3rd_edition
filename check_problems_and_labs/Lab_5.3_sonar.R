# Lab Chapter 5
# Using Sonar data from package mllbench

library(mlbench)
data(Sonar)
df <- Sonar[]
str(df)
head(df)
attach(df)
summary(df$Class) # M=111  R=97

# check if there are missing values
sum(is.na(df))  # no NAs

# train-test split
set.seed(1234)
i <- sample(1:nrow(df), .8*nrow(df), replace=FALSE)
train <- df[i,]
test <- df[-i,]

# try logistic regression
glm1 <- glm(Class~., data=train, family=binomial)
summary(glm1)

probs <- predict(glm1, newdata=test, type="response")
pred <- ifelse(probs>0.5, 1, 0)
table_glm <- table(pred, test$Class)
table_glm
#pred   0   1
#0 17  4
#1  10 11
acc_glm <- mean(pred==as.integer(test$Class))
# accuracy = 24%

# try naive Bayes
library(e1071)
nb1 <- naiveBayes(Class~., data=train)
nb1
pred <- predict(nb1, newdata=test, type="class")
table_nb <- table(pred, test$Class)
table_nb
# 11  1
# 16 14
acc_nb <- mean(pred==test$Class)

# Compare
# Both classifiers did poorly on classifying Rock
# Naive Bayes did better on classifying Mine
# since it is more important to identify a Mine, I would choose Naive Bayes
