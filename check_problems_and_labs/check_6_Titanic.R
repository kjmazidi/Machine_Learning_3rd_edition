# Check Your Understanding Chapter 6
# Titanic data

# 6.1
df <- read.csv("titanic3.csv", header=TRUE)
df <- df[,c(1,2,4,5)]
df$survived <- factor(df$survived)
df$pclass <- factor(df$pclass)
str(df)
attach(df)

# any NA?
sapply(df, function(x) sum(is.na(x)))
# replace age with mean
df$age[is.na(df$age)] <- mean(df$age, na.rm=TRUE)
df <- df[complete.cases(df),]

# train-test-validate split
set.seed(1234)
spec <- c(train=.6, test=.2, validate=.2)
i <- sample(cut(1:nrow(df),
                nrow(df)*cumsum(c(0,spec)), labels=names(spec)))
train <- df[i=="train",]
test <- df[i=="test",]
vald <- df[i=="validate",]

# logistic regression
glm1 <- glm(survived~., data=train, family="binomial")
glm1
probs <- predict(glm1, newdata=test, type="response")
pred <- ifelse(probs>0.5, 1, 0)
results_glm <- table(pred, test$survived)
results_glm
acc_glm <- mean(pred==test$survived)

# naive Bayes
library(e1071)
nb1 <- naiveBayes(survived~., data=train)
nb1
pred <- predict(nb1, newdata=test)
results_nb <- table(pred, test$survived)
results_nb
acc_nb <- mean(pred==test$survived)

# try svm
svm1 <- svm(survived~., data=train, kernel="linear", cost=10, scale=TRUE)
summary(svm1)
pred <- predict(svm1, newdata=test)
results_svm1 <- table(pred, test$survived)
results_svm1
acc_svm1 <- mean(pred==test$survived)


# experiment with cost hyper parameter
set.seed(1234)
tune.out <- tune(svm, survived~., data=vald, kernel="linear",
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
best_model <- tune.out$best.model
summary(best_model)
pred <- predict(best_model, newdata=test)
results_svm2 <- table(pred, test$survived)
results_svm2
acc_svm2 <- mean(pred==test$survived)

# Check 6.2
# try polynomial
svm3 <- svm(survived~., data=train, kernel="polynomial", gamma=1, cost=1)
summary(svm3)
pred <- predict(svm3, newdata=test)
results_svm3 <- table(pred, test$survived)
results_svm3
acc_svm3 <- mean(pred==test$survived)

# try radial
svm4 <- svm(survived~., data=train, kernel="radial", gamma=1, cost=1)
summary(svm4)
pred <- predict(svm4, newdata=test)
results_svm4 <- table(pred, test$survived)
results_svm4
acc_svm4 <- mean(pred==test$survived)

# tune parameters
set.seed(1234)
tune.out <- tune(svm, survived~., data=vald, kernel="radial",
                 ranges=list(cost=c(0.1,1,10,100,1000),
                             gamma=c(0.5,1,2,3,4)))
summary(tune.out)
best_model <- tune.out$best.model
summary(best_model)
pred <- predict(best_model, newdata=test)
results_svm5 <- table(pred, test$survived)
results_svm5
acc_svm5 <- mean(pred==test$survived)


