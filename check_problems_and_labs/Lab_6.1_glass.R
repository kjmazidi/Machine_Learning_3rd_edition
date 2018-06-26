# Check Your Understanding Chapter 6
# Glass data from mlbench

# 6.1
library(mlbench)
data(Glass)
df <- Glass[]
str(df)
attach(df)

# any NA?
sum(sapply(df, function(x) sum(is.na(x))))

# train-test split
set.seed(1234)
spec <- c(train=.6, test=.2, validate=.2)
i <- sample(cut(1:nrow(df),
                nrow(df)*cumsum(c(0,spec)), labels=names(spec)))
train <- df[i=="train",]
test <- df[i=="test",]
vald <- df[i=="validate",]

# try naive Bayes
library(e1071)
nb1 <- naiveBayes(Type~., data=train)
nb1
pred <- predict(nb1, newdata=test, type="class")
results_nb1 <- table(pred, test$Type)
results_nb1
acc_nb1 <- mean(pred==test$Type)

# try svm
svm1 <- svm(Type~., data=train, kernel="linear", cost=10, scale=TRUE)
summary(svm1)
pred <- predict(svm1, newdata=test)
results_svm1 <- table(pred, test$Type)
results_svm1
acc_svm1 <- mean(pred==test$Type)


# experiment with cost parameter
set.seed(1234)
tune.out <- tune(svm, Type~., data=vald, kernel="linear",
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
best_model <- tune.out$best.model
summary(best_model)
pred <- predict(best_model, newdata=test)
results_svm2 <- table(pred, test$Type)
results_svm2
acc_svm2 <- mean(pred==test$Type)

# try polynomial
svm3 <- svm(Type~., data=train, kernel="polynomial", gamma=1, cost=1)
summary(svm3)
pred <- predict(svm3, newdata=test)
results_svm3 <- table(pred, test$Type)
results_svm3
acc_svm3 <- mean(pred==test$Type)

# try radial
svm4 <- svm(Type~., data=train, kernel="radial", gamma=1, cost=1)
summary(svm4)
pred <- predict(svm4, newdata=test)
results_svm4 <- table(pred, test$Type)
results_svm4
acc_svm4 <- mean(pred==test$Type)

# tune parameters
set.seed(1234)
tune.out <- tune(svm, Type~., data=vald, kernel="radial",
                 ranges=list(cost=c(0.1,1,10,100,1000),
                             gamma=c(0.5,1,2,3,4)))
summary(tune.out)
best_model <- tune.out$best.model
summary(best_model)
pred <- predict(best_model, newdata=test)
results_svm5 <- table(pred, test$Type)
results_svm5
acc_svm5 <- mean(pred==test$Type)


