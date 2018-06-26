# Lab Chapter 4
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

# see if some columns are redundant
# yes, there are several correlated variables
# but the model worked better with them included
cor(abalone[,-1])
pairs(abalone)

# train-test split
set.seed(1234)
i <- sample(1:nrow(abalone), .8*nrow(abalone), replace=FALSE)
train <- abalone[i,]
test <- abalone[-i,]

# try a regression task
lm1 <- lm(rings~., data=train)
summary(lm1)

# evaluate
pred <- predict(lm1, newdata=test)
cor1 <- cor(pred, test$rings)
mse1 <- mean((pred-test$rings)^2)
print(paste("rmse=", sqrt(mse1)))



# try another model with fewer predictors
lm2 <- lm(rings~sex+diameter+weight.viscera+weight.shucked, data=train)
summary(lm2)

# test on lm2
pred <- predict(lm2, newdata=test)
cor2 <- cor(pred, test$rings)
mse2 <- mean((pred-test$rings)^2)
print(paste("rmse=", sqrt(mse2)))

# recursive feature selection
library(caret)
set.seed(1234)
control <- rfeControl(functions=rfFuncs, method="repeatedcv", 
                      number=5, verbose = TRUE)
rfe(abalone[,1:8], abalone[,9], sizes=1:8, rfeControl=control)
# takes a few minutes, results: top 5 are:
# weight.shucked, sex, weight.shell, weight.viscera, height

lm3 <- lm(rings~weight.shucked+sex+weight.shell+weight.viscera+height, data=train)
summary(lm3)
pred <- predict(lm3, newdata=test)
cor3 <- cor(pred, test$rings)
mse3 <- mean((pred-test$rings)^2)
print(paste("rmse=", sqrt(mse3)))

# search for interactions
x <- model.matrix(rings~.^2, data=abalone)
rfe(x, abalone[,9], sizes=1:8, rfeControl=control)
# takes a long time; results are top 5 variables:
# weight.shucked, length:weight.shucked, diameter:weight.shucked,
#   weight.shell, height:weight.shell

lm4 <- lm(rings~weight.shucked+length*weight.shucked+diameter*weight.shucked+
            weight.shell+height*weight.shell, data=train)
summary(lm4)
pred <- predict(lm4, newdata=test)
cor4 <- cor(pred, test$rings)
mse4 <- mean((pred-test$rings)^2)
print(paste("rmse=", sqrt(mse4)))

lm5 <- lm(log(rings)~sex+length+weight.whole, data=train)
summary(lm5)
pred <- predict(lm5, newdata=test)
cor5 <- cor(pred, test$rings)
mse5 <- mean((pred-test$rings)^2)
print(paste("rmse=", sqrt(mse5)))
