# Check Your Understanding
# Neural Network


#############################################
# 12.1
# load abalone
abalone <- read.csv("abalone.csv", header=FALSE)
names(abalone) <- c("sex", "length", "diameter", "height", "weight.whole",
                    "weight.shucked", "weight.viscera", "weight.shell", "rings")
abalone$sex <- as.integer(abalone$s)
str(abalone)
head(abalone)
attach(abalone)

# train-test split
set.seed(1234)
i <- sample(1:nrow(abalone), .8*nrow(abalone), replace=FALSE)
train <- abalone[i,]
test <- abalone[-i,]

# try linear regression 
lm1 <- lm(rings~., data=train)
summary(lm1)
# evaluate
pred <- predict(lm1, newdata=test)
cor1 <- cor(pred, test$rings)  # 0.72
mse1 <- mean((pred-test$rings)^2) # 4.59
print(paste("rmse=", sqrt(mse1))) # 2.14

# try knn
# scale the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_norm <- as.data.frame(lapply(train[,1:9], normalize))
test_norm <- as.data.frame(lapply(test[,1:9], normalize))
library(caret)
knn1 <- knnreg(train_norm, train$rings, k=30)
pred1 <- predict(knn1, test_norm)
cor2 <- cor(pred1, test$rings)  # 0.9495877
mse2 <- mean((pred1 - test$rings)^2) # 2.486
rmse <- sqrt(mse2) # 1.06

# try neural network
library(neuralnet)
n <- names(train_norm)
f <- as.formula(paste("rings ~", paste(n[!n %in% "rings"], collapse = " + ")))
set.seed(1234)  # this should not be necessary but it is
nn1 <- neuralnet(f, data=train_norm, hidden=c(6,4), threshold = 0.01, linear.output=TRUE)
plot(nn1)
# evaluate
pred <- compute(nn1, test_norm[,-9])
pred <- pred$net.result
pred_unscale <- pred * (max(test$rings) - min(test$rings)) + min(test$rings)
cor_nn1 <- cor(pred_unscale, test$rings)
mse_nn1 <- mean((pred_unscale - test$rings)^2)

# results cor and mse
# linear 0.72 and 4.59
# knn    0.96 and 1.13
# nn     0.43 and 12.9
## nn did poorly
