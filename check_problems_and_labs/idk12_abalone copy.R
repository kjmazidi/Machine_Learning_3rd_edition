# Chapter 12
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

# try neural net
library(neuralnet)
train$sex <- as.integer(train$sex)
test$sex <- as.intger(test$sex)
n <- names(train)
f <- as.formula(paste("rings ~ ", paste(n[!n %in% "rings"], collapse = " + ")))
nn1 <- neuralnet(f, data=train, hidden=3, threshold = 0.05,
                 linear.output=TRUE)
plot(nn1)
pred <- compute(nn1, test)
cor_nn <- cor(pred$net.result, test$rings)
