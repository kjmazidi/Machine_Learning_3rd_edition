# Check Your Understanding
# Neural Network
# Using a combined red and white wine data from UCI ML Repository
# https://archive.ics.uci.edu/ml/datasets/Wine+Quality
# red and white wine csv files were combined into one
# with new column type


#############################################
# 10.1
# load wine data  6497 x 12
wine <- read.csv("wine_all.csv", header=TRUE)
str(wine)

# train and test
set.seed(1234)
i <- sample(1:nrow(wine), .8*nrow(wine), replace=FALSE)
train <- wine[i,]
test <- wine[-i,]
# check distrib of white/red 
summary(train$type)  # red 1284  white 3913
summary(test$type)   # red 315   white 985

# scale the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_norm <- as.data.frame(lapply(train[,1:12], normalize))
test_norm <- as.data.frame(lapply(test[,1:12], normalize))

#############################################
# try neural net
library(neuralnet)
train_norm <- cbind(train_norm, train[,13])
colnames(train_norm)[[13]] <- "type"
str(train_norm)
n <- length(train_norm$type)
x <- matrix(0, n, 2)  # 2 levels
x[(1L:n) + n * (unclass(train_norm$type) - 1L)] <- 1
dimnames(x) <- list(names(train_norm$type), levels(train_norm$type))
train_norm <- cbind(train_norm[,-13], x)
str(train_norm)
n <- names(train_norm)
f <- as.formula(paste("red + white ~ ", paste(n[!n %in% c("red", "white")], collapse = " + ")))
set.seed(1234)
nn1 <- neuralnet(f, data=train_norm, hidden=c(16, 12, 8), act.fct="logistic", linear.output=FALSE, lifesign="full")

pred <- compute(nn1, test_norm[-13])
pred <- pred$net.result
pred <- ifelse(pred[,1] > 0.5, 1, 2)
mean(pred==as.integer(test$type))  
table(pred, as.integer(test$type))

# results
# hidden (16)  acc = 0.633
# hidden (16, 12, 8) = 0.79