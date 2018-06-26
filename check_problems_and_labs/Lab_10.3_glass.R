# Lab Chapter 10


library(mlbench)
data(Glass)
df <- Glass[]
str(df)
summary(df$Type)

# train-test split
set.seed(1234)
i <- sample(1:nrow(df), .8*nrow(df), replace=FALSE)
train <- df[i,]
test <- df[-i,]

# try knn 
# scale the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_norm <- as.data.frame(lapply(train[,1:9], normalize))
test_norm <- as.data.frame(lapply(test[,1:9], normalize))
library(class)
pred <- knn(train=train_norm, test=test_norm, cl=train$Type, k=7)
table(pred, test$Type)
acc_knn <- mean(pred==test$Type) # 0.37

# try different k
test_acc <- rep(0, 20)
for (i in 1:20){
  pred <- knn(train=train_norm, test=test_norm, cl=train$Type, k=i)
  test_acc[i] <- mean(pred==test$Type)
}
which.max(test_acc) # k=1
# try k=1
pred <- knn(train=train_norm, test=test_norm, cl=train$Type, k=)
table(pred, test$Type)
acc_knn2 <- mean(pred==test$Type) # 0.51

# Decision Tree
library(tree)
tree1 <- tree(Type~., data=train)
plot(tree1)
text(tree1, cex=0.5, pretty=0)
# evaluate
pred <- predict(tree1, newdata=test, type="class")
table(pred, test$Type)
acc_tree <- mean(pred==test$Type)  # .51


# random Forest
library(randomForest)
set.seed(1234)
tree_bagged <- randomForest(Type~., data=train)
pred <- predict(tree_bagged, newdata=test)
table(pred, test$Type)
acc_forest <- mean(pred==test$Type) # 0.79

# results
# forest did best by far
# tree was better than knn