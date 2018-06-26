# Check Your Understanding
# Decision Trees
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

# try logistic regression to pick type based on all other columns
glm1 <- glm(type~., data=train, family=binomial)
summary(glm1)
probs <- predict(glm1, newdata=test, type="response")
pred <- ifelse(probs>0.5, 2, 1)
table(pred, test$type)
#pred red white
#   1 311     3
#   2   4   982
mean(pred==as.integer(test$type))  # .994

# try knn on scaled data
library(class)
# scale the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train_norm <- as.data.frame(lapply(train[,1:12], normalize))
test_norm <- as.data.frame(lapply(test[,1:12], normalize))
knn_pred2 <- knn(train=train_norm, test=test_norm, cl=train$type, k=2)
table(knn_pred2, test$type)
#knn_pred2 red white
#    red   309    25
#    white   6   960
mean(knn_pred2==test$type)  # 0.975

# try decision trees
library(tree)
tree_wine <- tree(type~., data=train)
plot(tree_wine)
text(tree_wine, cex=0.5, pretty=0)

# evaluate
pred <- predict(tree_wine, newdata=test, type="class")
table(pred, test$type)
mean(pred==test$type)  # 0.979
#############################################
# 10.2 Cross Validation
cv_tree <- cv.tree(tree_wine)
plot(cv_tree$size, cv_tree$dev, type="b")
tree_pruned <- prune.tree(tree_wine, best=5)
plot(tree_pruned)
text(tree_pruned, pretty=0)
# evaluate on pruned tree
pred <- predict(tree_pruned, newdata=test, type="class")
table(pred, test$type)
mean(pred==test$type) # 0.977
#############################################
# 10.3 Bagging, Random Forests
library(randomForest)
set.seed(1234)
tree_bagged <- randomForest(type~., data=train, importance=TRUE)
tree_bagged
pred <- predict(tree_bagged, newdata=test, type="class")
table(pred, test$type)
mean(pred==test$type)  # 0.996
# it made 500 trees
# OOB out of bag errors are used to test the tree
# the tree is grown on a bootstrap sample so there are still 
#   observations "in the bag" that can be used for validation
# random Forest (remove importance=TRUE)
set.seed(1234)
forest <- randomForest(type~., data=train)
forest
pred <- predict(forest, newdata=test, type="class")
table(pred, test$type)
mean(pred==test$type)  # 0.996
# very similar to bagging output