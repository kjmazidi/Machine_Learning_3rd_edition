# Check Your Understanding
# Linear Regression

# 8.1
# load swiss data
# read about the data, type at console: ?swiss
data(swiss)
str(swiss)
head(swiss)
attach(swiss)

# look at fertility
range(Fertility)   # 35.0  92.5
median(Fertility)  # 70.4
# create a new column HighFert > 70
df <- swiss[]
df$HighFert <- factor(ifelse(Fertility>70, 1, 0))
summary(df$HighFert)
# 23=0 and 24=1

# train-test 80-20
set.seed(1234)
i <- sample(1:nrow(swiss), .8*nrow(swiss), replace=FALSE)
# remove Fertility from train and test
train <- df[i,-1]
test <- df[-i, -1]

# try a logistic regression model
glm1 <- glm(HighFert~., data=train, family=binomial)
summary(glm1)
probs <- predict(glm1, newdata=test, type="response")
pred <- ifelse(probs>0.5, 1, 0)
table(pred, test$HighFert)
#pred 0 1
#   0 4 0
#   1 2 4
mean(pred==test$HighFert)  # 0.8

# try knn with unscaled data
library(class)
knn_pred <- knn(train=train[,1:5], test=test[,1:5], cl=train$HighFert, k=2)
table(knn_pred, test$HighFert)
#knn_pred 0 1
#       0 5 2
#       1 1 2
mean(knn_pred==test$HighFert)  # 0.7



