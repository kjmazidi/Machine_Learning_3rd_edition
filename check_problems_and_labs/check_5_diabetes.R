# Check Your Understanding Chapter 5
# Pima Indians data from mlbench

# 5.1
library(mlbench)
data(PimaIndiansDiabetes2)
df <- PimaIndiansDiabetes2[]
str(df)
attach(df)

# which columns?
sapply(df, function(x) sum(is.na(x)))

# replace triceps and insulin NAs with mean
df$triceps[is.na(df$triceps)] <- mean(df$triceps, na.rm=TRUE)
df$insulin[is.na(df$insulin)] <- mean(df$insulin, na.rm=TRUE)
df <- df[complete.cases(df),]
str(df)

# train-test split
set.seed(1234)
i <- sample(1:nrow(df), .8*nrow(df), replace=FALSE)
train <- df[i,]
test <- df[-i,]

# create a logstic regression model on cleaned data
glm1 <- glm(diabetes~., data=train, family=binomial)
summary(glm1)
probs <- predict(glm1, newdata=test, type="response")
pred <- ifelse(probs>0.5, 2, 1)
results_glm11 <- table(pred, test$diabetes)
results_glm1
acc_glm1 <- mean(pred==as.integer(test$diabetes), na.rm=TRUE)

# try naive Bayes
library(e1071)
nb1 <- naiveBayes(diabetes~., data=train)
nb1
pred <- predict(nb1, newdata=test, type="class")
results_nb1 <- table(pred, test$diabetes)
results_nb1
acc_nb1 <- mean(pred==test$diabetes)

# did replacing triceps and insuling NAs with means hurt the performance?
# exploring triceps and insulin by class
df <- PimaIndiansDiabetes2[]
n <- which(df$diabetes=="pos")
mean(df$triceps[n], na.rm=TRUE)  # 33
mean(df$triceps[-n], na.rm=TRUE)  # 27
mean(df$insulin[n], na.rm=TRUE)  # 206.8
mean(df$insulin[-n], na.rm=TRUE) # 130

# replace triceps and insulin NAs with class-conditional mean
df$triceps[which(df$diabetes=="pos" & is.na(df$triceps))] <- mean(df$triceps[n], na.rm=TRUE)
df$insulin[which(df$diabetes=="pos" & is.na(df$insulin))] <- mean(df$insulin[n], na.rm=TRUE)
df$triceps[which(df$diabetes=="neg" & is.na(df$triceps))] <- mean(df$triceps[-n], na.rm=TRUE)
df$insulin[which(df$diabetes=="neg" & is.na(df$insulin))] <- mean(df$insulin[-n], na.rm=TRUE)
df <- df[complete.cases(df),]
str(df)


# train-test split again
set.seed(1234)
i <- sample(1:nrow(df), .8*nrow(df), replace=FALSE)
train <- df[i,]
test <- df[-i,]

# create a logstic regression model again
glm2 <- glm(diabetes~., data=train, family=binomial)
summary(glm2)
probs <- predict(glm2, newdata=test, type="response")
pred <- ifelse(probs>0.5, 2, 1)
results_glm2 <- table(pred, test$diabetes)
results_glm2
acc_glm2 <- mean(pred==as.integer(test$diabetes), na.rm=TRUE)

# try naive Bayes again
nb2 <- naiveBayes(diabetes~., data=train)
nb2
pred <- predict(nb2, newdata=test, type="class")
results_nb2 <- table(pred, test$diabetes)
results_nb2
acc_nb2 <- mean(pred==test$diabetes)
