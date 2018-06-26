# Check Your Understanding
# Neural Network
# Using a combined red and white wine data from UCI ML Repository
# https://archive.ics.uci.edu/ml/datasets/Wine+Quality
# red and white wine csv files were combined into one
# with new column type


#############################################
# 13.2
# load wine data  6497 x 12
library(keras)
wine <- read.csv("wine_all.csv", header=TRUE)
str(wine)

N <- nrow(wine)
p <- ncol(wine)
t <- 13             # target is type

X <- wine[, -t]
Y <- wine[, t]
Y <- as.integer(Y) 
Y <- Y - 1L

# train and test
set.seed(1234)
i <- sample(1:nrow(wine), .8*nrow(wine), replace=FALSE)
X_train <- data.matrix(X[i, -t])
Y_train <- Y[i]
X_test <- data.matrix(X[-i, -t])
Y_test <- Y[-i]

# normalize data
means <- apply(X_train, 2, mean)
stdvs <- apply(X_train, 2, sd)
X_train <- scale(X_train, center=means, scale=stdvs)
X_test <- scale(X_test, center=means, scale=stdvs)
Y_train <- to_categorical(Y_train, 2)
Y_test <- to_categorical(Y_test, 2)

# build a model
model <- keras_model_sequential()
model %>%
  layer_dense(units=16,  activation='relu', input_shape = dim(X_train)[[2]]) %>%
  layer_dense(units=8, activation='relu') %>%
  layer_dense(units=2, activation='sigmoid')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'rmsprop',
  metrics = c('accuracy')
)

model %>% fit(X_train, Y_train, epochs=10, batch_size=1, verbose=1)

results <- model %>% evaluate(X_test, Y_test, verbose=0)
results
pred <- predict(model, X_test)
pred <- ifelse(pred[,1] > 0.5, 1, 2)
Y_test_fact <- Y[-i]
Y_test_fact <- Y_test_fact + 1L
table(pred, Y_test_fact)
mean(pred==Y_test_fact)  # 99%
