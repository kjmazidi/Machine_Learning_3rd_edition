# Check 13.1
# Using Keras for Regression
# Karen Mazidi

# Boston housing data is in the Keras package, but here
#   we want to learn how to load exteranal data sets for Keras

library(keras)
library(MASS)
df <- Boston[]
str(df)

N <- nrow(df)
p <- ncol(df)
t <- 14   # target column

X <- Boston[, -t]
Y <- Boston[, t]

set.seed(1234)
i <- sample(1:nrow(df), 0.8*nrow(df), replace=FALSE)
X_train <- data.matrix(X[i, -t])
Y_train <- Y[i]
X_test <- data.matrix(X[-i, -t])
Y_test <- Y[-i]

# normalize data
# in keeping with advice from keras book, use train data for normalization
#   of train and test data (not labels)
means <- apply(X_train, 2, mean)
stdvs <- apply(X_train, 2, sd)
X_train <- scale(X_train, center=means, scale=stdvs)
X_test <- scale(X_test, center=means, scale=stdvs)


# build a model
model <- keras_model_sequential()
model %>%
  layer_dense(units=64, activation='relu', input_shape = dim(X_train)[[2]]) %>%
  layer_dense(units=64, activation='relu') %>%
  layer_dense(units=1)

model %>% compile(
  loss = 'mse',
  optimizer = 'rmsprop',
  metrics = c("mae")
)

model %>% fit(X_train, Y_train, epochs=100, batch_size=1, verbose=1)

results <- model %>% evaluate(X_test, Y_test, verbose=0)
results$mean_absolute_error 
# 5.98 on unscaled data, 1 hidden layer with 6 units
# 4.5 on scaled data
# 2.54 on 2 layers 64/64
# 2.43 epochs increased from 10 to 100
# off about $2,436 dollars per house

# how do you get predictions?
pred <- predict(model, X_test)
cor(pred, Y_test)  # 0.948799
mse <- mean((pred - Y_test)^2)
sqrt(mse)
# rmse =3.24, is higher than the absolute mean error 
mae <- mean(abs(pred - Y_test))
# the formula above gives us the mae

