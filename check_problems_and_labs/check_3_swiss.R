# Check Your Understanding
# Linear Regression

# 3.1
# load swiss data
# read about the data, type at console: ?swiss
data(swiss)
str(swiss)
head(swiss)

# plot
plot(swiss$Fertility~swiss$Education)
# there seems to be a general downward trend

# build a model predicting
lm1 <- lm(Fertility~Education, data=swiss)
summary(lm1)

# Education had a low p-value which indicates
#  that Education helps predict Fertility
# Education has a negative coefficient, meaning 
#  that higher education leads to lower birth rate
# The R-squared of 0.44 indicates that some variance
#  in the data is explained by Education, but it is not high
# Formula: Fertility = 79.6101 - 0.8624*Education
# for Education=11:  = 70.1 (mean is 70.1 as well)

# 3.2
cov(swiss$Fertility, swiss$Education)
cor(swiss$Fertility, swiss$Education)
# the covariance is negative, meaning 
#  that Fertility and Education are trending opposite
# the correlation is strongly negative, meaning
#  that as Education increases, Fertility decreases
# order does not matter for cov or cor
# t-value =pre
-0.8624/0.1448
# this is same absolute value as t-value
# compute metrics manually:
rss <- sum(lm1$residuals^2)
rse <- sqrt(rss*(1/(length(swiss$Fertility)-2)))
r_2 <- 1 - rss/sum((swiss$Fertility - mean(swiss$Fertility))^2)
# make up some test data
test <- swiss[c(4, 17, 28, 31, 43),]
pred <- predict(lm1, newdata=test)
# print side by side, surprisingly not that close
print(cbind(pred, test$Fertility))
cor(pred, test$Fertility)
mse <- mean((pred - test$Fertility)^2)
sqrt(mse)
mean(lm1$residuals)
# the rmse for the randomly chosen items was 7.5
#   and the mean of the residuals was -7.2, 
#   which is close in absolute value;
#   not surprising  because they are measuring the same thing

# 3.3
x <- swiss$Education
y <- swiss$Fertility
x_mean <- mean(x)
y_mean <- mean(y)
w_hat <- sum((x-x_mean)*(y-y_mean)) / sum((x-x_mean)^2)
b_hat <- y_mean - w_hat  * x_mean
print(paste("w and b estimates = ", w_hat, b_hat))
lm1$coefficients
# means are used to compute average spread of x, y
#    from the means and this determines the regression line

# 3.4
x_m <- matrix(cbind(rep(1, nrow(swiss)), x), ncol=2)
y_m <- matrix(y)
solve(t(x_m) %*% x_m) %*% t(x_m) %*% y
lm1$coefficients

# 3.5
cor(swiss)
pairs(swiss)
# Education and Examination seem to be correlated
# Education and Examination are both correlated with Fertililty
lm2 <- lm(Fertility~., data=swiss)
summary(lm2)
# just use best predictors
lm3 <- lm(Fertility~Education+Catholic+Infant.Mortality, data=swiss)
summary(lm3)
# metrics  lm1, lm2, lm3
# RSE:     9.4  7.2  7.5
# R2:      .44  .7   .66
anova(lm1, lm2, lm3)
# lm2 is the best model
plot(lm2)
# plot 1 - a dip in the red line, possibly indicating
#   that Rive Gauche and Porrenruy and Sierre may be outliers
# plot 2
# some of the residuals veer off the diagonal line
# plot 3
# the line is not horizontal
# plot 4
# there seem to be some outliers which require further investigation

# 3.6
lm4 <- lm(Fertility~poly(Education, 2), data=swiss)
lm5 <- lm(Fertility~poly(Education, 3), data=swiss)
anova(lm1, lm4, lm5)
# The polynomial models only slightly lowered RSS and 
#   did not have low p-values, so probably only
#   getting slightly better results from overfitting


# 3.7
lm6 <- lm(Fertility~.+Education*Infant.Mortality, data=swiss)
anova(lm2, lm6)
summary(lm6)
# the interaction predictor did not have a low p-value but
#   it was lower than Infant.Mortality by itself. 
# the anova results provide little justification for including it

