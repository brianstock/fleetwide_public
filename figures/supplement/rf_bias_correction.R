# ---
# title: "Bias correction in random forests with simulated data"
# output:
#   html_document: default
#   pdf_document: default
# ---

library(randomForest)
library(ggplot2)
library(ggsidekick)
library(knitr)
library(dplyr)

# ## Simulating data
# We can use the glmmfields package to simulate data from a gaussian random field model. 

# ```{r install, warning=FALSE, message=FALSE}
devtools::install_github("seananderson/glmmfields")
library(glmmfields)
# ```

# The data here is simulated from a single time slice, using 40 knots and 200 data points. The spatial random field is generated in log-space and the observation error model is assumed to be a gamma distribution.  

# ```{r, warning=FALSE, message=FALSE}
set.seed(123)
s = sim_glmmfields(n_knots = 40, n_draws = 1, gp_theta = 0.5, gp_sigma = 0.2,
  mvt=FALSE, n_data_points = 200, sd_obs = 1, obs_error = "gamma")

dat = s$dat
dat$set = sample(c("test", "train"), size = nrow(dat), replace = TRUE)
# ```

# ```{r fig1, fig.cap = "Histogram of simulated data", echo=FALSE, warning=FALSE, message=FALSE}
ggplot(s$dat, aes(y)) + geom_histogram(col="dodgerblue4", fill
 ="dodgerblue4") + theme_sleek() + ylab("Count")
# ```

# \pagebreak  
  
## Fitting a random forest model to the simulated dataset

# We can load the random forest library, and fit a random forest model, using the 2D locations as predictors. 

# ```{r}
library(randomForest)
rf = randomForest(y ~ lon + lat + I(lon^2) + I(lat^2), data = filter(dat, set=="train"))
y_test = filter(dat, set=="test") %>% select(y)
y_test = y_test$y
# ```

# Next we can calculated the predicted out of sample bias on the test set,

# ```{r}
test_predict = predict(rf, filter(dat, set=="test"))
test_bias = test_predict - y_test
# ```

# We can look at the bias (expected - observed) and see that the random forest model is overpredicting on average. 

# ```{r, echo=FALSE, warning=FALSE, message=FALSE}
m = matrix(0, 2, 2)
colnames(m) = c("Estimator", "Bias")
m[,1] = c("Mean bias (test set)", "Median bias (test set)")
m[,2] = c(round(mean(test_bias), 3), round(median(test_bias), 3))
kable(m)
# ```

## Fitting a second random forest model to correct bias

# We can treat the bias from the first model as the response, and use the latitude / longitude again as predictors to generate expecations of the bias at each sample point. 
# ```{r}
test_dat = dat[dat$set=="test",]
test_dat$bias = test_bias
rf_bias = randomForest(test_bias ~ lon + lat + I(lon^2) + I(lat^2), test_dat)

bias_corrected_prediction = test_predict - rf_bias$predicted
bias_corrected_bias = bias_corrected_prediction - dat$y[dat$set=="test"]
# ```

# ```{r, echo=FALSE, warning=FALSE, message=FALSE}
m = matrix(0, 2, 2)
colnames(m) = c("Estimator", "Bias")
m[,1] = c("Mean bias (test set, RF)", "Median bias (test set, RF)")
m[,2] = c(round(mean(bias_corrected_bias), 3), round(median(bias_corrected_bias), 3))
kable(m)
# ```

## Improving bias correction via iteration

# The model prediction above performs the out of sample bias correction once, but we may be able to generate multiple samples by generating replicates from our training data set. 


# ```{r}
iter = 10
train = dat[dat$set=="train",]
test = dat[dat$set=="test",]
pred = matrix(0, nrow = nrow(test), ncol = iter)
for(i in 1:iter) {
  # generate random sample with replacement
  sim = train[sample(seq(1,nrow(train)), size=nrow(train), replace=T),]
  rf = randomForest(y ~ lon + lat + I(lon^2) + I(lat^2), data = sim)
  
  # make a prediction to new data and calculate bias
  test_pred = predict(rf, test)
  test$test_bias = test_pred - test$y
  # grow new random forest to generate expectation of bias @ test locations
  rf_bias = randomForest(test_bias ~ lon + lat + I(lon^2) + I(lat^2), data=test)
  # calculate the bias corrected predictions for this rf
  pred[,i] = (test_pred - predict(rf_bias, test))
    
}
colnames(pred) = paste0("sim_",seq(1,iter))

# ```

# We can average the bias-corrected values and compare the bias in those prediction to the original,

# ```{r, echo=FALSE, warning=FALSE, message=FALSE}
bias_iter = apply(pred - test$y, 2, mean)
m = matrix(0, 3, 3)
colnames(m) = c("Estimator", "Bias", "RMSE")

m[1,1] = c("Mean bias (raw)")
m[1,2] = round(mean(test_bias), 3)
m[1,3] = round(sqrt(mean((test_bias)^2)),3)

m[2,1] = c("Mean bias (test set, RF)")
m[2,2] = round(mean(bias_corrected_prediction - test$y), 3)
m[2,3] = round(sqrt(mean((bias_corrected_prediction - test$y)^2)),3)

m[3,1] = c("Mean bias (test set, 100 RF)")
m[3,2] = round(mean(bias_iter), 3)
m[3,3] = round(sqrt(mean((bias_iter)^2)),3)
kable(m)
# ```

## How many iterations is enough? 

# 10 iterations was used above somewhat arbitrarily. But we can calculate the RMSE as a function of number of iterations to show how RMSE improves with increasing sample size. 

# ```{r}
rf_correct = function(formula, combined_data, iter=5) {
  #combined_data = rename_(combined_data, y = response)
  test_data = filter(combined_data, set=="test")
  n_train = nrow(combined_data) - nrow(test_data)
  
  corrected_predictions = rep(0, nrow(test_data))
  for(i in 1:iter) {
    rf = randomForest(as.formula(paste0("y", formula)),
                      data = filter(combined_data, set!="test")[sample(1:n_train, size=n_train, replace=TRUE),])
    test_data$test_pred = predict(rf, test_data)
  
    rf_bias = randomForest(as.formula(paste0("(test_pred - test_data$y)", formula)), data=test_data)
    corrected_predictions = corrected_predictions + (test_pred - predict(rf_bias, test_data))
  }
  return(corrected_predictions/iter)
}

formula = " ~ lon + lat + I(lon^2) + I(lat^2)"
rf_correct(formula, combined_data=dat)

# ```

# RF bias correction from Xu (2013) pages 40-43

# ```{r}
# 1 iteration = default RF,
# 2 iterations = 1 bias correction
rf_xu = function(formula, combined_data, iter=2) {
  test_data = filter(combined_data, set=="test")
  n_test = nrow(test_data)
  train_data = filter(combined_data, set=="train")
  n_train = nrow(train_data)
  train_data$y_i <- train_data$y
  
  train_oob_pred = matrix(NA, nrow=n_train, ncol=iter+1)
  train_oob_pred[,1] <- train_data$y
  test_pred = test_error = as.data.frame(matrix(NA, nrow=n_test, ncol=iter))
  colnames(test_pred) <- paste0("b_",1:iter)
  colnames(test_error) <- paste0("b_",1:iter)
  for(i in 1:iter) {
    rf = randomForest(as.formula(formula), data = train_data, ntree=1000)
    train_data$y_i <- train_oob_pred[,i+1] <- as.vector(rf$predicted) - train_oob_pred[,i]
    test_pred[,i] <- as.vector(predict(rf, newdata=test_data))
    test_error[,i] <- apply(as.matrix(test_pred[,1:i]),1,sum) - test_data$y
  }
  return(list(test_pred=test_pred, test_error=test_error))
}

formula = "y_i ~ lon + lat + I(lon^2) + I(lat^2)"
iter = 10
res <- rf_correct_bcs(formula, combined_data=dat, iter=iter)
bias <- apply(res$test_error,2,mean) # average error (bias) by iteration
MSE <- apply(abs(res$test_error),2,mean) # MSE by iteration
plot(1:iter, bias/bias[1], type='o', xlab="Iteration", ylab="Normalized error", ylim=c(0,max(max(bias)/bias[1], max(MSE)/MSE[1])))
lines(1:iter, MSE/MSE[1], type='o', lty=2, pch=19)
legend(1, 2.2, c("Bias", "MSE"), lty = 1:2, pch = c(1, 19), box.lwd =NA)

# ```