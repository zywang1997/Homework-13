library(caret)
library(dplyr)

# fit the data to a neural net, nnet model in caret
# the nnet model has the following parameters:  size, decay
tuning_df <- data.frame(size=9, decay=.1) #Size and decay have to be the same size
#tuning_df <- data.frame(size=8:12, decay=0)

fitControl <- trainControl(method="none")

fitControl <- trainControl(## 2-fold CV
  method = "repeatedcv",
  number = 2,
  repeats = 2)

t_out <- caret::train(x=x, y=y, method="nnet",
                      trControl = fitControl,
                      tuneGrid=tuning_df, maxit=1000, MaxNWts=10000)

true_y <- y
pred_y <- predict(t_out, x)

n_samples <- nrow(x)
error <- sum(true_y != pred_y)/n_samples

pred_error <- error
cat("train1 prediction error", pred_error, "\n")

#Now recreating that to compare to the test data
true_y2 <- y2
pred_y2 <- predict(t_out, x2)

n_samples2 <- nrow(x2)
error2 <- sum(true_y2 != pred_y2)/n_samples2

pred_error2 <- error2
cat("test prediction error", pred_error2, "\n")