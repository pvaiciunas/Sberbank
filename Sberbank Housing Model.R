library(dplyr)
library(xgboost)


# Load the data

train <- read.csv("C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/train.csv/train.csv")
test <- read.csv("C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/test.csv/test.csv")
macro <- read.csv("C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/macro.csv/macro.csv")

full_train <- merge(train, macro)
full_test <- merge(test, macro)

# Keep only the factors that have at least half the data available in training set
full_train <- full_train[colSums(is.na(full_train)) < nrow(full_train)/2]

# Use those same factors in the test set
full_test <- full_test[, which(names(full_test) %in% names(full_train))]

# Change factors to integers
for(i in 1:ncol(full_train)){
  if(is.factor(full_train[,i])) {
    full_train[,i] <- as.integer(full_train[,i])
  }
}

for(i in 1:ncol(full_test)){
  if(is.factor(full_test[,i])) {
    full_test[,i] <- as.integer(full_test[,i])
  }
}

# Fill in the missing data with median
# This might make the sub-area factor useless. Might want to remove.
# Also curently using median values from test set within the test set. Might be
# better to use the median values from the training set
for(i in 1:ncol(full_train)){
  full_train[is.na(full_train[,i]), i] <- median(full_train[,i], na.rm = TRUE)
}

for(i in 1:ncol(full_test)){
  full_test[is.na(full_test[,i]), i] <- median(full_test[,i], na.rm = TRUE)
}


# Fit a simple linear model
# model <- lm(price_doc ~ ., data = processed_train)
# fit <- predict(model, processed_test)
# 
# submission <- data.frame(id = processed_test$id, price_doc = fit)

# Fit an XGBoost model

train_x <- full_train
train_x$price_doc <- NULL
train_y <- full_train$price_doc

test_x <- full_test

pmt = proc.time()
model = xgboost(data = as.matrix(train_x), 
                label = train_y,
                eta = 0.05,
                max_depth = 6, 
                nround=250, 
                subsample = 0.75,
                colsample_bytree = 0.5,
                seed = 100,
                booster = "gbtree",
                objective = "reg:linear",
                missing = NaN,
                silent = 1)
show(proc.time() - pmt)

pred = predict(model,  as.matrix(test_x), missing=NaN)
pred_matrix = matrix(pred, nrow = nrow(full_test), byrow = TRUE)

submission <- data.frame(id = full_test$id, price_doc = pred_matrix)
submission$price_doc <- ifelse(submission$price_doc < 0, median(train_y), submission$price_doc)

write.csv(as.matrix(submission), 
          "C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/submission.csv",
          row.names = FALSE)
