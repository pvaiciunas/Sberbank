library(dplyr)



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

# fill in the missing values with the median value

# Select the factors you want to use. Expand this later.
factors <- c("price_doc",
             "id",
             "unemployment",
             "salary_growth",
             "mortgage_growth",
             "pop_natural_increase",
             "deposits_rate",
             "gdp_quart_growth",
             "eurrub",
             "green_zone_part",
             "children_school",
             "full_sq",
             "area_m",
             "product_type",
             "preschool_education_centers_raion",
             "university_top_20_raion",
             "shopping_centers_raion",
             "office_raion")

processed_train <- full_train[factors]
processed_test <- full_test[factors]

# Fit a simple linear model
model <- lm(price_doc ~ ., data = processed_train)
fit <- predict(model, processed_test)

submission <- data.frame(id = processed_test$id, price_doc = fit)

