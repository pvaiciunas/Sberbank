
# This borrows from phillipssp:
# https://www.kaggle.com/philippsp/an-updated-collection-of-new-features/code

library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(caret)
library(Matrix)
library(xgboost)
library(geosphere)
library(rgdal)
library(sp)
library(mlr)


train <- read.csv("C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/train.csv/train.csv")
test <- read.csv("C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/test.csv/test.csv")
macro <- read.csv("C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/macro.csv/macro.csv")

################################################################
# Making use of piping formulas instead of regular dplyr approach
################################################################

# To save coding space we are going to combine the test and training sets together 
# and then split them apart at the end once we're ready for the model

test2 <- test
test2$price_doc <- NA
trest <- rbind(train, test2)

############################
# Clean up the data a little
############################
trest <- trest %>%
  mutate(max_floor = as.numeric(max_floor),
         kitch_sq = as.numeric(kitch_sq),
         num_room = as.numeric(num_room),
         build_year = as.numeric(build_year),
         sub_area = as.factor(sub_area),
         num_room = as.numeric(num_room),
         product_type = factor(product_type),
         sub_area = factor(sub_area))

trest <- trest %>%
  filter(build_year < 2020 | is.na(build_year))

trest <- trest %>%
  mutate(strange_full_sq = ifelse(full_sq <= 1, full_sq + 1, 0),
         full_sq = ifelse(full_sq > 800 | full_sq <= 1, NA, full_sq))

trest <- trest %>% 
  mutate(strange_life_sq = ifelse(life_sq <= 1, life_sq+1,0), 
         strange_life_sq = ifelse(is.na(strange_life_sq),0,strange_life_sq), 
         life_sq = ifelse(life_sq > 400 | life_sq <= 1, NA, life_sq))

trest <- trest %>% 
  mutate(kitch_sq = as.numeric(kitch_sq),
         strange_kitch_sq = ifelse(kitch_sq <= 1, kitch_sq+1,0),
         kitch_sq = ifelse(kitch_sq > 200 | kitch_sq <= 1, NA, kitch_sq))

trest <- trest %>% 
  mutate(build_year = as.numeric(build_year), 
         strange_build_year = ifelse(build_year <= 1, build_year+1,0), 
         build_year = ifelse(build_year > 2018 | build_year < 1860, NA, build_year))

trest <- trest %>% 
  mutate(floor = ifelse(floor > 45, NA, floor))

trest <- trest %>% 
  mutate(max_floor = as.numeric(max_floor), 
         strange_max_floor = ifelse(max_floor <= 1, max_floor+1,0), 
         max_floor = ifelse(max_floor > 60 | max_floor <=1, NA, max_floor))

trest <- trest %>% 
  mutate(state = as.numeric(state), 
         state = ifelse(state > 4, NA, state))

trest <- trest %>% 
  mutate(material = as.factor(material), 
         material = ifelse(material == 3, NA, material))



# more cleaning
trest <- trest %>% mutate(num_room = ifelse(num_room==0,NA,num_room))


#####################
# Timesteamp Features
#####################

trest <- trest %>%
  mutate(year = year(timestamp),
         year_month = make_date(year(timestamp), month(timestamp)),
         month_of_year = month(timestamp),
         week_of_year = week(timestamp),
         day_of_month = day(timestamp),
         day_of_week = wday(timestamp))

#######################
# House Characteristics
#######################

# number of floors to the top of house
trest <- trest %>% 
  mutate(floor_from_top = max_floor - floor)

# relative position of floor in house
trest <- trest %>% 
  mutate(floor_by_maxfloor = floor/max_floor)

# average room size
trest <- trest %>% 
  mutate(roomsize = (life_sq-kitch_sq)/num_room) 

# relative proportion of living area
trest <- trest %>% 
  mutate(life_proportion = life_sq/full_sq)

# relative proportion of kitchen area
trest <- trest %>% 
  mutate(kitchen_proportion = kitch_sq/full_sq)

# extra area
trest <- trest %>% 
  mutate(extra_area = full_sq - life_sq)

# age of house at time of sale
trest <- trest %>% 
  mutate(age_at_sale = interval(make_date(year=build_year),timestamp) / years(1))  


##############################
# Grouping Apartments together
##############################

# Assign a common name. A Combination of the sub-area and distance to metro
trest <- trest %>%
  mutate(apartment_name = factor(str_c(sub_area, format(metro_km_avto, digits = 3))))

# Get the number of apartments in the group
trest <- trest %>%
  group_by(apartment_name) %>%
  tally() %>%
  right_join(trest, by = "apartment_name")

######################
# Sale Characteristics
######################

trest <- trest %>% 
  group_by(year_month) %>% 
  summarize(n_sales_permonth = n()) %>% 
  right_join(trest,by="year_month")

###########################
# Sub-area characteristics
###########################

# We are using external data here that was created by a kaggler
# The file has all the Russian regions organized by coordinates that
# can be used to fit the existing data

# This is using the 'sp' package that works with Spatial Vector Objects
shp <- readOGR(dsn = "C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/administrative-divisions-of-moscow",
               layer = "moscow_adm")

centroids <- coordinates(shp)
sub_area <- shp$RAION
okrug <- shp$OKRUGS
location_data <- data.frame(sub_area = sub_area, okrug = okrug, longitude=centroids[,1], latitude=centroids[,2])

trest <- trest %>%
  left_join(location_data, by = "sub_area") # all rows from trest

# Calculate distance from Kremlin for each sub_area
kremlin = data.frame(longitude = 37.617664,latitude = 55.752121)
trest <- trest %>%
  group_by(sub_area) %>%
  top_n(n = 1, wt = id) %>%
  ungroup %>%
  mutate(distance_from_kremlin = distm(.[c("longitude", "latitude")], kremlin, fun = distHaversine)) %>%
  select(sub_area, distance_from_kremlin) %>%
  right_join(trest, by = "sub_area") # all rows from 'trest'
  

######################
# Pricing of sub areas
######################

# average price per raion
trest <- trest %>%
  group_by(sub_area) %>%
  summarize(mean_price_raion = mean(price_doc, na.rm = TRUE)) %>%
  right_join(trest, by = "sub_area")

# average price per raion per year
trest <- trest %>%
  group_by(sub_area, year) %>%
  summarize(mean_price_raion_year = mean(price_doc, na.rm = TRUE)) %>%
  right_join(trest, by = c("sub_area", "year"))

# average price per sqm per raion
trest <- trest %>%
  group_by(sub_area) %>%
  summarize(mean_price_persqm_raion = mean(price_doc / full_sq, na.rm = T)) %>%
  right_join(trest, by = "sub_area")


############################
# Population characteristics
############################
  
# Population density per raion
trest <- trest %>% 
  mutate(pop_density_raion = raion_popul/area_m)

# Demographic structure of the raions
trest <- trest %>% 
  mutate(young_proportion = young_all/full_all) # proportion of people younger than working age
trest <- trest %>% 
  mutate(work_proportion = work_all/full_all) # proportion of people in working age
trest <- trest %>% 
  mutate(retire_proportion = ekder_all/full_all) # proportion of people older than working age

###########################
# Building Information
###########################

# average building height per raion
trest <- trest %>% 
  group_by(sub_area) %>% 
  summarize(mean_building_height = mean(max_floor,na.rm=T)) %>% 
  right_join(trest,by="sub_area")

# Proportion of houses with certain build materials (e.g. 10% build_count_wood)
trest<-trest %>% 
  mutate_each(funs(pct = (.)/raion_build_count_with_builddate_info),matches('^build_count_[a-zA-Z]*$'))


#############################
# Educational Characteristics
#############################

# ratio of number of pupils and preschool seats
trest <- trest %>% 
  mutate(ratio_preschool = children_preschool / preschool_quota)

# ratio of number of pupils and school seats
trest <- trest %>% 
  mutate(ratio_school = children_school / school_quota)


#########################
# dataset related features
########################

# number of missing values per row (this is going to take a while)
trest <- trest %>% 
  mutate(count_na_perrow = apply(., 1, function(x) sum(is.na(x))))

###################
#Features
###################
trest <- left_join(trest, macro, by= "timestamp")

# Let's split the training and test sets apart again:
test2 <- trest[is.na(trest$price_doc),]
train2 <- trest[!is.na(trest$price_doc),]

basic_features <- c("full_sq", "life_sq", "kitch_sq", "num_room", "floor", "max_floor", "material", "build_year", "state", "product_type")
macro_features <- c()

new_features <- c("month_of_year","week_of_year", "day_of_month", "day_of_week", "floor_from_top", "floor_by_maxfloor", "roomsize", "life_proportion", "kitchen_proportion", "extra_area", "age_at_sale", "n_sales_permonth", "distance_from_kremlin", "young_proportion", "work_proportion", "retire_proportion", "mean_building_height", "ratio_preschool",
                  "ratio_school", "count_na_perrow")

predictors <- c(basic_features, macro_features, new_features)

train_outcomes <- train2$price_doc

train_set <- select(train2, one_of(predictors))
test_set <- select(test2, one_of(predictors))

# To use XGBoost, we need to replace all the factor and character variables
# There is only one here, product_type with two values, and this will break it up
# Into two separate variables whose values are 0s and 1s 
dummy_train <- dummyVars(~., train_set)
dummy_test <- dummyVars(~., test_set)

train_matrix <- predict(dummy_train,train_set)
test_matrix <- predict(dummy_test,test_set)

# HEre we create a sparse matrix, and then am XGBoost matrix object to use in the algo
train_sparse <- Matrix(train_matrix,sparse = T)
test_sparse <- Matrix(test_matrix,sparse = T)

dtrain <- xgb.DMatrix(data = train_sparse,label=outcomes)
dtest <- xgb.DMatrix(data = test_sparse)

############################
# Now let's create the model
############################

#xgboost params
param <- list(objective="reg:linear",
              eval_metric = "rmse",
              eta = .2,
              gamma = 1,
              max_depth = 4,
              min_child_weight = 1,
              subsample = .7,
              colsample_bytree = .7
)

# CV for number of rounds
xgb_cv <- xgb.cv(data = dtrain,
                 nfold = 2,
                 params = param,
                 nrounds = 150000,
                 maximize = FALSE,
                 prediction = TRUE,
                 early.stop.round = 50,
                 print_every_n = 50,
                 verbose = 0
)
rounds <- xgb_cv$best_iteration  

# Train model
xgb_model <- xgb.train(data = dtrain,
                       params = param,
                       watchlist = list(train = dtrain),
                       nrounds = rounds,
                       verbose = 0
)


# See what factors matter the most:
imp_matrix_all <- xgb.importance(colnames(train_sparse),xgb_model)
imp_matrix_all <- imp_matrix_all %>% mutate(group = ifelse(Feature %in% new_features,"new","old"))
ggplot(imp_matrix_all,aes(x=reorder(Feature,Gain),y=Gain, fill=group))+geom_bar(stat="identity")+coord_flip(ylim=c(0,0.2))+theme_bw()+labs(x="")

####################
# Predict and Submit
####################

xgbpred <- predict(xgb_model, dtest)

submission <- data.frame(id = test$id, price_doc = xgbpred)
submission$price_doc <- ifelse(submission$price_doc < 0, 
                               median(train$price_doc), 
                               submission$price_doc)

write.csv(as.matrix(submission), 
          "C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/submission2.csv",
          row.names = FALSE)



##########
#### TODO
##########


# Add more factors from the training data
# Add Macro factors
# Cross validate to rune parameters. See:
# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
