
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

basic_features <- c('full_sq',
                    'floor',
                    'material',
                    'num_room',
                    'state',
                    'raion_popul',
                    'indust_part',
                    'preschool_quota',
                    'children_school',
                    'school_education_centers_raion',
                    'hospital_beds_raion',
                    'university_top_20_raion',
                    'additional_education_raion',
                    'culture_objects_top_25_raion',
                    'office_raion',
                    'incineration_raion',
                    'radiation_raion',
                    'big_market_raion',
                    'detention_facility_raion',
                    'male_f',
                    'young_all',
                    'young_female',
                    'work_male',
                    'ekder_all',
                    'ekder_female',
                    'raion_build_count_with_material_info',
                    'build_count_wood',
                    'build_count_brick',
                    'build_count_panel',
                    'build_count_slag',
                    'raion_build_count_with_builddate_info',
                    'build_count_1921.1945',
                    'build_count_1971.1995',
                    'ID_metro',
                    'metro_km_avto',
                    'metro_km_walk',
                    'school_km',
                    'green_zone_km',
                    'water_treatment_km',
                    'incineration_km',
                    'railroad_station_walk_min',
                    'railroad_station_avto_km',
                    'ID_railroad_station_avto',
                    'public_transport_station_min_walk',
                    'water_1line',
                    'ttk_km',
                    'bulvar_ring_km',
                    'big_road1_km',
                    'big_road1_1line',
                    'ID_big_road2',
                    'railroad_1line',
                    'ID_railroad_terminal',
                    'ID_bus_terminal',
                    'nuclear_reactor_km',
                    'power_transmission_line_km',
                    'ts_km',
                    'market_shop_km',
                    'swim_pool_km',
                    'stadium_km',
                    'hospice_morgue_km',
                    'public_healthcare_km',
                    'workplaces_km',
                    'office_km',
                    'preschool_km',
                    'church_synagogue_km',
                    'theater_km',
                    'exhibition_km',
                    'ecology',
                    'green_part_1000',
                    'office_count_1000',
                    'trc_count_1000',
                    'cafe_count_1000',
                    'cafe_sum_1000_max_price_avg',
                    'cafe_count_1000_na_price',
                    'cafe_count_1000_price_1000',
                    'cafe_count_1000_price_2500',
                    'cafe_count_1000_price_high',
                    'church_count_1000',
                    'leisure_count_1000',
                    'market_count_1000',
                    'green_part_5000',
                    'office_count_5000',
                    'trc_count_5000',
                    'cafe_count_5000',
                    'cafe_sum_5000_max_price_avg',
                    'cafe_count_5000_na_price',
                    'cafe_count_5000_price_1000',
                    'cafe_count_5000_price_2500',
                    'cafe_count_5000_price_high',
                    'church_count_5000',
                    'leisure_count_5000',
                    'market_count_5000',
                    'life_sq',
                    'max_floor',
                    'build_year',
                    'kitch_sq',
                    'product_type',
                    'area_m',
                    'green_zone_part',
                    'children_preschool',
                    'preschool_education_centers_raion',
                    'school_quota',
                    'school_education_centers_top_20_raion',
                    'healthcare_centers_raion',
                    'sport_objects_raion',
                    'culture_objects_top_25',
                    'shopping_centers_raion',
                    'thermal_power_plant_raion',
                    'oil_chemistry_raion',
                    'railroad_terminal_raion',
                    'nuclear_reactor_raion',
                    'full_all',
                    'female_f',
                    'young_male',
                    'work_all',
                    'work_female',
                    'ekder_male',
                    'build_count_block',
                    'build_count_frame',
                    'build_count_monolith',
                    'build_count_foam',
                    'build_count_mix',
                    'build_count_before_1920',
                    'build_count_1946.1970',
                    'build_count_after_1995',
                    'metro_min_avto',
                    'metro_min_walk',
                    'kindergarten_km',
                    'park_km',
                    'industrial_km',
                    'cemetery_km',
                    'railroad_station_walk_km',
                    'ID_railroad_station_walk',
                    'railroad_station_avto_min',
                    'public_transport_station_km',
                    'water_km',
                    'mkad_km',
                    'sadovoe_km',
                    'kremlin_km',
                    'ID_big_road1',
                    'big_road2_km',
                    'railroad_km',
                    'zd_vokzaly_avto_km',
                    'bus_terminal_avto_km',
                    'oil_chemistry_km',
                    'radiation_km',
                    'thermal_power_plant_km',
                    'big_market_km',
                    'fitness_km',
                    'ice_rink_km',
                    'basketball_km',
                    'detention_facility_km',
                    'university_km',
                    'shopping_centers_km',
                    'additional_education_km',
                    'big_church_km',
                    'mosque_km',
                    'museum_km',
                    'catering_km',
                    'prom_part_1000',
                    'office_sqm_1000',
                    'trc_sqm_1000',
                    'cafe_sum_1000_min_price_avg',
                    'cafe_avg_price_1000',
                    'cafe_count_1000_price_500',
                    'cafe_count_1000_price_1500',
                    'cafe_count_1000_price_4000',
                    'big_church_count_1000',
                    'mosque_count_1000',
                    'sport_count_1000',
                    'prom_part_5000',
                    'office_sqm_5000',
                    'trc_sqm_5000',
                    'cafe_sum_5000_min_price_avg',
                    'cafe_avg_price_5000',
                    'cafe_count_5000_price_500',
                    'cafe_count_5000_price_1500',
                    'cafe_count_5000_price_4000',
                    'big_church_count_5000',
                    'mosque_count_5000',
                    'sport_count_5000')
macro_features <- c('oil_urals',
                    'gdp_quart_growth',
                    'ppi',
                    'usdrub',
                    'deposits_rate',
                    'mortgage_growth',
                    'overdue_wages_per_cap',
                    'marriages_per_1000_cap',
                    'pop_natural_increase',
                    'mortality',
                    'lodging_sqm_per_cap',
                    'baths_share',
                    'gas_share',
                    'electric_stove_share',
                    'old_house_share',
                    'infant_mortarity_per_1000_cap',
                    'rent_price_3room_bus',
                    'rent_price_1room_bus',
                    'rent_price_2room_eco',
                    'apartment_fund_sqm',
                    'cpi',
                    'gdp_deflator',
                    'balance_trade_growth',
                    'eurrub',
                    'gdp_annual_growth',
                    'micex_cbi_tr',
                    'deposits_growth',
                    'mortgage_rate',
                    'grp_growth',
                    'real_dispos_income_per_cap_growth',
                    'salary_growth',
                    'retail_trade_turnover_growth',
                    'unemployment',
                    'divorce_rate',
                    'invest_fixed_assets_phys',
                    'childbirth',
                    'housing_fund_sqm',
                    'water_pipes_share',
                    'sewerage_share',
                    'hot_water_share',
                    'heating_share',
                    'average_life_exp',
                    'perinatal_mort_per_1000_cap',
                    'rent_price_4.room_bus',
                    'rent_price_2room_bus',
                    'rent_price_3room_eco',
                    'rent_price_1room_eco',
                    'apartment_build')

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

dtrain <- xgb.DMatrix(data = train_sparse,label=train_outcomes)
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
imp_matrix_all <- imp_matrix_all %>% mutate(group = ifelse(Feature %in% names(train2),"new","old"))
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
          "C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/submission4.csv",
          row.names = FALSE)



##########
#### TODO
##########


# Add more factors from the training data
# Add Macro factors
# Cross validate to rune parameters. See:
# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
