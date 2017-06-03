
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


train <- read.csv("C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/train.csv/train.csv")
test <- read.csv("C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/test.csv/test.csv")
macro <- read.csv("C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/macro.csv/macro.csv")

################################################################
# Making use of piping formulas instead of regular dplyr approach
################################################################

############################
# Clean up the data a little
############################
train <- train %>%
  mutate(max_floor = as.numeric(max_floor),
         kitch_sq = as.numeric(kitch_sq),
         num_room = as.numeric(num_room),
         build_year = as.numeric(build_year),
         sub_area = as.factor(sub_area),
         num_room = as.numeric(num_room),
         product_type = factor(product_type),
         sub_area = factor(sub_area))

train <- train %>%
  filter(build_year < 2020 | is.na(build_year))

train <- train %>%
  mutate(strange_full_sq = ifelse(full_sq <= 1, full_sq + 1, 0),
         full_sq = ifelse(full_sq > 800 | full_sq <= 1, NA, full_sq))

train <- train %>% 
  mutate(strange_life_sq = ifelse(life_sq <= 1, life_sq+1,0), 
         strange_life_sq = ifelse(is.na(strange_life_sq),0,strange_life_sq), 
         life_sq = ifelse(life_sq > 400 | life_sq <= 1, NA, life_sq))

train <- train %>% 
  mutate(kitch_sq = as.numeric(kitch_sq),
         strange_kitch_sq = ifelse(kitch_sq <= 1, kitch_sq+1,0),
         kitch_sq = ifelse(kitch_sq > 200 | kitch_sq <= 1, NA, kitch_sq))

train <- train %>% 
  mutate(build_year = as.numeric(build_year), 
         strange_build_year = ifelse(build_year <= 1, build_year+1,0), 
         build_year = ifelse(build_year > 2018 | build_year < 1860, NA, build_year))

train <- train %>% 
  mutate(floor = ifelse(floor > 45, NA, floor))

train <- train %>% 
  mutate(max_floor = as.numeric(max_floor), 
         strange_max_floor = ifelse(max_floor <= 1, max_floor+1,0), 
         max_floor = ifelse(max_floor > 60 | max_floor <=1, NA, max_floor))

train <- train %>% 
  mutate(state = as.numeric(state), 
         state = ifelse(state > 4, NA, state))

train <- train %>% 
  mutate(material = as.factor(material), 
         material = ifelse(material == 3, NA, material))



# more cleaning
train <- train %>% filter(kitch_sq < full_sq | is.na(kitch_sq))
train <- train %>% filter(kitch_sq < life_sq | is.na(kitch_sq))

train <- train %>% mutate(num_room = ifelse(num_room==0,NA,num_room))


#####################
# Timesteamp Features
#####################

train <- train %>%
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
train <- train %>% 
  mutate(floor_from_top = max_floor - floor)

# relative position of floor in house
train <- train %>% 
  mutate(floor_by_maxfloor = floor/max_floor)

# average room size
train <- train %>% 
  mutate(roomsize = (life_sq-kitch_sq)/num_room) 

# relative proportion of living area
train <- train %>% 
  mutate(life_proportion = life_sq/full_sq)

# relative proportion of kitchen area
train <- train %>% 
  mutate(kitchen_proportion = kitch_sq/full_sq)

# extra area
train <- train %>% 
  mutate(extra_area = full_sq - life_sq)

# age of house at time of sale
train <- train %>% 
  mutate(age_at_sale = interval(make_date(year=build_year),timestamp) / years(1))  


##############################
# Grouping Apartments together
##############################

# Assign a common name. A Combination of the sub-area and distance to metro
train <- train %>%
  mutate(apartment_name = factor(str_c(sub_area, format(metro_km_avto, digits = 3))))

# Get the number of apartments in the group
train <- train %>%
  group_by(apartment_name) %>%
  tally() %>%
  right_join(train, by = "apartment_name")

######################
# Sale Characteristics
######################

train <- train %>% 
  group_by(year_month) %>% 
  summarize(n_sales_permonth = n()) %>% 
  right_join(train,by="year_month")

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

train <- train %>%
  left_join(location_data, by = "sub_area") # all rows from train

# Calculate distance from Kremlin for each sub_area
kremlin = data.frame(longitude = 37.617664,latitude = 55.752121)
train <- train %>%
  group_by(sub_area) %>%
  top_n(n = 1, wt = id) %>%
  ungroup %>%
  mutate(distance_from_kremlin = distm(.[c("longitude", "latitude")], kremlin, fun = distHaversine)) %>%
  select(sub_area, distance_from_kremlin) %>%
  right_join(train, by = "sub_area") # all rows from 'train'
  

######################
# Pricing of sub areas
######################

# average price per raion
train <- train %>%
  group_by(sub_area) %>%
  summarize(mean_price_raion = mean(price_doc)) %>%
  right_join(train, by = "sub_area")

# average price per raion per year
train <- train %>%
  group_by(sub_area, year) %>%
  summarize(mean_price_raion_year = mean(price_doc)) %>%
  right_join(train, by = c("sub_area", "year"))

# average price per sqm per raion
train <- train %>%
  group_by(sub_area) %>%
  summarize(mean_price_persqm_raion = mean(price_doc / full_sq, na.rm = T)) %>%
  right_join(train, by = "sub_area")


############################
# Population characteristics
############################
  
# Population density per raion
train <- train %>% 
  mutate(pop_density_raion = raion_popul/area_m)

# Demographic structure of the raions
train <- train %>% 
  mutate(young_proportion = young_all/full_all) # proportion of people younger than working age
train <- train %>% 
  mutate(work_proportion = work_all/full_all) # proportion of people in working age
train <- train %>% 
  mutate(retire_proportion = ekder_all/full_all) # proportion of people older than working age

###########################
# Building Information
###########################

# average building height per raion
train <- train %>% 
  group_by(sub_area) %>% 
  summarize(mean_building_height = mean(max_floor,na.rm=T)) %>% 
  right_join(train,by="sub_area")

# Proportion of houses with certain build materials (e.g. 10% build_count_wood)
train<-train %>% 
  mutate_each(funs(pct = (.)/raion_build_count_with_builddate_info),matches('^build_count_[a-zA-Z]*$'))


#############################
# Educational Characteristics
#############################

# ratio of number of pupils and preschool seats
train <- train %>% 
  mutate(ratio_preschool = children_preschool / preschool_quota)

# ratio of number of pupils and school seats
train <- train %>% 
  mutate(ratio_school = children_school / school_quota)


#########################
# dataset related features
########################

# number of missing values per row (this is going to take a while)
train <- train %>% 
  mutate(count_na_perrow = apply(., 1, function(x) sum(is.na(x))))

###################
#Feature Importance
###################

outcomes <- train$price_doc

basic_features <- c("full_sq", "life_sq", "kitch_sq", "num_room", "floor", "max_floor", "material", "build_year", "state", "product_type")

new_features <- c("month_of_year","week_of_year", "day_of_month", "day_of_week", "floor_from_top", "floor_by_maxfloor", "roomsize", "life_proportion", "kitchen_proportion", "extra_area", "age_at_sale", "n_sales_permonth", "distance_from_kremlin", "young_proportion", "work_proportion", "retire_proportion", "mean_building_height", "ratio_preschool",
                  "ratio_school", "count_na_perrow")

