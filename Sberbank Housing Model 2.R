
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


