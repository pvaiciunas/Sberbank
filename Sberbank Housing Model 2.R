
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


train <- read.csv("C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/train.csv/train.csv")
test <- read.csv("C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/test.csv/test.csv")
macro <- read.csv("C:/Users/pvaiciunas/Google Drive/Programming/R/Kaggle/Sberbank Russian Housing/macro.csv/macro.csv")