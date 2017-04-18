###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting work directory
work.folder <- "/Users/Kozodoi/Documents/Competitions/DSG_2017"
#work.folder <- "C:/Users/kozodoin3.hub/Desktop/DSG_2017"
setwd(work.folder)

# setting inner folders
code.folder <- "codes"
data.folder <- "data"
func.folder <- "functions"
subm.folder <- "submissions"

# loading libraries
library(caret)
library(xgboost)
library(randomForest)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))


###################################
#                                 #
#         SIMPLE MODELING         #
#                                 #
###################################

##### 1. DATA PREPARATIONS

# loading original data
load(file.path(data.folder, "data_train.Rda"))
load(file.path(data.folder, "data_test.Rda"))

# loading data samples
load(file.path(data.folder, "sample_train.Rda"))
load(file.path(data.folder, "sample_valid.Rda"))

# computing features [takes 10-15 minutes]
sample.valid <- compute_features(sample.train, sample.valid)
data.test <- compute_features(data.train, data.test)



##### 2. MODEL ESTIMATIONS

# model equation
equation_num <- as.formula(is_listened ~ ratio_per_user +
                              user_song_plays  + user_album_plays  + user_artist_plays  + user_genre_plays  + 
                              user_song_skips  + user_album_skips  + user_artist_skips  + user_genre_skips  + 
                              total_song_plays + total_album_plays + total_artist_plays + total_genre_plays + 
                              total_song_skips + total_album_skips + total_artist_skips + total_genre_skips)

# training RF models
rf.model <- randomForest(equation_num, data = sample.valid, ntree = 100, importance = T)
rf.model

# displaying variable importance
varImpPlot(rf.model, type = 1)

# training XGB models with CV-based tuning 
control  <- trainControl(method = "cv", number = 4)
xg.model <- train(equation, data = sample.valid, method = "xgbLinear", trControl = control)


##### 3. PRODUCING PREDICTIONS

# predicting unknown data
rf.pred <- predict(rf.model, newdata = data.test, type = "prob")[, "1"]
xg.pred <- predict(xg.model, newdata = data.test, type = "prob")[, "1"]

# creating submission
submit(rf.pred, data = data.test, folder = subm.folder, file = "rf_basic.csv")
submit(xg.pred, data = data.test, folder = subm.folder, file = "xgb_basic.csv")