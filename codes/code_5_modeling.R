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

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))


###################################
#                                 #
#         SIMPLE MODELING         #
#                                 #
###################################

# loading original data
load(file.path(data.folder, "data_train.Rda"))
load(file.path(data.folder, "data_test.Rda"))

# loading data samples
load(file.path(data.folder, "sample_train.Rda"))
load(file.path(data.folder, "sample_valid.Rda"))

# computing features [takes 5-10 minutes]
sample.valid <- compute_features(sample.train, sample.valid)
data.test <- compute_features(data.train, data.test)

# model equation
equation <- as.formula(is_listened ~ play_ratio + new_song + new_album + new_artist + new_genre + top_song + top_album + top_artist + top_genre)

# training XGB model [TAKES MUCH TIME]
control <- trainControl(method = "cv", number = 5)
models  <- train(equation, data = sample.valid, method = "xgbLinear", trControl = control)

# predicting unknown data
pred <- predict(models, newdata = data.test, type = "prob")[, "1"]

# creating submission
submit(pred, data = data.test, folder = subm.folder, file = "xgboost_basic.csv")