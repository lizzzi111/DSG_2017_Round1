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
library(randomForest)
library(AUC)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))


###################################
#                                 #
#         SIMPLE MODELING         #
#                                 #
###################################

##### 1. DATA PREPARATIONS

# loading data samples
load(file.path(data.folder, "sample_train.Rda"))
load(file.path(data.folder, "sample_valid.Rda"))

# keeping only flow data in validation
sample.valid <- sample.valid[sample.valid$listen_type == 1, ]

# computing features [takes 15 minutes]
sample.valid <- compute_features(sample.train, sample.valid)

# adding hours variable
sample.train$hours <- as.factor(format(as.POSIXct(sample.train$ts_listen, format = "%H:%M:%S"),"%H"))
sample.valid$hours <- as.factor(format(as.POSIXct(sample.valid$ts_listen, format = "%H:%M:%S"),"%H"))

# partitioning validation data
valid.part <- createDataPartition(sample.valid$is_listened, p = 0.5, list = F)
sample.valid.1 <- sample.valid[ valid.part, ]
sample.valid.2 <- sample.valid[-valid.part, ]


##### 2. MODEL ESTIMATIONS

# model equation
equation_num <- as.formula(is_listened ~ ratio_per_user +
                             total_song_plays + total_album_plays + 
                             user_top_artist  + user_top_genre +
                             user_new_song + user_new_artist)

# training RF model
rf.model <- randomForest(equation_num, data = sample.valid.1, ntree = 300, importance = T)
rf.model

# displaying var. importance
varImpPlot(rf.model, type = 1)

# predicting
rf.pred <- predict(rf.model, newdata = sample.valid.2, type = "prob")[, "1"]
rf.real <- sample.valid.2$is_listened
  
# computing AUC
auc(roc(rf.pred, rf.real))


##### 3. PRODUCING PREDICTIONS

# loading original data
load(file.path(data.folder, "data_train.Rda"))
load(file.path(data.folder, "data_test.Rda"))

# computing features [takes 15 minutes]
data.test <- compute_features(data.train, data.test)

# adding hours variable
data.train$hours <- as.factor(format(as.POSIXct(data.train$ts_listen, format = "%H:%M:%S"),"%H"))
data.test$hours  <- as.factor(format(as.POSIXct(data.test$ts_listen,  format = "%H:%M:%S"),"%H"))

# predicting unknown data
rf.model <- randomForest(equation_num, data = sample.valid, ntree = 300, importance = T)
rf.pred  <- predict(rf.model, newdata = data.test, type = "prob")[, "1"]

# creating submission
submit(rf.pred, data = data.test, folder = subm.folder, file = "rf_basic.csv")