###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting work directory
work.folder <- "/Users/Kozodoi/Documents/Competitions/DSG_2017"
setwd(work.folder)

# setting inner folders
code.folder <- "codes"
data.folder <- "data"
func.folder <- "functions"
subm.folder <- "submissions"

# loading libraries
if(require(pacman)==FALSE) install.packages("pacman")
library(pacman)
p_load(data.table, anytime, randomForest, AUC, caret, beepr, xgboost)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))



###################################
#                                 #
#         DATA PREPARATION        #
#                                 #
###################################

########## 1. LOADING THE DATA

# loading training data
data.train <- fread(file.path(data.folder, "tr.csv"), sep = ",", dec = ".", header = T)
data.test  <- fread(file.path(data.folder, "ts.csv"), sep = ",", dec = ".", header = T)

# merging data sets
data.test$dataset  <- "test"
data.train$dataset <- "train"
data.full <- rbind(data.train, data.test)
setkey(data.full, user_id, media_id)


########## 2. CONVERTING VARIABLES

# converting factors
temp <- c("genre_id", "media_id", "album_id", "user_id", "artist_id", "user_gender", "context_type", "platform_name",
          "platform_family", "listen_type", "is_listened")
data.full[, (temp) := lapply(.SD, factor), .SDcols = temp]

# converting timestamps
data.full[, release_date := as.Date(as.character(data.full$release_date), "%Y%m%d")]
data.full[, ts_listen := anytime(data.full$ts_listen, asUTC = T)]


########## 3. CREATING FEATURES

### Add time-related variables
source(file.path(code.folder, "code_2_features_time_related.R"))

### Add total plays and skips as features
source(file.path(code.folder, "code_2_features_total_plays.R"))

### Add naive skip ratios as features
source(file.path(code.folder, "code_2_features_naive_ratios.R"))


########## 4. DATA PARTITIONING

# converting and partitioning
data.full <- as.data.frame(data.full)
data.train <- data.full[data.full$dataset == "train", ]
data.test  <- data.full[data.full$dataset == "test",  ]
rm(list = "data.full")



###################################
#                                 #
#             MODELING            #
#                                 #
###################################

# model equation
equation <- as.formula(is_listened ~ hours + time_diff + platform_name + user_age + user_gender + media_duration +
                             song_plays + artist_plays + album_plays + genre_plays + 
                             song_skips + artist_skips + album_skips + genre_skips)

# training XGB model
xg.grid  <- expand.grid(nrounds = 500, lambda = 1, alpha = 1, eta = 0.3)
tr.grid  <- trainControl(method = "none")
xg.model <- train(equation, data = data.train, method = "xgbLinear", trControl = tr.grid, tuneGrid = xg.grid)
xg.model

# training RF model
rf.trees <- 500
rf.model <- randomForest(equation, data = data.train, ntree = rf.trees, importance = T)
rf.model

# variable importance
varImpPlot(rf.model, type = 1)

# predicting
xg.pred <- predict(xg.model, newdata = data.test, type = "prob")[, "1"]
rf.pred <- predict(rf.model, newdata = data.test, type = "prob")[, "1"]
real <- data.test$is_listened
  
# computing AUC
auc(roc(xg.pred, real))
auc(roc(rf.pred, real))

# saving predictions
rf <- data.frame(user_id = data.test$user_id, media_id = data.test$media_id, is_listened = rf.pred)
xg <- data.frame(user_id = data.test$user_id, media_id = data.test$media_id, is_listened = xg.pred)
write.table(rf, file = file.path("predictions_valid", "rf_basic.csv"), quote = F, sep = ",", dec = ".")
write.table(xg, file = file.path("predictions_valid", "xg_basic.csv"), quote = F, sep = ",", dec = ".")



###################################
#                                 #
#      MODELING FOR SUBMISSION    #
#                                 #
###################################

########## 1. LOADING THE DATA

# loading training data
data.train <- fread(file.path(data.folder, "train.csv"), sep = ",", dec = ".", header = T)
data.test  <- fread(file.path(data.folder, "test.csv"),  sep = ",", dec = ".", header = T)

# merging data sets
data.test$is_listened <- NA
data.train$sample_id  <- NA
data.test$dataset  <- "test"
data.train$dataset <- "train"
data.full <- rbind(data.train, data.test)
setkey(data.full, user_id, media_id)


########## 2. CONVERTING VARIABLES

# converting factors
temp <- c("sample_id", "genre_id", "media_id", "album_id", "user_id", "artist_id", "user_gender", "context_type", "platform_name",
          "platform_family", "listen_type", "is_listened")
data.full[, (temp) := lapply(.SD, factor), .SDcols = temp]

# converting timestamps
data.full[, release_date := as.Date(as.character(data.full$release_date), "%Y%m%d")]
data.full[, ts_listen := anytime(data.full$ts_listen, asUTC = T)]


########## 3. CREATING FEATURES

### Add time-related variables
source(file.path(code.folder, "code_2_features_time_related.R"))

### Add total plays and skips as features
source(file.path(code.folder, "code_2_features_total_plays.R"))

### Add naive skip ratios as features
source(file.path(code.folder, "code_2_features_naive_ratios.R"))


########## 4. DATA PARTITIONING

# converting and partitioning
data.full    <- as.data.frame(data.full)
data.known   <- data.full[data.full$dataset == "train", ]
data.unknown <- data.full[data.full$dataset == "test",  ]
rm(list = "data.full")


########## 5. MODELING

# training XGB model
xg.model <- train(equation, data = data.known, method = "xgbLinear", trControl = tr.grid, tuneGrid = xg.grid)
xg.model

# training RF model
rf.model <- randomForest(equation, data = data.known, ntree = rf.trees, importance = T)
rf.model


######### 6. FORECASTING

# predicting
rf.pred <- predict(rf.model, newdata = data.unknown, type = "prob")[, "1"]
xg.pred <- predict(xg.model, newdata = data.unknown, type = "prob")[, "1"]

# creating submission
submit(rf.pred, data = data.unknown, folder = "predictions_unknown", file = "rf_basic.csv")
submit(xg.pred, data = data.unknown, folder = "predictions_unknown", file = "xg_basic.csv")