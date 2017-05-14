###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting inner folders
code.folder <- "codes"
data.folder <- "data"
func.folder <- "functions"
subm.folder <- "submissions"

# loading libraries
if(require(pacman)==FALSE) install.packages("pacman")
library(pacman)
p_load(data.table, anytime, randomForest, AUC, caret, beepr, xgboost, caret)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))

source(file.path(code.folder, "createModelLibrary_caret.R"))

##### Setting up parallelization
cl <- makeCluster(min(detectCores()-1,coreNumber))
#, outfile="") # This redirects the output to the R master console but not in RStudio
#on.exit(stopCluster(cl))
registerDoParallel(cl)
cat(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))


# ###################################
# #                                 #
# #         DATA PREPARATION        #
# #                                 #
# ###################################
# 
# ########## 1. LOADING THE DATA
# 
# # loading training data
# data.train <- fread(file.path(data.folder, "tr.csv"), sep = ",", dec = ".", header = T)
# data.test  <- fread(file.path(data.folder, "ts.csv"), sep = ",", dec = ".", header = T)
# 
# # merging data sets
# data.test$dataset  <- "test"
# data.train$dataset <- "train"
# data.full <- rbind(data.train, data.test)
# setkey(data.full, user_id, media_id)
# 
# 
# ########## 2. CONVERTING VARIABLES
# 
# # converting factors
# temp <- c("genre_id", "media_id", "album_id", "user_id", "artist_id", "user_gender", "context_type", "platform_name",
#           "platform_family", "listen_type", "is_listened")
# data.full[, (temp) := lapply(.SD, factor), .SDcols = temp]
# 
# # converting timestamps
# data.full[, release_date := as.Date(as.character(data.full$release_date), "%Y%m%d")]
# data.full[, ts_listen := anytime(data.full$ts_listen, asUTC = T)]
# 
# 
# ########## 3. CREATING FEATURES
# 
# ### Add naive skip ratios as features
# source(file.path(code.folder, "code_2_features_naive_ratios.R"))
# 
# ### Add total plays and skips as features
# source(file.path(code.folder, "code_2_features_total_plays.R"))
# 
# ### Add time-related variables
# source(file.path(code.folder, "code_2_features_time_related.R"))
# 
# 
# ########## 4. DATA PARTITIONING
# 
# # converting and partitioning
# data.full <- as.data.frame(data.full)
# data.train <- data.full[data.full$dataset == "train", ]
# data.test  <- data.full[data.full$dataset == "test",  ]
# rm(list = "data.full")

data.full <- fread("data/data_full.csv")
data.full[, is_listened := factor(is_listened, labels = c("no", "yes"))]
data.tr <- data.full[dataset == "train"]
data.tr[, c("sample_id", "user_id", "media_id", "artist_id", "genre_id", "album_id", "session_id", "dataset", "context_type") := NULL]
data.test <- data.full[dataset == "test"]

###################################
#                                 #
#      MODELING FOR VALIDATION    #
#                                 #
###################################

#modelLib <- createModelLibrary(equation, data = data.tr, preProcess = c("center", "scale"))

# training XGB model
xg.grid  <- expand.grid(nrounds = 100, lambda = 0.01, alpha = 0.01, eta = 0.3)
tr.grid  <- trainControl(method = "none", allowParallel = TRUE, returnData = FALSE)
xg.model <- train(is_listened~., data = data.tr, method = "xgbLinear", trControl = tr.grid, tuneGrid = xg.grid, preProcess = c("center", "scale"), subsample = 0.1, colsample_bytree = 0.2)
xg.model

# training RF model
rf.trees <- 500
rf.model <- randomForest(is_listened~., data = data.tr, ntree = rf.trees, importance = T)
rf.model

# variable importance
varImpPlot(rf.model, type = 1)

# predicting
xg.pred <- predict(xg.model, newdata = data.test, type = "prob")[, 2]
rf.pred <- predict(rf.model, newdata = data.test, type = "prob")[, 2]
  
# computing AUC
real <- data.test$is_listened
ModelMetrics::auc(predicted = xg.pred, real)
ModelMetrics::auc(predicted = rf.pred, real)

# saving predictions
xg <- data.frame(row_index = data.test$row_index, is_listened = xg.pred)
rf <- data.frame(row_index = data.test$row_index, is_listened = rf.pred)
write.table(xg, file = file.path("pred_valid", "xg_full_features_0510.csv"), quote = F, sep = ",", dec = ".")
write.table(rf, file = file.path("pred_valid", "rf_ratio_lag_context.csv"), quote = F, sep = ",", dec = ".")



###################################
#                                 #
#     MODELING FOR SUBMISSION     #
#                                 #
###################################

########## 1. LOADING THE DATA

# loading training data
data.train <- fread(file.path(data.folder, "train.csv"), sep = ",", dec = ".", header = T)
data.test  <- fread(file.path(data.folder, "test.csv"),  sep = ",", dec = ".", header = T)

# keeping only Flow songs
data.train <- data.train[data.train$listen_type == "1", ]

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

### Add naive skip ratios as features
source(file.path(code.folder, "code_2_features_naive_ratios.R"))

### Add total plays and skips as features
#source(file.path(code.folder, "code_2_features_total_plays.R"))

### Add time-related variables
source(file.path(code.folder, "code_2_features_time_related.R"))
data.full$song_first <- as.factor(data.full$song_session_position == 1)


########## 4. DATA PARTITIONING

# converting and partitioning
data.full    <- as.data.frame(data.full)
data.known   <- data.full[data.full$dataset == "train", ]
data.unknown <- data.full[data.full$dataset == "test",  ]
rm(list = "data.full", "data.train", "data.test", "temp")

# sorting the unknown data
data.unknown <- data.unknown[order(data.unknown$sample_id), ]


########## 5. MODELING

# training XGB model
xg.model <- train(equation, data = data.known, method = "xgbLinear", trControl = tr.grid, tuneGrid = xg.grid)
xg.model

# training RF model
rf.model <- randomForest(equation, data = data.known, ntree = rf.trees, importance = T)
rf.model


######### 6. FORECASTING

# predicting
xg.pred <- predict(xg.model, newdata = data.unknown, type = "prob")[, "1"]
rf.pred <- predict(rf.model, newdata = data.unknown, type = "prob")[, "1"]

# creating submission
submit(xg.pred, data = data.unknown, folder = "pred_unknown", file = "xg_ratio_lag_context.csv")
submit(rf.pred, data = data.unknown, folder = "pred_unknown", file = "rf_ratio_lag_context.csv")
