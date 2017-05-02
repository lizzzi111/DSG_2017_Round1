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
data.test  <- fread(file.path(data.folder, "ts_100.csv"), sep = ",", dec = ".", header = T)

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


########## 5. LOADING KERAS PREDICTION
deep.pred <- fread(file.path(data.folder, "deep_128_64_flow_cont.csv"), sep = ",", dec = ".", header = T)
temp <- c("media_id", "user_id")
deep.pred[, (temp) := lapply(.SD, factor), .SDcols = temp]
deep.pred <- deep.pred[order(deep.pred$user_id, deep.pred$media_id), ]
data.test <- data.test[order(data.test$user_id, data.test$media_id), ]
data.test$deep_pred <- deep.pred$is_listened



###################################
#                                 #
#             MODELING            #
#                                 #
###################################

# model equation
equation <- as.formula(is_listened ~ user_ratio_flow + hours + time_diff +
                             song_plays + artist_plays + album_plays + 
                             song_skips + artist_skips + album_skips)

# parameter grid: XGB
xg.nrounds <- 300
xg.lambda  <- 1
xg.alpha   <- 1
xg.eta     <- 0.3
xg.grid <- expand.grid(nrounds = xg.nrounds, lambda = xg.lambda, alpha = xg.alpha, eta = xg.eta)
tr.grid <- trainControl(method = "none")

# training XGB model
xg.model <- train(equation, data = data.train, method = "xgbLinear", trControl = tr.grid, tuneGrid = xg.grid, verbose = 1)
xg.model

# training RF model
rf.model <- randomForest(equation, data = data.train, ntree = 300, importance = T)
rf.model

# displaying var. importance
varImpPlot(rf.model, type = 1)

# predicting
xg.pred <- predict(xg.model, newdata = data.test, type = "prob")[, "1"]
rf.pred <- predict(rf.model, newdata = data.test, type = "prob")[, "1"]
ur.pred <- data.test$user_ratio_flow
nn.pred <- data.test$deep_pred
mn.pred <- (xg.pred + rf.pred + ur.pred + nn.pred)/4
real <- data.test$is_listened
  
# computing AUC
auc(roc(xg.pred, real))
auc(roc(rf.pred, real))
auc(roc(ur.pred, real))
auc(roc(nn.pred, real))
auc(roc(mn.pred, real))

#### ENSEMBLING TWO BEST PREDICTIONS
seq <- seq(0, 1, 0.01)
auc <- rep(NA, length(seq))
for (s in 1:length(seq)) {
  print(s)
  pred <- nn.pred*seq[s] + ur.pred*(1-seq[s])
  auc[s] <- auc(roc(pred, real))
}
plot(auc, type = "l")
max(auc)
seq[which.max(auc)]



###################################
#                                 #
#      MODELING FOR SUBMISSION    #
#                                 #
###################################

########## 1. LOADING THE DATA

# loading training data
data.train <- fread(file.path(data.folder, "train.csv"), sep = ",", dec = ".", header = T)
data.test  <- fread(file.path(data.folder, "test.csv"), sep = ",", dec = ".", header = T)

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
data.full <- as.data.frame(data.full)
data.train <- data.full[data.full$dataset == "train", ]
data.test  <- data.full[data.full$dataset == "test",  ]


########## 5. LOADING DEEP PREDICTION
deep.pred <- fread(file.path(subm.folder, "deep_128_64_flow_cont.csv"), sep = ",", dec = ".", header = T)
temp <- c("sample_id")
deep.pred[, (temp) := lapply(.SD, factor), .SDcols = temp]
deep.pred <- deep.pred[order(deep.pred$sample_id), ]
data.test <- data.test[order(data.test$sample_id), ]
data.test$deep_pred <- deep.pred$is_listened


########## 6. MODELING

# training XGB model
xg.model <- train(equation, data = data.known, method = "xgbLinear", trControl = tr.grid, tuneGrid = xg.grid, verbose = 1)
xg.model

# training RF model
rf.model <- randomForest(equation, data = data.known, ntree = 100, importance = T)
rf.model


######### 7. FORECASTING

# predicting
xg.pred <- predict(xg.model, newdata = data.test, type = "prob")[, "1"]
rf.pred <- predict(rf.model, newdata = data.test, type = "prob")[, "1"]
ur.pred <- data.test$user_ratio_flow
nn.pred <- data.test$deep_pred
mn.pred <- (xg.pred + rf.pred + ur.pred + nn.pred)/4
av.pred <- 0.5*ur.pred + 0.5*nn.pred

# creating submission
#submit(av.pred, data = data.test, folder = subm.folder, file = "av_pred.csv")