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
p_load(data.table, anytime, randomForest, AUC, caret, beepr, xgboost, caret)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))



###################################
#                                 #
#         DATA PREPARATION        #
#                                 #
###################################

# loading data
data.full <- as.data.frame(fread(file.path(data.folder, "data_flow.csv"), sep = ",", dec = ".", header = T))

# converting factors
data.full$is_listened <- as.factor(data.full$is_listened)
data.full$context_type <- as.factor(data.full$context_type)
data.full$release_year <- as.factor(data.full$release_year)
data.full$user_gender <- as.factor(data.full$user_gender)
data.full$favorite_album <- as.factor(data.full$favorite_album)
data.full$favorite_artist <- as.factor(data.full$favorite_artist)
data.full$platform_name <- as.factor(data.full$platform_name)
data.full$platform_family <- as.factor(data.full$platform_family)
data.full$song_lyrics_explicit <- as.factor(data.full$song_lyrics_explicit)
data.full$hour_of_day <- as.factor(data.full$hour_of_day)
data.full$weekday <- as.factor(data.full$weekday)
data.full$is_listened_lag1 <- as.factor(data.full$is_listened_lag1)
data.full$is_listened_lag2 <- as.factor(data.full$is_listened_lag2)
data.full$context_type_same_as_lag <- as.factor(data.full$context_type_same_as_lag)
data.full$genre_equal_last_song <- as.factor(data.full$genre_equal_last_song)
data.full$album_equal_last_song <- as.factor(data.full$album_equal_last_song)
data.full$is_listened_lag2 <- as.factor(data.full$is_listened_lag2)

# partitioning
data.train   <- data.full[data.full$dataset == "train", ]
data.test    <- data.full[data.full$dataset == "test",  ]
data.unknown <- data.full[data.full$dataset == "unknown",  ]
data.known   <- data.full[data.full$dataset != "unknown",  ]
rm(list = "data.full")

# sorting the testing data
#data.test$row_index <- as.numeric(as.character(data.test$row_index))
#data.test <- data.test[order(data.test$row_index), ]

# sorting unknown data
data.unknown$sample_id <- as.numeric(as.character(data.unknown$sample_id))
data.unknown <- data.unknown[order(data.unknown$sample_id), ]



###################################
#                                 #
#      MODELING FOR VALIDATION    #
#                                 #
###################################

# model equation
equation <- as.formula(is_listened ~ user_ratio_flow + user_ratio_full + user_age + user_gender + 
                         favorite_album + favorite_artist + context_type + platform_name +
                         song_plays + artist_plays + album_plays + song_plays +
                         song_skips + artist_skips + album_skips + song_skips +
                         song_lyrics_explicit + media_duration + weekday + time_diff_release_listen)

# training XGB model
xg.grid  <- expand.grid(nrounds = 300, lambda = 1, alpha = 1, eta = 0.3)
tr.grid  <- trainControl(method = "none")
xg.model <- train(equation, data = data.train, method = "xgbLinear", trControl = tr.grid, tuneGrid = xg.grid)
xg.model

# training RF model
#rf.trees <- 500
#rf.model <- randomForest(equation, data = data.train, ntree = rf.trees, importance = T)
#rf.model

# training LR model
lr.model <- glm(equation, data = data.train, family = "binomial")
lr.model

# variable importance
#varImpPlot(rf.model, type = 1)

# predicting
xg.pred.valid <- data.frame(row_index = data.test$row_index, is_listened = predict(xg.model, newdata = data.test, type = "prob")[, "1"])
#rf.pred.valid <- data.frame(row_index = data.test$row_index, is_listened = predict(rf.model, newdata = data.test, type = "prob")[, "1"])
lr.pred.valid <- data.frame(row_index = data.test$row_index, is_listened = predict(lr.model, newdata = data.test, type = "response"))

# computing AUC
real <- data.test$is_listened
auc(roc(xg.pred.valid$is_listened, real))
#auc(roc(rf.pred.valid$is_listened, real))
auc(roc(lr.pred.valid$is_listened, real))

# saving predictions
write.table(xg.pred.valid, file = file.path("pred_valid", "xg_flow_0527.csv"), quote = F, sep = ",", dec = ".")
#write.table(rf.pred.valid, file = file.path("pred_valid", "rf_flow_0527.csv"), quote = F, sep = ",", dec = ".")
write.table(lr.pred.valid, file = file.path("pred_valid", "lr_flow_0527.csv"), quote = F, sep = ",", dec = ".")



###################################
#                                 #
#     MODELING FOR SUBMISSION     #
#                                 #
###################################

# training XGB model
xg.model <- train(equation, data = data.known, method = "xgbLinear", trControl = tr.grid, tuneGrid = xg.grid)
xg.model

# training RF model
#rf.model <- randomForest(equation, data = data.known, ntree = rf.trees, importance = T)
#rf.model

# training LR model
lr.model <- glm(equation, data = data.known, family = "binomial")
lr.model

# predicting
xg.pred.unknown <- predict(xg.model, newdata = data.unknown, type = "prob")[, "1"]
#rf.pred.unknown <- predict(rf.model, newdata = data.unknown, type = "prob")[, "1"]
lr.pred.unknown <- predict(lr.model, newdata = data.unknown, type = "response")

# creating submission
submit(xg.pred.unknown, data = data.unknown, folder = "pred_unknown", file = "xg_flow_0527.csv")
#submit(rf.pred.unknown, data = data.unknown, folder = "pred_unknown", file = "rf_flow_0527.csv")
submit(lr.pred.unknown, data = data.unknown, folder = "pred_unknown", file = "lr_flow_0527.csv")