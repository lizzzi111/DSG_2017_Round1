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
data.full <- read.csv2(file.path(data.folder, "data_flow.csv"), sep = ",", dec = ".", header = T)
data.full$is_listened <- as.factor(data.full$is_listened)

# partitioning
data.train   <- data.full[data.full$dataset == "train", ]
data.test    <- data.full[data.full$dataset == "test",  ]
data.unknown <- data.full[data.full$dataset == "unknown",  ]
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
                         context_type + hours + time_diff + first_flow + 
                         song_session_position + platform_name1 + platform_name2 + 
                         song_plays + artist_plays + album_plays)


# training XGB model
xg.grid  <- expand.grid(nrounds = 100, lambda = 1, alpha = 1, eta = 0.3)
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
  
# computing AUC
real <- data.test$is_listened
auc(roc(xg.pred, real))
auc(roc(rf.pred, real))

# saving predictions
write.table(xg, file = file.path("pred_valid", "xg_basic.csv"), quote = F, sep = ",", dec = ".")
write.table(rf, file = file.path("pred_valid", "rf_basic.csv"), quote = F, sep = ",", dec = ".")



###################################
#                                 #
#     MODELING FOR SUBMISSION     #
#                                 #
###################################

# training XGB model
xg.model <- train(equation, data = data.known, method = "xgbLinear", trControl = tr.grid, tuneGrid = xg.grid)
xg.model

# training RF model
rf.model <- randomForest(equation, data = data.known, ntree = rf.trees, importance = T)
rf.model

# predicting
xg.pred <- predict(xg.model, newdata = data.unknown, type = "prob")[, "1"]
rf.pred <- predict(rf.model, newdata = data.unknown, type = "prob")[, "1"]

# creating submission
submit(xg.pred, data = data.unknown, folder = "pred_unknown", file = "xg_basic.csv")
submit(rf.pred, data = data.unknown, folder = "pred_unknown", file = "rf_basic.csv")