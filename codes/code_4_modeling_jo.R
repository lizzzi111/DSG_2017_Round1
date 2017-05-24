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
p_load(data.table, anytime, randomForest, AUC, caret, beepr, xgboost, caret, parallel)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))

source(file.path(code.folder, "createModelLibrary_caret.R"))

##### Setting up parallelization
cl <- makeCluster(min(detectCores()-1,coreNumber))
#, outfile="") # This redirects the output to the R master console but not in RStudio
#on.exit(stopCluster(cl))
registerDoParallel(cl)
cat(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))


data.full <- fread("data/data_full.csv")
#data.full[, is_listened := factor(is_listened, labels = c("no", "yes"))]
data.unknown <- data.full[dataset == "unknown",]
data.unknown[, dataset := NULL]
setorder(data.unknown, sample_id)

data.full[, c("sample_id", "user_id", "media_id", "artist_id", "genre_id", "album_id", "session_id", "context_type") := NULL]
data.tr <- data.full[dataset == "train"]
data.ts <- data.full[dataset == "test"]
data.tr[, dataset := NULL]
data.ts[, dataset := NULL]

###################################
#                                 #
#      MODELING FOR VALIDATION    #
#                                 #
###################################

#modelLib <- createModelLibrary(equation, data = data.tr, preProcess = c("center", "scale"))

# training XGB model
xg.tr <- xgb.DMatrix(data = as.matrix(data.tr[, !("is_listened")]), label = data.tr$is_listened)
xg.ts <- xgb.DMatrix(data = as.matrix(data.ts[, !("is_listened")]), label = data.ts$is_listened)
xg.unknown <- xgb.DMatrix(data = as.matrix(data.unknown[, !("is_listened")]))

# Train xgb model
xg.model <- xgb.train(data = xg.tr,
                      watchlist = list(trainset = xg.tr, testset = xg.ts),
                      nrounds = 500, 
                      params = list(booster = "gbtree", eta = 0.3, gamma = 0, max_depth = 6, min_child_weight = 1, subsample = 0.1, colsample_by_tree = 0.7, 
                                    num_parallel_tree = 3,  objective = 'binary:logistic', eval_metric = "auc"), 
                      verbose = 1, early_stopping_rounds = 4)

ggplot(xg.model$evaluation_log, aes(iter)) + geom_line(aes(y=trainset_auc)) + geom_line(aes(y= testset_auc))

xg.pred <- predict(xg.model, xg.ts)
# saving predictions
xg <- data.frame(row_index = data.ts$row_index, is_listened = xg.pred)
write.table(xg, file = file.path("pred_valid", "xg_full_features_eta015_0523.csv"), quote = F, sep = ",", dec = ".")


########## 5. MODELING



######### 6. FORECASTING

# predicting
xg.pred <- predict(xg.model, newdata = xg.unknown)
xg <-  data.frame(sample_id = data.unknown$sample_id, is_listened = xg.pred)

# creating submission
write.table(xg, file = file.path("pred_unknown", "xg_full_features_eta015_0523.csv"), quote = F, sep = ",", dec = ".", row.names = FALSE)
submit(xg.pred, data = data.unknown, folder = "pred_unknown", file = "xg_full_features_0523.csv")
