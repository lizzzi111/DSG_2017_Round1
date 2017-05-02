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
p_load(data.table, AUC, beepr, compiler)

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
#source(file.path(code.folder, "code_2_features_time_related.R"))

### Add total plays and skips as features
#source(file.path(code.folder, "code_2_features_total_plays.R"))

### Add naive skip ratios as features
source(file.path(code.folder, "code_2_features_naive_ratios.R"))


########## 4. DATA PARTITIONING

# converting and partitioning
data.full <- as.data.frame(data.full)
data.train <- data.full[data.full$dataset == "train", ]
data.test  <- data.full[data.full$dataset == "test",  ]
rm(list = c("data.full", "data.train", "artist.ratio", "song.ratio", "genre.ratio", "user.ratio.flow", "user.ratio.full"))


########## 5. LOADING KERAS PREDICTION

# loading data
keras1 <- fread(file.path(data.folder, "keras_ts_1.csv"), sep = ",", dec = ".", header = T)
keras2 <- fread(file.path(data.folder, "keras_ts_2.csv"), sep = ",", dec = ".", header = T)
keras3 <- fread(file.path(data.folder, "keras_ts_3.csv"), sep = ",", dec = ".", header = T)
keras4 <- fread(file.path(data.folder, "keras_ts_4.csv"), sep = ",", dec = ".", header = T)
keras5 <- fread(file.path(data.folder, "keras_ts_5.csv"), sep = ",", dec = ".", header = T)

# converting IDs to factors
temp <- c("media_id", "user_id")
keras1[, (temp) := lapply(.SD, factor), .SDcols = temp]
keras2[, (temp) := lapply(.SD, factor), .SDcols = temp]
keras3[, (temp) := lapply(.SD, factor), .SDcols = temp]
keras4[, (temp) := lapply(.SD, factor), .SDcols = temp]
keras5[, (temp) := lapply(.SD, factor), .SDcols = temp]

# sorting all data sets similarly
keras1 <- keras1[order(keras1$user_id, keras1$media_id), ]
keras2 <- keras2[order(keras2$user_id, keras2$media_id), ]
keras3 <- keras3[order(keras3$user_id, keras3$media_id), ]
keras4 <- keras4[order(keras4$user_id, keras4$media_id), ]
keras5 <- keras5[order(keras5$user_id, keras5$media_id), ]
data.test <- data.test[order(data.test$user_id, data.test$media_id), ]



###################################
#                                 #
#            ENSEMBLING           #
#                                 #
###################################

# extracting real values
real <- data.test$is_listened

# matrix with predictions
pred.matrix <- data.frame(user_ratio = data.test$user_ratio_flow)
pred.matrix$keras1 <- keras1$is_listened
pred.matrix$keras2 <- keras2$is_listened
pred.matrix$keras3 <- keras3$is_listened
pred.matrix$keras4 <- keras4$is_listened
pred.matrix$keras5 <- keras5$is_listened

# mean and median predictions
pred.matrix$mean   <- apply(pred.matrix[,1:6], 1, mean)
pred.matrix$median <- apply(pred.matrix[,1:6], 1, median)

# ensemble selection
es.weights  <- ES(X = pred.matrix[,1:6],  Y = real, iter = 100)
bes.weights <- BES(X = pred.matrix[,1:6], Y = real, iter = 100, bags = 5, p = 0.5)
pred.matrix$es     <- apply(pred.matrix[,1:6], 1, function(x) sum(x*es.weights))
pred.matrix$bag_es <- apply(pred.matrix[,1:6], 1, function(x) sum(x*bes.weights))

# computing AUC
apply(pred.matrix, 2, function(x) auc(roc(x, real)))



###################################
#                                 #
#            SUBMITTING           #
#                                 #
###################################

# loading keras predictions
keras1 <- fread(file.path(subm.folder, "deep_keras_1.csv"), sep = ",", dec = ".", header = T)$is_listened
keras2 <- fread(file.path(subm.folder, "deep_keras_2.csv"), sep = ",", dec = ".", header = T)$is_listened
keras3 <- fread(file.path(subm.folder, "deep_keras_3.csv"), sep = ",", dec = ".", header = T)$is_listened
keras4 <- fread(file.path(subm.folder, "deep_keras_4.csv"), sep = ",", dec = ".", header = T)$is_listened
keras5 <- fread(file.path(subm.folder, "deep_keras_5.csv"), sep = ",", dec = ".", header = T)$is_listened

# loading user_ratio
ratio <- fread(file.path(subm.folder, "naive_ratio_user.csv"), sep = ",", dec = ".", header = T)$is_listened

# matrix with predictions
pred.matrix <- data.frame(user_ratio = ratio)
pred.matrix$keras1 <- keras1
pred.matrix$keras2 <- keras2
pred.matrix$keras3 <- keras3
pred.matrix$keras4 <- keras4
pred.matrix$keras5 <- keras5

# mean and median predictions
pred.matrix$mean   <- apply(pred.matrix[,1:6], 1, mean)
pred.matrix$median <- apply(pred.matrix[,1:6], 1, median)

# ensemble selection
pred.matrix$es     <- apply(pred.matrix[,1:6], 1, function(x) sum(x*es.weights))
pred.matrix$bag_es <- apply(pred.matrix[,1:6], 1, function(x) sum(x*bes.weights))

# loading unknown data
unknown  <- read.csv2(file.path(data.folder, "test.csv"), sep = ",", dec = ".", header = T)
unknown$sample_id <- as.factor(unknown$sample_id)
unknown$is_listened <- NA

# submitting the best method (ES)
submit(pred.matrix$es, data = unknown, folder = subm.folder, file = "keras_es.csv")