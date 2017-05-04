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
p_load(data.table, AUC, anytime, beepr, compiler)

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
temp <- c("genre_id", "media_id", "album_id", "user_id", "artist_id", "user_gender", "context_type", 
          "platform_name", "platform_family", "listen_type", "is_listened")
data.full[, (temp) := lapply(.SD, factor), .SDcols = temp]

# converting timestamps
#data.full[, release_date := as.Date(as.character(data.full$release_date), "%Y%m%d")]
#data.full[, ts_listen := anytime(data.full$ts_listen, asUTC = T)]


########## 3. CREATING FEATURES

### Add naive skip ratios as features
#source(file.path(code.folder, "code_2_features_naive_ratios.R"))

### Add total plays and skips as features
#source(file.path(code.folder, "code_2_features_total_plays.R"))

### Add time-related variables
#source(file.path(code.folder, "code_2_features_time_related.R"))


########## 4. DATA PARTITIONING

# converting and partitioning
data.full <- as.data.frame(data.full)
data.train <- data.full[data.full$dataset == "train", ]
data.test  <- data.full[data.full$dataset == "test",  ]
rm(list = c("data.full", "data.train"))

# sorting the testing data
data.test <- data.test[order(data.test$user_id, data.test$media_id), ]



###################################
#                                 #
#        LOADING PREDICTIONS      #
#                                 #
###################################

# getting the list of files
file.list <- list.files("pred_valid")
preds <- list()

# loading all predictions
for (i in 1:length(file.list)) {
  preds[[i]] <- read.csv2(file.path("pred_valid", file.list[i]), sep = ",", dec = ".", header = T)
  print(file.path("Loading ", file.list[i]))
}

# loading ts index
ts.index <- read.csv2(file.path(data.folder, "ts_index.csv"), sep = ",", dec = ".", header = T)
ts.index <- ts.index[order(ts.index$user_id, ts.index$media_id), ]

# creating preddiction matrix
pred.matrix <- data.frame(user_id = data.test$user_id, media_id = data.test$media_id, row_names = ts.index$row_names)
pred.matrix <- pred.matrix[order(pred.matrix$row_names), ]

# aligning predictions with row index
for (i in 1:nrow(summary(preds))) {
  
  # extracting prediction
  data <- preds[[i]]
  
  # correcting observation
  if (!("row_names" %in% colnames(data))) {
    data <- data[order(data$user_id, data$media_id), ]
    data$row_names <- ts.index$row_names
    data <- data[order(data$row_names), ]
  }else{
    data <- data[data$row_names %in% ts.index$row_names, ]
    data <- data[order(data$row_names), ]
  }
  
  # merging into one data frame
  pred.matrix <- cbind(pred.matrix, data$is_listened)
}

# assigning colnames
pred.matrix <- pred.matrix[order(pred.matrix$user_id, pred.matrix$media_id), ]
pred.matrix <- pred.matrix[, 4:ncol(pred.matrix)]
colnames(pred.matrix) <- c(file.list)



###################################
#                                 #
#            ENSEMBLING           #
#                                 #
###################################

# extracting real values
real <- data.test$is_listened

# drop weak classifiers
aucs <- apply(pred.matrix, 2, function(x) auc(roc(x, real)))
good <- names(aucs)[aucs > 0.8]
pred.matrix <- pred.matrix[, colnames(pred.matrix) %in% good]

# extracting number of models
k <- ncol(pred.matrix)

# mean and median predictions
pred.matrix$mean   <- apply(pred.matrix[,1:k], 1, mean)
pred.matrix$median <- apply(pred.matrix[,1:k], 1, median)

# ensemble selection
es.weights  <- ES(X = pred.matrix[,1:k],  Y = real, iter = 100)
bes.weights <- BES(X = pred.matrix[,1:k], Y = real, iter = 100, bags = 10, p = 0.5)
pred.matrix$es     <- apply(pred.matrix[,1:k], 1, function(x) sum(x*es.weights))
pred.matrix$bag_es <- apply(pred.matrix[,1:k], 1, function(x) sum(x*bes.weights))

# computing AUC
apply(pred.matrix, 2, function(x) auc(roc(x, real)))

# displaying ES weights
names(es.weights) <- colnames(pred.matrix)[1:length(es.weights)]
es.weights



###################################
#                                 #
#            SUBMITTING           #
#                                 #
###################################

# getting the list of files
file.list <- list.files("pred_unknown")
preds <- list()

# loading user_ratio
ratio <- fread(file.path(subm.folder, "naive_ratio_user_flow.csv"), sep = ",", dec = ".", header = T)$is_listened

# loading all predictions
for (i in 1:length(file.list)) {
  preds[[i]] <- read.csv2(file.path("pred_unknown", file.list[i]), sep = ",", dec = ".", header = T)
  print(file.path("Loading ", file.list[i]))
}

# creating preddiction matrix
pred.matrix <- data.frame(user_ratio = ratio)

# aligning predictions with row index
for (i in 1:nrow(summary(preds))) {
  
  # extracting prediction
  data <- preds[[i]]
  
  # merging into one data frame
  pred.matrix <- cbind(pred.matrix, data$is_listened)
}

# extracting number of models
colnames(pred.matrix) <- c("user_ratio", file.list)
k <- ncol(pred.matrix)

# mean and median predictions
pred.matrix$mean   <- apply(pred.matrix[,1:k], 1, mean)
pred.matrix$median <- apply(pred.matrix[,1:k], 1, median)

# ensemble selection
pred.matrix$es     <- apply(pred.matrix[,1:k], 1, function(x) sum(x*es.weights))
pred.matrix$bag_es <- apply(pred.matrix[,1:k], 1, function(x) sum(x*bes.weights))

# loading unknown data
data.unknown  <- read.csv2(file.path(data.folder, "test.csv"), sep = ",", dec = ".", header = T)
data.unknown$sample_id <- as.factor(data.unknown$sample_id)
data.unknown$is_listened <- NA

# submitting the best method (ES)
submit(pred.matrix$es, data = data.unknown, folder = subm.folder, file = "keras_es_21.csv")