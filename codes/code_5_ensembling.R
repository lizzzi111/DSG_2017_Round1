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
#       1. VALIDATION DATA        #
#                                 #
###################################

###################################
#                                 #
#       1.1. DATA PREPARATION     #
#                                 #
###################################

# loading data
data.full <- read.csv2(file.path(data.folder, "data_flow.csv"), sep = ",", dec = ".", header = T)

# converting and partitioning
data.test  <- data.full[data.full$dataset == "test",  ]
data.unknown  <- data.full[data.full$dataset == "unknown",  ]
rm(list = c("data.full", "data.train"))

# sorting the testing data
data.test$row_index <- as.numeric(as.character(data.test$row_index))
data.test <- data.test[order(data.test$row_index), ]


###################################
#                                 #
#     1.2. LOADING PREDICTIONS    #
#                                 #
###################################

# getting the list of files
file.list <- list.files("pred_valid")
preds <- list()

# loading all predictions
for (i in 1:length(file.list)) {
  print(file.path("Loading ", file.list[i]))
  preds[[i]] <- read.csv2(file.path("pred_valid", file.list[i]), sep = ",", dec = ".", header = T)
  #preds[[i]]$row_index <- as.numeric(as.character(preds[[i]]$row_index))
  #preds[[i]] <- preds[[i]][order(preds[[i]]$row_index), ]
}

# creating preddiction matrix
pred.matrix <- data.frame(dataset = data.test$dataset)

# merging all predicctions
for (i in 1:length(file.list)) {
  pred.matrix <- cbind(pred.matrix, preds[[i]]$is_listened)
}

# assigning colnames
pred.matrix <- pred.matrix[, 2:ncol(pred.matrix)]
colnames(pred.matrix) <- file.list


###################################
#                                 #
#         1.3. ENSEMBLING         #
#                                 #
###################################

# extracting real values
real <- as.factor(data.test$is_listened)

# droping weak classifiers
#aucs <- apply(pred.matrix, 2, function(x) auc(roc(x, real)))
#good <- names(aucs)[aucs >= 0.8]
#pred.matrix <- pred.matrix[, colnames(pred.matrix) %in% good]

# extracting number of models
k <- ncol(pred.matrix)

# mean and median predictions
pred.matrix$mean   <- apply(pred.matrix[,1:k], 1, mean)
pred.matrix$median <- apply(pred.matrix[,1:k], 1, median)

# ensemble selection
es.weights <- ES(X = pred.matrix[,1:k],  Y = real, iter = 100)
pred.matrix$es <- apply(pred.matrix[,1:k], 1, function(x) sum(x*es.weights))

# bagged ensemble selection
#bes.weights <- BES(X = pred.matrix[,1:k], Y = real, iter = 100, bags = 10, p = 0.5)
#pred.matrix$bag_es <- apply(pred.matrix[,1:k], 1, function(x) sum(x*bes.weights))

# computing AUC
apply(pred.matrix, 2, function(x) auc(roc(x, real)))

# displaying ES weights
names(es.weights) <- colnames(pred.matrix)[1:length(es.weights)]
es.weights[es.weights > 0]



###################################
#                                 #
#         2. UNKNOWN DATA         #
#                                 #
###################################

###################################
#                                 #
#       1.1. DATA PREPARATION     #
#                                 #
###################################

# loading unknown data
data.unknown$is_listened <- NA

# sorting the data
data.unknown$sample_id <- as.numeric(as.character(data.unknown$sample_id))
data.unknown <- data.unknown[order(data.unknown$sample_id), ]

# getting the list of files
file.list <- list.files("pred_unknown")
preds <- list()


###################################
#                                 #
#     2.2. LOADING PREDICTIONS    #
#                                 #
###################################

# loading all predictions
for (i in 1:length(file.list)) {
  print(file.path("Loading ", file.list[i]))
  preds[[i]] <- read.csv2(file.path("pred_unknown", file.list[i]), sep = ",", dec = ".", header = T)
  preds[[i]]$sample_id <- as.numeric(as.character(preds[[i]]$sample_id))
  preds[[i]] <- preds[[i]][order(preds[[i]]$sample_id), ]
}

# creating prediction matrix
pred.matrix <- data.frame(sample_id = data.unknown$sample_id)

# merging all predictions
for (i in 1:nrow(summary(preds))) {
  pred.matrix <- cbind(pred.matrix, preds[[i]]$is_listened)
}

# assigning colnames
pred.matrix <- pred.matrix[, 2:ncol(pred.matrix)]
colnames(pred.matrix) <- file.list


###################################
#                                 #
#         2.3. ENSEMBLING         #
#                                 #
###################################

# drop weak classifiers
#pred.matrix <- pred.matrix[, colnames(pred.matrix) %in% good]

# extracting number of models
k <- ncol(pred.matrix)

# mean and median predictions
pred.matrix$mean   <- apply(pred.matrix[,1:k], 1, mean)
pred.matrix$median <- apply(pred.matrix[,1:k], 1, median)

# ensemble selection
pred.matrix$es <- apply(pred.matrix[,1:k], 1, function(x) sum(x*es.weights))

# bagged ensemble selection
#pred.matrix$bag_es <- apply(pred.matrix[,1:k], 1, function(x) sum(x*bes.weights))

# submitting the best method (ES)
submit(pred.matrix$es, data = data.unknown, folder = subm.folder, file = "es_9_keras_newdata.csv")