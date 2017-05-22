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
#        1. DATA PREPARATION      #
#                                 #
###################################

# loading data
data.full <- fread(file.path(data.folder, "data_flow.csv"), sep = ",", dec = ".", header = T)

# converting and partitioning
data.test <- data.full[data.full$dataset == "test",  ]
data.unknown <- data.full[data.full$dataset == "unknown",  ]
rm(list = c("data.full"))

# sorting unknown data
data.unknown$sample_id <- as.numeric(as.character(data.unknown$sample_id))
data.unknown <- data.unknown[order(data.unknown$sample_id), ]



###################################
#                                 #
#    2. ENSEMBLING: VALIDATION    #
#                                 #
###################################

###################################
#                                 #
#     2.1. LOADING PREDICTIONS    #
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
#     2.2. BUILDING ENSEMBLES     #
#                                 #
###################################

# extracting real values
real <- as.factor(data.test$is_listened)

# droping weak classifiers
aucs <- apply(pred.matrix, 2, function(x) auc(roc(x, real)))
good <- names(aucs)[aucs > 0.7]
pred.matrix.good <- pred.matrix[, colnames(pred.matrix) %in% good]

# extracting number of models
k <- ncol(pred.matrix)
k.good <- ncol(pred.matrix.good)
  
# mean and median predictions
pred.matrix$mean   <- apply(pred.matrix[,1:k], 1, mean)
pred.matrix$median <- apply(pred.matrix[,1:k], 1, median)

# TOP-N mean ensembles
top3 <- names(aucs)[order(aucs, decreasing = T)[1:3]]
top5 <- names(aucs)[order(aucs, decreasing = T)[1:5]]
top7 <- names(aucs)[order(aucs, decreasing = T)[1:7]]
pred.matrix$top3 <- apply(pred.matrix[,top3], 1, mean)
pred.matrix$top5 <- apply(pred.matrix[,top5], 1, mean)
pred.matrix$top7 <- apply(pred.matrix[,top7], 1, mean)

# ensemble selection: all methods
es_all.weights <- ES(X = pred.matrix[,1:k], Y = real, iter = 100)
pred.matrix$es_all <- apply(pred.matrix[,1:k], 1, function(x) sum(x*es_all.weights))

# ensemble selection: best methods
es_trim.weights <- ES(X = pred.matrix.good[,1:k.good], Y = real, iter = 100)
pred.matrix$es_trim <- apply(pred.matrix.good[,1:k.good], 1, function(x) sum(x*es_trim.weights))

# bagged ensemble selection
#bes.weights <- BES(X = pred.matrix[,1:k], Y = real, iter = 50, bags = 10, p = 0.5)
#pred.matrix$bag_es <- apply(pred.matrix[,1:k], 1, function(x) sum(x*bes.weights))

# computing AUC
apply(pred.matrix, 2, function(x) auc(roc(x, real)))

# saving ES weights
names(es_all.weights)  <- colnames(pred.matrix)[1:length(es_all.weights)]
names(es_trim.weights) <- colnames(pred.matrix)[1:length(es_trim.weights)]
best.weights <- es_all.weights[es_all.weights > 0]



###################################
#                                 #
#      3. ENSEMBLING: UNKNOWN     #
#                                 #
###################################

###################################
#                                 #
#     3.1. LOADING PREDICTIONS    #
#                                 #
###################################

# loading all predictions
for (i in 1:length(best.weights)) {
  print(file.path("Loading ", names(best.weights)[i]))
  preds[[i]] <- read.csv2(file.path("pred_unknown", names(best.weights)[i]), sep = ",", dec = ".", header = T)
  preds[[i]]$sample_id <- as.numeric(as.character(preds[[i]]$sample_id))
  preds[[i]] <- preds[[i]][order(preds[[i]]$sample_id), ]
}

# creating prediction matrix
pred.matrix <- data.frame(sample_id = data.unknown$sample_id)

# merging all predictions
for (i in 1:length(best.weights)) {
  pred.matrix <- cbind(pred.matrix, preds[[i]]$is_listened)
}

# assigning colnames
pred.matrix <- pred.matrix[, 2:ncol(pred.matrix)]
colnames(pred.matrix) <- names(best.weights)


###################################
#                                 #
#         3.2. ENSEMBLING         #
#                                 #
###################################

# extracting number of models
k <- ncol(pred.matrix)

# ensemble selection
pred.matrix$es <- apply(pred.matrix[,1:k], 1, function(x) sum(x*best.weights))

# submitting the best method (ES)
submit(pred.matrix$es, data = data.unknown, folder = subm.folder, file = "es_30keras_12factor_1matrix.csv")