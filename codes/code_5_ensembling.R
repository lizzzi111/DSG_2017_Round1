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
file.list <- readRDS("./data/best_stacking_model_set.rds")
preds <- list()

# loading all predictions
for (i in 1:length(file.list)) {
  print(file.path("Loading ", file.list[i]))
  preds[[i]] <- read.csv2(file.path("pred_valid", file.list[i]), sep = ",", dec = ".", header = T)
  #preds[[i]]$row_index <- as.numeric(as.character(preds[[i]]$row_index))
  #preds[[i]] <- preds[[i]][order(preds[[i]]$row_index), ]
}

# sorting the new predictions
#preds[[63]] <- merge(preds[[61]], preds[[63]], by = "row_index", sort = F)
#preds[[63]] <- preds[[63]][, c("row_index", "is_listened.y")]
#colnames(preds[[63]]) <- c("row_index", "is_listened")
#write.csv(preds[[63]], "./pred_valid/xg_full_features_eta03_0524.csv", row.names = F)

# creating preddiction matrix
pred.matrix <- data.frame(dataset = data.test$dataset)

# merging all predicctions
for (i in 1:length(file.list)) {
  pred.matrix <- cbind(pred.matrix, preds[[i]]$is_listened)
}

# assigning colnames
pred.matrix <- pred.matrix[, 2:ncol(pred.matrix)]
colnames(pred.matrix) <- file.list

# extracting real values
real <- as.factor(data.test$is_listened)


#######################################
#                                     #
#   2.2. REMOVING CORRELATED MODELS   #
#                                     #
#######################################

# computing correlations
cors <- cor(pred.matrix)

# setting matrix to triangle form
for (i in 1:nrow(cors)) {
  for (j in 1:nrow(cors)) {
    if (i >= j) {cors[i,j] <- 0}
  }
}

# creating objects
t <- 1
m1 <- list()
m2 <- list()

# finding corelations > threshold
threshold <- 0.93
for (i in 1:nrow(cors)) {
  for (j in 1:nrow(cors)) {
    if (cors[i,j] > threshold) {
      m1[[t]] <- rownames(cors)[i]
      m2[[t]] <- colnames(cors)[j]
      t <- t + 1
    }
  }
}

# computing AUC on validation
aucs <- apply(pred.matrix, 2, function(x) auc(roc(x, real)))

# selecting correlated models with lower AUC
bad <- list()
for (t in 1:length(m1)) {
  au <- c(aucs[m1[[t]]], aucs[m2[[t]]])
  bad[[t]] <- names(which.min(au))
}

# removing correlated models with lower AUC
pred.matrix <- pred.matrix[, !(colnames(pred.matrix) %in% unique(bad))]

# saving the list of models
good.models <- colnames(pred.matrix)


###################################
#                                 #
#     2.3. BUILDING ENSEMBLES     #
#                                 #
###################################

# droping weak classifiers
#aucs <- apply(pred.matrix, 2, function(x) auc(roc(x, real)))
#good <- names(aucs)[aucs > 0.7]
#pred.matrix <- pred.matrix[, colnames(pred.matrix) %in% good]

# extracting number of models
k <- ncol(pred.matrix)

# mean and median predictions
pred.matrix$mean   <- apply(pred.matrix[,1:k], 1, mean)
pred.matrix$median <- apply(pred.matrix[,1:k], 1, median)

# TOP-N mean ensembles
aucs <- apply(pred.matrix, 2, function(x) auc(roc(x, real)))
top3 <- names(aucs)[order(aucs, decreasing = T)[1:3]]
top5 <- names(aucs)[order(aucs, decreasing = T)[1:5]]
top7 <- names(aucs)[order(aucs, decreasing = T)[1:7]]
pred.matrix$top3 <- apply(pred.matrix[,top3], 1, mean)
pred.matrix$top5 <- apply(pred.matrix[,top5], 1, mean)
pred.matrix$top7 <- apply(pred.matrix[,top7], 1, mean)

# ensemble selection
es.weights <- ES(X = pred.matrix[,1:k], Y = real, iter = 1000)
names(es.weights) <- colnames(pred.matrix)[1:length(es.weights)]
pred.matrix$es <- apply(pred.matrix[,1:k], 1, function(x) sum(x*es.weights))

# bagged ensemble selection
#bes.weights <- BES(X = pred.matrix[,1:k], Y = real, iter = 50, bags = 10, p = 0.5)
#pred.matrix$bag_es <- apply(pred.matrix[,1:k], 1, function(x) sum(x*bes.weights))

# computing AUC
aucs <- apply(pred.matrix, 2, function(x) auc(roc(x, real)))
aucs



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
for (i in 1:length(good.models)) {
  print(file.path("Loading ", good.models[i]))
  preds[[i]] <- read.csv2(file.path("pred_unknown", good.models[i]), sep = ",", dec = ".", header = T)
  preds[[i]]$sample_id <- as.numeric(as.character(preds[[i]]$sample_id))
  preds[[i]] <- preds[[i]][order(preds[[i]]$sample_id), ]
}

# creating prediction matrix
pred.matrix <- data.frame(sample_id = data.unknown$sample_id)

# merging all predictions
for (i in 1:length(good.models)) {
  pred.matrix <- cbind(pred.matrix, preds[[i]]$is_listened)
}

# assigning colnames
pred.matrix <- pred.matrix[, 2:ncol(pred.matrix)]
colnames(pred.matrix) <- names(es.weights)


###################################
#                                 #
#         3.2. ENSEMBLING         #
#                                 #
###################################

# extracting number of models
k <- ncol(pred.matrix)

# ensemble selection
pred.matrix$es <- apply(pred.matrix[,1:k], 1, function(x) sum(x*es.weights))

# computing correlation with the best submission
best.sub <- read.csv(paste0("./submissions/stacking_glm_2factors_1sim25_allothers_drop093.csv"))$is_listened
aucs
cor(pred.matrix$es, best.sub)

# exporting submissions
submit(pred.matrix$es,   data = data.unknown, folder = subm.folder, file = "es_1000i_2factors_1sim25_allothers_drop093.csv")