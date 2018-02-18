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
p_load(data.table, beepr, caret, pROC, doParallel, e1071)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))
source(file.path(code.folder, "createModelLibrary_caret.R"))

##### Setting up parallelization
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
cat(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))



###################################
#                                 #
#      1. LOADING PREDICTIONS     #
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

# loading list of models to use
# there are two options for that:
# 1) All models which are listed in the folders
# 2) The model set which is currently the best
temp <- list.files("./pred_valid/")
temp <- readRDS("./data/best_stacking_model_set_0525.rds")

# loading predictions (unknown)
unknown = as.data.frame(sapply(temp, function(file) read.csv(paste0("./pred_unknown/",file))[2]))
unknown$sample_id = read.csv(paste0("./pred_unknown/", temp[1]))$sample_id

# loading predictions (validation)
full = as.data.frame(sapply(temp, function(file) read.csv(paste0("./pred_valid/",file))[2]))

# adding real values
full$real = data.test$is_listened

# renaming colomns
colnames(full)    <- gsub(".is_listened", "", colnames(full))
colnames(unknown) <- gsub(".is_listened", "", colnames(unknown))



###################################
#                                 #
#  2. REMOVING CORRELATED MODELS  #
#                                 #
###################################

# computing correlations
cors <- cor(full)
cors <- cors[, !(colnames(cors) %in% "real")]
cors <- cors[  !(rownames(cors) %in% "real"), ]

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
aucs <- apply(full, 2, function(x) auc(roc(predictor = x, response = as.factor(full$real))))

# selecting correlated models with lower AUC
bad <- list()
for (t in 1:length(m1)) {
  au <- c(aucs[m1[[t]]], aucs[m2[[t]]])
  bad[[t]] <- names(which.min(au))
}

# removing correlated models with lower AUC
full <- full[, !(colnames(full) %in% unique(bad))]



###################################
#                                 #
#     3. STACKING - MODELING      #
#                                 #
###################################

# converting to factor
full$real = as.factor(full$real)
levels(full$real) <- make.names(levels(factor(full$real)))

# converting to the data frame
full <- as.data.frame(full)
unknown <- as.data.frame(unknown)

# training the model library
modelLib <- createModelLibrary(real ~ ., data = full, mini = F, metric = "ROC", runParallel = TRUE,
                               modellist = c("lasso", "ridge", "elasticNet", "C5.0", "NaiveBayes"))

# creating AUC vector
aucs <- rep(NA, nrow(summary(modelLib)))

# merging all predictions
for (i in 1:nrow(summary(modelLib))) {
  aucs[i] <- max(modelLib[[i]]$results$ROC)
}

# assigning colnames
names(aucs) <- rownames(summary(modelLib))

# displaying AUCs
sort(aucs, decreasing = T)



###################################
#                                 #
#    4. STACKING - FORECASTING    #
#                                 #
###################################

# forecasting the unknown data
preds <- predict(modelLib, newdata = unknown, type = "prob")

# creating prediction matrix
pred.matrix <- data.frame(sample_id = unknown$sample_id)

# merging all predictions
for (i in 1:nrow(summary(preds))) {
  pred.matrix <- cbind(pred.matrix, preds[[i]][,"X1"])
}

# assigning colnames
pred.matrix <- pred.matrix[, 2:ncol(pred.matrix)]
colnames(pred.matrix) <- rownames(summary(preds))

# loading the best submission
best.sub <- read.csv(paste0("./submissions/stacking_glm_2factors_1sim25_allothers_drop093.csv"))$is_listened
pred.matrix <- cbind(pred.matrix, best.sub)
colnames(pred.matrix)[ncol(pred.matrix)] <- "glm"
best.sub <- read.csv(paste0("./submissions/stacking_mean2_2factors_1sim25_allothers_drop093.csv"))$is_listened

# extracting number of models
k <- ncol(pred.matrix)

# mean and median predictions
pred.matrix$mean   <- apply(pred.matrix[,1:k], 1, mean)
pred.matrix$median <- apply(pred.matrix[,1:k], 1, median)

# displaying predicitions
summary(pred.matrix)
cor(pred.matrix)

# exporting submissions
submit(pred.matrix$elasticNet, data = data.unknown, folder = subm.folder, file = "stacking_elasticNet_2factors_1sim25_allothers_drop093.csv")
submit(pred.matrix$lasso,      data = data.unknown, folder = subm.folder, file = "stacking_lasso_2factors_1sim25_allothers_drop093.csv")
submit(pred.matrix$ridge,      data = data.unknown, folder = subm.folder, file = "stacking_ridge_2factors_1sim25_allothers_drop093.csv")
submit(pred.matrix$mean,       data = data.unknown, folder = subm.folder, file = "stacking_mean6_2factors_1sim25_allothers_drop093.csv")
submit(g,       data = data.unknown, folder = subm.folder, file = "stacking_mean2_glm_svmrad_tuned_2factors_1sim25_allothers_drop093.csv")

# training SVM model
svm_model <- svm(real ~ ., data = full, probability = T, kernel = "radial")
svm_model
svm_pred <- predict(svm.model, unknown, type = "prob")
svm <- attr(svm_pred, "probabilities")[,"X1"]
cor(svm, best.sub)
submit(svm, data = data.unknown, folder = subm.folder, file = "stacking_svmsig_2factors_1sim25_allothers_drop093.csv")

svmrad <- read.csv(paste0("./submissions/stacking_svm_2factors_1sim25_allothers_drop093.csv"))$is_listened
svmlin <- read.csv(paste0("./submissions/stacking_svmlin_2factors_1sim25_allothers_drop093.csv"))$is_listened
svmsig <- read.csv(paste0("./submissions/stacking_svmsig_2factors_1sim25_allothers_drop093.csv"))$is_listened



# tuning settings
tune.ctrl <- trainControl(method = "cv", number = 5, summaryFunction = twoClassSummary, classProbs = T)
svm.cost  <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5)
svm.sigma <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5)
svm.grid  <- expand.grid(C = svm.cost, sigma = svm.sigma)
svm.model <- train(real ~., data = full, probability = T, method = "svmRadial", trControl = tune.ctrl, tuneGrid = svm.grid)
svm.model