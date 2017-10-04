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
p_load(data.table, AUC, beepr, caret, e1071)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))



###################################
#                                 #
#      1. LOADING PREDICTIONS     #
#                                 #
###################################

# loading data
data.full <- fread(file.path(data.folder, "data_flow.csv"), sep = ",", dec = ".", header = T)

# converting and partitioning
data.test <- data.full[data.full$dataset == "test",  ]
rm(list = c("data.full"))

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
aucs <- apply(full, 2, function(x) auc(roc(x, as.factor(full$real))))

# selecting correlated models with lower AUC
bad <- list()
for (t in 1:length(m1)) {
  au <- c(aucs[m1[[t]]], aucs[m2[[t]]])
  bad[[t]] <- names(which.min(au))
}

# removing correlated models with lower AUC
full <- full[, !(colnames(full) %in% unique(bad))]

# converting to factor
full$real = as.factor(full$real)



###################################
#                                 #
#       3. STACKING: STAGE 1      #
#                                 #
###################################

# preparations
iter <- 100
perf <- matrix(NA, nrow = 101, ncol = iter)
weig <- seq(0, 1, by = 0.01)
rownames(perf) <- weig

# validation loop
for (t in 1:iter) {
  
  # print iteration number
  print(paste0("Iteration ", t, "/", iter))
  
  # data partitioning
  part <- createDataPartition(full$real, p = 0.7, list = F)
  full.train <- full[ part, ]
  full.valid <- full[-part, ]
  
  # training models
  glm_fit <- glm(real ~ ., full.train, family = "binomial")
  svm_fit <- svm(real ~ ., full.train, probability = T, kernel = "radial")
  
  # predicting
  glm_pred <- predict(glm_fit, newdata = full.valid, type = "response")
  svm_pred <- attr(predict(svm_fit, newdata = full.valid, probability = T), "probabilities")[,"1"]
  
  # saving AUCs
  for (i in 1:101) {
    ensemble <- weig[i]*glm_pred + (1 - weig[i])*svm_pred
    perf[i, t] <- auc(roc(ensemble, full.valid$real))
  }
}

# finding the optial weight
perf <- apply(perf, 1, mean)
weight <- as.numeric(names(which.max(perf)))
weight



###################################
#                                 #
#      4. STACKING: STAGE 2       #
#                                 #
###################################

# training GLM
glm_fit = glm(real ~ ., full, family = "binomial")

# training SVM
svm_fit <- svm(real ~ ., data = full, probability = T, kernel = "radial")

# predicting validation
glm_pred_valid  = predict(glm_fit, newdata = full, type = "response")
svm_pred_valid  = attr(predict(svm_fit, newdata = full, probability = T), "probabilities")[,"1"]
ens_pred_valid  = weight*glm_pred_valid + (1-weight)*svm_pred_valid

# predicting unknown
glm_pred_unknown  = predict(glm_fit, newdata = unknown, type = "response")
svm_pred_unknown  = attr(predict(svm_fit, newdata = unknown, probability = T), "probabilities")[,"1"]
ens_pred_unknown  = weight*glm_pred_unknown + (1-weight)*svm_pred_unknown

# displaying AUCs
#print(paste0("GLM: AUC on Validation = ",      round(auc(roc(glm_pred_valid, full$real)), digits = 6)))
#print(paste0("SVM: AUC on Validation = ",      round(auc(roc(svm_pred_valid, full$real)), digits = 6)))
#print(paste0("ENSEMBLE: AUC on Validation = ", round(auc(roc(ens_pred_valid, full$real)), digits = 6)))

# displaying AUCs
print(perf)

# correlation with the best submission
best.sub <- read.csv(paste0("./submissions/stacking_mean2_2factors_1sim25_allothers_drop093.csv"))$is_listened
print(paste0("Correlation with the best submission = ", round(cor(ens_pred_unknown, best.sub), digits = 6)))
print(paste0("Mean out-of-sample AUC = ", round(max(perf), digits = 6)))

# saving submission
unknown$is_listened <- ens_pred_unknown
write.csv(unknown[,c("sample_id", "is_listened")], "./submissions/stacking_ens_2factors_1sim25_allothers_drop093.csv", row.names = F)