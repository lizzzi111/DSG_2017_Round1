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
p_load(data.table, AUC, anytime, beepr, caret, compiler, randomForest, nnet)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))


###################################
#                                 #
#      2. LOADING PREDICTIONS     #
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
temp <- list.files("./pred_unknown/")
temp <- readRDS("./data/best_stacking_model_set_0525.rds")
#temp[45] <- "xg_full_features_eta03_0524.csv"

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
#  3. REMOVING CORRELATED MODELS  #
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


###################################
#                                 #
#       4. STACKING: STAGE 1      #
#                                 #
###################################

# converting to factor
full$real = as.factor(full$rea)

# preparations
iter <- 10
a <- rep(NA, iter)

# validation loop
for (t in 1:iter) {

  # data partitioning
  part <- createDataPartition(full$real, p = 0.6, list = F)
  full.train <- full[ part, ]
  full.valid <- full[-part, ]
  
  # training GLM
  glm_fit = glm(real ~ ., full.train, family = "binomial")
  glm_pred = predict(glm_fit, newdata = full.valid, type = "response")
  
  # saving AUC
  a[t] <- auc(roc(glm_pred, full.valid$real))
}


###################################
#                                 #
#      5. STACKING: STAGE 2       #
#                                 #
###################################

# training GLM
glm_fit = glm(real ~ ., full, family = "binomial")

# predicting
glm_pred = predict(glm_fit, newdata = full, type = "response")
unknown$is_listened = predict(glm_fit, newdata = unknown, type = "response")

# displaying AUCs
print(paste0("AUC on Validation = ", round(auc(roc(glm_pred, full$real)), digits = 6)))
print(paste0("Out-of-sample AUC = ", round(mean(a), digits = 6)))

# correlation with the best submission
best.sub <- read.csv(paste0("./submissions/stacking_glm_2factors_1sim25_1xgbold_11ratios_drop093.csv"))$is_listened
print(paste0("Correlation with the best submission = ", round(cor(unknown$is_listened, best.sub), digits = 6)))

# saving submission
write.csv(unknown[,c("sample_id", "is_listened")], "./submissions/stacking_glm_2factors_1sim25_1xgbold_11ratios_drop093.csv", row.names = F)