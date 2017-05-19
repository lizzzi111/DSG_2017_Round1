#### STACKING

#setwd("/Users/lizzzi111/Desktop/DSG_2017/data/")
setwd("N:/DSG_2017/data")

# LOAD PACKAGES
library(caret)
library(caretEnsemble)
library(randomForest)
library(pROC)
# LOAD DATA
#tr = read.csv("./tr_stacking_flow.csv") 
#ts = read.csv("./ts_stacking_flow.csv")
#tr = read.csv("./tr_stacking_all.csv") 
#ts = read.csv("./ts_stacking_all.csv")

tr = read.csv("./tr_stacking_with_factorization.csv") 
ts = read.csv("./ts_stacking_with_factorization.csv")

# DATA PREPARATION
tr$real = factor(tr$real)
ts$real = factor(ts$real)

names(tr) = make.names(names(tr))
names(ts) = make.names(names(ts))

#levels(tr$real) <- make.names(levels(factor(tr$real)))
#levels(ts$real) <- make.names(levels(factor(ts$real)))

#prop.table(table(tr$real))

# FEATURE SELECTION RF
# Default Parameters
fit = randomForest(real~., tr, importance =T)
prognose = predict(fit, ts, type = "prob")
auc(ts$real, prognose[,2] )

# check the importance
imp = importance(fit)
important_features = rownames(fit$importance[imp[,3]>22,])

write.csv(important_features, "./important_names.csv", row.names = F)
# rF default parameter (set.seed(1))
# rF all 0.73
# rF imporance decrease > 25, auc 0.7299
# rF imporance decrease > 30, auc 0.7266
# rF imporance decrease > 28, auc 0.7295
# rF imporance decrease > 24, auc 0.7293
# rF imporance decrease > 23, auc 0.7304
# rF imporance decrease > 22, auc 0.7295


# rF 0.7306, mtry = 3, ntrees = 1000

# just mean 0.7299
# mean of > 25 0.7297

fit_imp = randomForest(real~., tr[,c("real",important_features)], importance =T)
prognose_imp = predict(fit_imp, ts[,c(important_features)], type = "prob")

auc(ts$real, prognose_imp[,2] )
auc(ts$real, as.numeric(rowMeans(ts[,-57])) )
auc(ts$real, as.numeric(rowMeans(ts[,important_features])) )

### TUNING
tunegrid <- expand.grid(mtry=c(3:6))
seed = 1
set.seed(seed)
metric = "ROC"


model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 10, # number of folds in cross validation
  #repeats = 3, # number for repeated cross validation
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = FALSE # The training data will not be included in the ouput training object
)


# applying
custom <- train(real~., 
                data=t, 
                method="rf", 
                ntree = 1000,
                metric=metric, 
                tuneGrid=tunegrid, 
                trControl=model.control)
summary(custom)
plot(custom)
#mtry 3, haven't tried to vary ntrees
pred <- predict(custom, newdata=ts, type = "prob")
auc(ts$real, pred[,2] )

rf_params_feat <- train(real~., 
                data=tr[,c("real",important_features)], 
                method="rf", 
                ntree = 1000,
                metric=metric, 
                trControl=model.control)
prog = predict(rf_params_feat, ts[,important_features], type="prob")
auc(ts$real, prog[,2] )
# Area under the curve: 0.7323 
# it is useless to use RF, sice the simplest logistic regression gives 0.7374

# Tuning logistic Regression
# only on the important features

important_features = c(read.csv("./important_names.csv"))
important_features = levels(important_features$x)
glm_fit = glm(real~., tr[, c("real",important_features)], family = "binomial")
glm_fit_imp = predict(glm_fit, newdata = ts[,important_features], type = "response" )
auc(ts$real, glm_fit_imp )


# with only important feautures = 0.7375
# all features (not only flow, but also user ratios, and fulls) 0.7489
# all features with factorization 0.7608
# Regularization?
fullmod = glm(real~., tr[, c("real", important_features)], family = "binomial")
backwards = step(fullmod) 

best_formula = backwards$formula
glm_bf = glm(best_formula, tr, family = "binomial")
glm_bf_fit = predict(fullmod, newdata = ts, type = "response" )
auc(ts$real, glm_bf_fit )

saveRDS(best_formula, "./best_form_factorization.rds")


### with regularization
library(glmnet)


# Fitting the model (Ridge: Alpha = 0)
set.seed(1)
cv.ridge <- cv.glmnet(as.matrix(tr[,-57]), as.matrix(tr[,57]), family='binomial', alpha=0, parallel=TRUE, standardize=TRUE, type.measure='auc')

# Results
plot(cv.ridge)
cv.ridge$lambda.min
cv.ridge$lambda.1se
coef(cv.ridge, s=cv.ridge$lambda.min)

ridge_fit = predict(cv.ridge, newx = as.matrix(ts[,-57]), type = "response" )
pROC::auc(ts$real, as.numeric(ridge_fit) )
# ridge: 0.7564

cv.lasso <- cv.glmnet(as.matrix(tr[,-57]), as.matrix(tr[,57]), family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')
lasso_fit = predict(cv.lasso, newx = as.matrix(ts[,-57]), type = "response" )
pROC::auc(ts$real, as.numeric(lasso_fit) )
# lasso: 0.7581
plot(cv.lasso)
