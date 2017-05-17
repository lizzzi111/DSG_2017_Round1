#### STACKING

setwd("/Users/lizzzi111/Desktop/DSG_2017/data/")

# LOAD PACKAGES
library(caret)
library(caretEnsemble)
library(randomForest)
library(pROC)
# LOAD DATA
tr = read.csv("./tr_stacking_flow.csv") 
ts = read.csv("./ts_stacking_flow.csv")

# DATA PREPARATION
tr$real = factor(tr$real)
ts$real = factor(ts$real)

# FEATURE SELECTION RF
# Default Parameters
fit = randomForest(real~., tr, importance =T)
prognose = predict(fit, ts, type = "prob")
auc(ts$real, prognose[,2] )

# check the importance
imp = importance(fit)
important_features = rownames(fit$importance[imp[,3]>25,])

# rF default parameter (set.seed(1))
# rF all 0.7294
# rF imporance decrease > 25, auc 0.7224
# rF imporance decrease > 30, auc 0.7203
# 
# just mean 0.7222
# rF imporance decrease > 28, auc 0.7235
# 

fit_imp = randomForest(real~., tr[,c("real",important_features)], importance =T)
prognose_imp = predict(fit_imp, ts[,c(important_features)], type = "prob")

auc(ts$real, as.numeric(rowMeans(ts[,-28])) )
auc(ts$real, prognose_imp[,2] )

varImpPlot(fit)

min.model = glm(real~1, tr, family = "binomial")
biggest <- formula(glm(real~., tr, family = "binomial"))
fwd.model = step(min.model, direction='forward', scope=biggest)

library(caretEnsemble)
library(randomForest)
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('rf', 'glm', 'knn', 'svmRadial')
set.seed(1)
full <- rbind(data,rows)
names(full) <- make.names(names(full))
models <- caretList(real~., data=full, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)