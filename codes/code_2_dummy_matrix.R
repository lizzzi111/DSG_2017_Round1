### Drop dummies for now
facVars <- c("context_type", "platform_name", "platform_family")
temp <- model.matrix(~.-1, data = data.full[, (facVars), with = FALSE])
data.full <- cbind(data.full, temp)

# Weight of evidence
library(klaR)
data.train <- data.full[dataset == 'train']
woe.obj <- woe(x= data.full$context_type, grouping = factor(data.full$is_listened), zeroadj = 0.1)
data.full <- predict(woe.obj, newdata = data.full)