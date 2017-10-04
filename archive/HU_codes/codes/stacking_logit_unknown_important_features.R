### PREDCTION ON UNKNOWNs


# read in the list of important features
# manually deleted .is_listened from the csv
important_features = c(read.csv("../data/important_names.csv"))
important_features = levels(important_features$x)
unknown = as.data.frame(sapply(important_features, function(file) read.csv(file)[2]))
unknown$sample_id = read.csv(important_features[1])$sample_id

setwd("N:/DSG_2017/data")

# LOAD PACKAGES
library(caret)
library(pROC)
# LOAD DATA
tr = read.csv("./tr_stacking_flow.csv") 
ts = read.csv("./ts_stacking_flow.csv")
full = rbind(tr,ts)
full = full[,c("real",paste0(important_features,".is_listened"))]

# LOGIT
glm_fit = glm(real~., full, family = "binomial")
unknown$is_listened = predict(glm_fit, newdata = unknown[,-26], type = "response" )

write.csv(unknown[,c("sample_id", "is_listened")], "../pred_unknown/stacking_glm_imp_fet.csv", row.names = F)
