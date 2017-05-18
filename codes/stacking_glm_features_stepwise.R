### PREDCTION ON UNKNOWNs
setwd("N:/DSG_2017/")

# read in the list of important features
# manually deleted .is_listened from the csv
temp = list.files("./pred_unknown/")
unknown = as.data.frame(sapply(temp, function(file) read.csv(paste0("./pred_unknown/",file))[2]))
unknown$sample_id = read.csv(paste0("./pred_unknown/", temp[1]))$sample_id

# LOAD PACKAGES
library(caret)
library(pROC)

# LOAD DATA
temp_full = list.files("./pred_valid/")
full = as.data.frame(sapply(temp_full, function(file) read.csv(paste0("./pred_valid/",file))[2]))

# LOAD ROWS
real = read.csv("./data/known_test_rows.csv")
#check = read.csv("./pred_valid/keras_newdata_flow_200.csv")
#all(check$row_index==real$row_index)
#TRUE
full$real = real$is_listened

# LOAD GLM FORMULA
bf = readRDS("./data/best_form.rds")
# LOGIT
#glm_fit = glm(bf., full, family = "binomial")
glm_fit = glm(real~., full, family = "binomial")
unknown$is_listened = predict(glm_fit, newdata = unknown, type = "response" )

#write.csv(unknown[,c("sample_id", "is_listened")], "./pred_unknown/stacking_glm_stepwise.csv", row.names = F)
# leaderbord  	0.66792, validation 0.7485
write.csv(unknown[,c("sample_id", "is_listened")], "./pred_unknown/stacking_glm_stepwise_all.csv", row.names = F)
# leaderboard 0.66883, val 0.7489