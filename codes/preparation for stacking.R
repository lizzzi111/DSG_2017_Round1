### preparation for stacking
setwd("/Users/lizzzi111/Desktop/DSG_2017/pred_valid/")
#setwd("N:/DSG_2017/pred_valid")
temp = list.files(pattern="keras_newdata_flow")
temp = list.files()
myfiles = as.data.frame(sapply(temp, function(file) read.csv(file)[2]))

test = read.csv("../data/rows.csv")
test = test[,c("rows", "is_listened")]

library(caret)
data = cbind(myfiles, test$is_listened)
names(data)[names(data)=="test$is_listened"] = "real"
set.seed(1)
ind = unlist(createDataPartition(test$rows, p = 0.6))

tr = data[ind,]
ts = data[-ind,]

write.csv(data, "../data/full_stacking_flow.csv", row.names = F)
write.csv(tr, "../data/tr_stacking_flow.csv", row.names = F)
write.csv(ts, "../data/ts_stacking_flow.csv", row.names = F)

write.csv(tr, "../data/tr_stacking_all.csv", row.names = F)
write.csv(ts, "../data/ts_stacking_all.csv", row.names = F)

write.csv(tr, "../data/tr_stacking_with_factorization.csv", row.names = F)
write.csv(ts, "../data/ts_stacking_with_factorization.csv", row.names = F)
