# library
library(data.table)

dt <- as.data.table(data.train)
dt[,user_listened:=.N, by=user_id]
sam <- dt[user_listened>=100]
sam[order(ts_listen),.SD, by=user_id]

#sam_train <- sam[, sample(.N, length(.N)*0.1), by=user_id]
sam[, for_val:=round(mean(user_listened)*0.1), by=user_id]
sam[, for_train:=user_listened-for_val, by=user_id]

sam_v <- sam[, tail(.SD, mean(for_val)), by=user_id]
sam_t <- sam[, head(.SD, mean(for_train)), by=user_id]

# delete useless variables
sam_v[, for_val:=NULL]
sam_t[, for_val:=NULL]
sam_v[, for_train:=NULL]
sam_t[, for_train:=NULL]

# convert to data frame
sample.train <- as.data.frame(sam_t)
sample.valid <- as.data.frame(sam_v)

# saving data samples
save(sample.train, file = file.path(data.folder, "sample_train.Rda"))
save(sample.valid, file = file.path(data.folder, "sample_valid.Rda"))