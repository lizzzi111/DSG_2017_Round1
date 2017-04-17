###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting work directory
work.folder <- "/Users/Kozodoi/Documents/Competitions/DSG_2017"
#work.folder <- "C:/Users/kozodoin3.hub/Desktop/DSG_2017"
setwd(work.folder)

# setting inner folders
code.folder <- "codes"
data.folder <- "data"
func.folder <- "functions"
subm.folder <- "submissions"

# loading libraries
library(data.table)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))


###################################
#                                 #
#         DATA PARTITIONING       #
#                                 #
###################################

# loading data set
load(file.path(data.folder, "data_train.Rda"))

# 1) Selecting first 90% history of each user as a training sample
# 2) Selecting last 10% history of each user as a validation sample

dt <- as.data.table(data.train)
dt[,user_listened:=.N, by=user_id]
sam <- dt[user_listened>=100]
sam <- sam[order(ts_listen),.SD, by=user_id]

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