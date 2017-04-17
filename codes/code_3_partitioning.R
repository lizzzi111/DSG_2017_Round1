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
library(anytime)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))


###################################
#                                 #
#         DATA PARTITIONING       #
#                                 #
###################################

# 1) Selecting first 90% history of each user as a training sample
# 2) Selecting last 10% history of each user as a validation sample

# partitionin ratio
ratio <- 0.9

# loading data sets
load(file.path(data.folder, "data_train.Rda"))
load(file.path(data.folder, "data_test.Rda"))

# extracting user IDs
users <- levels(data.train$user_id)

# creating empty data frames
sample.train <- data.frame()
sample.valid <- data.frame()

# partitinoning the data
for (id in users) {
  
  # displaying user ID
  print(paste0(id, "/", length(users)))
  
  # extracting user data
  user.data <- data.train[data.train$user_id == id, ]
  user.data <- user.data[order(user.data$ts_listen), ]
  
  # partitioning user data
  user.train <- user.data[1:round(ratio*nrow(user.data)), ]
  user.valid <- user.data[(round(ratio*nrow(user.data))+1):nrow(user.data), ]
  
  # saving data
  sample.train <- rbind(sample.train, user.train)
  sample.valid <- rbind(sample.valid, user.valid)
}

# saving data samples
save(sample.train, file = file.path(data.folder, "sample_train.Rda"))
save(sample.valid, file = file.path(data.folder, "sample_valid.Rda"))