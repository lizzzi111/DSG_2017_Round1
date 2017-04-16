###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting work directory
work.folder <- "/Users/Kozodoi/Documents/Competitions/DSG_2017"
#work.folder <- "C:/Users/kozodoin3.hub/Desktop/DSG_2017-master"
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
#        NAIVE MODEL (ARTIST)     #
#                                 #
###################################

# loading data sets
load(file.path(data.folder, "data_train.Rda"))
load(file.path(data.folder, "data_test.Rda"))

# keeping the artist data
data.train <- data.train[, c("user_id", "artist_id", "sample_id", "is_listened")]
data.test  <- data.test[,  c("user_id", "artist_id", "sample_id", "is_listened")]

# dropping duplicates and skipped tracks
data.train <- data.train[data.train$is_listened == 1, ]
data.train <- data.train[!(duplicated(data.train)),   ]

# empty prediction vector
naive.pred <- rep(NA, nrow(data.test))

# checking if the artist has already been played
for (i in 1:nrow(data.test)) {
  
  # displaying observation number
  print(paste0(i, "/", nrow(data.test)))
  
  # extracting artist and user
  user   <- data.test$user_id[i]
  artist <- data.test$artist_id[i]
  
  # extracting artists played by user
  history <- data.train$artist_id[data.train$user_id == user]
  
  # checking if this artist has already been played
  if (artist %in% history) {
    naive.pred[i] <- 1
  }else{
    naive.pred[i] <- 0
  }
}

# creating submission
submit(naive.pred, data = data.test, folder = subm.folder, file = "naive_artist.csv")