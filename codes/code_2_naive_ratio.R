###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting work directory
work.folder <- "/Users/Kozodoi/Documents/Competitions/DSG_2017"
work.folder <- "C:/Users/kozodoin3.hub/Desktop/DSG_2017-master"
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
#        NAIVE MODEL (RATIO)      #
#                                 #
###################################

# loading data sets
load(file.path(data.folder, "data_train.Rda"))
load(file.path(data.folder, "data_test.Rda"))

# keeping the genre data
data.train <- data.train[, c("user_id", "sample_id", "media_id", "is_listened")]
data.test  <- data.test[,  c("user_id", "sample_id", "media_id", "is_listened")]

# empty variables
data.test$play_skip_user <- NA
data.test$play_skip_song <- NA

### computing play/skip ratio of each user
for (id in unique(data.test$user_id)) {
  
  # displaying user ID
  print(paste0(which(unique(data.test$user_id) == id), "/", length(unique(data.test$user_id))))
  
  # extracting play/skip history
  plays <- data.train$is_listened[data.train$user_id == id]
  
  # saving play/skip ratio
  data.test$play_skip_user[data.test$user_id == id] <- mean(plays)
}

### computing play/skip ratio of each song
for (song in unique(data.test$media_id)) {
  
  # displaying song ID
  print(paste0(which(unique(data.test$media_id) == song), "/", length(unique(data.test$media_id))))
  
  # extracting play/skip history
  plays <- data.train$is_listened[data.train$media_id == song]
  
  # saving play/skip ratio
  data.test$play_skip_song[data.test$media_id == song] <- mean(plays)
}

# computing average play/skip ratio
data.test$play_skip_mean <- (data.test$play_skip_user + data.test$play_skip_song)/2

# creating submission
submit(data.test$play_skip_user, data = data.test, folder = subm.folder, file = "naive_ratio_user.csv")
submit(data.test$play_skip_song, data = data.test, folder = subm.folder, file = "naive_ratio_song.csv")
submit(data.test$play_skip_mean, data = data.test, folder = subm.folder, file = "naive_ratio_mean.csv")