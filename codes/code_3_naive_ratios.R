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
#       NAIVE MODEL (RATIOS)      #
#                                 #
###################################

##### 1. PREPARATIONS

# loading data sets
load(file.path(data.folder, "data_train.Rda"))
load(file.path(data.folder, "data_test.Rda"))

# converting variable to numeric
data.train$is_listened <- as.numeric(data.train$is_listened)


##### 2. COMPUTING RATIOS

# computing play/skip ratio by user
user.ratio <- aggregate(is_listened ~ user_id, data.train, mean)

# computing play/skip ratio by genre
genre.ratio <- aggregate(is_listened ~ genre_id, data.train, mean)

# computing play/skip ratio by artist
artist.ratio <- aggregate(is_listened ~ artist_id, data.train, mean)

# computing play/skip ratio by song
song.ratio <- aggregate(is_listened ~ media_id, data.train, mean)


##### 3. SAVING TO TESTING DATA

# saving all ratios
data.test$user_ratio   <- merge(data.test[, c("sample_id", "user_id")],   user.ratio,   sort = F, all.x = T)$is_listened
data.test$genre_ratio  <- merge(data.test[, c("sample_id", "genre_id")],  genre.ratio,  sort = F, all.x = T)$is_listened
data.test$artist_ratio <- merge(data.test[, c("sample_id", "artist_id")], artist.ratio, sort = F, all.x = T)$is_listened
data.test$song_ratio   <- merge(data.test[, c("sample_id", "media_id")],  song.ratio,   sort = F, all.x = T)$is_listened

# imputing NAs with means
data.test$user_ratio[is.na(data.test$user_ratio)]     <- mean(data.test$user_ratio, na.rm = T)
data.test$genre_ratio[is.na(data.test$genre_ratio)]   <- mean(data.test$genre_ratio, na.rm = T)
data.test$artist_ratio[is.na(data.test$artist_ratio)] <- mean(data.test$artist_ratio, na.rm = T)
data.test$song_ratio[is.na(data.test$song_ratio)]     <- mean(data.test$song_ratio, na.rm = T)

# computing average ratio
data.test$mean_ratio <- (data.test$user_ratio + data.test$genre_ratio + data.test$artist_ratio + data.test$song_ratio)/4
summary(data.test$mean_ratio)

# creating submission
submit(data.test$user_ratio,   data = data.test, folder = subm.folder, file = "naive_ratio_user.csv")
submit(data.test$genre_ratio,  data = data.test, folder = subm.folder, file = "naive_ratio_genre.csv")
submit(data.test$artist_ratio, data = data.test, folder = subm.folder, file = "naive_ratio_artist.csv")
submit(data.test$song_ratio,   data = data.test, folder = subm.folder, file = "naive_ratio_song.csv")
submit(data.test$mean_ratio,   data = data.test, folder = subm.folder, file = "naive_ratio_mean.csv")