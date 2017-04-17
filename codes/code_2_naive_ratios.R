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

# adding simple counter
data.train$count <- 1


##### 2. COMPUTING VARIABLES

# computing historical play/skip ratios
user.ratio   <- aggregate(is_listened ~ user_id,             data.train, mean)
genre.ratio  <- aggregate(is_listened ~ user_id + genre_id,  data.train, mean)
artist.ratio <- aggregate(is_listened ~ user_id + artist_id, data.train, mean)
song.ratio   <- aggregate(is_listened ~ user_id + media_id,  data.train, mean)

# computing total appearance counts
genre.count  <- aggregate(count ~ user_id + genre_id,  data.train, sum)
artist.count <- aggregate(count ~ user_id + artist_id, data.train, sum)
song.count   <- aggregate(count ~ user_id + media_id,  data.train, sum)

# keeping genre/artist/song data with at least 30 appearances
genre.count  <- genre.count[genre.count$count   >= 30, ]
artist.count <- artist.count[artist.count$count >= 30, ]
song.count   <- song.count[song.count$count     >= 30, ]

# subsetting play/skip ratios
genre.ratio  <- merge(genre.count,  genre.ratio,  sort = F, all.x = T)
artist.ratio <- merge(artist.count, artist.ratio, sort = F, all.x = T)
song.ratio   <- merge(song.count,   song.ratio,   sort = F, all.x = T)


##### 3. CREATING PREDICTIONS

# saving simple ratios
data.test$user_ratio   <- merge(data.test[, c("sample_id", "user_id")],              user.ratio,   sort = F, all.x = T)$is_listened
data.test$genre_ratio  <- merge(data.test[, c("sample_id", "user_id", "genre_id")],  genre.ratio,  sort = F, all.x = T)$is_listened
data.test$artist_ratio <- merge(data.test[, c("sample_id", "user_id", "artist_id")], artist.ratio, sort = F, all.x = T)$is_listened
data.test$song_ratio   <- merge(data.test[, c("sample_id", "user_id", "media_id")],  song.ratio,   sort = F, all.x = T)$is_listened

# displaying resulted ratios
summary(data.test[, c("user_ratio", "genre_ratio", "artist_ratio", "song_ratio")])
cor(data.test[, c("user_ratio", "genre_ratio", "artist_ratio", "song_ratio")], use = "complete")

# imputing NAs with user ratio
data.test$genre_ratio[is.na(data.test$genre_ratio)]   <- data.test$user_ratio[is.na(data.test$genre_ratio)]
data.test$artist_ratio[is.na(data.test$artist_ratio)] <- data.test$user_ratio[is.na(data.test$artist_ratio)]
data.test$song_ratio[is.na(data.test$song_ratio)]     <- data.test$user_ratio[is.na(data.test$song_ratio)]

# computing average ratio
data.test$mean_ratio <- (data.test$user_ratio + data.test$genre_ratio + data.test$artist_ratio + data.test$song_ratio)/4
summary(data.test$mean_ratio)

# creating submissions
submit(data.test$user_ratio,   data = data.test, folder = subm.folder, file = "naive_ratio_user.csv")
submit(data.test$genre_ratio,  data = data.test, folder = subm.folder, file = "naive_ratio_genre.csv")
submit(data.test$artist_ratio, data = data.test, folder = subm.folder, file = "naive_ratio_artist.csv")
submit(data.test$song_ratio,   data = data.test, folder = subm.folder, file = "naive_ratio_song.csv")
submit(data.test$mean_ratio,   data = data.test, folder = subm.folder, file = "naive_ratio_mean.csv")