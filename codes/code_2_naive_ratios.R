

data.train <- data.full[dataset == 'train',]
###################################
#                                 #
#       NAIVE MODEL (RATIOS)      #
#                                 #
###################################

# converting variable to numeric
data.train$is_listened <- as.numeric(data.train$is_listened) - 1

# adding simple counter
data.train$count <- 1

##### 2. COMPUTING VARIABLES

message("Calculate ratios")
# computing historical play/skip ratios
user.ratio <- data.train[, .(user_ratio = mean(is_listened)) , by = .(user_id)]
genre.ratio <- data.train[, .(genre_ratio = mean(is_listened), count = .N) , by = .(user_id, genre_id)]
artist.ratio <- data.train[, .(artist_ratio = mean(is_listened), count = .N) , by = .(user_id, artist_id)]
song.ratio <- data.train[, .(song_ratio = mean(is_listened), count = .N) , by = .(user_id, media_id)]
# user.ratio   <- aggregate(is_listened ~ user_id,             data.train, mean)
# genre.ratio  <- aggregate(is_listened ~ user_id + genre_id,  data.train, mean)
# artist.ratio <- aggregate(is_listened ~ user_id + artist_id, data.train, mean)
# song.ratio   <- aggregate(is_listened ~ user_id + media_id,  data.train, mean)

# computing total appearance counts
# genre.count <- data.train[, .(genre_count =.N), by = .(user_id, genre_id)]
# artist.count <- data.train[, .(artist_count =.N), by = .(user_id, artist_id)]
# song.count <- data.train[, .(song_count =.N), by = .(user_id, media_id)]
# genre.count  <- aggregate(count ~ user_id + genre_id,  data.train, sum)
# artist.count <- aggregate(count ~ user_id + artist_id, data.train, sum)
# song.count   <- aggregate(count ~ user_id + media_id,  data.train, sum)

# keeping genre/artist/song data with at least 30 appearances
genre.ratio  <- genre.ratio[count   >= 30, ]
artist.ratio <- artist.ratio[count >= 30, ]
song.ratio   <- song.ratio[count     >= 30, ]
genre.ratio  <- genre.ratio[, count := NULL]
artist.ratio  <- artist.ratio[, count := NULL]
song.ratio  <- song.ratio[, count := NULL]

# subsetting play/skip ratios
# genre.ratio  <- merge(genre.count,  genre.ratio,  sort = F, all.x = T)
# artist.ratio <- merge(artist.count, artist.ratio, sort = F, all.x = T)
# song.ratio   <- merge(song.count,   song.ratio,   sort = F, all.x = T)


##### 3. CREATING PREDICTIONS

message("Merge ratio with full data")
# saving simple ratios
data.full <- merge(data.full, user.ratio, all.x = TRUE, sort = F, by = "user_id")
data.full <- merge(data.full, genre.ratio, all.x = TRUE, sort = F, by = c("user_id","genre_id"))
data.full <- merge(data.full, artist.ratio, all.x = TRUE, sort = F, by = c("user_id", "artist_id"))
data.full <- merge(data.full, song.ratio, all.x = TRUE, sort = F, by = c("user_id", "media_id"))
# data.full$user_ratio   <- merge(data.full[, c("sample_id", "user_id")],              user.ratio,   sort = F, all.x = T)$is_listened
# data.full$genre_ratio  <- merge(data.full[, c("sample_id", "user_id", "genre_id")],  genre.ratio,  sort = F, all.x = T)$is_listened
# data.full$artist_ratio <- merge(data.full[, c("sample_id", "user_id", "artist_id")], artist.ratio, sort = F, all.x = T)$is_listened
# data.full$song_ratio   <- merge(data.full[, c("sample_id", "user_id", "media_id")],  song.ratio,   sort = F, all.x = T)$is_listened

# displaying resulted ratios
#summary(data.full[, c("user_ratio", "genre_ratio", "artist_ratio", "song_ratio")])
#cor(data.full[, c("user_ratio", "genre_ratio", "artist_ratio", "song_ratio")], use = "complete")

# imputing NAs with user ratio
data.full[is.na(user_ratio), user_ratio := mean(data.train$is_listened)]
data.full[is.na(genre_ratio), genre_ratio := user_ratio]
data.full[is.na(artist_ratio), artist_ratio := user_ratio]
data.full[is.na(song_ratio), song_ratio := user_ratio]
# data.full$genre_ratio[is.na(data.full$genre_ratio)]   <- data.full$user_ratio[is.na(data.full$genre_ratio)]
# data.full$artist_ratio[is.na(data.full$artist_ratio)] <- data.full$user_ratio[is.na(data.full$artist_ratio)]
# data.full$song_ratio[is.na(data.full$song_ratio)]     <- data.full$user_ratio[is.na(data.full$song_ratio)]

# computing average ratio
data.full[, mean_ratio := (user_ratio + genre_ratio + artist_ratio + song_ratio)/4]
#summary(data.full$mean_ratio)

# creating submissions
# submit(data.test$user_ratio,   data = data.test, folder = subm.folder, file = "naive_ratio_user.csv")
# submit(data.test$genre_ratio,  data = data.test, folder = subm.folder, file = "naive_ratio_genre.csv")
# submit(data.test$artist_ratio, data = data.test, folder = subm.folder, file = "naive_ratio_artist.csv")
# submit(data.test$song_ratio,   data = data.test, folder = subm.folder, file = "naive_ratio_song.csv")
# submit(data.test$mean_ratio,   data = data.test, folder = subm.folder, file = "naive_ratio_mean.csv")