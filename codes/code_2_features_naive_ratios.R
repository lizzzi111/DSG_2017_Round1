# subsetting 
data.train <- data.full[dataset == 'train',]

##### COMPUTING VARIABLES

# computing historical play/skip ratios ####
message("Calculate ratios")
user.ratio.flow <- data.train[listen_type == 1, .(user_ratio_flow = mean(is_listened)), by = .(user_id)]
user.ratio.full <- data.train[, .(user_ratio_full = mean(is_listened)), by = .(user_id)]
genre.ratio     <- data.train[, .(genre_ratio = mean(is_listened), count = .N),  by = genre_id]
artist.ratio    <- data.train[, .(artist_ratio = mean(is_listened), count = .N), by = artist_id]
song.ratio      <- data.train[, .(song_ratio = mean(is_listened), count = .N),   by = media_id]
# Computing user specific play/skip ratios
user.genre.ratio     <- data.train[, .(user_genre_ratio = mean(is_listened), count = .N),  by = .(user_id, genre_id)]
user.artist.ratio    <- data.train[, .(user_artist_ratio = mean(is_listened), count = .N), by = .(user_id, artist_id)]
user.song.ratio      <- data.train[, .(user_song_ratio = mean(is_listened), count = .N),   by = .(user_id, media_id)]


# keeping genre/artist/song data with at least 30 appearances
genre.ratio  <- genre.ratio[count   >= 30, ]
artist.ratio <- artist.ratio[count  >= 30, ]
song.ratio   <- song.ratio[count    >= 30, ]
user.genre.ratio  <- user.genre.ratio[count   >= 30, ]
user.artist.ratio <- user.artist.ratio[count  >= 30, ]
user.song.ratio   <- user.song.ratio[count    >= 30, ]

genre.ratio[, count  := NULL]
artist.ratio[, count := NULL]
song.ratio[, count   := NULL]
user.genre.ratio[, count  := NULL]
user.artist.ratio[, count := NULL]
user.song.ratio[, count   := NULL]

##### WRITING VALUES 

# saving simple ratios
message("Merge ratio with full data")
data.full <- merge(data.full, user.ratio.flow, all.x = TRUE, sort = F, by = "user_id")
data.full <- merge(data.full, user.ratio.full, all.x = TRUE, sort = F, by = "user_id")
data.full <- merge(data.full, genre.ratio,     all.x = TRUE, sort = F, by = "genre_id")
data.full <- merge(data.full, artist.ratio,    all.x = TRUE, sort = F, by = "artist_id")
data.full <- merge(data.full, song.ratio,      all.x = TRUE, sort = F, by = "media_id")
data.full <- merge(data.full, user.genre.ratio,     all.x = TRUE, sort = F, by = c("user_id","genre_id"))
data.full <- merge(data.full, user.artist.ratio,    all.x = TRUE, sort = F, by = c("user_id", "artist_id"))
data.full <- merge(data.full, user.song.ratio,      all.x = TRUE, sort = F, by = c("user_id", "media_id"))

# displaying resulted ratios
#summary(data.full[, c("user_ratio", "genre_ratio", "artist_ratio", "song_ratio")])
#cor(data.full[, c("user_ratio", "genre_ratio", "artist_ratio", "song_ratio")], use = "complete")

# imputing NAs with user ratio
data.full[is.na(user_ratio_full), user_ratio_full := mean(data.train$is_listened)]
data.full[is.na(user_ratio_flow), user_ratio_flow := user_ratio_othr]
data.full[is.na(genre_ratio),  genre_ratio  := mean(data.train$is_listened)]
data.full[is.na(artist_ratio), artist_ratio := mean(data.train$is_listened)]
data.full[is.na(song_ratio),   song_ratio   := mean(data.train$is_listened)]
data.full[is.na(user_genre_ratio),  user_genre_ratio  := mean(data.train$is_listened)]
data.full[is.na(user_artist_ratio), user_artist_ratio := mean(data.train$is_listened)]
data.full[is.na(user_song_ratio),   user_song_ratio   := mean(data.train$is_listened)]
