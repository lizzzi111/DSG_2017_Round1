# subsetting 
data.train <- data.full[dataset == 'train',]

# converting to numeric
#data.train$is_listened <- as.numeric(data.train$is_listened)-1

##### COMPUTING VARIABLES

# computing historical play/skip ratios
message("Calculate ratios")
user.ratio.flow <- data.train[listen_type == "1", .(user_ratio_flow = mean(is_listened)), by = .(user_id)]
user.ratio.full <- data.train[, .(user_ratio_full = mean(is_listened)), by = .(user_id)]
genre.ratio     <- data.train[, .(genre_ratio = mean(is_listened), count = .N),  by = .(user_id, genre_id)]
artist.ratio    <- data.train[, .(artist_ratio = mean(is_listened), count = .N), by = .(user_id, artist_id)]
song.ratio      <- data.train[, .(song_ratio = mean(is_listened), count = .N),   by = .(user_id, media_id)]

# keeping genre/artist/song data with at least 30 appearances
genre.ratio  <- genre.ratio[count   >= 30, ]
artist.ratio <- artist.ratio[count  >= 30, ]
song.ratio   <- song.ratio[count    >= 30, ]
genre.ratio  <- genre.ratio[, count  := NULL]
artist.ratio <- artist.ratio[, count := NULL]
song.ratio   <- song.ratio[, count   := NULL]



##### WRITING VALUES 

# saving simple ratios
message("Merge ratio with full data")
data.full <- merge(data.full, user.ratio.flow, all.x = TRUE, sort = F, by = "user_id")
data.full <- merge(data.full, user.ratio.full, all.x = TRUE, sort = F, by = "user_id")
data.full <- merge(data.full, genre.ratio,     all.x = TRUE, sort = F, by = c("user_id","genre_id"))
data.full <- merge(data.full, artist.ratio,    all.x = TRUE, sort = F, by = c("user_id", "artist_id"))
data.full <- merge(data.full, song.ratio,      all.x = TRUE, sort = F, by = c("user_id", "media_id"))

# displaying resulted ratios
#summary(data.full[, c("user_ratio", "genre_ratio", "artist_ratio", "song_ratio")])
#cor(data.full[, c("user_ratio", "genre_ratio", "artist_ratio", "song_ratio")], use = "complete")

# imputing NAs with user ratio
data.full[is.na(user_ratio_full), user_ratio_full := mean(data.train$is_listened)]
data.full[is.na(user_ratio_flow), user_ratio_flow := user_ratio_full]
data.full[is.na(genre_ratio),  genre_ratio  := user_ratio_full]
data.full[is.na(artist_ratio), artist_ratio := user_ratio_full]
data.full[is.na(song_ratio),   song_ratio   := user_ratio_full]