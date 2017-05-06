# subsetting 
data.train <- data.full[dataset == 'train',]

# converting to numeric
#data.train$is_listened <- as.numeric(data.train$is_listened)-1

##### COMPUTING VARIABLES

# computing total plays (refer to overall popularity)
message("Calculating play counts")
genre.plays  <- data.train[is_listened == 1, .(genre_plays =.N),  by = .(genre_id)]
artist.plays <- data.train[is_listened == 1, .(artist_plays =.N), by = .(artist_id)]
album.plays  <- data.train[is_listened == 1, .(album_plays =.N),  by = .(album_id)]
song.plays   <- data.train[is_listened == 1, .(song_plays =.N),   by = .(media_id)]

# computing total skips (refer to overall popularity)
message("Calculating skip counts")
genre.skips  <- data.train[is_listened == 0, .(genre_skips =.N),  by = .(genre_id)]
artist.skips <- data.train[is_listened == 0, .(artist_skips =.N), by = .(artist_id)]
album.skips  <- data.train[is_listened == 0, .(album_skips =.N),  by = .(album_id)]
song.skips   <- data.train[is_listened == 0, .(song_skips =.N),   by = .(media_id)]


##### WRITING VALUES 

# saving simple ratios
message("Merging with full data")
data.full <- merge(data.full, genre.plays,  all.x = T, sort = F, by = "genre_id")
data.full <- merge(data.full, genre.skips,  all.x = T, sort = F, by = "genre_id")
data.full <- merge(data.full, artist.plays, all.x = T, sort = F, by = "artist_id")
data.full <- merge(data.full, artist.skips, all.x = T, sort = F, by = "artist_id")
data.full <- merge(data.full, album.plays,  all.x = T, sort = F, by = "album_id")
data.full <- merge(data.full, album.skips,  all.x = T, sort = F, by = "album_id")
data.full <- merge(data.full, song.plays,   all.x = T, sort = F, by = "media_id")
data.full <- merge(data.full, song.skips,   all.x = T, sort = F, by = "media_id")

# imputing NAs with true zeros
data.full[is.na(genre_plays),  genre_plays  := 0]
data.full[is.na(genre_skips),  genre_skips  := 0]
data.full[is.na(artist_plays), artist_plays := 0]
data.full[is.na(artist_skips), artist_skips := 0]
data.full[is.na(album_plays),  album_plays  := 0]
data.full[is.na(album_skips),  album_skips  := 0]
data.full[is.na(song_plays),   song_plays   := 0]
data.full[is.na(song_skips),   song_skips   := 0]