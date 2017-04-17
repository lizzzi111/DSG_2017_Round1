##### function to extract features
compute_features <- function(train, test) {
  
  # converting to numeric
  train$is_listened <- as.numeric(train$is_listened) - 1
  
  # computing historical play/skip ratio
  play_ratio <- aggregate(is_listened ~ user_id, train, mean)
  
  # keeping only played songs
  train <- train[train$is_listened == 1, ]
  
  # extracting play counts
  played_songs   <- aggregate(is_listened ~ user_id + media_id,  train, sum)
  played_albums  <- aggregate(is_listened ~ user_id + album_id,  train, sum)
  played_artists <- aggregate(is_listened ~ user_id + artist_id, train, sum)
  played_genres  <- aggregate(is_listened ~ user_id + genre_id,  train, sum)
  
  # extracting top plays [1/2]
  top_songs   <- aggregate(is_listened ~ user_id, played_songs,   max)
  top_albums  <- aggregate(is_listened ~ user_id, played_albums,  max)
  top_artists <- aggregate(is_listened ~ user_id, played_artists, max)
  top_genres  <- aggregate(is_listened ~ user_id, played_genres,  max)
  
  # extracting top plays [2/2]
  top_songs   <- merge(top_songs,   played_songs,   sort = F, all.x = T)
  top_albums  <- merge(top_albums,  played_albums,  sort = F, all.x = T)
  top_artists <- merge(top_artists, played_artists, sort = F, all.x = T)
  top_genres  <- merge(top_genres,  played_genres,  sort = F, all.x = T)
  
  # creating features for plays and TOPs
  song_plays   <- merge(test[, c("sample_id", "user_id", "media_id")],  played_songs,   sort = F, all.x = T)$is_listened
  song_tops    <- merge(test[, c("sample_id", "user_id", "media_id")],  top_songs,      sort = F, all.x = T)$is_listened
  album_plays  <- merge(test[, c("sample_id", "user_id", "album_id")],  played_albums,  sort = F, all.x = T)$is_listened
  album_tops   <- merge(test[, c("sample_id", "user_id", "album_id")],  top_albums,     sort = F, all.x = T)$is_listened
  artist_plays <- merge(test[, c("sample_id", "user_id", "artist_id")], played_artists, sort = F, all.x = T)$is_listened
  artist_tops  <- merge(test[, c("sample_id", "user_id", "artist_id")], top_artists,    sort = F, all.x = T)$is_listened
  genre_plays  <- merge(test[, c("sample_id", "user_id", "genre_id")],  played_genres,  sort = F, all.x = T)$is_listened
  genre_tops   <- merge(test[, c("sample_id", "user_id", "genre_id")],  top_genres,     sort = F, all.x = T)$is_listened  
  
  # saving play ratio
  test$play_ratio <- merge(test[, c("sample_id", "user_id")], play_ratio, sort = F, all.x = T)$is_listened
  
  # computing listening dummies
  test$new_song   <- as.factor(is.na(song_plays))
  test$new_album  <- as.factor(is.na(album_plays))
  test$new_artist <- as.factor(is.na(artist_plays))
  test$new_genre  <- as.factor(is.na(genre_plays))
  
  # computing TOP dummies
  test$top_song   <- as.factor(!is.na(song_tops))
  test$top_album  <- as.factor(!is.na(album_tops))
  test$top_artist <- as.factor(!is.na(artist_tops))
  test$top_genre  <- as.factor(!is.na(genre_tops))
  
  # checking the data
  summary(test)
  
  # returning the data
  return(test)
}