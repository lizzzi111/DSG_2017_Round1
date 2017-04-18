##### function to extract features
compute_features <- function(train, test) {
  
  ##### 1. PREPARATION
  
  # converting to numeric
  train$is_listened <- as.numeric(train$is_listened) - 1
  train$is_skipped  <- 1- train$is_listened
  
  
  ##### 2. FEATURES ON USER LEVEL
  
  # computing historical play/skip ratios
  ratio_per_user   <- aggregate(is_listened ~ user_id,   train, mean)
  ratio_per_song   <- aggregate(is_listened ~ media_id,  train, mean)
  ratio_per_album  <- aggregate(is_listened ~ album_id,  train, mean)
  ratio_per_artist <- aggregate(is_listened ~ artist_id, train, mean)
  ratio_per_genre  <- aggregate(is_listened ~ genre_id,  train, mean)
  
  # computing user-specific play counts
  user_plays_song   <- aggregate(is_listened ~ user_id + media_id,  train, sum)
  user_plays_album  <- aggregate(is_listened ~ user_id + album_id,  train, sum)
  user_plays_artist <- aggregate(is_listened ~ user_id + artist_id, train, sum)
  user_plays_genre  <- aggregate(is_listened ~ user_id + genre_id,  train, sum)
  
  # computing user-specific skip counts
  user_skips_song   <- aggregate(is_skipped ~ user_id + media_id,  train, sum)
  user_skips_album  <- aggregate(is_skipped ~ user_id + album_id,  train, sum)
  user_skips_artist <- aggregate(is_skipped ~ user_id + artist_id, train, sum)
  user_skips_genre  <- aggregate(is_skipped ~ user_id + genre_id,  train, sum)
  
  # computing user-specific top plays [1/2]
  user_top_songs   <- aggregate(is_listened ~ user_id, user_plays_song,   max)
  user_top_albums  <- aggregate(is_listened ~ user_id, user_plays_album,  max)
  user_top_artists <- aggregate(is_listened ~ user_id, user_plays_artist, max)
  user_top_genres  <- aggregate(is_listened ~ user_id, user_plays_genre,  max)
  
  # computing user-specific top plays [2/2]
  user_top_songs   <- merge(user_top_songs,   user_plays_song,   sort = F, all.x = T)
  user_top_albums  <- merge(user_top_albums,  user_plays_album,  sort = F, all.x = T)
  user_top_artists <- merge(user_top_artists, user_plays_artist, sort = F, all.x = T)
  user_top_genres  <- merge(user_top_genres,  user_plays_genre,  sort = F, all.x = T)
  
  # computing user-specific top skips [1/2]
  user_bad_songs   <- aggregate(is_skipped ~ user_id, user_skips_song,   max)
  user_bad_albums  <- aggregate(is_skipped ~ user_id, user_skips_album,  max)
  user_bad_artists <- aggregate(is_skipped ~ user_id, user_skips_artist, max)
  user_bad_genres  <- aggregate(is_skipped ~ user_id, user_skips_genre,  max)
  
  # computing user-specific top skips [2/2]
  user_bad_songs   <- merge(user_bad_songs,   user_skips_song,   sort = F, all.x = T)
  user_bad_albums  <- merge(user_bad_albums,  user_skips_album,  sort = F, all.x = T)
  user_bad_artists <- merge(user_bad_artists, user_skips_artist, sort = F, all.x = T)
  user_bad_genres  <- merge(user_bad_genres,  user_skips_genre,  sort = F, all.x = T)
  
  
  ##### 3. FEATURES ON AGGREGATE LEVEL
  
  # computing aggregate play counts 
  total_plays_song   <- aggregate(is_listened ~ media_id,  train, sum)
  total_plays_album  <- aggregate(is_listened ~ album_id,  train, sum)
  total_plays_artist <- aggregate(is_listened ~ artist_id, train, sum)
  total_plays_genre  <- aggregate(is_listened ~ genre_id,  train, sum)
  
  # computing aggregate skip counts 
  total_skips_song   <- aggregate(is_skipped ~ media_id,  train, sum)
  total_skips_album  <- aggregate(is_skipped ~ album_id,  train, sum)
  total_skips_artist <- aggregate(is_skipped ~ artist_id, train, sum)
  total_skips_genre  <- aggregate(is_skipped ~ genre_id,  train, sum)
  
  
  ##### 4. SAVING FEATURES TO DATA
  
  # saving play/skip ratios
  test$ratio_per_user   <- merge(test[, c("sample_id", "user_id")],   ratio_per_user,   sort = F, all.x = T)$is_listened
  test$ratio_per_song   <- merge(test[, c("sample_id", "media_id")],  ratio_per_song,   sort = F, all.x = T)$is_listened
  test$ratio_per_album  <- merge(test[, c("sample_id", "album_id")],  ratio_per_album,  sort = F, all.x = T)$is_listened
  test$ratio_per_artist <- merge(test[, c("sample_id", "artist_id")], ratio_per_artist, sort = F, all.x = T)$is_listened
  test$ratio_per_genre  <- merge(test[, c("sample_id", "genre_id")],  ratio_per_genre,  sort = F, all.x = T)$is_listened
  
  # saving play and skip counts: user level
  test$user_song_plays   <- merge(test[, c("sample_id", "user_id", "media_id")],  user_plays_song,   sort = F, all.x = T)$is_listened
  test$user_album_plays  <- merge(test[, c("sample_id", "user_id", "album_id")],  user_plays_album,  sort = F, all.x = T)$is_listened
  test$user_artist_plays <- merge(test[, c("sample_id", "user_id", "artist_id")], user_plays_artist, sort = F, all.x = T)$is_listened
  test$user_genre_plays  <- merge(test[, c("sample_id", "user_id", "genre_id")],  user_plays_genre,  sort = F, all.x = T)$is_listened
  test$user_song_skips   <- merge(test[, c("sample_id", "user_id", "media_id")],  user_skips_song,   sort = F, all.x = T)$is_skipped
  test$user_album_skips  <- merge(test[, c("sample_id", "user_id", "album_id")],  user_skips_album,  sort = F, all.x = T)$is_skipped
  test$user_artist_skips <- merge(test[, c("sample_id", "user_id", "artist_id")], user_skips_artist, sort = F, all.x = T)$is_skipped
  test$user_genre_skips  <- merge(test[, c("sample_id", "user_id", "genre_id")],  user_skips_genre,  sort = F, all.x = T)$is_skipped
  
  # saving play and skip counts: aggregate level
  test$total_song_plays   <- merge(test[, c("sample_id", "user_id", "media_id")],  total_plays_song,   sort = F, all.x = T)$is_listened
  test$total_album_plays  <- merge(test[, c("sample_id", "user_id", "album_id")],  total_plays_album,  sort = F, all.x = T)$is_listened
  test$total_artist_plays <- merge(test[, c("sample_id", "user_id", "artist_id")], total_plays_artist, sort = F, all.x = T)$is_listened
  test$total_genre_plays  <- merge(test[, c("sample_id", "user_id", "genre_id")],  total_plays_genre,  sort = F, all.x = T)$is_listened
  test$total_song_skips   <- merge(test[, c("sample_id", "user_id", "media_id")],  total_skips_song,   sort = F, all.x = T)$is_skipped
  test$total_album_skips  <- merge(test[, c("sample_id", "user_id", "album_id")],  total_skips_album,  sort = F, all.x = T)$is_skipped
  test$total_artist_skips <- merge(test[, c("sample_id", "user_id", "artist_id")], total_skips_artist, sort = F, all.x = T)$is_skipped
  test$total_genre_skips  <- merge(test[, c("sample_id", "user_id", "genre_id")],  total_skips_genre,  sort = F, all.x = T)$is_skipped  
  
  # saving TOP and BAD: user level
  test$user_top_song   <- merge(test[, c("sample_id", "user_id", "media_id")],  user_top_songs,   sort = F, all.x = T)$is_listened
  test$user_top_album  <- merge(test[, c("sample_id", "user_id", "album_id")],  user_top_albums,  sort = F, all.x = T)$is_listened
  test$user_top_artist <- merge(test[, c("sample_id", "user_id", "artist_id")], user_top_artists, sort = F, all.x = T)$is_listened
  test$user_top_genre  <- merge(test[, c("sample_id", "user_id", "genre_id")],  user_top_genres,  sort = F, all.x = T)$is_listened  
  test$user_bad_song   <- merge(test[, c("sample_id", "user_id", "media_id")],  user_bad_songs,   sort = F, all.x = T)$is_skipped
  test$user_bad_album  <- merge(test[, c("sample_id", "user_id", "album_id")],  user_bad_albums,  sort = F, all.x = T)$is_skipped
  test$user_bad_artist <- merge(test[, c("sample_id", "user_id", "artist_id")], user_bad_artists, sort = F, all.x = T)$is_skipped
  test$user_bad_genre  <- merge(test[, c("sample_id", "user_id", "genre_id")],  user_bad_genres,  sort = F, all.x = T)$is_skipped  
  
  # creating dummies for new media
  test$user_new_song   <- as.factor(is.na(test$user_song_plays))
  test$user_new_album  <- as.factor(is.na(test$user_album_plays))
  test$user_new_artist <- as.factor(is.na(test$user_artist_plays))
  test$user_new_genre  <- as.factor(is.na(test$user_genre_plays))
  
  # converting TOP and BAD variables to dummies
  test$user_top_song   <- as.factor(!is.na(test$user_top_song))
  test$user_top_album  <- as.factor(!is.na(test$user_top_album))
  test$user_top_artist <- as.factor(!is.na(test$user_top_artist))
  test$user_top_genre  <- as.factor(!is.na(test$user_top_genre))
  test$user_bad_song   <- as.factor(!is.na(test$user_bad_song))
  test$user_bad_album  <- as.factor(!is.na(test$user_bad_album))
  test$user_bad_artist <- as.factor(!is.na(test$user_bad_artist))
  test$user_bad_genre  <- as.factor(!is.na(test$user_bad_genre))
  
  # imputing NAs for play and skip counts with true zeroes
  test$user_song_plays[is.na(test$user_song_plays)]       <- 0
  test$user_album_plays[is.na(test$user_album_plays)]     <- 0
  test$user_artist_plays[is.na(test$user_artist_plays)]   <- 0
  test$user_genre_plays[is.na(test$user_genre_plays)]     <- 0
  test$user_song_skips[is.na(test$user_song_skips)]       <- 0
  test$user_album_skips[is.na(test$user_album_skips)]     <- 0
  test$user_artist_skips[is.na(test$user_artist_skips)]   <- 0
  test$user_genre_skips[is.na(test$user_genre_skips)]     <- 0
  test$total_song_plays[is.na(test$total_song_plays)]     <- 0
  test$total_album_plays[is.na(test$total_album_plays)]   <- 0
  test$total_artist_plays[is.na(test$total_artist_plays)] <- 0
  test$total_genre_plays[is.na(test$total_genre_plays)]   <- 0
  test$total_song_skips[is.na(test$total_song_skips)]     <- 0
  test$total_album_skips[is.na(test$total_album_skips)]   <- 0
  test$total_artist_skips[is.na(test$total_artist_skips)] <- 0
  test$total_genre_skips[is.na(test$total_genre_skips)]   <- 0
  
  # checking the data
  summary(test)
  
  # returning the data
  return(test)
}