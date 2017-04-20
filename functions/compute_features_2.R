##### function to extract features
compute_features_2 <- function(train, test) {
  
  ##### 1. PREPARATION
  
  # converting to numeric
  train$is_listened <- as.numeric(train$is_listened) - 1
  train$is_skipped  <- 1- train$is_listened
  
  # separating "Flow" data and "Normal" data 
  train.flow <- train[train$listen_type == 1, ]
  train.norm <- train[train$listen_type == 0, ]
  
  
  ##### 2. FEATURES ON USER LEVEL
  
  ##### 2.1. FULL DATA
  ratio_per_user <- aggregate(is_listened ~ user_id, train, mean)
  
  
  ##### 2.2. "NORMAL" DATA

  # computing user-specific play counts on "Normal" data 
  user_plays_song_norm   <- aggregate(is_listened ~ user_id + media_id,  train.norm, sum)
  user_plays_album_norm  <- aggregate(is_listened ~ user_id + album_id,  train.norm, sum)
  user_plays_artist_norm <- aggregate(is_listened ~ user_id + artist_id, train.norm, sum)
  user_plays_genre_norm  <- aggregate(is_listened ~ user_id + genre_id,  train.norm, sum)
  
  # computing user-specific skip counts on "Normal" data 
  user_skips_song_norm   <- aggregate(is_skipped ~ user_id + media_id,  train.norm, sum)
  user_skips_album_norm  <- aggregate(is_skipped ~ user_id + album_id,  train.norm, sum)
  user_skips_artist_norm <- aggregate(is_skipped ~ user_id + artist_id, train.norm, sum)
  user_skips_genre_norm  <- aggregate(is_skipped ~ user_id + genre_id,  train.norm, sum)
  
  
  ##### 2.3. "FLOW" DATA
  
  # computing user-specific play counts on "Flow" data 
  user_plays_song_flow   <- aggregate(is_listened ~ user_id + media_id,  train.flow, sum)
  user_plays_album_flow  <- aggregate(is_listened ~ user_id + album_id,  train.flow, sum)
  user_plays_artist_flow <- aggregate(is_listened ~ user_id + artist_id, train.flow, sum)
  user_plays_genre_flow  <- aggregate(is_listened ~ user_id + genre_id,  train.flow, sum)
  
  # computing user-specific skip counts on "Flow" data 
  user_skips_song_flow   <- aggregate(is_skipped ~ user_id + media_id,  train.flow, sum)
  user_skips_album_flow  <- aggregate(is_skipped ~ user_id + album_id,  train.flow, sum)
  user_skips_artist_flow <- aggregate(is_skipped ~ user_id + artist_id, train.flow, sum)
  user_skips_genre_flow  <- aggregate(is_skipped ~ user_id + genre_id,  train.flow, sum)
  
  
  ##### 3. FEATURES ON AGGREGATE LEVEL
  
  ##### 3.1. "NORMAL" DATA
  
  # computing aggregate play counts on "Normal" data
  total_plays_song_norm   <- aggregate(is_listened ~ media_id,  train.norm, sum)
  total_plays_album_norm  <- aggregate(is_listened ~ album_id,  train.norm, sum)
  total_plays_artist_norm <- aggregate(is_listened ~ artist_id, train.norm, sum)
  total_plays_genre_norm  <- aggregate(is_listened ~ genre_id,  train.norm, sum)
  
  # computing aggregate skip counts on "Normal" data
  total_skips_song_norm   <- aggregate(is_skipped ~ media_id,  train.norm, sum)
  total_skips_album_norm  <- aggregate(is_skipped ~ album_id,  train.norm, sum)
  total_skips_artist_norm <- aggregate(is_skipped ~ artist_id, train.norm, sum)
  total_skips_genre_norm  <- aggregate(is_skipped ~ genre_id,  train.norm, sum)
  

  ##### 3.2. "FLOW" DATA
  
  # computing aggregate play counts on "Flow" data
  total_plays_song_flow   <- aggregate(is_listened ~ media_id,  train.flow, sum)
  total_plays_album_flow  <- aggregate(is_listened ~ album_id,  train.flow, sum)
  total_plays_artist_flow <- aggregate(is_listened ~ artist_id, train.flow, sum)
  total_plays_genre_flow  <- aggregate(is_listened ~ genre_id,  train.flow, sum)
  
  # computing aggregate skip counts on "Flow" data
  total_skips_song_flow   <- aggregate(is_skipped ~ media_id,  train.flow, sum)
  total_skips_album_flow  <- aggregate(is_skipped ~ album_id,  train.flow, sum)
  total_skips_artist_flow <- aggregate(is_skipped ~ artist_id, train.flow, sum)
  total_skips_genre_flow  <- aggregate(is_skipped ~ genre_id,  train.flow, sum)
  
  
  ##### 4. SAVING FEATURES TO DATA
  
  ##### 4.1. FULL DATA
  
  # saving play/skip ratios
  test$ratio_per_user <- merge(test[, c("sample_id", "user_id")], ratio_per_user, sort = F, all.x = T)$is_listened

  
  ##### 4.2. "NORMAL" DATA

  # saving play and skip counts: user level
  test$user_song_plays_norm   <- merge(test[, c("sample_id", "user_id", "media_id")],  user_plays_song_norm,   sort = F, all.x = T)$is_listened
  test$user_album_plays_norm  <- merge(test[, c("sample_id", "user_id", "album_id")],  user_plays_album_norm,  sort = F, all.x = T)$is_listened
  test$user_artist_plays_norm <- merge(test[, c("sample_id", "user_id", "artist_id")], user_plays_artist_norm, sort = F, all.x = T)$is_listened
  test$user_genre_plays_norm  <- merge(test[, c("sample_id", "user_id", "genre_id")],  user_plays_genre_norm,  sort = F, all.x = T)$is_listened
  test$user_song_skips_norm   <- merge(test[, c("sample_id", "user_id", "media_id")],  user_skips_song_norm,   sort = F, all.x = T)$is_skipped
  test$user_album_skips_norm  <- merge(test[, c("sample_id", "user_id", "album_id")],  user_skips_album_norm,  sort = F, all.x = T)$is_skipped
  test$user_artist_skips_norm <- merge(test[, c("sample_id", "user_id", "artist_id")], user_skips_artist_norm, sort = F, all.x = T)$is_skipped
  test$user_genre_skips_norm  <- merge(test[, c("sample_id", "user_id", "genre_id")],  user_skips_genre_norm,  sort = F, all.x = T)$is_skipped
  
  # saving play and skip counts: aggregate level
  test$total_song_plays_norm   <- merge(test[, c("sample_id", "user_id", "media_id")],  total_plays_song_norm,   sort = F, all.x = T)$is_listened
  test$total_album_plays_norm  <- merge(test[, c("sample_id", "user_id", "album_id")],  total_plays_album_norm,  sort = F, all.x = T)$is_listened
  test$total_artist_plays_norm <- merge(test[, c("sample_id", "user_id", "artist_id")], total_plays_artist_norm, sort = F, all.x = T)$is_listened
  test$total_genre_plays_norm  <- merge(test[, c("sample_id", "user_id", "genre_id")],  total_plays_genre_norm,  sort = F, all.x = T)$is_listened
  test$total_song_skips_norm   <- merge(test[, c("sample_id", "user_id", "media_id")],  total_skips_song_norm,   sort = F, all.x = T)$is_skipped
  test$total_album_skips_norm  <- merge(test[, c("sample_id", "user_id", "album_id")],  total_skips_album_norm,  sort = F, all.x = T)$is_skipped
  test$total_artist_skips_norm <- merge(test[, c("sample_id", "user_id", "artist_id")], total_skips_artist_norm, sort = F, all.x = T)$is_skipped
  test$total_genre_skips_norm  <- merge(test[, c("sample_id", "user_id", "genre_id")],  total_skips_genre_norm,  sort = F, all.x = T)$is_skipped  
  
  # imputing NAs for play and skip counts with true zeroes
  test$user_song_plays_norm[is.na(test$user_song_plays_norm)]       <- 0
  test$user_album_plays_norm[is.na(test$user_album_plays_norm)]     <- 0
  test$user_artist_plays_norm[is.na(test$user_artist_plays_norm)]   <- 0
  test$user_genre_plays_norm[is.na(test$user_genre_plays_norm)]     <- 0
  test$user_song_skips_norm[is.na(test$user_song_skips_norm)]       <- 0
  test$user_album_skips_norm[is.na(test$user_album_skips_norm)]     <- 0
  test$user_artist_skips_norm[is.na(test$user_artist_skips_norm)]   <- 0
  test$user_genre_skips_norm[is.na(test$user_genre_skips_norm)]     <- 0
  test$total_song_plays_norm[is.na(test$total_song_plays_norm)]     <- 0
  test$total_album_plays_norm[is.na(test$total_album_plays_norm)]   <- 0
  test$total_artist_plays_norm[is.na(test$total_artist_plays_norm)] <- 0
  test$total_genre_plays_norm[is.na(test$total_genre_plays_norm)]   <- 0
  test$total_song_skips_norm[is.na(test$total_song_skips_norm)]     <- 0
  test$total_album_skips_norm[is.na(test$total_album_skips_norm)]   <- 0
  test$total_artist_skips_norm[is.na(test$total_artist_skips_norm)] <- 0
  test$total_genre_skips_norm[is.na(test$total_genre_skips_norm)]   <- 0
  
  
  ##### 4.3. "FLOW" DATA
  
  # saving play and skip counts: user level
  test$user_song_plays_flow   <- merge(test[, c("sample_id", "user_id", "media_id")],  user_plays_song_flow,   sort = F, all.x = T)$is_listened
  test$user_album_plays_flow  <- merge(test[, c("sample_id", "user_id", "album_id")],  user_plays_album_flow,  sort = F, all.x = T)$is_listened
  test$user_artist_plays_flow <- merge(test[, c("sample_id", "user_id", "artist_id")], user_plays_artist_flow, sort = F, all.x = T)$is_listened
  test$user_genre_plays_flow  <- merge(test[, c("sample_id", "user_id", "genre_id")],  user_plays_genre_flow,  sort = F, all.x = T)$is_listened
  test$user_song_skips_flow   <- merge(test[, c("sample_id", "user_id", "media_id")],  user_skips_song_flow,   sort = F, all.x = T)$is_skipped
  test$user_album_skips_flow  <- merge(test[, c("sample_id", "user_id", "album_id")],  user_skips_album_flow,  sort = F, all.x = T)$is_skipped
  test$user_artist_skips_flow <- merge(test[, c("sample_id", "user_id", "artist_id")], user_skips_artist_flow, sort = F, all.x = T)$is_skipped
  test$user_genre_skips_flow  <- merge(test[, c("sample_id", "user_id", "genre_id")],  user_skips_genre_flow,  sort = F, all.x = T)$is_skipped
  
  # saving play and skip counts: aggregate level
  test$total_song_plays_flow   <- merge(test[, c("sample_id", "user_id", "media_id")],  total_plays_song_flow,   sort = F, all.x = T)$is_listened
  test$total_album_plays_flow  <- merge(test[, c("sample_id", "user_id", "album_id")],  total_plays_album_flow,  sort = F, all.x = T)$is_listened
  test$total_artist_plays_flow <- merge(test[, c("sample_id", "user_id", "artist_id")], total_plays_artist_flow, sort = F, all.x = T)$is_listened
  test$total_genre_plays_flow  <- merge(test[, c("sample_id", "user_id", "genre_id")],  total_plays_genre_flow,  sort = F, all.x = T)$is_listened
  test$total_song_skips_flow   <- merge(test[, c("sample_id", "user_id", "media_id")],  total_skips_song_flow,   sort = F, all.x = T)$is_skipped
  test$total_album_skips_flow  <- merge(test[, c("sample_id", "user_id", "album_id")],  total_skips_album_flow,  sort = F, all.x = T)$is_skipped
  test$total_artist_skips_flow <- merge(test[, c("sample_id", "user_id", "artist_id")], total_skips_artist_flow, sort = F, all.x = T)$is_skipped
  test$total_genre_skips_flow  <- merge(test[, c("sample_id", "user_id", "genre_id")],  total_skips_genre_flow,  sort = F, all.x = T)$is_skipped  
  
  # imputing NAs for play and skip counts with true zeroes
  test$user_song_plays_flow[is.na(test$user_song_plays_flow)]       <- 0
  test$user_album_plays_flow[is.na(test$user_album_plays_flow)]     <- 0
  test$user_artist_plays_flow[is.na(test$user_artist_plays_flow)]   <- 0
  test$user_genre_plays_flow[is.na(test$user_genre_plays_flow)]     <- 0
  test$user_song_skips_flow[is.na(test$user_song_skips_flow)]       <- 0
  test$user_album_skips_flow[is.na(test$user_album_skips_flow)]     <- 0
  test$user_artist_skips_flow[is.na(test$user_artist_skips_flow)]   <- 0
  test$user_genre_skips_flow[is.na(test$user_genre_skips_flow)]     <- 0
  test$total_song_plays_flow[is.na(test$total_song_plays_flow)]     <- 0
  test$total_album_plays_flow[is.na(test$total_album_plays_flow)]   <- 0
  test$total_artist_plays_flow[is.na(test$total_artist_plays_flow)] <- 0
  test$total_genre_plays_flow[is.na(test$total_genre_plays_flow)]   <- 0
  test$total_song_skips_flow[is.na(test$total_song_skips_flow)]     <- 0
  test$total_album_skips_flow[is.na(test$total_album_skips_flow)]   <- 0
  test$total_artist_skips_flow[is.na(test$total_artist_skips_flow)] <- 0
  test$total_genre_skips_flow[is.na(test$total_genre_skips_flow)]   <- 0
  
  # checking the data
  summary(test)
  
  # returning the data
  return(test)
}