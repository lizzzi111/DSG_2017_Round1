##### function to extract features
compute_features_dt <- function(train, test) {
  
  # convert to data table
  train_dt <- as.data.table(train)
  test_dt  <- as.data.table(test)
  
  # convert to numeric
  train_dt$is_listened <- as.numeric(train_dt$is_listened) - 1
  train_dt$is_skipped  <- 1- train_dt$is_listened
  
  # computing historical play/skip ratios
  ratio_per_user <- train_dt[, .(is_listened=mean(is_listened))]

  
  ##### FEATURES ON USER LEVEL
  
  # computing user-specific play counts
  user_plays_song   <- train_dt[,.(is_listened=sum(is_listened)), by = list(media_id)]
  user_plays_album  <- train_dt[,.(is_listened=sum(is_listened)), by = list(album_id)]
  user_plays_artist <- train_dt[,.(is_listened=sum(is_listened)), by = list(artist_id)]
  user_plays_genre  <- train_dt[,.(is_listened=sum(is_listened)), by = list(genre_id)]
  
  # computing user-specific skip counts
  user_skips_song   <- train_dt[,.(is_skipped=sum(is_skipped)),  by = list(media_id)]
  user_skips_album  <- train_dt[,.(is_skipped=sum(is_skipped)), by = list(album_id)]
  user_skips_artist <-train_dt[,.(is_skipped=sum(is_skipped)), by = list(artist_id)]
  user_skips_genre  <- train_dt[,.(is_skipped=sum(is_skipped)), by = list(genre_id)]
  
  
  ##### SAVING FEATURES TO DATA
  
  # saving play/skip ratios
  test_dt$ratio_per_user <- merge(test_dt[, c("sample_id")], ratio_per_user, sort = F, all.x = T)$is_listened
  
  # saving play and skip counts: user level
  test_dt$user_song_plays   <- merge(test_dt[, c("sample_id", "user_id", "media_id")],  user_plays_song,   sort = F, all.x = T)$is_listened
  test_dt$user_album_plays  <- merge(test_dt[, c("sample_id", "user_id", "album_id")],  user_plays_album,  sort = F, all.x = T)$is_listened
  test_dt$user_artist_plays <- merge(test_dt[, c("sample_id", "user_id", "artist_id")], user_plays_artist, sort = F, all.x = T)$is_listened
  test_dt$user_genre_plays  <- merge(test_dt[, c("sample_id", "user_id", "genre_id")],  user_plays_genre,  sort = F, all.x = T)$is_listened
  test_dt$user_song_skips   <- merge(test_dt[, c("sample_id", "user_id", "media_id")],  user_skips_song,   sort = F, all.x = T)$is_skipped
  test_dt$user_album_skips  <- merge(test_dt[, c("sample_id", "user_id", "album_id")],  user_skips_album,  sort = F, all.x = T)$is_skipped
  test_dt$user_artist_skips <- merge(test_dt[, c("sample_id", "user_id", "artist_id")], user_skips_artist, sort = F, all.x = T)$is_skipped
  test_dt$user_genre_skips  <- merge(test_dt[, c("sample_id", "user_id", "genre_id")],  user_skips_genre,  sort = F, all.x = T)$is_skipped
  
  # imputing NAs for play and skip counts with true zeroes
  test_dt$user_song_plays[is.na(test_dt$user_song_plays)]       <- 0
  test_dt$user_album_plays[is.na(test_dt$user_album_plays)]     <- 0
  test_dt$user_artist_plays[is.na(test_dt$user_artist_plays)]   <- 0
  test_dt$user_genre_plays[is.na(test_dt$user_genre_plays)]     <- 0
  test_dt$user_song_skips[is.na(test_dt$user_song_skips)]       <- 0
  test_dt$user_album_skips[is.na(test_dt$user_album_skips)]     <- 0
  test_dt$user_artist_skips[is.na(test_dt$user_artist_skips)]   <- 0
  test_dt$user_genre_skips[is.na(test_dt$user_genre_skips)]     <- 0
  
  # converting to data frame
  test <- as.data.frame(test_dt)
  
  # returning the data
  return(test)
}