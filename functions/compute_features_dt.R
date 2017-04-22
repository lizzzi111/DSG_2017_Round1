##### function to extract features
compute_features <- function(train, test) {
  #load(file.path(data.folder, "data_train.Rda"))
  #load(file.path(data.folder, "data_test.Rda"))
  # train <- data.train
  # test <- data.test
  #### 1. CONVERT TO DATA.TABLE
  library(data.table)
  train_dt <- as.data.table(train)
  test_dt <- as.data.table(test)
  
  ##### 2. PREPARATION
  
  # converting to numeric
  train_dt$is_listened <- as.numeric(train_dt$is_listened) - 1
  train_dt$is_skipped  <- 1- train_dt$is_listened
  
  ##### 3. FEATURES ON USER LEVEL
  
  # computing historical play/skip ratios
  ratio_per_user <- train_dt[, .(is_listened=mean(is_listened)), by = user_id]
  ratio_per_song <- train_dt[,.(is_listened=mean(is_listened)), by = media_id]
  ratio_per_album <- train_dt[,.(is_listened=mean(is_listened)), by = album_id]
  ratio_per_artist <- train_dt[,.(is_listened=mean(is_listened)), by = artist_id]
  ratio_per_genre <- train_dt[,.(is_listened=mean(is_listened)), by = genre_id]

  # computing user-specific play counts
  user_plays_song <- train_dt[,.(is_listened=sum(is_listened)), by = list(user_id,media_id)]
  user_plays_album <- train_dt[,.(is_listened=sum(is_listened)), by = list(user_id,album_id)]
  user_plays_artist <- train_dt[,.(is_listened=sum(is_listened)), by = list(user_id,artist_id)]
  user_plays_genre <- train_dt[,.(is_listened=sum(is_listened)), by = list(user_id,genre_id)]
  
  # computing user-specific skip counts
  user_skips_song <- train_dt[,.(is_skipped=sum(is_skipped)), by = list(user_id,media_id)]
  user_skips_album <- train_dt[,.(is_skipped=sum(is_skipped)), by = list(user_id,album_id)]
  user_skips_artist <-train_dt[,.(is_skipped=sum(is_skipped)), by = list(user_id,artist_id)]
  user_skips_genre <- train_dt[,.(is_skipped=sum(is_skipped)), by = list(user_id,genre_id)]
  
  # computing user-specific top plays [1/2]
  user_top_songs <- user_plays_song[,.(is_listened=max(is_listened)), by = list(user_id)]
  user_top_albums <- user_plays_album[,.(is_listened=max(is_listened)), by = list(user_id)]
  user_top_artists <- user_plays_artist[,.(is_listened=max(is_listened)), by = list(user_id)]
  user_top_genres <- user_plays_genre[,.(is_listened=max(is_listened)), by = list(user_id)]

  # computing user-specific top plays [2/2]
  user_top_songs   <- merge(user_top_songs,   user_plays_song,   sort = F, all.x = T)
  user_top_albums  <- merge(user_top_albums,  user_plays_album,  sort = F, all.x = T)
  user_top_artists <- merge(user_top_artists, user_plays_artist, sort = F, all.x = T)
  user_top_genres  <- merge(user_top_genres,  user_plays_genre,  sort = F, all.x = T)
  
  # computing user-specific bad skips [1/2]
  user_bad_songs <- user_skips_song[,.(is_skipped=max(is_skipped)), by = list(user_id)]
  user_bad_albums <- user_skips_album[,.(is_skipped=max(is_skipped)), by = list(user_id)]
  user_bad_artists <- user_skips_artist[,.(is_skipped=max(is_skipped)), by = list(user_id)]
  user_bad_genres <- user_skips_genre[,.(is_skipped=max(is_skipped)), by = list(user_id)]
  
  # computing user-specific bad skips [2/2]
  user_bad_songs   <- merge(user_bad_songs,   user_skips_song,   sort = F, all.x = T)
  user_bad_albums  <- merge(user_bad_albums,  user_skips_album,  sort = F, all.x = T)
  user_bad_artists <- merge(user_bad_artists, user_skips_artist, sort = F, all.x = T)
  user_bad_genres  <- merge(user_bad_genres,  user_skips_genre,  sort = F, all.x = T)
  
  # computing previous song listened
  previous_media <- train_dt[order(ts_listen),.(is_listened, ts_listen, media_id,previous_media_listened=shift(is_listened)), by=user_id]
  previous_media[is.na(previous_media_listened), previous_media_listened:=0]
  previous_media[,previous_media_listened:=as.factor(previous_media_listened)]
  ##### 3. FEATURES ON AGGREGATE LEVEL
  
  # computing aggregate play counts 
  total_plays_song   <- train_dt[,.(is_listened=sum(is_listened)), by = media_id]
  total_plays_album  <- train_dt[,.(is_listened=sum(is_listened)), by = album_id]
  total_plays_artist <- train_dt[,.(is_listened=sum(is_listened)), by = artist_id]
  total_plays_genre  <- train_dt[,.(is_listened=sum(is_listened)), by = genre_id]

  # computing aggregate skip counts 
  total_skips_song   <- train_dt[,.(is_skipped=sum(is_skipped)), by = media_id]
  total_skips_album  <- train_dt[,.(is_skipped=sum(is_skipped)), by = album_id]
  total_skips_artist <- train_dt[,.(is_skipped=sum(is_skipped)), by = artist_id]
  total_skips_genre  <- train_dt[,.(is_skipped=sum(is_skipped)), by = genre_id]
  
  
  ##### 4. SAVING FEATURES TO DATA
  
  # saving play/skip ratios
  test_dt$ratio_per_user   <- merge(test_dt[, c("sample_id", "user_id")],   ratio_per_user,   sort = F, all.x = T)$is_listened
  test_dt$ratio_per_song   <- merge(test_dt[, c("sample_id", "media_id")],  ratio_per_song,   sort = F, all.x = T)$is_listened
  test_dt$ratio_per_album  <- merge(test_dt[, c("sample_id", "album_id")],  ratio_per_album,  sort = F, all.x = T)$is_listened
  test_dt$ratio_per_artist <- merge(test_dt[, c("sample_id", "artist_id")], ratio_per_artist, sort = F, all.x = T)$is_listened
  test_dt$ratio_per_genre  <- merge(test_dt[, c("sample_id", "genre_id")],  ratio_per_genre,  sort = F, all.x = T)$is_listened
  
  # saving play and skip counts: user level
  test_dt$user_song_plays   <- merge(test_dt[, c("sample_id", "user_id", "media_id")],  user_plays_song,   sort = F, all.x = T)$is_listened
  test_dt$user_album_plays  <- merge(test_dt[, c("sample_id", "user_id", "album_id")],  user_plays_album,  sort = F, all.x = T)$is_listened
  test_dt$user_artist_plays <- merge(test_dt[, c("sample_id", "user_id", "artist_id")], user_plays_artist, sort = F, all.x = T)$is_listened
  test_dt$user_genre_plays  <- merge(test_dt[, c("sample_id", "user_id", "genre_id")],  user_plays_genre,  sort = F, all.x = T)$is_listened
  test_dt$user_song_skips   <- merge(test_dt[, c("sample_id", "user_id", "media_id")],  user_skips_song,   sort = F, all.x = T)$is_skipped
  test_dt$user_album_skips  <- merge(test_dt[, c("sample_id", "user_id", "album_id")],  user_skips_album,  sort = F, all.x = T)$is_skipped
  test_dt$user_artist_skips <- merge(test_dt[, c("sample_id", "user_id", "artist_id")], user_skips_artist, sort = F, all.x = T)$is_skipped
  test_dt$user_genre_skips  <- merge(test_dt[, c("sample_id", "user_id", "genre_id")],  user_skips_genre,  sort = F, all.x = T)$is_skipped
  
  # saving play and skip counts: aggregate level
  test_dt$total_song_plays   <- merge(test_dt[, c("sample_id", "user_id", "media_id")],  total_plays_song,   sort = F, all.x = T)$is_listened
  test_dt$total_album_plays  <- merge(test_dt[, c("sample_id", "user_id", "album_id")],  total_plays_album,  sort = F, all.x = T)$is_listened
  test_dt$total_artist_plays <- merge(test_dt[, c("sample_id", "user_id", "artist_id")], total_plays_artist, sort = F, all.x = T)$is_listened
  test_dt$total_genre_plays  <- merge(test_dt[, c("sample_id", "user_id", "genre_id")],  total_plays_genre,  sort = F, all.x = T)$is_listened
  test_dt$total_song_skips   <- merge(test_dt[, c("sample_id", "user_id", "media_id")],  total_skips_song,   sort = F, all.x = T)$is_skipped
  test_dt$total_album_skips  <- merge(test_dt[, c("sample_id", "user_id", "album_id")],  total_skips_album,  sort = F, all.x = T)$is_skipped
  test_dt$total_artist_skips <- merge(test_dt[, c("sample_id", "user_id", "artist_id")], total_skips_artist, sort = F, all.x = T)$is_skipped
  test_dt$total_genre_skips  <- merge(test_dt[, c("sample_id", "user_id", "genre_id")],  total_skips_genre,  sort = F, all.x = T)$is_skipped  
  
  # saving TOP and BAD: user level
  test_dt$user_top_song   <- merge(test_dt[, c("sample_id", "user_id", "media_id")],  user_top_songs,   sort = F, all.x = T)$is_listened
  test_dt$user_top_album  <- merge(test_dt[, c("sample_id", "user_id", "album_id")],  user_top_albums,  sort = F, all.x = T)$is_listened
  test_dt$user_top_artist <- merge(test_dt[, c("sample_id", "user_id", "artist_id")], user_top_artists, sort = F, all.x = T)$is_listened
  test_dt$user_top_genre  <- merge(test_dt[, c("sample_id", "user_id", "genre_id")],  user_top_genres,  sort = F, all.x = T)$is_listened  
  test_dt$user_bad_song   <- merge(test_dt[, c("sample_id", "user_id", "media_id")],  user_bad_songs,   sort = F, all.x = T)$is_skipped
  test_dt$user_bad_album  <- merge(test_dt[, c("sample_id", "user_id", "album_id")],  user_bad_albums,  sort = F, all.x = T)$is_skipped
  test_dt$user_bad_artist <- merge(test_dt[, c("sample_id", "user_id", "artist_id")], user_bad_artists, sort = F, all.x = T)$is_skipped
  test_dt$user_bad_genre  <- merge(test_dt[, c("sample_id", "user_id", "genre_id")],  user_bad_genres,  sort = F, all.x = T)$is_skipped  
  
  # saving previous media listened
  #test_dt$previous_media_listened <- merge(test_dt[, .(user_id,media_id,ts_listen)], previous_media[,.(user_id,ts_listen,media_id,previous_media_listened)], sort = F, all.x = T)
  # creating dummies for new media
  test_dt$user_new_song   <- as.factor(is.na(test_dt$user_song_plays))
  test_dt$user_new_album  <- as.factor(is.na(test_dt$user_album_plays))
  test_dt$user_new_artist <- as.factor(is.na(test_dt$user_artist_plays))
  test_dt$user_new_genre  <- as.factor(is.na(test_dt$user_genre_plays))
  
  # converting TOP and BAD variables to dummies
  test_dt$user_top_song   <- as.factor(!is.na(test_dt$user_top_song))
  test_dt$user_top_album  <- as.factor(!is.na(test_dt$user_top_album))
  test_dt$user_top_artist <- as.factor(!is.na(test_dt$user_top_artist))
  test_dt$user_top_genre  <- as.factor(!is.na(test_dt$user_top_genre))
  test_dt$user_bad_song   <- as.factor(!is.na(test_dt$user_bad_song))
  test_dt$user_bad_album  <- as.factor(!is.na(test_dt$user_bad_album))
  test_dt$user_bad_artist <- as.factor(!is.na(test_dt$user_bad_artist))
  test_dt$user_bad_genre  <- as.factor(!is.na(test_dt$user_bad_genre))
  
  # imputing NAs for play and skip counts with true zeroes
  test_dt$user_song_plays[is.na(test_dt$user_song_plays)]       <- 0
  test_dt$user_album_plays[is.na(test_dt$user_album_plays)]     <- 0
  test_dt$user_artist_plays[is.na(test_dt$user_artist_plays)]   <- 0
  test_dt$user_genre_plays[is.na(test_dt$user_genre_plays)]     <- 0
  test_dt$user_song_skips[is.na(test_dt$user_song_skips)]       <- 0
  test_dt$user_album_skips[is.na(test_dt$user_album_skips)]     <- 0
  test_dt$user_artist_skips[is.na(test_dt$user_artist_skips)]   <- 0
  test_dt$user_genre_skips[is.na(test_dt$user_genre_skips)]     <- 0
  test_dt$total_song_plays[is.na(test_dt$total_song_plays)]     <- 0
  test_dt$total_album_plays[is.na(test_dt$total_album_plays)]   <- 0
  test_dt$total_artist_plays[is.na(test_dt$total_artist_plays)] <- 0
  test_dt$total_genre_plays[is.na(test_dt$total_genre_plays)]   <- 0
  test_dt$total_song_skips[is.na(test_dt$total_song_skips)]     <- 0
  test_dt$total_album_skips[is.na(test_dt$total_album_skips)]   <- 0
  test_dt$total_artist_skips[is.na(test_dt$total_artist_skips)] <- 0
  test_dt$total_genre_skips[is.na(test_dt$total_genre_skips)]   <- 0
  
  # checking the data
  summary(test_dt)
  
  # converting back to data.frame
  test <- as.data.frame(test_dt)
  # returning the data
  return(test)
}
