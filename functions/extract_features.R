##### function to extract features
extract_features <- function(data) {
  
  # extracting user IDs
  users <- levels(data$user_id)
  
  # creating a data frame
  user.data <- data.frame()
  
  # feature creation loop
  for (id in users) {
    
    # displaying user ID
    print(paste0(id, "/", length(users)))
    
    # extracting musical history
    history <- data[data$user_id == id, ]
    
    # counting play/skip ratio
    play_skip <- mean(history$is_listened)
    
    # counting total songs
    songs <- nrow(history)
    
    # counting genre shares
    genres <- t(as.matrix(table(history$genre_id)/nrow(history)))
    colnames(genres) <- paste0("genre_", colnames(genres))

    # counting artist shares
    artists <- t(as.matrix(table(history$artist_id)/nrow(history)))
    colnames(artists) <- paste0("artist_", colnames(artists))
    
    # age and gender
    age    <- unique(history$user_age)
    gender <- unique(history$user_gender)
  
    # building the data frame
    new.data <- data.frame(user_id = id, age = age, gender = gender, play_skip = play_skip, songs = songs)
    new.data <- cbind(new.data, genres, artists)

    # merging with previous users
    user.data <- rbind(user.data, new.data)
  }

  # converting factors
  user.data$user_id <- as.factor(user.data$user_id)
  user.data$gender  <- as.factor(user.data$gender)
  
  # returning the data
  return(user.data)
}