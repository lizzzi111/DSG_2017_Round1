###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting work directory
# work.folder <- "N:/DSG2017/DSG_2017/"
#work.folder <- "/Users/Kozodoi/Documents/Competitions/DSG_2017"
#setwd(work.folder)

# setting inner folders
code.folder <- "codes"
data.folder <- "data"
func.folder <- "functions"
subm.folder <- "submissions"

# loading libraries
if(require(pacman)==FALSE) install.packages("pacman")
library(pacman)
p_load(data.table, anytime)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))



###################################
#                                 #
#         DATA PREPARATION        #
#                                 #
###################################

########## 1. LOADING THE DATA ####

# loading training data
data.train <- fread(file.path(data.folder, "train.csv"), sep = ",", dec = ".", header = T)

# loading testing data
data.test <- fread(file.path(data.folder, "test.csv"), sep = ",", dec = ".", header = T)

# merging data sets
data.test$is_listened <- NA
data.train$sample_id  <- NA
data.test$dataset <- "unknown"
data.train$dataset <- "train"
data.full <- rbind(data.train, data.test)
# Assign a unique index to every observation that we have
data.full[, row_index := 1:nrow(data.full)]

setkey(data.full, user_id, media_id)
rm(list = c("data.test",  "data.train"))



########## 2. ADDING API DATA

# adding data on songs
api.data <- fread(file.path(data.folder, "api_final.txt"), sep = ",", dec = ".", header = T)
api.data[fans == -1, fans := 0]
api.data <- api.data[!duplicated(api.data),]
colnames(api.data) <- c("media_id", "song_rank", "song_bpm", "song_position", "song_lyrics_explicit", "song_gain", "album_id", "album_fans")
data.full <- merge(data.full, api.data[,.(media_id, song_rank, song_bpm, song_position, song_lyrics_explicit, song_gain, album_fans)], 
                   by = "media_id", all.x = T, all.y = F)
# A good improvement would be to impute based on median in genre_id
for (var in c("song_rank", "song_bpm", "song_position", "song_lyrics_explicit", "song_gain", "album_fans")) {                                                                                                                                            
  set(data.full, which(is.na(data.full[[var]])), var, median(data.full[[var]], na.rm=T))                                                                                              
} 

## adding data on artists
artist.data <- fread(file.path(data.folder, "user_artists.txt"), sep = ",", dec = ".", header = T)
artist.data[, favorite_artist := 1]
data.full <- merge(data.full, artist.data[, .(user_id, artist_id, favorite_artist)],
                   by = c("user_id", "artist_id"), all.x = TRUE, all.y = FALSE)
data.full[is.na(favorite_artist), favorite_artist := 0]

## adding data on albums
album.data  <- fread(file.path(data.folder, "user_favourite_albums.txt"), sep = ",", dec = ".", header = T)
album.data[, favorite_album := 1]
data.full <- merge(data.full, album.data[, .(user_id, album_id, favorite_album)],
                   by = c("user_id", "album_id"), all.x = TRUE, all.y = FALSE)
data.full[is.na(favorite_album), favorite_album := 0]

# adding data on radio/categories
radio.data <- fread(file.path(data.folder, "user_radio.txt"), sep = ",", dec = ".", header = T)
#radio.data[, radio := factor(make.names(radio))]
#radio.data <- cbind(radio.data$user_id, model.matrix(~-1 radio, data = radio.data))
radio.data[, radio_selecter := .N, by = user_id]
data.full <- merge(data.full, radio.data[!duplicated(user_id), .(user_id, radio_selecter)],
                   by = c("user_id"), all.x = TRUE, all.y = FALSE)
data.full[is.na(radio_selecter), radio_selecter := 0]

# removing API data from memory
rm(list = c("api.data", "artist.data", "album.data", "radio.data"))



########## 3. CONVERTING VARIABLES ####

# converting factors
#temp <- c("genre_id", "media_id", "album_id", "user_id", "artist_id", "context_type", "platform_name", "platform_family")
#data.full[, (temp) := lapply(.SD, factor), .SDcols = temp]

# converting timestamps
data.full[, release_date := as.Date(as.character(data.full$release_date), "%Y%m%d")]
data.full[, ts_listen := anytime(data.full$ts_listen, asUTC = T)]



########## 4. CREATING FEATURES ON FULL DATA ####

### add data from json file, info on song name, album name, artist name
#if(file.exists(file.path(data.folder, "info_json.rds"))){
#  extra_info <- readRDS(file.path(data.folder, "info_json.rds")) 
#}else{
#  source(file.path(code.folder,"code_2_features_json_file.R"))
#  saveRDS(extra_info, file = file.path(data.folder, "info_json.Rds"))
#}
#data.full <- merge(data.full, extra_info, by = "media_id", all.x = TRUE)

### add time-related variables
source(file.path(code.folder, "code_2_features_time_related.R"))
rm(list = "temp")



########## 5. DATA PARTITIONING

# testing set: last 3 first_flow songs per user (if available)
# training set: all remaining songs 
data.full[first_flow == 1 & dataset == "train", dataset := "test_candidate"] # here: 169,498 cases in testing
data.full[data.full[dataset == "test_candidate", list(index = tail(.I, 3)), by = user_id]$index, dataset := "test"] # here: 39,821 cases in testing
data.full[dataset == "test_candidate", dataset := "train"]
table(data.full$dataset)



########## 6. CREATING FEATURES ON PARTITIONED DATA

### Compute total plays and skips as features
source(file.path(code.folder, "code_2_features_total_plays.R"))
rm(list = "data.train")

### Compute naive skip ratios as features
source(file.path(code.folder, "code_2_features_naive_ratios.R"))
rm(list = "data.train")



########## 7. TRANSFORMING IDs 

# Drop non-FLow songs from the training data [OPTIONAL]
#data.full <- data.full[dataset != "train" | listen_type == 1, ]

# Make nice IDs for embedding
# Note that rare IDs are replaced and all original ID info dropped 
source(file.path(func.folder, "createEmbeddingID.R"))
trainIdx <- which(data.full$dataset == "train")
data.full[, user_id := createEmbeddingID(user_id, trainIdx = trainIdx)]
idCols <- c("user_id", "artist_id", "media_id", "genre_id", "album_id", "context_type")
data.full[, (idCols) := lapply(.SD, createEmbeddingID, trainIdx = trainIdx), .SDcols = idCols]

# Remove everything not needed for estimation 
data.full[, c("ts_listen", "release_date", "time_lag") := NULL]

# Transform factor to dummy
factorCols <- c("platform_name", "platform_family", "hour_of_day", "weekday")
data.full <- cbind(data.full, model.matrix(~.-1, data = data.full[, (factorCols), with=FALSE]))
data.full[, (factorCols) := NULL]

########### IMPORTING KERAS EMBEDDINGS
# Import user and song bias from simple recommender (dot product of embeddings with added bias)
user_bias <- fread(file.path(data.folder, "user_bias_recommender0519.csv"))
data.full <- merge(data.full, user_bias, by.x = "user_id", by.y = "V1", all.x = TRUE, all.y = FALSE)

song_bias <- fread(file.path(data.folder, "song_bias_recommender0519.csv"))
data.full <- merge(data.full, song_bias, by.x = "media_id", by.y = "V1", all.x = TRUE, all.y = FALSE)

# Import and prepare user and song embeddings
user_embeddings <- fread(file.path(data.folder, "user_embeddings_recommender0519.csv"), header = TRUE, check.names = TRUE, col.names = c("user_id", paste0("ub", 1:50)))
song_embeddings <- fread(file.path(data.folder, "song_embeddings_recommender0519.csv"), header = TRUE, check.names = TRUE, col.names = c("media_id", paste0("sb", 1:50)))
temp <- merge(data.full[, .(user_id, media_id)], user_embeddings, by = "user_id", all.x = TRUE)
temp <- merge(temp, song_embeddings, by = "media_id", all.x = TRUE)
embDiffMatrix <- as.matrix(temp[,3:52, with=FALSE]) - as.matrix(temp[,53:102, with=FALSE])
data.full[, meanDistUserSongEmbeddings := rowMeans(embDiffMatrix)]
data.full[, maxDistUserSongEmbeddings := apply(embDiffMatrix, 1, max)]
# Calculate the first 5 principal components of the difference matrix
embDiffMatrix_pca <- prcomp(embDiffMatrix, center = T, scale. = T, tol = 0)
data.full[, paste0("pcaDistUserSongEmbeddings", 1:5) := lapply(1:5, function(i) embDiffMatrix_pca$x[,i])]
# Calculate principal components of the original embeddings matrices for users and songs
user_embeddings_pca <- data.table(user_embeddings$V1, prcomp(user_embeddings, center = T, scale. = T, tol = 0)$x[,1:5])
setnames(user_embeddings_pca, c('user_id', paste0("userEmbPCA", 1:5)))
data.full <- merge(data.full, user_embeddings_pca, by = "user_id")
song_embeddings_pca <- data.table(song_embeddings$V1, prcomp(song_embeddings, center = T, scale. = T, tol = 0)$x[,1:6])
setnames(user_embeddings_pca, c('media_id', paste0("userEmbPCA", 1:6)))
data.full <- merge(data.full, song_embeddings_pca, by = "media_id")

########## 8. EXPORTING DATA

# saving the data as data_flow.csv [if Flow songs are dropped]
#fwrite(data.full, file.path(data.folder, "data_flow.csv"), sep = ",", dec = ".", quote = F)

# saving the data as data_full.csv [if Flow songs are kept]
fwrite(data.full, file.path(data.folder, "data_full.csv"), sep = ",", dec = ".", quote = F)