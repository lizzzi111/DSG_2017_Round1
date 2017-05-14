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

# adding API data
api.data <- fread(file.path(data.folder, "api_final.txt"), sep = ",", dec = ".", header = T)
api.data[fans == -1, fans := 0]
api.data <- api.data[!duplicated(api.data),]
adata.full <- merge(data.full, api.data[,.(track_id,rank,bpm,position,lyrics_explicit,gain,fans)], by.x = "media_id", by.y = "track_id", all.x = T, all.y = F)



########## 2. CONVERTING VARIABLES ####

# converting factors
#temp <- c("genre_id", "media_id", "album_id", "user_id", "artist_id", "context_type", "platform_name", "platform_family")
#data.full[, (temp) := lapply(.SD, factor), .SDcols = temp]

# converting timestamps
data.full[, release_date := as.Date(as.character(data.full$release_date), "%Y%m%d")]
data.full[, ts_listen := anytime(data.full$ts_listen, asUTC = T)]

########## 3. CREATING FEATURES ON FULL DATA ####

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


########## 4. DATA PARTITIONING

# Extract last 10 observations for each user, if possible
# Will move rare users completely to the test set
#data.full[data.full[dataset == "train", list(index = head(.I, 10)), by = user_id]$index, dataset := "test"]

# testing set: last 3 first_flow songs per user (if available)
# training set: all remaining songs 
data.full[first_flow == 1 & dataset == "train", dataset := "test_candidate"] # here: 169,498 cases in testing
data.full[data.full[dataset == "test_candidate", list(index = tail(.I, 3)), by = user_id]$index, dataset := "test"] # here: 39,821 cases in testing
data.full[dataset == "test_candidate", dataset := "train"]
table(data.full$dataset)


########## 5. CREATING FEATURES ON PARTITIONED DATA

### Compute total plays and skips as features
source(file.path(code.folder, "code_2_features_total_plays.R"))
rm(list = "data.train")

### Compute naive skip ratios as features
source(file.path(code.folder, "code_2_features_naive_ratios.R"))
rm(list = "data.train")


########## 6. EXPORTING DATA

# Drop non-FLow songs from the training data [OPTIONAL]
data.full <- data.full[dataset != "train" | listen_type == 1, ]

# Make nice IDs for embedding
# Note that rare IDs are replaced and all original ID info dropped 
source(file.path(func.folder, "createEmbeddingID.R"))
trainIdx <- which(data.full$dataset == "train")
data.full[, user_id := createEmbeddingID(user_id, trainIdx = trainIdx)]
idCols <- c("user_id", "artist_id", "media_id", "genre_id", "album_id", "context_type")
data.full[, (idCols) := lapply(.SD, createEmbeddingID, trainIdx = trainIdx), .SDcols = idCols]

# Remove everything not needed for estimation 
data.full[, c("ts_listen", "release_date", "time_lag", "album_id") := NULL]

# Transform factor to dummy
factorCols <- c("platform_name", "platform_family", "hour_of_day", "weekday")
data.full <- cbind(data.full, model.matrix(~.-1, data = data.full[, (factorCols), with=FALSE]))
data.full[, (factorCols) := NULL]

# saving the data: data_full.csv for all songs, data_flow.csv for flow songs
#write.table(data.full, file.path(data.folder, "data_full.csv"), sep = ",", dec = ".", quote = F)
write.table(data.full, file.path(data.folder, "data_flow.csv"), sep = ",", dec = ".", quote = F)