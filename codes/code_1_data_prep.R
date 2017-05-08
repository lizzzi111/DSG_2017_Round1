###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
#rm(list = ls())

# setting work directory
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
setkey(data.full, user_id, media_id)

########## 2. CONVERTING VARIABLES ####

# converting factors
#temp <- c("genre_id", "media_id", "album_id", "user_id", "artist_id", "context_type", "platform_name", "platform_family")
#data.full[, (temp) := lapply(.SD, factor), .SDcols = temp]

# converting timestamps
data.full[, release_date := as.Date(as.character(data.full$release_date), "%Y%m%d")]
data.full[, ts_listen := anytime(data.full$ts_listen, asUTC = T)]

########## 3. CREATING FEATURES ####

##### 3.1. FEATURES ON FULL DATA ####

#################### 
# Outsource some code to be more flexible
# Also saves the results, we could reload here if possible instead of doing again
#################### 

### Add data from json file, info on song name, album name, artist name
if(file.exists(file.path(data.folder, "info_json.rds"))){
  extra_info <- readRDS(file.path(data.folder, "info_json.rds")) 
}else{
  source(file.path(code.folder,"code_2_features_json_file.R"))
  saveRDS(extra_info, file = file.path(data.folder, "info_json.Rds"))
}
data.full <- merge(data.full, extra_info, by = "media_id", all.x = TRUE)

### Add time-related variables
source(file.path(code.folder, "code_2_features_time_related.R"))

##### 3.2. FEATURES ON PARTITIONED DATA ####

# Add data split training/test variable
data.full[first_flow == 1 & dataset == 'train', dataset := "test"] 
# Extract last 10 observations for each user, if possible
# Will move rare users completely to the test set
#data.full[data.full[dataset == 'train',list(index = head(.I, 10)), by = user_id]$index, dataset := 'test']

### Compute total plays and skips as features
source(file.path(code.folder, "code_2_features_total_plays.R"))

### Compute naive skip ratios as features
source(file.path(code.folder, "code_2_features_naive_ratios.R"))

##### 4. EXPORTING DATA ####
# Make nice IDs for embedding
# Note that rare IDs are replaced and all original ID info dropped 
source(file.path(func.folder, "createEmbeddingID.R"))
trainIdx <- which(data.full$dataset == "train" & data.full$listen_type == 1)
#data.full[, user_id := createEmbeddingID(user_id, trainIdx = trainIdx)]
idCols <- c("user_id", "artist_id","media_id", "genre_id", "context_type", "album_id")
data.full[, (idCols) := lapply(.SD, createEmbeddingID, trainIdx = trainIdx),
          .SDcols = idCols]

##### Avoid work in Python:
# Remove everything not needed for estimation 
data.full[, c("ts_listen", "release_date", "sng_title", "alb_title", "art_name", "songs_in_the_alb",
              "songs_by_the_art", "alb_by_art") := NULL]

# Transform factor to dummy
factorCols <- c("platform_name", "platform_family", "hour_of_day", "weekday")
data.full <- cbind(data.full, model.matrix(~.-1, data = data.full[, (factorCols), with=FALSE]))
data.full[, (factorCols) := NULL]


# saving Data
fwrite(data.full, file.path(data.folder, "data_full.csv"))
