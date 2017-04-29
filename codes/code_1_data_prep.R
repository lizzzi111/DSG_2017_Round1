###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# setting work directory
#work.folder <- "/Users/Kozodoi/Documents/Competitions/DSG_2017"
#work.folder <- "C:/Users/kozodoin3.hub/Desktop/DSG_2017"
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

##### 1. LOADING THE DATA

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

##### 2. CONVERTING VARIABLES

# converting factors
temp <- c("sample_id", "genre_id", "media_id", "album_id", "user_id", "artist_id", "user_gender", "context_type", "platform_name",
          "platform_family", "listen_type", "is_listened")
data.full[, (temp) := lapply(.SD, factor), .SDcols = temp]

# converting timestamps
data.full[, release_date := as.Date(as.character(data.full$release_date), "%Y%m%d")]
data.full[, ts_listen := anytime(data.full$ts_listen, asUTC = T)]

# looking at the data
#summary(data.full)

# Outsource some code to be more flexible
# Also saves the results , we could reload here if possible instead of doing again
# Add data from json file, info on song name, album name, artist name
if(file.exists(file.path(data.folder, "info_json.rds"))){
  extra_info <- readRDS(file.path(data.folder, "info_json.rds")) 
}else{
  source(file.path(code.folder,"code_1_3_data_prep_json_file.R"))
  saveRDS(extra_info, file = file.path(data.folder, "info_json.Rds"))
}
data.full <- merge(data.full, extra_info, by = "media_id", all.x = TRUE)

# Add time related variables
source(file.path(code.folder,"code_1_4_data_prep_time_lag.R"))

# Add data split training/test variable
#data.full[flow_position == 1 & dataset == 'train', dataset := "test"] # Only 6034 obs..
# Extract last 10 observations for each user, if possible
# Will move rare users completely to the test set
data.full[data.full[dataset == 'train',list(index = tail(.I, 10)), by = user_id]$index, dataset := 'test']

# Compute naive skip ratios as features
source(file.path(code.folder,"code_2_naive_ratios.R"))

# Make nice IDs for embedding
source(file.path(func.folder, "createEmbeddingID.R"))
data.full[, c("user_id_embedding", "artist_id_embedding","media_id_embedding", "genre_id_embedding") :=
            lapply(list(user_id, artist_id, media_id, genre_id), createEmbeddingID)]

# saving Data
write.csv(data.full, file.path(data.folder, "data_full.csv"))
