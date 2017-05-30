###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting work directory
work.folder <- "/Users/Kozodoi/Documents/Competitions/DSG_2017"
setwd(work.folder)

# setting inner folders
code.folder <- "codes"
data.folder <- "data"
func.folder <- "functions"
subm.folder <- "submissions"

# loading libraries
if(require(pacman)==FALSE) install.packages("pacman")
library(pacman)
p_load(data.table, anytime, randomForest, AUC, caret, beepr, xgboost, caret)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))

# loading data
data.full <- as.data.frame(fread(file.path(data.folder, "data_flow.csv"), sep = ",", dec = ".", header = T))
data.test <- data.full[data.full$dataset == "test", ]
data.unknown <- data.full[data.full$dataset == "unknown", ]
rm(list = "data.full")

# sorting unknown data
data.unknown$sample_id <- as.numeric(as.character(data.unknown$sample_id))
data.unknown <- data.unknown[order(data.unknown$sample_id), ]

# saving ratios on validation
write.table(data.test[,c("row_index", "user_skip_ratio_last3")], "./pred_valid/user_ratio_last3.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "user_skip_ratio_last5")], "./pred_valid/user_ratio_last5.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "user_skip_ratio_last10")], "./pred_valid/user_ratio_last10.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "user_ratio_flow")], "./pred_valid/user_ratio_flow.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "user_ratio_full")], "./pred_valid/user_ratio_full.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "user_artist_ratio")], "./pred_valid/user_ratio_artist.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "user_genre_ratio")], "./pred_valid/user_ratio_genre.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "user_context_ratio")], "./pred_valid/user_ratio_context.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "user_song_ratio")], "./pred_valid/user_ratio_song.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "user_album_ratio")], "./pred_valid/user_ratio_album.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "artist_ratio")], "./pred_valid/ratio_artist.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "genre_ratio")], "./pred_valid/ratio_genre.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "context_ratio")], "./pred_valid/ratio_context.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "song_ratio")], "./pred_valid/ratio_song.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")
write.table(data.test[,c("row_index", "album_ratio")], "./pred_valid/ratio_album.csv", row.names = F, col.names = c("row_index", "is_listened"), quote = F, sep = ",")

# saving ratios on unknown
write.table(data.unknown[,c("sample_id", "user_skip_ratio_last3")], "./pred_unknown/user_ratio_last3.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "user_skip_ratio_last5")], "./pred_unknown/user_ratio_last5.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "user_skip_ratio_last10")], "./pred_unknown/user_ratio_last10.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "user_ratio_flow")], "./pred_unknown/user_ratio_flow.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "user_ratio_full")], "./pred_unknown/user_ratio_full.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "user_artist_ratio")], "./pred_unknown/user_ratio_artist.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "user_genre_ratio")], "./pred_unknown/user_ratio_genre.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "user_context_ratio")], "./pred_unknown/user_ratio_context.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "user_song_ratio")], "./pred_unknown/user_ratio_song.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "user_album_ratio")], "./pred_unknown/user_ratio_album.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "artist_ratio")], "./pred_unknown/ratio_artist.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "genre_ratio")], "./pred_unknown/ratio_genre.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "context_ratio")], "./pred_unknown/ratio_context.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "song_ratio")], "./pred_unknown/ratio_song.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
write.table(data.unknown[,c("sample_id", "album_ratio")], "./pred_unknown/ratio_album.csv", row.names = F, col.names = c("sample_id", "is_listened"), quote = F, sep = ",")
