###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting work directory
work.folder <- "/Users/Kozodoi/Documents/Competitions/DSG_2017"
#work.folder <- "C:/Users/kozodoin3.hub/Desktop/DSG_2017"
setwd(work.folder)

# setting inner folders
code.folder <- "codes"
data.folder <- "data"
func.folder <- "functions"
subm.folder <- "submissions"

# loading libraries
library(anytime)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))


###################################
#                                 #
#         DATA PREPARATION        #
#                                 #
###################################

##### 1. LOADING THE DATA

# loading training data
data.train <- read.csv2(file.path(data.folder, "train.csv"), sep = ",", dec = ".", header = T)

# loading training data
data.test <- read.csv2(file.path(data.folder, "test.csv"), sep = ",", dec = ".", header = T)

# merging data sets
data.test$is_listened <- NA
data.train$sample_id  <- NA
data.full <- rbind(data.train, data.test)


##### 2. CONVERTING VARIABLES

# converting IDs
data.full$sample_id <- as.factor(data.full$sample_id)
data.full$genre_id  <- as.factor(data.full$genre_id)
data.full$media_id  <- as.factor(data.full$media_id)
data.full$album_id  <- as.factor(data.full$album_id)
data.full$user_id   <- as.factor(data.full$user_id)
data.full$artist_id <- as.factor(data.full$artist_id)

# converting timestamps
data.full$release_date <- as.Date(as.character(data.full$release_date), "%Y%m%d")
data.full$ts_listen <- anytime(data.full$ts_listen, asUTC = T)

# converting other factors
data.full$context_type    <- as.factor(data.full$context_type)
data.full$platform_name   <- as.factor(data.full$platform_name)
data.full$platform_family <- as.factor(data.full$platform_family)
data.full$listen_type     <- as.factor(data.full$listen_type)
data.full$user_gender     <- as.factor(data.full$user_gender)

# looking at the data
summary(data.full)

# separating data sets
data.train <- data.full[!(is.na(data.full$is_listened)), ]
data.test  <- data.full[is.na(data.full$is_listened),    ]

# saving all data sets
save(data.train, file = file.path(data.folder, "data_train.Rda"))
save(data.test,  file = file.path(data.folder, "data_test.Rda"))