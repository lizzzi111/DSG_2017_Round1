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
list.of.packages <- c("stringr", "jsonlite", "textcat","anytime", "dplyr", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))


###################################
#                                 #
#        COMPUTING FEATURES       #
#                                 #
###################################

# loading data
load(file.path(data.folder, "data_train_sessions.Rda"))

# droping first sessions
sessions <- sessions[sessions$sessionId > 1, ]

# list of users
user_list <- sort(unique(sessions$user_id))

# setting max sessions per user
max_sessions <- 5

# the master loop
data <- data.frame()
for (user in user_list) {
  
  # extracting sessions
  ses <- unique(sessions$sessionId[sessions$user_id == user])

  # keeping last sessions
  if (length(ses) > max_sessions) {
    ses <- ses[(length(ses)-max_sessions+1):length(ses)]
  }

  # loop for data creation
  for (s in ses) {
    
    # displaying user ans session name
    print(paste0("User ", user, ", session ", s))
    
    # partitioning data 
    train_full <- sessions[sessions$user_id == user & sessions$sessionId <  s,     ]
    train_last <- sessions[sessions$user_id == user & sessions$sessionId == (s-1), ]
    valid      <- sessions[sessions$user_id == user & sessions$sessionId == s,     ]
   
    # computing features
    valid <- compute_features_dt(train_full, valid)
    valid$ratio_per_user_last <- mean(as.numeric(train_last$is_listened)-1)
    data <- rbind(data, valid)
  }
}

# saving data
save(data, file = file.path(data.folder, "data_train_sessions_vars.Rda"))