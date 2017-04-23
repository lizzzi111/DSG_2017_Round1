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

# the master loop
data <- data.frame()
for (user in sort(unique(sessions$user_id), decreasing = T)) {
  print(paste0("User ", user))
  for (session in unique(sessions$sessionId[sessions$user_id == user])) {
    if (session > 1) {
      print(paste0("User ", user, ", session ", session))
      
      train <- sessions[sessions$user_id == user & sessions$sessionId <  session, ]
      valid <- sessions[sessions$user_id == user & sessions$sessionId == session, ]
      
      valid <- compute_features_dt(train, valid)
      
      data <- rbind(data, valid)
    }
  }
}

# saving data
save(data, file = file.path(data.folder, "data_train_sessions_vars.Rda"))
