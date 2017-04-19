###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting work directory
work.folder <- "N:/DSG2017/DSG_2017"
#work.folder <- "C:/Users/kozodoin3.hub/Desktop/DSG_2017-master"
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

load(file.path(data.folder, "data_train.Rda"))

dt <- as.data.table(data.train)
# Order data by user_id and ts_listen
# Using set* functions is faster
setorder(dt, user_id, ts_listen)
#dt <- dt[order(ts_listen),.SD, by=user_id]

dt[,time_lag:=c(NA, difftime(tail(ts_listen,-1), head(ts_listen,-1), units = "secs")), by = user_id]
dt[is.na(time_lag),time_lag := 0]

#let's look at the most typical time lags
#summary(dt[time_lag>0&time_lag<167,time_lag])
hist(dt[time_lag>0 & time_lag<30, time_lag])

#let's take more than 15 mins as a start for a new session
# 15 mins = 900 sec
#dt[,session_id := seq_along(time_lag), by = time_lag < 900]

other_user <- which(diff(as.numeric(dt$user_id))!=0)+1
time_lag_over <- which(dt$time_lag>900)
ind <- sort(c(other_user,time_lag_over))
dt[ind,sess_ind := FALSE]
dt[is.na(sess_ind),sess_ind := TRUE]
# try on small sam: x <- dt[530:640]
# we have 0 to 479294 different sessions
start.time <- Sys.time()
counter = 0
for (i in 1:length(dt[,time_lag])){
  if(dt[i,sess_ind==FALSE]){
    counter = counter + 1
    dt[i,session_id:=counter]
    print(counter)
  } else {
    dt[i,session_id:=counter]
  }
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# Time difference of 2.395967 hours
# saving data samples
save(dt, file = file.path(data.folder, "dt_with_session_id.Rda"))
session_id <- dt$session_id
# easier to load, however use rbind, after ordering dt <- dt[order(ts_listen),.SD, by=user_id]
save(session_id, file = file.path(data.folder, "session_id_vector.Rda"))
