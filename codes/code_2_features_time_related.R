# loading libraries
library(pacman)
pacman::p_load("anytime","data.table")

# Order data by user_id and ts_listen
# Using set* functions is faster
setorder(data.full, user_id, ts_listen)
#dt <- dt[order(ts_listen),.SD, by=user_id]

data.full[,time_lag:=c(NA, difftime(tail(ts_listen,-1), head(ts_listen,-1), units = "mins")), by = user_id]
data.full[is.na(time_lag), time_lag := 0]

#let's look at the most typical time lags
#summary(data.full$time_lag)

#let's take more than 20 mins as a start for a new session
#dt[,session_id := seq_along(time_lag), by = time_lag < 20]
data.full[, session_id := cumsum(time_lag>20)+1, by = user_id]

# Count the absolute position of the song in the session
data.full[, song_session_position := 1:.N, by = c("user_id", "session_id")]

# Find the index of the song in the flow
temp <- data.full[, list(listen_type = listen_type, user_id, session_id)]
temp[, flow_lag := shift(listen_type, fill = 0), by = c("user_id", "session_id")]
temp[, first_flow := as.numeric(listen_type == 1 & flow_lag == 0)]
#data.full[listen_type == "1", flow_position := cumsum(listen_type == "1" & shift(listen_type, fill = "0") == "0"), by = session_id]
data.full[, first_flow := temp$first_flow]

# Compute difference between the release date and listening date (number of days)
# NAs: release_date later than 2018; ts_listen before 2010 => substituted by mean values
# There are still 29,091 cases with time_diff < 0
data.full[, time_diff := as.numeric(as.Date(ts_listen) - release_date)]
data.full[ts_listen    < "2010-01-01", time_diff := NA]
data.full[release_date > "2018-01-01", time_diff := NA]
data.full[is.na(time_diff), time_diff := mean(data.full$time_diff, na.rm = T)]
#summary(data.full$time_diff)

# Create a factor variable for the hour of the day when the song is played
data.full[, hours := as.factor(format(as.POSIXct(data.full$ts_listen, format = "%H:%M:%S"),"%H"))]

# Create a lagged is_listened (for the previous song)
data.full[, is_listened_lag :=  shift(.SD), by = user_id, .SDcols = "is_listened"]
data.full$is_listened_lag[is.na(data.full$is_listened_lag)] <- "none"
data.full$is_listened_lag[data.full$song_session_position == 1] <- "none"

# # easier to load, however use rbind, after ordering dt <- dt[order(ts_listen),.SD, by=user_id]
# save(session_id, file = file.path(data.folder, "session_id_vector.Rda"))
# 
# sessionize = function(ts) {
#   delta.t = diff(ts)
#   is.new = c(TRUE, delta.t >= 900)
#   cumsum(is.new)
# }
# sessions = data.train %>%
#   group_by(user_id) %>%
#   arrange(ts_listen) %>%
#   mutate(sessionId = sessionize(ts_listen))