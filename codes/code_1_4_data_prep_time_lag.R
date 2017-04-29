
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

#let's take more than 30 mins as a start for a new session
#dt[,session_id := seq_along(time_lag), by = time_lag < 20]
data.full[, session_id := cumsum(time_lag>20)+1, by = user_id]

# Count the absolute position of the song in the session
data.full[, song_session_position := 1:.N, by = session_id]

# Find the index of the song in the flow
temp <- data.full[, list(listen_type = as.numeric(levels(listen_type))[listen_type], session_id)]
temp[, flow_lag := shift(listen_type, fill = 0), by=session_id]
temp[, first_flow := listen_type == 1 & flow_lag == 0]
#data.full[listen_type == "1", flow_position := cumsum(listen_type == "1" & shift(listen_type, fill = "0") == "0"), by = session_id]
data.full[, first_flow := temp$first_flow]

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
