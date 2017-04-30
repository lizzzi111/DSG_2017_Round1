# Sessions for the whole set
require(dplyr);require(data.table)
data.train <- fread("N:/DSG2017/Data/train.csv")
data.test <- fread("N:/DSG2017/Data/test.csv")

data.test[, train:=0] 
data.train[, train:=1]

full <- rbind(data.train, data.test, fill = TRUE)

sessionize = function(ts) {
  delta.t = diff(ts)
  is.new = c(TRUE, delta.t >= 900)
  cumsum(is.new)
}
sessions = full %>%
  group_by(user_id) %>%
  arrange(ts_listen) %>%
  mutate(sessionId = sessionize(ts_listen))

sessions <- as.data.table(sessions)
#sessions[, songs_sessions := sum(.N), by = list(user_id, sessionId) ]
sessions[order(ts_listen),`:=`( COUNT = .N , IDX = 1:.N ), by = list(user_id, sessionId) ]
sessions[order(ts_listen), rel_ps:= IDX/COUNT, by = list(user_id, sessionId) ]

train <- sessions[train==1]
train[, train := NULL]
#write.csv(train, "./train_sess.csv")

test <- sessions[train==0]
test[, train := NULL]
#write.csv(test, "../../test_sess.csv")
