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
#      SIMILARITY COMPUTATION     #
#                                 #
###################################

# loading data sets
load(file.path(data.folder, "data_train.Rda"))
data.train <- data.table(data.train)

# keeping only played tracks
# JO: rather not. it doesn't matter for the sum, but we can do a 'rating' using the mean
#data.train <- data.train[data.train$is_listened == 1, ]
#data.train <- data.train[1:1000, ]

# converting variable to numeric
# careful when transforming factors back to numeric, is.numeric doesn't work properly (-> 1, 2)
data.train[, is_listened := as.numeric(levels(is_listened))[is_listened]]

# computing play count by user-artist
user.artist.count <- data.train[, list( 'count' = sum(is_listened, na.rm = TRUE)), by = c("user_id", "artist_id")]
user.artist.mean <- data.train[, list( 'mean' = mean(is_listened, na.rm = TRUE)), by = c("user_id", "artist_id")]
#user.artist.count <- aggregate(is_listened ~ user_id + artist_id, data.train, sum)

# droping factor levels
user.artist.count$artist_id <- droplevels(user.artist.count$artist_id)

# reshaping the data
# JO: This gives a matrix with 1.4 * 10^9, which is too big.
#     I'll check how to do collaborative filtering with data of this size
history <- reshape(user.artist.count, idvar = "user_id", timevar = "artist_id", direction = "wide")
history[is.na(history)] <- 0
history <- apply(history, 2, as.numeric)
history <- as.data.frame(history)

# drop user_id
history.ubs <- (history[, !(names(history) %in% c("user_id"))])

# user similarity matrix
similarity <- matrix(NA, nrow = nrow(history), ncol = nrow(history))
rownames(similarity) <- history$user
colnames(similarity) <- history$user

# create a helper function to calculate the cosine between two vectors
getCosine <- function(x,y) {
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

# JO: The resulting matrix has 100 million entries, that's too much
# loop through the rows
for (i in 1:nrow(history.ubs)) {
  for (j in 1:nrow(history.ubs)) {
    print(paste0(i, "-", j))
    similarity[i,j] <- getCosine(as.matrix(history.ubs[i,]), as.matrix(history.ubs[j, ]))
  }
}

# back to dataframe
similarity <- as.data.frame(similarity)
