createEmbeddingID <- function(x, trainIdx){
  full <- data.table(ID = x)
  full[, ID_group := .GRP, by = ID]
  
  #Count values in train
  tab <- table(full[trainIdx, ID_group])
  tabTable <- data.table(ID_group = as.integer(names(tab)), count = as.numeric(tab))
  # Values in train less than 10 times or minimum number of occurences +2
  #rare <- names(tab[tab <= max(min(tab)+2, 10)])
  
  full <- merge(full, tabTable, by ="ID_group", all.x = TRUE)
  full[is.na(count), count := 0]
  
  full[, ID_embedding := ID_group]
  full[count <= max(min(count)+2, 10), ID_embedding := -1]
  
  full[, ID_embedding := .GRP, by = ID_embedding]
  
  return(full$ID_embedding)
}


# createEmbeddingID <- function(x, trainIdx){
#   # Setup input
#   library(data.table)
#   org <- data.table(ID = as.character(x))
#   lvls <- data.table(ID = unique(org$ID))
#   # Count occurence of ID only in train set
#   x_train <- org[trainIdx]
#   count <- x_train[, list(count = .N), by = ID]
# 
#   # Replace count 1 or not in training set by some value
#   temp <- merge(lvls, count, by = "ID", all.x = TRUE)
#   temp[, ID_embedding := ID]
#   # Replace rare values by placeholder id
#   threshold <- max(min(temp$count, na.rm = TRUE)+1, 3)
#   temp[count <= threshold, ID_embedding := "PLACEHOLDER"]
#   # Replace values that don't occur by placeholder id
#   temp[is.na(count), ID_embedding := "PLACEHOLDER"]
# 
#   # Replace the IDs by IDs from 1 to N
#   temp[, ID_output := .GRP, by = ID_embedding]
#   # Make sure that the ordering is the same as in the original vector
#   output <- merge(org, temp[,.(ID, ID_output)], by = "ID")
#   return(output$ID_output)
# }
