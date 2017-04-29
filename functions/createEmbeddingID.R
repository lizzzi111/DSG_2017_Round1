createEmbeddingID <- function(x, trainIdx){
  library(data.table)
  x <- as.character(x)
  lvls <- data.table(ID = unique(x))
  
  x_train <- data.table(ID = x[trainIdx])
  count <- x_train[, list(count = .N), by = ID]
  
  test <- merge(lvls, count, by = "ID", keep.x = TRUE)
  
  temp[, ID_embedding := ID]
  temp[count < 3, ID_embedding := "PLACEHOLDER"]
  temp[, ID_embedding := as.numeric(factor(ID_embedding))]
  output <- merge(org, temp[,.(ID, ID_embedding)], by = "ID")$ID_embedding
  return(output)
}
