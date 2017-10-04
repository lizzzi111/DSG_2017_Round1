##### function to submit predictions
submit <- function(prediction, data, folder, file = "test.csv") {
  
  # displaying error messages
  if (length(prediction) != nrow(data)) {stop("Predictions and dataset are not the same length")}
  
  # creadting dataset with relevant coloumns
  data <- data[, c("sample_id", "is_listened")]

  # adding predictions to the data
  data$is_listened <- prediction
  
  # exporting predictions
  write.table(data, row.names = F, col.names = T, quote = F, sep = ",", file = file.path(folder, file))
}
