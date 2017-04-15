# setting working directory
load_functions <- function(path) {
  
  # getting file list
  file.list <- list.files(path)
  
  # processing functions
  for(i in 1:length(file.list)) {
    
    # loading function
    source(file.path(path, file.list[i]))
    
    # printing the name
    print(file.path("Loading ", file.list[i]))
  }
}

# loading the functions
load_functions(func.folder)