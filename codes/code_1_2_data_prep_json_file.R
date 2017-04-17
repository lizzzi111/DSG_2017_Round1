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
library(anytime)

# loading functions
source(file.path(code.folder, "code_0_helper_functions.R"))

work.folder <- "N:/DSG2017/DSG_2017"

### READ In THE JSON FILE
infile <- "../Data/extra_infos.json"
inf_lines <- read_lines(infile,  progress = FALSE)
inf_lines

### Combine to a large string
inf_combined <- str_c("[", str_c(inf_lines, collapse = ", "), "]")

### flatten it to a data frame
extra_info <- fromJSON(inf_combined) %>%
  flatten() %>%
  tbl_df()

### save to the data file
save(extra_info, file = file.path(data.folder, "info_json.Rda"))
