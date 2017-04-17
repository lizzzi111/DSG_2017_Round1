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

work.folder <- "N:/DSG2017/DSG_2017"


### detect the language of the song
#extra_info$language  <- textcat(extra_info$sng_title)
#extra_info$language <- factor(extra_info$language)
#extra_info$english <- ifelse()

### the result is very inaccurate, so, we'd rather have to search for another similar solution or drop the idea completely
### save to the data file

load(file.path(data.folder, "info_json.Rda"))

#preprosess the variables having characters before converting to factors:
#' Remove everything that is not a number or letter (may want to keep more 
#' stuff in your actual analyses). 
extra_info$sng_title <- stringr::str_replace_all(extra_info$sng_title,"[^a-zA-Z\\s]", " ")
extra_info$alb_title <- stringr::str_replace_all(extra_info$alb_title,"[^a-zA-Z\\s]", " ")
extra_info$art_name <- stringr::str_replace_all(extra_info$art_name,"[^a-zA-Z\\s]", " ")
# Shrink down to just one white space
extra_info$sng_title <- stringr::str_replace_all(extra_info$sng_title,"[\\s]+", " ")
extra_info$alb_title <- stringr::str_replace_all(extra_info$alb_title,"[\\s]+", " ")
extra_info$art_name <- stringr::str_replace_all(extra_info$art_name,"[\\s]+", " ")


### convert to factor variables
extra_info$media_id <- factor(extra_info$media_id)
#extra_info$sng_title <- factor(extra_info$sng_title)
extra_info$alb_title <- factor(extra_info$alb_title)
extra_info$art_name <- factor(extra_info$art_name)

### overt to data table for handy and fast feature creation
dt <- as.data.table(extra_info)

# combine artist and the album to make it unique (diff artists can have the same name for an album)
#dt[,alb_art_name := factor(paste(substr(art_name,-1,-5),substr(alb_title,1,4), sep="_"))]
#bad idea, a lot of things start with "the" the beatles the bamboos, etc.
# combine artist and the song name to make it unique (diff artists can have the same name for a song)
# useless?
#dt[,sng_art_name := factor(paste(substr(sng_title,1,4),substr(alb_title,1,4), sep="_"))]

#songs in the album
#dt[,songs_in_the_alb := .N, by=alb_art_name ]
dt[,songs_in_the_alb := .N, by=alb_title ]
#songs by the artist
dt[, songs_by_the_art := .N, by=art_name ]

# we get 2830 songs which have neither artist names nor album name
# and 1334 by the artist, it gives us wrong info, I'll impute it with one
dt[,songs_in_the_alb:=ifelse(songs_in_the_alb==2830,1,songs_in_the_alb)]
dt[,songs_by_the_art:=ifelse(songs_by_the_art==1334,1,songs_by_the_art)]

#albums by the artist
# this one takes very long, I haven't measured the time exactly, but it took at least an hour. So 
# if you want to try it yourself, be prepared to wait :)
# or probably we could make a more efficient version of it with dplyr and mutate
aggr_info <-as_data_frame(dt)
aggr_info <- aggr_info %>% 
  group_by(art_name) %>%
  mutate(alb_by_art = n_distinct(alb_title)) %>% 
  ungroup()

# we get 212 for the missing artist, impute with one
aggr_info$alb_by_art <- ifelse(aggr_info$alb_by_art==212,1,aggr_info$alb_by_art)

#what is better to do with albums such as greatest hits, chanson etc -outliers? impute it or leave it as it is
# the same question for the art_name = "Glee Cast" 
#save(aggr_info, file = file.path(data.folder, "json_with_aggr.Rda"))
