library(pacman)
p_load("stringr", "jsonlite", "data.table")

### read in the JSON file
# This is actually streamed json data (whatever that means), so fromJSON doesn't work, but stream_in does
infile <- file.path(data.folder, "extra_infos.json")
extra_info <- stream_in(file(infile))
extra_info <- data.table(extra_info)

### detect the language of the song
#extra_info$language  <- textcat(extra_info$sng_title)
#extra_info$language <- factor(extra_info$language)
#extra_info$english <- ifelse()

### the result is very inaccurate, so, we'd rather have to search for another similar solution or drop the idea completely

#preprosess the variables having characters before converting to factors:
#' Remove everything that is not a number or letter (may want to keep more 
#' stuff in your actual analyses). 
# Shrink down to just one white space

chrVars <- which(sapply(extra_info, is.character))
extra_info[, (chrVars) := lapply(.SD, function(x)str_replace_all(x,"[^a-zA-Z\\s]", " ")), .SDcols = chrVars]
extra_info[, (chrVars) := lapply(.SD, function(x)str_replace_all(x,"[\\s]+", " ")), .SDcols = chrVars]

# combine artist and the album to make it unique (diff artists can have the same name for an album)
#dt[,alb_art_name := factor(paste(substr(art_name,-1,-5),substr(alb_title,1,4), sep="_"))]
#bad idea, a lot of things start with "the" the beatles the bamboos, etc.
# combine artist and the song name to make it unique (diff artists can have the same name for a song)
# useless?
#dt[,sng_art_name := factor(paste(substr(sng_title,1,4),substr(alb_title,1,4), sep="_"))]

#songs in the album
#dt[,songs_in_the_alb := .N, by=alb_art_name ]
extra_info[,songs_in_the_alb := .N, by = alb_title ]
extra_info[alb_title == " ", songs_in_the_alb := 1]
#songs by the artist
extra_info[, songs_by_the_art := .N, by=art_name ]

# we get 2830 songs which have neither artist names nor album name
# and 1334 by the artist, it gives us wrong info, I'll impute it with one
extra_info[art_name == " ",songs_by_the_art := 0]
extra_info[sng_title == " ", songs_in_the_alb := 0]


#albums by the artist
# this one takes very long, I haven't measured the time exactly, but it took at least an hour. So 
# if you want to try it yourself, be prepared to wait :)
# or probably we could make a more efficient version of it with dplyr and mutate

# JO: I think this should work and be fast, but haven't tested it on the full data
extra_info[, alb_by_art := uniqueN(alb_title), by = art_name]
extra_info[art_name == " ", alb_by_art := 0]

#what is better to do with albums such as greatest hits, chanson etc -outliers? impute it or leave it as it is
# the same question for the art_name = "Glee Cast" 

### save to the data file
#extra_info[, (chrVars) := lapply(.SD, factor), .SDcols = chrVars]
setkey(extra_info, media_id)