# Theo Pleizier, 10-3-2023
# find sermon startlines: 
# startline is defined as the line after the one where the Bibletext is mentioned
# in other words: find the first reference to Scripture

#library(here)
#library(stringr)
#inputpathvolumes <- "gen/spurgeon/txt" 
#if(!file.exists(here("gen","spurgeon_sermons_df.csv"))) source("compare-titles_barnes_downloads.R")
#sermons_df <- read.csv(here("gen","spurgeon_sermons_df.csv"))
# source(here("src","common_functions.R"))

pattern_scripture1 <- "\\w+[,.]?\\s?\\d{1,3}?\\s?:\\s?\\d{1,3}(([-,.]\\s?)?\\d{1,3}){0,}\\.?" # 99%, 18 NAs - 6 Barnes (no text) = 12 NAs 

currentvolume <- 0

fetch_volume <- function(x){ # x is row in sermons_df 
  currentvolume <<- sermons_df$volume[x]
  readLines(here(inputpathvolumes,sermons_df$volumefile[x])) # returns text volume
}

fetch_sermon <- function(x){ # sermon number from sermons_df
  sermon <- readLines(here(inputpathvolumes,sermons_df$volumefile[sermons_df$nr == x]))
  sermon <- sermon[sermons_df$start[sermons_df$nr == x]:sermons_df$end[sermons_df$nr == x]]
  sermon
}

find_scripture <- function(x, pattern = pattern_scripture){# row in sermons_df
  volume <- sermons_df$volume[x]
  if(volume != currentvolume) volumetext <<- fetch_volume(x)
  sermon <<- volumetext[sermons_df$start[x]:sermons_df$end[x]]
  headingline <<- character(0)
  scripturefound <- FALSE
  for (h in 1:90){
    headingline <<- str_trim(paste0(headingline," ",str_trim(sermon[h]))) # \n\n iso space?
    if(str_detect(headingline, pattern)){
      scripturefound <- TRUE
      break # h: line where scripture is found; headingline the complete heading
    } 
  }
  if(!scripturefound) h <- NA
  return(h) # linenumber with scripture-reference, or NA if reference has not been found
}

headinglines <- character(0)
scripturelines <- integer(0)
for(s in seq_len(nrow(sermons_df))){
  scriptureline <- find_scripture(s, pattern = pattern_scripture1)
  scripturelines <- c(scripturelines,scriptureline)
  headinglines <- c(headinglines, headingline)
}

sermonnrs <- sermons_df$nr[which(is.na(scripturelines))]
barnesNA <- sermons_df$nr[is.na(sermons_df$barnes_scripture)]
restant <- sermonnrs[!sermonnrs %in% barnesNA]

pattern_scripture2 <- "(Psalm|Jude)\\s\\d{1,3}" 
pattern_scripture3 <- "\\d\\s\\w+\\.?\\s(v\\s)?\\d{1,2}"
pattern_scripture4 <- paste0(pattern_scripture2,"|",pattern_scripture3)

scripturelines2 <- integer(0)
headinglines2 <- integer(0)
for(s in seq_along(restant)){
  x <- which(sermons_df$nr == restant[s])
  scriptureline <- find_scripture(x, pattern = pattern_scripture4)
  scripturelines2 <- c(scripturelines2,scriptureline)
  headinglines2 <- c(headinglines2, headingline)
}

# create column with start sermon
sermons_df$start_sermon <- NA
sermons_df$start_sermon <- scripturelines 
sermons_df$start_sermon[sermons_df$nr %in% restant] <- scripturelines2 

# two sermons need manual sermon_start lines; one with Exposition first, then SERMON (154); 
# the other has only 'Joshua' as biblical reference (1358). 
sermons_df$start_sermon[sermons_df$nr == 154] <- 326
sermons_df$start_sermon[sermons_df$nr == 1358] <- 14

# add headinglines
sermons_df$heading <- headinglines
sermons_df$heading[sermons_df$nr %in% restant] <- headinglines2

print("Start lines of the sermons determined. Headinglines added.")





