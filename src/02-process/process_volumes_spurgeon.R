# process text-files volumes Spurgeon's sermons
# Theo Pleizier, 7-3-2023 (4-12-2023)
# extract titles, numbers, expositions 
# script belongs to project computational sermon analysis

library(here)
library(stringr)

# check if table exists with structures of volume-files: endings/beginnings of sermons
if(!file.exists(here("gen/spurgeon","spurgeon_sermons.csv"))) source(here("src/02-process","spurgeon_locate-sermons.R"))

# input/output settings
input_txt <- "gen/spurgeon/volumes"
output_txt <- "gen/spurgeon/sermons"
savetofile <- TRUE
if(savetofile) if(!dir.exists(here(output_txt))) dir.create(here(output_txt))

# add fields to table with sermons
sermons_df <- read.csv(here("gen/spurgeon","spurgeon_sermons.csv"))
sermons_df$title <- NA
sermons_df$nr <- NA
sermons_df$filename <- NA
sermons_df$exposition <- NA
sermons_df$problem <- FALSE
sermons_df$remove <- FALSE

# loop over volumes 
for(v in seq_along(unique(sermons_df$volume))){
  #if(v > 2) break #test
  volumesermons <- sermons_df[sermons_df$volume == v,]
  volumefile <- readLines(here(input_txt,volumesermons$volumefile[1]))
  sermonnrs <- character(0)
  sermontitles <- character(0)
  expositions <- character(0)
  
  # loop over sermons
  for(s in seq_along(volumesermons$start)){
    #if(s > 2) break #test
    sermon <- volumefile[(volumesermons$start[s] + 1):volumesermons$end[s]]
    
    # metadata: nr and title
    sermonnr <- sermon[which(str_detect(str_trim(sermon), "^\\(?No\\.?\\s\\d{1,4}"))]
    sermonnr <- str_extract(sermonnr, "\\d{1,3}-?\\d{0,3}")
    sermonnr <- ifelse(length(sermonnr)==0,NA,sermonnr)
    sermonnrs <- c(sermonnrs, sermonnr)
    
    sermontitle <- str_trim(sermon[1])
    sermontitle <- ifelse(str_detect(sermontitle, "SPURGEON"), NA, sermontitle)
    sermontitles <- c(sermontitles,sermontitle)
    
    # locate exposition 
    pattern_exposition <- "EXPOSI?TION(S)?(.*)SP?URGE?(O|0)N|^EXPOSITION(\\.)?$|EXPOSITION:?\\s\\w+\\s\\d{1,2}:" # regex to locate expositions
    exposition_loc <- which(str_detect(str_trim(sermon),pattern_exposition))
    exposition <- sermon[exposition_loc]
    exposition <- ifelse(length(exposition)==0,NA,exposition_loc)
    expositions <- c(expositions, exposition)
    
  }
  
  # write metadata to sermons_df
  sermons_df$nr[sermons_df$volume == v] <- sermonnrs
  sermons_df$title[sermons_df$volume == v] <- sermontitles
  sermons_df$exposition[sermons_df$volume == v] <- expositions
  sermons_df$problem[sermons_df$volume ==v] <- ifelse(is.na(sermons_df$nr[sermons_df$volume == v]),TRUE,sermons_df$problem[sermons_df$volume ==v])
  sermons_df$problem[sermons_df$volume ==v] <- ifelse(is.na(sermons_df$title[sermons_df$volume == v]),TRUE,sermons_df$problem[sermons_df$volume ==v])
}

# write dataframe to gen --------------------------------------------------

print("Volumes processed! Titles, sermon-numbers and expositions extracted from volumes")
write.csv(sermons_df, here("gen/spurgeon","spurgeon_sermons.csv"), row.names = FALSE)

# housekeeping
rm(list = ls())
