# extract the sermons from Spurgeon from the raw volume-files
# Theo Pleizier, (21-3-2023) 4-12-2023
# script belongs to computational sermon analysis

library(here)
library(stringr)

if(!file.exists(here("gen/spurgeon","spurgeon_sermons.csv"))) source(here("src/03-clean","spurgeon_clean.R"))

inputpathvolumes <- "gen/spurgeon/volumes"
outputpath <- "gen/spurgeon/sermons"

headings <- paste0(outputpath,"/full")
noheadings <- paste0(outputpath,"/body")
expositions <- paste0(outputpath, "/expositions")
if(!dir.exists(headings)) dir.create(headings, recursive = TRUE)
if(!dir.exists(noheadings)) dir.create(noheadings, recursive = TRUE)
if(!dir.exists(expositions)) dir.create(expositions, recursive = TRUE)

sermons_df <- read.csv(here("gen/spurgeon","spurgeon_sermons.csv"))

currentvolume <- 0

fetch_volume <- function(x){ # x is row in sermons_df 
  currentvolume <<- sermons_df$volume[x]
  readLines(here(inputpathvolumes,sermons_df$volumefile[x])) # returns text volume
}

extract_sermon <- function(x, # x is sermon number from sermons_df
                           inclexposition = FALSE,
                           heading = TRUE,
                           expositiononly = FALSE){ 
  if(expositiononly) inclexposition <- TRUE 
  ind <- which(sermons_df$nr == x)
  if(currentvolume != sermons_df$volume[ind]) sermons <<- fetch_volume(ind)
  start <- sermons_df$start[ind]
  if(!heading & !is.na(sermons_df$start_sermon[ind])) {
    start <- sermons_df$start[ind] + sermons_df$start_sermon[ind]
  }
  
  sermon <- sermons[start:sermons_df$end[ind]]
  
  if(!inclexposition & !is.na(sermons_df$exposition[ind])){
    if(sermons_df$exposition[ind] > sermons_df$start_sermon[ind]){
      end_exposition <- sermons_df$end[ind]
      start_exposition <- sermons_df$exposition[ind] - sermons_df$start_sermon[ind] # trek start van de preek is al verwerkt!
      sermon <- sermon[-c(start_exposition:end_exposition)]  
    }
  }
  
  # TODO: write function to check final paragraph: end of sermons
  # bijv. waar zit de laatste regel met alleen HOOFDLETTERS (vaak een indicatie dat er iets volgt dat niet bij de preek hoort)
  # of: is het laatste woord van het document 'Amen'? 
  
  if(expositiononly) {
    if(!is.na(sermons_df$exposition[ind])){
      startexp <- sermons_df$start[ind] + sermons_df$exposition[ind]
      end_exposition <- sermons_df$end[ind]
      if(sermons_df$exposition[ind] < sermons_df$start_sermon[ind]){
        end_exposition <- sermons_df$start[ind] + sermons_df$start_sermon[ind] 
      } 
      sermon <- sermons[startexp:end_exposition]  
    } else return(NA)
  }

  sermon

}

# create files: body (without heading and exposition), full sermon (incl heading and exposition), only expositions
for(s in seq_along(sermons_df$nr)){
  if(is.na(sermons_df$start_sermon[s])) next # exclude sermons without heading
  sermon_body <- extract_sermon(sermons_df$nr[s], heading = FALSE, inclexposition = FALSE)
  sermon_full <- extract_sermon(sermons_df$nr[s], heading = TRUE, inclexposition = TRUE)
  expositiontxt <- extract_sermon(sermons_df$nr[s], expositiononly = TRUE, heading = FALSE)
  filename <- sermons_df$filename[s]
  writeLines(sermon_full, paste0(headings,"/",filename))
  writeLines(sermon_body, paste0(noheadings,"/",filename))
  if(sum(is.na(expositiontxt)) < length(expositiontxt))  writeLines(expositiontxt, paste0(expositions,"/",filename))
  print(sermons_df$nr[s])
}



