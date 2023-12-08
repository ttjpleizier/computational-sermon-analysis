# check downloaded sermons against Barnes (2013) index
# Theo Pleizier, (7-3-2023) 4-12-2023
# works with two functions, see below: MAIN LOOP
# only needed in the first round of cleaning: 
# it results in complete list of sermons to be removed from the dataset.

# settings and functions --------------------------------------------------

library(here)
library(stringr)

# check input
if(!file.exists(here("gen/spurgeon","spurgeon_volumes.csv"))) source(here("src/02-process","process_volumes_spurgeon.R"))
if(!file.exists(here("gen/spurgeon","index_barnes.csv"))) source(here("src/03-clean","build_index_spurgeon.R"))

# functions
check_against_barnes <- function(){
  source(here("src/03-clean","spurgeon_remove-sermons.R"))
  sermons_df <<- sermons_df[!sermons_df$remove,]
  index_barnes <<- index_barnes[!index_barnes$remove,]
  # compare volume_df with barnes' index
  sermons_index_barnes <- sapply(unique(index_barnes$volume), function(x){length(index_barnes$sermon[index_barnes$volume==x])})
  sermons_index <- sapply(unique(sermons_df$volume), function(x){length(sermons_df$title[sermons_df$volume==x])})
  difference <<- sermons_index_barnes - sermons_index 
  nrvolumes <- sum(difference != 0) 
  nrsermons <- sum(abs(difference)) 
  checkvolumes_df <- data.frame(volume = paste("volume",1:length(difference)),
                                difference = difference)
  checkvolumes_df <- checkvolumes_df[checkvolumes_df$difference != 0,]
  check_next_volume <<- which(difference != 0)[1]
  print(checkvolumes_df)
  print(paste("In",nrvolumes,"volumes,",nrsermons,"sermons should be checked"))
}

compare_titles <- function(volume_to_check = NA, all = FALSE, view = TRUE){
  if(is.na(volume_to_check)) volume_to_check <- unique(sermons_df$volume)
  closingpattern <- ",\\s(The|A|An)$"
  
  # dataframe: compare titles in barnes with generated dataframe
  comparetitles <- as.data.frame(cbind(sermons_df$volume[!sermons_df$remove],
                                       sermons_df$start[!sermons_df$remove],
                                       sermons_df$title[!sermons_df$remove],
                                       index_barnes$volume[!index_barnes$remove],
                                       index_barnes$title[!index_barnes$remove],
                                       index_barnes$sermon[!index_barnes$remove]))
  names(comparetitles) <- c("volume","startline","sermons","barnes_vol","barnes","barnes_nr")
  
  comparetitles$barnes_new <- comparetitles$barnes
  comparetitles$barnes_new <- paste(str_extract(comparetitles$barnes_new,
                                                closingpattern),
                                    comparetitles$barnes_new)
  comparetitles$barnes_new <- str_remove(comparetitles$barnes_new,"^NA\\s|^,\\s")
  comparetitles$barnes_new <- str_remove(comparetitles$barnes_new,closingpattern)
  
  comparetitles$barnes_new <- tolower(comparetitles$barnes_new)
  comparetitles$barnes_new <- str_remove_all(comparetitles$barnes_new, "[^a-z]")
  comparetitles$sermons_new <- tolower(comparetitles$sermons)
  comparetitles$sermons_new <- str_remove_all(comparetitles$sermons_new, "[^a-z]")
  
  comparetitles$check <- ifelse(comparetitles$sermons_new == comparetitles$barnes_new,F,T)
  comparetitles <<- comparetitles
  if(all){
    if(view) View(comparetitles[(comparetitles$volume %in% volume_to_check 
                        | comparetitles$barnes_vol %in% volume_to_check),])
    sum((comparetitles$volume %in% volume_to_check 
         | comparetitles$barnes_vol %in% volume_to_check))
    
  } else {
    if(view) View(comparetitles[(comparetitles$volume %in% volume_to_check 
                        | comparetitles$barnes_vol %in% volume_to_check) 
                       & comparetitles$check,-c(7:9)])
    sum((comparetitles$volume %in% volume_to_check 
         | comparetitles$barnes_vol %in% volume_to_check)
        & comparetitles$check)
  }
}


# MAIN LOOP ---------------------------------------------------------------

# step 1: check against barnes 
check_against_barnes() # repeat until all differences have been set to zero

# step 2: compare titles
check_volume <- check_next_volume
#check_volume <- 62
compare_titles(check_volume, view = FALSE) # handmatig titels controleren die niet overeenkomen
# step 3: manual changes: remove items from sermons_df or from index_barnes
# add removed items to 'wrangle_remove_sermons.R'

# step 4: back to step 1 until all problems are resolved

# step 5: final overview
#compare_titles(all = FALSE, view = FALSE) # complete overzicht voor controle van titels

# finished checking all 63 volumes: 8-3-2023, 10:34h, 329 titles need to be checked


# calculate value title differences downloads and barnes -------------------------------------------------------------

# tel het aantal letters: hoe kleiner het verschil, hoe meer gelijkend
# tel het aantal verschillende letters: hoe kleiner het verschil, hoe meer gelijkend

letters_title <- function(x){
  x <- as.character(str_split(x,"", simplify = TRUE))
  a <- length(x) # aantal letters
  u <- length(unique(x)) # aantal unieke letters
  return(c(a,u))
}

ratio_sermonbarnes <- function(x,y){
  ratio <- min(c(x,y)) / max(c(x,y))
  return(ratio)
  }

calc_value_titlewords <- function(x){ # x is row in dataframe comparetitles
  #word_sermons <- comparetitles$sermons_new[comparetitles$check][x]
  word_sermons <- comparetitles$sermons_new[x]
  #word_barnes <- comparetitles$barnes_new[comparetitles$check][x]
  word_barnes <- comparetitles$barnes_new[x]
  #print(paste(word_sermons, ". Barnes:",word_barnes))
  values_sermons <- letters_title(word_sermons)
  values_barnes <- letters_title(word_barnes)
  compare_length <- ratio_sermonbarnes(values_sermons[1],values_barnes[1])
  compare_unique <- ratio_sermonbarnes(values_sermons[2],values_barnes[2])
  calculated_value <- round(mean(c(compare_length,compare_unique)),2)
  return(calculated_value)
}

comparetitles$value_compare_titles <- 1 # new column: perfect match is standard value
comparetitles$value_compare_titles[comparetitles$check] <- as.numeric(sapply(which(comparetitles$check), calc_value_titlewords))

titles_to_be_checked <- comparetitles[comparetitles$value_compare_titles < .9,
                                      c("volume","startline","sermons","barnes","value_compare_titles")]
titles_to_be_checked <- titles_to_be_checked[order(titles_to_be_checked$value_compare_titles, decreasing = FALSE),]

# TODO: De titels van 80 preken moeten beter bekeken worden


# save DFs -----------------------------------------------------------------
sermons_df$remove <- NULL
sermons_df$problem <- NULL
sermons_df$barnes_titles <- comparetitles$barnes # needed for joining numbers and verses

print(paste("Titles Barnes' index combined with downloaded Volumes. Differences resolved by removing",nrow(sermons_df_original)-nrow(sermons_df),"sermons from the dataset."))
write.csv(sermons_df,here("gen/spurgeon","spurgeon_sermons.csv"), row.names = FALSE)
#write.csv(sermons_df,here("gen/spurgeon","spurgeon_sermons_original.csv"), row.names = FALSE)
write.csv(index_barnes, here("gen/spurgeon","index_barnes.csv"), row.names = FALSE)
write.csv(index_barnes, here("gen/spurgeon","index_barnes_original.csv"), row.names = FALSE)
if(nrow(titles_to_be_checked)>0){
  print(paste(nrow(titles_to_be_checked),"Titles from downloaded files need to be checked against Barnes' index, see check_titles.csv."))
  write.csv(titles_to_be_checked, here("gen/spurgeon","check_titles.csv"), row.names = FALSE)
}

# house keeping
rm(list = ls())

