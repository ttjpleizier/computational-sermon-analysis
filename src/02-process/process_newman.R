# processes files with Newman's sermons
# create table with metadata
# Theo Pleizier, (20-3-2023) 4-12-2023
# script belongs to project 'computational sermon analysis'

library(here)
library(stringr)
library(readtext)

inputpath <- "gen/newman/txt"

# check if sermons are downloaded
if(length(list.files(path = here(inputpath))) == 0) source(here("src/01-fetch","fetch_newman.R"))
sermonfiles <- list.files(path = here(inputpath), full.names = TRUE)

# create empty table (dataframe) to store metadata
sermons <- data.frame(volume = NULL, sermon = NULL, title = NULL, scripture = NULL)

# fill table through looping over all files
for(s in seq_along(sermonfiles)){
  sermontext <- readLines(sermonfiles[s])
  sermonvol <- str_extract(sermonfiles[s], "\\d(?=_\\d{1,2}\\.txt$)") # regex: volume-number
  sermonnr <- str_extract(sermonfiles[s], "\\d{1,2}(?=\\.txt$)") # regex: sermon-number
  sermonfile <- str_extract(sermonfiles[s], "\\d{2}_\\d{2}\\.txt$") # regex: filename
  sermon <- data.frame(volume = sermonvol, nr = sermonnr,
                       title = sermontext[1], 
                       scripture = sermontext[3], # heading is consistent: biblical text in line 3
                       filename = sermonfile)
  sermons <- rbind(sermons, sermon)
}

# process title column
sermons$title <- str_extract(sermons$title, "(?<=\\d\\.\\s)\\w+(.*)?$") # remove 'Sermon nr' from title field to extract the title of the sermon

# process scripture column: text, book, chapter, verse
sermons$scripture_text <- str_extract(sermons$scripture, '(?<=\\"\\s)(.*)?$')
sermons$scripture_book <- str_extract(sermons$scripture_text, "(\\d\\s)?\\w+\\.?")
sermons$scripture_chapter <- str_extract(sermons$scripture_text, "(?<=\\s)[ivxlc].*?\\..*?$")
sermons$scripture_verse <- str_trim(str_extract(sermons$scripture_chapter, "\\s\\d{1,2}.*?$"))
sermons$scripture_verse <- str_remove(sermons$scripture_verse, "\\.$")
sermons$scripture_chapter <- str_extract(sermons$scripture_chapter, "\\w+")
sermons$scripture_chapter <- as.integer(as.roman(sermons$scripture_chapter))

# wrangling:
# tail: Top | Contents | Works | Home

sermonfiles <- list.files(here(inputpath))

tail_menuline <- sapply(sermonfiles, function(x){
  sermon <- readLines(here(inputpath,x))
  which(str_starts(sermon,"Top\\s\\|"))[1] # find: Top | Content | Works  
}, simplify = TRUE)

sermons$tail <- tail_menuline

# save metadata
write.csv(sermons, here("gen/newman", "newman_sermons.csv"), row.names = FALSE)  # metadata is stored in gen/newman/newman_df.csv

# housekeeping
rm(list=ls())


