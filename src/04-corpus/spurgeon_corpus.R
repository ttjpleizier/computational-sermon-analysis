# create corpus spurgon: quanteda with readtext
# Theo Pleizier (13-3-2023) 4-12-2023
# script belongs to computational sermon analysis

library(here)
library(quanteda)
library(readtext)
library(stringr)

sermonspath <- "gen/spurgeon/sermons/body/"

if(!file.exists(here(sermonspath,"0001_1.txt"))) source(here("src/03-clean","spurgeon_extract-sermons.R"))

fetch_sermonfile <- function(x, #x is sermonid
                             path = "gen/spurgeon/sermons",
                             type = "full"){
  x <- stringr::str_pad(x, 4, pad = "0")
  selectfiles <- list.files(path = paste0(path,"/",type), 
                            pattern = paste0("^",x), 
                            recursive = TRUE, full.names = TRUE)
  selectfile <- selectfiles[str_detect(selectfiles, type)] 
  findsermon <- selectfile
  sermon <- readLines(findsermon[1])
  return(sermon)
}


metadata <- read.csv(here("gen/spurgeon","spurgeon_sermons.csv"))
metadata <- metadata[metadata$include_corpus,]

# load sermons in readtext dataframe 
sermons <- readtext(paste0(sermonspath,"*"),
                    docvarsfrom = "filename",
                    docvarnames = c("nr","volume"))

# wrangle 
#sermons$text <- str_squish(sermons$text) # remove whitespace from documents # levert probleem op bij corpus_reshape paragrafen
sermons$text <- str_replace_all(sermons$text, "St\\.", "St")
sermons$text <- str_replace_all(sermons$text, "--", " - ")

# add metadata as docvars
#sermons$title <- metadata$title
sermons$scripture <- metadata$barnes_scripture
sermons$year <- metadata$delivered_year
sermons$delivered <- metadata$delivered
sermons$sunday <- metadata$sunday
sermons$weekday <- metadata$weekday
sermons$read <- metadata$read
sermons$published <- metadata$published
sermons$printed <- metadata$publication


# quanteda corpus
spurgeon_corpus <- corpus(sermons)
print(object.size(spurgeon_corpus), units = "MB")

summary(corpus_sample(spurgeon_corpus, 5))

#save(spurgeon_sermons, file = here("gen", "spurgeon_sermons"))
save(spurgeon_corpus, file = here("gen", "spurgeon_corpus"))





