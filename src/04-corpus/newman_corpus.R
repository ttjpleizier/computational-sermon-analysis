# create newman-corpus
# Theo Pleizier, 5-4-2023
# script belongs to computational sermon analysis

library(here)
library(readtext)
library(quanteda)
library(stringr)

if(!file.exists(here("gen/newman/sermons","01_01.txt"))) source(here("src/03-clean","newman_extract-sermons.R"))
metadata <- read.csv(here("gen/newman","newman_sermons.csv"))

inputpath <- "gen/newman/sermons"

newman_sermons <- readtext(paste0(inputpath,"/*"),
                           docvarsfrom = "filename",
                           docvarnames = c("volume","nr"))

newman_sermons$nr <- 1:nrow(metadata)
#newman_sermons$sermon <- str_remove(metadata$filename,"\\.txt$")
#newman_sermons$title <- metadata$title

newman_sermons$scripture <- paste0(metadata$scripture_book," ",
                               metadata$scripture_chapter,":",
                               metadata$scripture_verse)
newman_sermons$scripture <- str_remove_all(newman_sermons$scripture, ":?NA:?")

newman_sermons$year <- metadata$year
newman_sermons$printed <- metadata$published

# replace strings
newman_sermons$text <- str_replace_all(newman_sermons$text, "St\\.", "St")

#note: tail nog verwijderen? 
#note: {paragraaf} nog verwijderen?

newman_corpus <- corpus(newman_sermons)

summary(corpus_sample(newman_corpus, 5))

save(newman_corpus, file = here("gen","newman_corpus"))




