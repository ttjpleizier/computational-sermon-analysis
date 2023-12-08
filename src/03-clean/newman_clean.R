# clean newman: metadata on biblical books and years of preaching
# Theo Pleizier, (21-3-2023) 4-12-2023
# script belongs to computational sermon analysis

# Source biblical texts: https://www.newmanreader.org/guides/scripturetexts.html 
# Source dates: https://www.newmanreader.org/controversies/guides/dates.html
# Source seaons: https://www.newmanreader.org/guides/selection.html



library(here)
library(stringr)

# settings
inputpath <- "gen/newman/txt"

# check if table with newman's sermons exists
if(!file.exists(here("gen/newman","newman_sermons.csv"))) source(here("src/02-process","process_newman.R"))
newman <- read.csv(here("gen/newman","newman_sermons.csv"))

# Metadata: Biblical books

# list with biblical books 
books <- str_trim(str_remove_all(newman$scripture_book,"\\s|\\.$"))
length(unique(books)) # check unique biblebooks in newman's sermons
newman$books_compare <- books

# compare to barnes' register for consistent biblical books
if(!file.exists(here("gen/spurgeon", "index_barnes.csv"))) source(here("src/03-clean","build_index_spurgeon.R"))
barnes <- read.csv(here("gen/spurgeon", "index_barnes.csv"))

barnes_books <- str_extract(barnes$scripture, "\\d?\\s?(\\w+\\s)*?(?=\\d)") # regex from analysis_biblical_books.R
barnes_books <- unique(str_trim(barnes_books))

barnes_df <- data.frame(barnes = barnes_books,
                        compare = str_remove(barnes_books,"\\s"))

compare <- unlist(sapply(newman$books_compare, function(x){
  xlength <- nchar(x) # length of the book in newman differs, compare on lengths
  which(str_sub(barnes_df$compare,1,xlength) == x)[1]
}, simplify = TRUE)) # generates vector with corresponding elements in Barnes

newman$scripture_book <- barnes_df$barnes[compare]
newman$books_compare <- NULL

# Metadata: dates when preached
# metadata: dates of the sermons, dates of the publications

dates_volumes <- c(1834,1835,1836,1839,1840,1842,1842,1843,1843) # https://www.newmanreader.org/works/index.html
newman$published <- sapply(newman$volume, function(x) dates_volumes[x], simplify = TRUE)

# read newman-dates
source(here("src/02-process","process_newman_dates.R"))

# combine dates sermons with metadata dataframe
newman <- dplyr::left_join(newman,newman_dates[c("volume","nr","year")])

# save metadata
write.csv(newman, here("gen/newman", "newman_sermons.csv"), row.names = FALSE)  # metadata is stored in gen/newman/newman_df.csv



