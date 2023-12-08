# extract body sermons newman
# Theo Pleizier, (22-3-20230) 4-12-2023
# script belongs to computational sermon analysis

# Note: for each sermon the heading ends at line 3, the actual sermon starts at 4; the footer starts at linenumber in dataframe

library(here)
library(stringr)

inputpath <- "gen/newman/txt"
outputpath <- "gen/newman/sermons"
if(!dir.exists(here(outputpath))) dir.create(here(outputpath), recursive = TRUE)

# check if table with metadata exists, otherwise previous step in pipeline
if(!file.exists(here("gen/newman","newman_sermons.csv"))) source(here("src/03-clean","newman_clean.R"))
newman <- read.csv(here("gen/newman","newman_sermons.csv"))

# loop over all sermons to extract sermon text
for(s in seq_along(newman$filename)){
  #if(s == 3) break #test
  sermonfile <- newman$filename[s]
  print(sermonfile)
  sermontxt <- readLines(here(inputpath,sermonfile))
  
  # clean: tail, pagenrs, m-dashes, biblical references
  sermontxt <- sermontxt[-c(newman$tail[s]:length(sermontxt))] # remove tail
  sermontxt <- sermontxt[5:length(sermontxt)] # remove heading
  sermontxt <- str_replace_all(sermontxt, pattern = "\\{\\d{1,}\\}\\s?", "") # pagenrs {NR}
  sermontxt <- str_replace_all(sermontxt, pattern = "\\\u0097", " - ") # m-dash unicode \u0097
  sermontxt <- str_remove_all(sermontxt, pattern = "\\[.*?\\]")  # notes / biblical references: [XXX]
  
  writeLines(sermontxt, here(outputpath,sermonfile)) # save cleaned data in sermon-files
}


