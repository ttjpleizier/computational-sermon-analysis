# fetch Newman's Anglican sermons from https://www.newmanreader.org/works/
# Theo Pleizier, (20-3-2023) 4-12-2023
# script belongs to project 'computational sermon analysis'

# load packages
library(here)
library(rvest)
library(stringr)

# construction of urls

# collect volumes Plain & Parochial Sermons (PPS)
volumes <- 8
newmanurl <- "https://www.newmanreader.org/works/parochial/volume"
newmanurls <- paste0(newmanurl,1:volumes,"/")
outputpath <-"gen/newman/txt/" 
if(!dir.exists(here(outputpath))) dir.create(here(outputpath), recursive = TRUE)

# add volume Occasional Sermons
# 26 sermons, occasional, parish before 1843; includes Newman's last Anglican sermon:  the parting of friends
newmanurls <- c(newmanurls, "https://www.newmanreader.org/works/subjects/")
volumes <- length(newmanurls)

# loop over de pages with volumes
for (u in seq_len(volumes)){
  newman_volume <- read_html(newmanurls[u])
  
  sermonlist <- newman_volume %>% html_nodes("a") %>% html_attr("href")
  sermonlist <- sermonlist[str_detect(sermonlist,"sermon\\d")]
  sermonlist <- sermonlist[!is.na(sermonlist)]
  
  sermonurls <- paste0(newmanurls[u], sermonlist) # list with urls for individual sermons in volume
  
  # scrape content of each sermon 
  for(s in seq_along(sermonurls)){
    newman_sermon <- read_html(sermonurls[s])
    sermontext <- newman_sermon %>% html_text2()
    sermonfile <- paste0(str_pad(u,2, pad = "0"),"_",
                         str_pad(s,2, pad = "0"),".txt")
    writeLines(sermontext, here(outputpath, sermonfile)) # store sermon in file gen/volume_sermonnr.txt
    print(sermonfile) # print filename to console
  }
}

