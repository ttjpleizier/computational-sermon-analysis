# fetch Spurgeon's sermons from https://ccel.org/ccel/s/spurgeon/sermons
# Theo Pleizier, (7-3-2023) 4-12-2023
# script belongs to project 'computational sermon analysis'

# the script scrapes all 63 volumes with sermons 
# two plain-text formats available at the source
# xml: https://ccel.org/ccel/s/spurgeon/sermons08.xml (theological markup), incomplete?
# txt: https://ccel.org/ccel/s/spurgeon/sermons15/cache/sermons15.txt
# script only downloads the txt-files 

# load packages
library(here)
library(curl) # library to download files from online repositories
library(stringr)

# construct urls to download the 63 volumes with sermons
spurgeonurl <- "https://ccel.org/ccel/s/spurgeon/sermons"
volumes <- as.character(1:63)
volumes <- paste0("0",volumes)
volumes <- str_sub(volumes, start = nchar(volumes)-1)

spurgeonurls <- paste0(spurgeonurl,volumes)
spurgeonurls_txt <- paste0(spurgeonurls,"/cache/sermons",volumes,".txt")

# create output-folder
outputfolder_txt <- "gen/spurgeon/volumes"
if(!dir.exists(outputfolder_txt)) dir.create(outputfolder_txt, recursive = TRUE)

# loop over the 63 volumes
for(v in seq_along(volumes)){
  filename <- paste0("volume",volumes[v])
  curl_download(spurgeonurls_txt[v], here(outputfolder_txt, paste0(filename,".txt"))) # download volume
  print(volumes[v]) # print volume number to console
}
