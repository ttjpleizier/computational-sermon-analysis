# creates a csv from a txt file based upon Barnes' docx with metadata on Spurgeon's sermons
# Theo Pleizier, (6-3-2023) 4-12-2023
# script belongs to computational sermon analysis

# information:
# Mark Barnes created a complete index of all Spurgeon's sermons (2013) in a .docx file (download 2-3-2023, in docs/)
# https://community.logos.com/forums/t/44342.aspx edition AGES (version of Spurgeon's sermons in the CCEL database) 

library(here)
library(stringr)

# pipeline starts with text file based upon Mark Barnes (2013) .docx with index
if(!file.exists(here("data/","index-barnes_spurgeon.txt"))){
  stop("Index file (.txt) of Spurgeon's sermons (edition AGES) not found in data/. Download docx: https://community.logos.com/forums/t/44342.aspx")
}

index_spurgeon_raw <- readLines(here("data/", "index-barnes_spurgeon.txt"))
index_spurgeon_raw <- index_spurgeon_raw[-c(1:5)] # first five rows is documentation
index_spurgeon_raw <- str_trim(index_spurgeon_raw)
head(index_spurgeon_raw)

# create vectors for the three indexes
index_rows <- which(str_detect(index_spurgeon_raw,"Index"))
index_spurgeon_raw[index_rows] # index names

index_volume <- index_spurgeon_raw[(index_rows[3]+1):length(index_spurgeon_raw)]

# volume index
head(index_volume)
ind_volumes <- which(str_detect(index_volume,"Headword")) 

# divide vector in lists per volume
nextvolume <- 0
sermons_per_volume <- NULL
for(s in seq_along(index_volume)){
  if(s %in% ind_volumes) nextvolume <- which(s == ind_volumes)
  sermons_per_volume <- c(sermons_per_volume,nextvolume)
}

table(sermons_per_volume)
split_volumes <- split(index_volume, rep(1:length(ind_volumes), table(sermons_per_volume)))

# create dataframe from volumes
volumes_df <- data.frame(volume = NULL, sermon = NULL) # later sermon splitsen in nr, title, scripture

for (v in seq_along(split_volumes)){
  volumes_df <- rbind(volumes_df,
                           data.frame(volume = v, sermon = split_volumes[[v]]))
}

# wrangle dataframe
volumes_df <- volumes_df[!str_detect(volumes_df$sermon,"Headword:Volume"),] # remove lines with headings of volumes
volumes_df[c("sermon","title")] <- str_split_fixed(volumes_df$sermon, "\\s", 2)
volumes_df$sermon <- str_remove(volumes_df$sermon, "^#")
volumes_df[c("title","scripture")] <- str_split_fixed(volumes_df$title, " \\(\\[{2}", 2)
volumes_df$scripture<- str_trim(str_remove(volumes_df$scripture, ">>.*$"))

# solve: duplicated items in the index 
volumes_df$duplicaten <- paste0(volumes_df$volume,volumes_df$title,volumes_df$scripture)
volumes_df$duplicate <- duplicated(volumes_df$duplicaten)
volumes_df$duplicaten <- NULL
volumes_df$roman <- NA

spv <- 1
vnr <- 1
for(i in seq_len(nrow(volumes_df))){
  if(volumes_df$duplicate[i]) next
  if(vnr < volumes_df$volume[i]){
    vnr <- vnr + 1
    spv <- 1
  }
  volumes_df$roman[i] <- spv
  spv <- spv + 1
}

volumes_df$roman <- tolower(as.roman(volumes_df$roman))
volumes_df$changed <- FALSE
volumes_df$remove <- FALSE

# write index to gen
print("Index Barnes imported into csv-file")
write.csv(volumes_df, here("gen/spurgeon","index_barnes.csv"), row.names = FALSE)
write.csv(volumes_df, here("gen/spurgeon","index_barnes_full.csv"), row.names = FALSE)

# house keeping
rm(list = ls())
