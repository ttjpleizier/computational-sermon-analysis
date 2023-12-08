# process 63 downloaded volumes with Spurgeon's sermons
# Script locates the starting lines for new sermons in the volumes
# Theo Pleizier, (6-3-2023) 4-12-2023
# script belongs to project 'computational sermon analysis'

library(here)
library(stringr)

inputpath <- "gen/spurgeon/volumes"
volumes <- list.files(here(inputpath))

# check if volume files are available, if not fetch the files from the website
if (length(volumes)==0) {
  source(here("src/01-fetch","fetch_spurgeon.R"))
  volumes <- list.files(here(inputpath))
}

# create empty tables (dataframes) with data on volumes and on sermons
volumes_df <- data.frame(volume = NULL, sermons = NULL, header_end = NULL, footer_start = NULL)
sermons_df <- data.frame(volume = NULL, volumefile = NULL, start = NULL, end = NULL)

# a horizontal line of 66 "_" in the volume files indicates the start of a new sermon
hor_line <- paste0(rep("_", 66), collapse = "")

# loop over all volumes with sermons
for (v in seq_along(volumes)){
  textvolume <- readLines(here(inputpath,volumes[v])) # read volume file
  
  # determine file structure: header / sermons / footer
  hor_lines <- which(str_trim(textvolume) == hor_line)
  index_line <- which(str_detect(textvolume, "Indexes"))
  hor_above_index <- hor_lines[which(sort(c(hor_lines,index_line)) == index_line) - 1]
  header <- textvolume[1:hor_lines[2]] # header: everything between first and second horizontal line
  sermons <- textvolume[hor_lines[2]:(hor_above_index-1)] # sermons: start with second horizontal line
  footer <- textvolume[hor_above_index:length(textvolume)] # footer: starts with horizontal line above "Indexes"
  
  # locate individual sermons
  no_sermons <- which(str_detect(sermons, hor_line))
  sermons_start <- hor_lines[-c(1,which(hor_lines >= hor_above_index))]
  sermons_end <- c(sermons_start[-1], hor_above_index)
  sermons_end <- as.integer(sermons_end - 1)
  
  # create tables with volumes and sermons 
  volumes_df <- rbind(volumes_df, data.frame(volume = volumes[v], 
                                             sermons = length(no_sermons),
                                             header_end = length(header), 
                                             footer_start = hor_above_index - 1))
  
  sermons_df <- rbind(sermons_df, data.frame(volume = v, 
                                             volumefile = rep(volumes[v], length(no_sermons)),
                                             start = sermons_start + 1, # remove horizontal line 
                                             end = sermons_end))
}

# solve problems
# 2883: volume 50, start at 11416 + 608; ends at 12656
# 2882: volume 50, starts at 11416, ends at 11416 + 607
sermons_df$end[sermons_df$volume == 50 & sermons_df$start == 11416] <- 11416 + 607
new_sermon <- data.frame(volume = 50, volumefile = volumes_df$volume[50],
                         start = 11416 + 608, end = 12656)
sermons_df <- rbind(sermons_df,new_sermon)

sermons_df <- sermons_df[order(sermons_df$volume, sermons_df$start),]

# save volume and sermons dfs
print("Volumes processed! Start and endlines of sermons in volumes determined.")
write.csv(volumes_df, here("gen/spurgeon", "spurgeon_volumes.csv"), row.names = FALSE)
write.csv(sermons_df, here("gen/spurgeon", "spurgeon_sermons.csv"), row.names = FALSE)

# house keeping
rm(list = ls())

                             