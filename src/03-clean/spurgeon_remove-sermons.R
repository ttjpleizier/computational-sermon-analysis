# remove items from sermons_df & barnes' index
# Theo Pleizier, 8-3-2023

library(here)
sermons_df <<- read.csv(here("gen/spurgeon","spurgeon_sermons.csv"))
index_barnes <<- read.csv(here("gen/spurgeon","index_barnes.csv"))
sermons_df_original <<- sermons_df
index_barnes_original <<- index_barnes

barnes <- TRUE


# REMOVE

# volume02: remove items
sermons_df$remove[sermons_df$volume == 2 & sermons_df$start == 4611] <- TRUE # nas, exposition
sermons_df$remove[sermons_df$volume == 2 & sermons_df$start == 15006] <- TRUE # nas, sermon to pirates
# volume04: remove
sermons_df$remove[sermons_df$volume == 4 & sermons_df$start == 32189 ] <- TRUE # nas, pastoral letter
# volume07: remove
sermons_df$remove[sermons_df$volume == 7 & sermons_df$start == 22026] <- TRUE # nas, footnote
sermons_df$remove[sermons_df$volume == 7 & sermons_df$start == 19568] <- TRUE # nas, exposition doctrines of grace
# volume22: remove
sermons_df$remove[sermons_df$volume == 22 & sermons_df$start == 39020] <- TRUE # the last one (two comes) is the same title as the first sermon in volume 23
# volume25: remove
sermons_df$remove[sermons_df$volume == 25 & sermons_df$start == 16] <- TRUE # not in barnes
sermons_df$remove[sermons_df$volume == 25 & sermons_df$start == 909] <- TRUE # not in barnes
sermons_df$remove[sermons_df$volume == 25 & sermons_df$start == 2240] <- TRUE # not in barnes
sermons_df$remove[sermons_df$volume == 25 & sermons_df$start == 4013] <- TRUE # not in barnes
sermons_df$remove[sermons_df$volume == 25 & sermons_df$start == 5589] <- TRUE # not in barnes
sermons_df$remove[sermons_df$volume == 25 & sermons_df$start == 6668] <- TRUE # not in barnes
sermons_df$remove[sermons_df$volume == 25 & sermons_df$start == 8198] <- TRUE # not in barnes
sermons_df$remove[sermons_df$volume == 25 & sermons_df$start == 8976] <- TRUE # not in barnes
sermons_df$remove[sermons_df$volume == 25 & sermons_df$start == 10654] <- TRUE # not in barnes
# volume27: remove
sermons_df$remove[sermons_df$volume == 27 & sermons_df$start == 37503] <- TRUE # not in barnes
# volume29: remove
sermons_df$remove[sermons_df$volume == 29 & sermons_df$start == 1921] <- TRUE # not in barnes
# volume32: remove
sermons_df$remove[sermons_df$volume == 32 & sermons_df$start == 7065] <- TRUE # not in barnes
# volume37: remove
sermons_df$remove[sermons_df$volume == 37 & sermons_df$start == 34450] <- TRUE # last is same as first in next volume
# volume 53: remove
sermons_df$remove[sermons_df$volume == 53 & sermons_df$start == 4914] <- TRUE # last is same as first in next volume
# volume62: remove
sermons_df$remove[sermons_df$volume == 62 & sermons_df$start == 26987] <- TRUE # not in barnes

if(barnes){
  if(!exists("index_barnes")) index_barnes <<- read.csv(here("gen/spurgeon","index_barnes.csv"))
  remove_sermons <- c(8,40,42,67,82,142,155,269,270,298,331,332,353,383,384,385,386,
                    387,388,389,390,414,423,424,425,426,454,469,470,631,1548,1876,1889,2002,
                    2019,2123,2137,3032) # 2883 removed from this list (part of 2882)
  index_barnes$remove[index_barnes$sermon %in% remove_sermons] <- TRUE
  #index_barnes <<- index_barnes[!index_barnes$sermon %in% remove_sermons,]
  sum(index_barnes$remove)
  write.csv(index_barnes, here("gen/spurgeon","index_barnes.csv"), row.names = FALSE)
}




