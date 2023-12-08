# clean and add metadata to Spurgeon's sermons
# Theo Pleizier, (8-3-2023) 4-12-2023
# metadata: filenames, verses, scripture references, date of publication, seperate headings and sermons for each sermon
# script belongs to computational sermon analysis

library(here)
library(stringr)

# compare titles in database with barnes index
source(here("src/03-clean","spurgeon_compare-titles-barnes.R"))


volumes_df <- read.csv(here("gen/spurgeon", "spurgeon_volumes.csv"))
inputpathvolumes <- "gen/spurgeon/volumes"

sermons_df <- read.csv(here("gen/spurgeon","spurgeon_sermons.csv"))
index_barnes <- read.csv(here("gen/spurgeon", "index_barnes.csv"))


# add sermon numbers and scripture references from Barnes' index
sermons_df$barnes_nrs <- index_barnes$sermon # unique ids sermons from barnes' index
sermons_df$barnes_scripture <- NA
sermons_df$barnes_scripture[sermons_df$barnes_scripture == ""] <- NA # 6 sermons without biblical text > church meetings
sermons_df$barnes_scripture <- index_barnes$scripture # consistent scripture referencing from barnes' index

pattern_book <- "\\d?\\s?(\\w+\\s)*?(?=\\d)"
pattern_chapter <- "\\d?\\s?(\\w+\\s)*?\\d{1,3}"

sermons_df$scripture_book <- str_trim(str_extract(sermons_df$barnes_scripture, pattern_book))
sermons_df$scripture_chapter <- str_remove(str_trim(str_extract(sermons_df$barnes_scripture, pattern_chapter)), pattern_book)
sermons_df$scripture_verse <- NA

# compare sermon numbers from downloaded data with barnes
sermons_with_numbers <- sermons_df[!is.na(sermons_df$nr),] # only numbers that could be fetched from downloaded data
sermons_with_numbers <- 
sermons_with_numbers[str_extract(sermons_with_numbers$nr,"\\d{1,4}") != sermons_with_numbers$barnes_nrs, 
                     c("volume","start","title","nr","barnes_nrs")]

sermons_with_numbers

# 2 sermons have different numbers, these should be checked on the title > checked, 8-3-2023
sermons_df$nr[sermons_df$nr == "98" & sermons_df$start == 24745] <- "101"
sermons_df$nr[sermons_df$nr == "2026" & sermons_df$start == 16675] <- "2029"
rm("sermons_with_numbers")

# replace the extracted numbers by the numbers from Barnes' index 
sermons_df$nr <- sermons_df$barnes_nrs

# construct filenames based upon the numbers
sermons_df$filename <- paste0(str_pad(sermons_df$nr, 4, pad = "0"),
                              "_",sermons_df$volume,".txt")
sermons_df[c("barnes_titles","barnes_nrs")] <- NULL

# years of publication
sermons_df$publication <- NA # column with years of publication of the volume
volumes <- unique(sermons_df$volumefile)
yearspublication <- sapply(volumes, function(x){
  voltext <- readLines(here(inputpathvolumes,x))
  years <- which(str_detect(voltext, "\\d{4}"))
  voltext[years[1]] # first instance of a year is the year of publication
})
yearspublication <- str_extract(yearspublication, "\\d{4}$")
volumes_df$publication <- yearspublication
volumes_df$nr <- 1:length(volumes)

for(v in seq_along(volumes)){
  sermons_df$publication[sermons_df$volumefile == volumes[v]] <- yearspublication[v]
}

# separate headings from sermons
keep <- c("index_barnes","sermons_df","inputpathvolumes", "volumes", "volumes_df")
source(here("src/03-clean","spurgeon_find-scripture-references.R"))
rm(list = ls()[!ls() %in% keep])

# 7 sermons with NAs in 'start_sermon'; do not have 'proper' heading
sum(is.na(sermons_df$start_sermon))
sermons_df[is.na(sermons_df$start_sermon),c("barnes_scripture","nr", "volume", "title")]
sermons_df$include_corpus <- TRUE
sermons_df$include_corpus[is.na(sermons_df$start_sermon)] <- FALSE

# extract date and place of the sermon and whether the sermon was delivered or read
headings <- sermons_df$heading
headings <- str_remove(headings, sermons_df$title)
headings <- str_trim(str_remove(headings, "A (S|s)ermon"))
headings <- str_trim(str_remove(headings, "Number\\s\\d{4}|\\(?No\\.\\s?\\d{1,4}.*?\\)"))
#headings <- str_trim(str_remove(headings, "\".*$"))

# alternative 1: extract published and reading before delivery dates
pattern_months <- tolower(paste0(month.name, collapse ="|"))
pattern_date <- paste0("(",pattern_months, ").\\d{1,2}")
pattern_year <- "\\d{4}"

pattern_published <- "published.*?\\d{4}"
pattern_reading <- "reading.*?\\d{4}"

published <- str_extract(tolower(headings), pattern_published)
reading <- str_extract(tolower(headings), pattern_reading)

reading <- str_replace(reading, "ARPIL", "APRIL") # error detected after analysis
reading <- str_replace(reading, "Febrary", "February") # errors detected after analysis

dateleft <- str_remove_all(tolower(headings), pattern = paste0(pattern_published,"|",pattern_reading))
delivery_date <- str_extract(dateleft, pattern_date)
delivery_year <- str_extract(dateleft, pattern_year)

delivery_year[delivery_year == 2400] <- 1887

delivered <- lubridate::make_date(
  year = delivery_year, 
  month = match(str_extract(delivery_date, pattern_months), tolower(month.name)),
  day = as.numeric(str_extract(delivery_date, "\\d{1,2}")))

read <- lubridate::make_date(
  year = str_extract(reading, pattern_year),
  month = match(str_extract(reading, pattern_months), tolower(month.name)),
  day = as.numeric(str_extract(reading, "\\d{1,2}")))

published <- lubridate::make_date(
  year = str_extract(published, pattern_year),
  month = match(str_extract(published, pattern_months), tolower(month.name)),
  day = as.numeric(str_extract(published, "\\d{1,2}")))

# add to metadata
sermons_df$published <- as.Date(NA)
sermons_df$published <- published

sermons_df$read <- as.Date(NA)
sermons_df$read <- read

sermons_df$delivered <- as.Date(NA)
sermons_df$delivered <- delivered
sermons_df$delivered_year <- delivery_year

# check
headings[str_detect(tolower(headings), "publisher")]
sermons_df[c(1401,1402),]  # the loss of Princess Alice (https://en.wikipedia.org/wiki/Sinking_of_SS_Princess_Alice). How to handle special occasions?

# check years: Spurgeon (1834-1892)
# first sermon preached: 1849? # https://victorianweb.org/religion/sermons/chschron.html
# last sermon preached: June 7, 1891 # https://victorianweb.org/religion/sermons/chsbio.html
# check sermons: range years between first sermon preached and last sermon published

check_years <- sermons_df$nr[!is.na(sermons_df$delivered_year) & (sermons_df$delivered_year < 1849 | sermons_df$delivered_year > 1891)]
sermons_df[sermons_df$nr %in% check_years, c("nr","volumefile", "title", "delivered_year")]

# given the sermons before and after, the years are reconstructed:
# 628: volume11, probably 1865 iso 1835
# 2271: volume38, probably 1890 iso 1900 (Sunday March 2 fits with 1890, not 1900)
# 2429: volume41, probably 1887 iso 1837
# 2437: volume41, probably 1887 iso 1837
# 3443: volume61, probably 1869 iso 1896 (Thursday September 9 fits with 1869, not 1896)

reconstructedyears <- c(1865, 1890, 1887, 1887,1869)
sermons_df$delivered_year[sermons_df$nr %in% c(628,2271,2429,2437,3443)] <- reconstructedyears
lubridate::year(sermons_df$delivered[sermons_df$nr %in% c(628,2271,2429,2437,3443)]) <- reconstructedyears

# details: days, time of day, season
sermons_df$sunday <- str_detect(dateleft, pattern = "sabbath|lord.s.day|sunday")
sermons_df$time <- str_extract(dateleft, pattern = "morning|evening|afternoon|night")

#daysofweek <- c("Sunday","Monday","Tuesday","Wednesday", "Thursday","Friday","Saturday")
daysofweek <- c("Monday","Tuesday","Wednesday", "Thursday","Friday","Saturday")
sermons_df$weekday <- str_extract(dateleft, pattern = tolower(paste0(daysofweek, collapse = "|")))
sermons_df$season <- str_extract(dateleft, pattern = "autumn|winter|summer|spring")

# place HIER GEBLEVEN
dateleft <- str_remove(dateleft, "\\[\\d{1,2}\\]") # footnotes
pattern_place <- paste0(c("(the\\s)?new park street(\\schapel)?(,)? southwark",
                          "exeter.hall(, strand)?",
                          "the music hall, royal surrey gardens",
                          "(the\\s)?metro(tro)?politan(\\stabernacle)?(,)?\\s?(newington)?",
                          "the agricultural hall(\\spulpit)?, newington",
                          "on the grand stand, epsom race-course",
                          "mentone"),
                        collapse = "|")
place <- str_extract(dateleft, pattern_place)

# empty places
nolocation <- dateleft[which(is.na(place))] # 111 sermons without location
# check locations: 
# in a field, conference, chapel, mentone, funeral, assembly, tabernacle, sick room, the agricultural hall, bayswater, crystal palace, christ church, the egyptian hall

pattern_place2 <- c("in a field", "conference", "chapel", "funeral", 
                    "assembly", "tabernacle", "sick room", "the agricultural hall", 
                    "bayswater", "crystal palace", "christ church", "the egyptian hall", "hall")

sum(is.na(str_extract(nolocation, paste0(pattern_place2, collapse = "|")))) # 43 sermons left to check

# CHECK most visible
# 71: we knew, then, we were newly cleansed sinners (checked)
# 66: this address has been lengthened a little in order to fill the usual number of pages(checked)
# 37: will prove exceedingly interesting to us if we have a mind to consider it (checked)
# 11: the first of the public meetings in connection with the opening (checked)
# 5: for the congregation of the rev. c.h. spurgeon took place on tuesday, august 16th, 1859 (checked)

# these results belong to the following sermons in the datast:
sermons_df[c(2088,1832,1327,357,260), c("nr","volume")]

sermons_df$location <- place

# check sunday TRUE and weekdays not NA

sermons_df[sermons_df$sunday & !is.na(sermons_df$weekday),c("nr", "sunday", "weekday", "volume")]
# 76: sermon 81, also delivered on a tuesday, two days later in 1856
# 184: sermon 192, sunday school, delivered on tuesday
sermons_df$sunday[sermons_df$nr==192] <- FALSE
# 212: sermon 220, notice by Spurgeon, Tuesday Evening (sermon delivered on Sunday)
# 307: sermon 318, notice by Spurgeon, Wednesday (sermon delivered on Sunday)
sermons_df$weekday[sermons_df$nr %in% c(220,318)] <- NA
# 386: sermon 408, special occasion (train collisions on Sunday and Monday)
sermons_df$start_sermon[sermons_df$nr == 408] # this sermon will reappear as it has an irregular headinglength (36 lines)

# reorder columns 

sermons_df <- sermons_df[,c(
  "volumefile", "filename", "publication",
  "volume", "nr", "title",
  "start", "end", "start_sermon","exposition",
  "barnes_scripture", "scripture_book", "scripture_chapter", "scripture_verse",
  "delivered", "delivered_year", "sunday", "time", "weekday", "season", "location",
  "heading", "published","read","include_corpus"
)]

# wrangle issues
# process collected errors in metadata (stumbled upon during analyses)
source(here("src/03-clean","spurgeon_wrangle-issues.R"))

# write DF ----------------------------------------------------------------
print("Metadata added: sermon numbers, scripture references, heading lines, years / dates of publication, reading and delivery; places of delivery.")
write.csv(sermons_df, here("gen/spurgeon","spurgeon_sermons.csv"), row.names = FALSE)
write.csv(volumes_df, here("gen/spurgeon", "spurgeon_volumes.csv"), row.names = FALSE)

# housekeeping
rm(list = ls())
