# create csv with sermon dates John Henry Newman
# based upon file scraped from newmanreader.org/controversies/guides/dates.html
# Theo Pleizier, december 2023


newman_dates <- read.csv(here("data","newman_dates.csv"), header = FALSE) # constructed from: "https://www.newmanreader.org/controversies/guides/dates.html"

# NOTE on Sources:
# (1) Dates for Parochial and Plain Sermons and Sermons on Subjects of the Day are given at the end of Longmans' Sermons on Subjects of the Day (pp. 411-424);
# (2) Dates for Oxford University Sermons and Sermons Preached on Various Occasions are taken from those volumes;
# (3) Dates for Discourses to Mixed Congregations are unknown to me, except for numbers 1 and 12, which are given by Ian Ker in John Henry Newman, pp. 342, 347.

names(newman_dates) <- c("date","source","title")

# split source column: source - volume - number
newman_dates <- newman_dates[str_starts(newman_dates$source,"PP|SSD"),] # parochial and plain sermons / sermons subjects of the day 
newman_dates[c("source","nr")] <- str_split_fixed(newman_dates$source, "-",2)
newman_dates$volume <- str_extract(newman_dates$source, "\\d$")
newman_dates$source <- str_extract(newman_dates$source,"\\w{3}")
newman_dates$volume[is.na(newman_dates$volume)] <- 9
newman_dates$volume <- as.numeric(newman_dates$volume)
newman_dates$nr <- as.numeric(str_trim(newman_dates$nr))

# split data column: year month day
newman_dates[c("year","month","day")] <- str_split_fixed(newman_dates$date,"\\s",3)
newman_dates$year <- as.numeric(newman_dates$year)
newman_dates$edited <- str_extract(newman_dates$day, "(R|E|E2|A)$") # Key: R=rewritten; E=enlarged; E2=expanded into two; A=altered
newman_dates$day <- str_remove(newman_dates$day, "(R|E|E2|A)$") # Key: R=rewritten; E=enlarged; E2=expanded into two; A=altered

save(newman_dates, file = here("gen/newman","newman_dates.csv"))
