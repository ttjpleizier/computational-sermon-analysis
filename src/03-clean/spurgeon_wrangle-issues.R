# wrangle issues: manual changes in the dataset

# collect errors in metadata 
# Theo Pleizier, 14-3-2023

# TODO: nog oplossing bedenken voor de volgende twee preken met expositions!!!

# 'verdwenen' preek "Prisoners Delivered", nr 2883. Start op regel 608 van /full/2882
# S2882 <- fetch_sermonfile(2882)
# volume 50: 2882 starts at 11416, ends at 12656; 2883 starts at 11416 + 608
# heading van 2883 eindigt met Zechariah 9:11,12.
# [1] "preeknr 2882 :    EXPOSITION BY C. H. SPURGEON: PSALMS 32; 130." HOORT BIJ 2882
# [1] "preeknr 2882 :    EXPOSITION BY C. H. SPURGEON: ZECHARIAH9." HOORT BIJ 2883 (VERDWENEN!)

# TODO: zitten er andere gaten in de nummering? 
# Zijn die preken ook opgenomen bij een andere preek?

# Twee expositions in één bestand (voorlopig niets mee doen, pas als ik iets met de expositions wil)
# [1] "preeknr 3056 :    EXPOSITION BY C. H. SPURGEON: PSALM51." HOORT BIJ 3056
# [1] "preeknr 3056 :    EXPOSITION BY C. H. SPURGEON: JOHN21." HOORT BIJ 3056 (NA PS51 EXPO)


library(stringr)

# adapt titles of sermons
sermons_df$title[sermons_df$nr == 2400] <- "\"Escape for Your Life!\"" # error in title

# sermonnrs 2351, 2360: read-year 1894 iso 1984
#read_year <- year(ymd(metadata$read))
#metadata[which(read_year == 1984),] 
sermons_df$read[sermons_df$nr %in% c(2351,2360)] <- str_replace(sermons_df$read[sermons_df$nr %in% c(2351,2360)], "1984", "1894")
#metadata$read[metadata$nr %in% c(2351,2360)]

# sermonnrs 

# exposition before sermon, sermon-start wrongly assumed from the data
sermons_df$start_sermon[sermons_df$nr == 59] <- 229
