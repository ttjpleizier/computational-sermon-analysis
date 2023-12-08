# exploratory data analysis
# Theo Pleizier, 5-12-2023
# scrip belongs to computational sermon analysis

library(here)
library(stringr)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)

load(here("gen","newman_corpus"))
load(here("gen","spurgeon_corpus"))
load(here("gen","balanced_sample_corpus"))

# distribution biblical texts

spurgeon_texts <- sort(table(spurgeon_corpus$scripture),decreasing = TRUE)
spurgeon_chapters <- sort(table(str_remove(spurgeon_corpus$scripture,":.*$")),decreasing = TRUE)

top5_spurgeon_texts <- spurgeon_texts[spurgeon_texts > 4]
top_spurgeon_chapters <- spurgeon_chapters[spurgeon_chapters > 19]

data.frame(top_spurgeon_chapters)

# length of sermons over time

library(tidyverse)

# corpus to tibble
newman <- summary(newman_corpus, n = Inf)
newman <- as_tibble(newman)
newman$preacher <- "newman"

newman %>% 
  select(-preacher) %>% 
  sample_n(5)

spurgeon <- summary(spurgeon_corpus, n = Inf)
spurgeon <- as_tibble(spurgeon)
spurgeon$preacher <- "spurgeon"
spurgeon$weekday <- !is.na(spurgeon$weekday) # dichotomized: T/F weekdays
spurgeon$readonly <- !is.na(spurgeon$read) & is.na(spurgeon$year) # 63 preken die alleen gelezen zijn en niet gehouden



# explore dates spurgeon: read and/or delivered
spurgeon %>% 
  #filter(xor(is.na(read),is.na(delivered))) %>% 
  filter(is.na(delivered) & !is.na(year)) %>% 
  select(Text,year,delivered,read,sunday)

delivery_check <- spurgeon %>% 
  filter(!is.na(year) & is.na(delivered)) %>% # 
  select(Text,year,delivered,read,sunday) %>% 
  select(Text) %>% .$Text

save(delivery_check,file = here("gen/spurgeon","delivery_check"))
delivery_check

# 2308 sermons: either delivery or read date
# 135 sermons: no delivery date, but with read date
# 63 sermons: read date only, without delivery year or delivery
# 72 sermons: read date, delivery year only
# 159 sermons: with delivery year, no delivery date > check headings (errors?), saved as delivery_check


# sermon dates in spurgeon
nrow(spurgeon[!spurgeon$sunday & 
                !spurgeon$weekday &
                is.na(spurgeon$delivered) &
                is.na(spurgeon$year),
              c("nr","sunday","weekday","delivered")])
# 583 sermons without any delivery indications

nrow(spurgeon[spurgeon$weekday,]) # 516 sermons on a weekday
nrow(spurgeon[!spurgeon$sunday,]) # 1185 sermons without sunday

# given the complexity of sermon dates in Spurgeon we use
# readr: for sermons that are read
# delivered: for sermons with a date of delivery
# published: for all other sermons, fall back on date of printing


sum(is.na(spurgeon$delivered) & is.na(spurgeon$read))
# sermons without delivery data or date of reading: 618
# these sermons are filtered from the corpus

spurgeon <- filter(spurgeon, !is.na(delivered) | !is.na(read))
# left in the corpus: 2898 sermons with date of delivery or date of reading

# test hypothesis whether weekday sermons have different length!
# spurgeon <- filter(spurgeon, sunday) # 2331 out of 3516 are sunday-sermons


sermons <- select(spurgeon, preacher, Tokens, nr, delivered, read, weekday, readonly)

sermons %>% 
  filter(preacher == "spurgeon") %>% 
  pivot_longer(cols = c("delivered","read"),
               names_to = "communication",
               values_to = "date",
               values_drop_na = TRUE) %>% 
  ggplot(aes(x = lubridate::ymd(date),
             y = Tokens)) +
  geom_point(aes(color = communication), alpha = .2) +
  xlab("")

sermons %>% 
  mutate(delivered = str_extract(delivered, "^\\d{4}")) %>% 
  filter(preacher == "spurgeon") %>% 
  pivot_longer(cols = c("delivered","read"),
               names_to = "communication",
               values_to = "date",
               values_drop_na = TRUE) %>% 
  ggplot(aes(x = communication, y = Tokens)) +
  geom_boxplot() 

# plot shows that sermons that were read, were considerably shorter

# mean length per year
newman_small <- newman %>% 
  select(preacher, Tokens, nr, delivered = year) %>% 
  mutate(read = NA)

spurgeon_small <- spurgeon %>% 
  select(preacher, Tokens, nr, delivered, read) %>% 
  mutate(delivered = as.numeric(str_extract(delivered, "^\\d{4}"))) %>% 
  mutate(read = as.numeric(str_extract(read, "^\\d{4}")))


sermons <- bind_rows(newman_small,spurgeon_small)

sermons %>% 
  pivot_longer(cols = c("delivered","read"),
               names_to = "communication",
               values_to = "date",
               values_drop_na = TRUE) %>% 
  select(preacher, Tokens, date) %>% 
  group_by(preacher, date) %>% 
  summarize(mean_tokens = round(mean(Tokens),0)) %>% 
  ggplot(aes(x = date, y = mean_tokens)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~ preacher, scales = "free", ncol = 1) +
  ylab("mean length in Tokens") +
  xlab("") 

# 1834 increase in Newman: July 1833 return from tour Europe (Dec 1832); start Oxford movement?
# 1891 decrease in Spurgeon: illness & death

table(newman$year)

# lexical dispersion

tokens_newman <- tokens(newman_corpus)

term_newman <- "advent"

dfm_newman <- dfm(tokens_select(tokens_compound(tokens_newman, 
                                                list(c(as.character(tokens(term_newman))))),
                                pattern = sub(" ","_",term_newman), 
                                padding = TRUE))

term2plot <- convert(dfm_newman, to = "data.frame") 
names(term2plot) <- c("docid", "length", "frequency")
term2plot$year <- newman$year
term2plot <- arrange(term2plot, year)

term2plot %>% 
  filter(!is.na(year)) %>% 
  dplyr::mutate(sermon = 1:n()) %>% 
  ggplot(aes(x = sermon, y = .3, color = frequency)) +
  geom_bar(stat = "identity") +
  scale_color_gradient(high = "#000000", low = "#FFFFFF", 
                       breaks = c(min(term2plot$frequency), max(term2plot$frequency)),
                       label = c("Min", "Max")) +
  #scale_x_continuous(breaks = c(seq(1,215,8),215), 
  #                   labels = term2plot$year[c(seq(1,215,8),215)]) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = -90)) +
  labs(y = NULL, x = NULL) + #"Sermons 1834-1843") +
  ggtitle(paste0("Lexical dispersion of <", term_newman,"> in J.H. Newman's sermons"))

# keyness

sermons_tokens <- tokens(balanced_sample, 
                         include_docvars = TRUE, 
                         remove_punct = TRUE, 
                         remove_numbers = TRUE, 
                         remove_symbols = TRUE)

keyness_tokens <- sermons_tokens
keyness_tokens <- tokens_compound(keyness_tokens, pattern = phrase(c("i am","jesus christ", 
                                                                     "state of", "st *")))

keyness_dfm <- dfm_group(dfm(keyness_tokens), groups = preacher)

keyness_spurgeon <- keyness_dfm %>% 
  textstat_keyness(target = "spurgeon",
                   measure = "chi2")

keyness_spurgeon %>% 
  textplot_keyness(n = 20) +
  ggplot2::labs(x = "keyness (chi2)")


# lexical diversity


