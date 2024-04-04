# exploratory data analysis
# Theo Pleizier, 5-12-2023
# script belongs to computational sermon analysis, section 5.1
# Note: the script contains more analyses than presented in the paper

# load packages
library(here)
library(stringr)
library(quanteda) # package for quantitative linguistics
library(quanteda.textplots)
library(quanteda.textstats)
library(tidyverse) # package for data analysis 

# plots in color used contrasts that work for colorblindness
# https://davidmathlogic.com/colorblind/
# orange (#E66100) and purple (#5D3A9B)

# test if corpora exist and load corpora
if(!file.exists(here("gen","spurgeon_corpus"))) source(here("src/04-corpus","spurgeon_corpus.R"))
load(here("gen","spurgeon_corpus"))

if(!file.exists(here("gen","newman_corpus"))) source(here("src/04-corpus","newman_corpus.R"))
load(here("gen","newman_corpus"))

if(!file.exists(here("gen","balanced_texts_corpus"))) source(here("src/04-corpus","balanced_corpus.R"))
load(here("gen","balanced_sample_corpus"))

# select most preached chapters from Spurgeon
spurgeon_texts <- sort(table(spurgeon_corpus$scripture),decreasing = TRUE)
spurgeon_chapters <- str_remove(spurgeon_corpus$scripture, ":.*$")
spurgeon_chapters <- sort(table(spurgeon_chapters), decreasing = TRUE)

top_spurgeon_texts <- spurgeon_texts[spurgeon_texts > 4] # 5 sermons per biblical text or more
top_spurgeon_chapters <- spurgeon_chapters[spurgeon_chapters > 19] # 20 sermons per biblical chapter or more

most_preached <- data.frame(top_spurgeon_chapters)
names(most_preached) <- c("chapter","freq")

# calculate the length of sermons over time

# convert corpus objects to tibble (tidyverse object)
newman <- summary(newman_corpus, n = Inf)
newman <- as_tibble(newman)
newman$preacher <- "newman"

spurgeon <- summary(spurgeon_corpus, n = Inf)
spurgeon <- as_tibble(spurgeon)
spurgeon$preacher <- "spurgeon"
spurgeon$weekday <- !is.na(spurgeon$weekday) # dichotomized: T/F weekdays
spurgeon$readonly <- !is.na(spurgeon$read) & is.na(spurgeon$year) # 63 sermons only read, not delivered?

# explore metadata spurgeon: dates of read and/or delivered sermons
# save the list of sermons for later use, e.g. to check and perhaps repair the dataset
spurgeon %>% 
  filter(is.na(delivered) & !is.na(year)) %>% 
  select(Text,year,delivered,read,sunday)

delivery_check <- spurgeon %>% 
  filter(!is.na(year) & is.na(delivered)) %>% # 
  select(Text,year,delivered,read,sunday) %>% 
  select(Text) %>% .$Text

save(delivery_check,file = here("gen/spurgeon","delivery_check"))

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
# read: for sermons that are read
# delivered: for sermons with a date of delivery
# published: for all other sermons, fall back on date of printing


sum(is.na(spurgeon$delivered) & is.na(spurgeon$read))
# sermons without delivery data or date of reading: 618
# these sermons are removed from the corpus

spurgeon <- filter(spurgeon, !is.na(delivered) | !is.na(read))
# left in the corpus: 2898 sermons with date of delivery or date of reading

# test hypothesis whether weekday sermons have different length!
# spurgeon <- filter(spurgeon, sunday) # 2331 out of 3516 are sunday-sermons

sermons <- select(spurgeon, preacher, Tokens, nr, delivered, read, weekday, readonly)

# plot difference in length (Tokens) between read/delivered sermons by Spurgeon
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

# boxplots to show the central tendencies in the two modes of communication
# result of the plot: delivered sermons are considerably longer
sermons %>% 
  mutate(delivered = str_extract(delivered, "^\\d{4}")) %>% 
  filter(preacher == "spurgeon") %>% 
  pivot_longer(cols = c("delivered","read"),
               names_to = "communication",
               values_to = "date",
               values_drop_na = TRUE) %>% 
  ggplot(aes(x = communication, y = Tokens)) +
  xlab("mode of communication") +
  geom_boxplot() 


# calculate mean sermon-length per year of both preachers

# create a combined dataset with a selected number of variables
newman_selection <- newman %>% 
  select(preacher, Tokens, nr, delivered = year) %>% 
  mutate(read = NA)

spurgeon_selection <- spurgeon %>% 
  select(preacher, Tokens, nr, delivered, read) %>% 
  mutate(delivered = as.numeric(str_extract(delivered, "^\\d{4}"))) %>% 
  mutate(read = as.numeric(str_extract(read, "^\\d{4}")))


sermons <- bind_rows(newman_selection,spurgeon_selection)

# define plot to project the mean length of the sermons of both preachers over time
sermons_length <- sermons %>% 
  pivot_longer(cols = c("delivered","read"),
               names_to = "communication",
               values_to = "date",
               values_drop_na = TRUE) %>% 
  select(preacher, Tokens, date) %>% 
  group_by(preacher, date) %>% 
  summarize(mean_tokens = round(mean(Tokens),0)) 

plot_sermon_length <- sermons_length %>% 
  ggplot(aes(x = date, y = mean_tokens)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~ preacher, scales = "free", ncol = 1) +
  ylab("mean length in Tokens") +
  xlab("") 

plot_sermon_length

# save plot
ggsave("plot_sermon_length.tiff", 
       plot = plot_sermon_length, 
       path = here("gen/images"), 
       width = 6,
       height = 3,
       dpi = 600)

ggsave("plot_sermon_length-s.tiff", 
       plot = plot_sermon_length, 
       path = here("gen/images"), 
       width = 6,
       height = 3,
       dpi = 100)

plot_sermon_length_color <- plot_sermon_length +
  aes(color = preacher) +
  scale_color_manual(values = c("#5D3A9B","#E66100")) + 
  theme(legend.position = "none")
  
ggsave("plot_sermon_length_color.tiff", 
       plot = plot_sermon_length_color, 
       path = here("gen/images"), 
       width = 6,
       height = 3,
       dpi = 600)


# 1834 increase in Newman: July 1833 return from tour Europe (Dec 1832); start Oxford movement?
# 1891 decrease in Spurgeon: illness & death

table(newman$year)

# calculate and plot lexical dispersion

# select terms for lexical dispersion
term_newman <- c("church","world")

set.seed(20231208) # set seed for reproducable sample
tokens_newman <- tokens(corpus_sample(newman_corpus, size = 5, replace = FALSE))

# plot dispersion (quanteda package: textplot_xray)
example_dispersion <- textplot_xray(kwic(tokens_newman, pattern = term_newman), sort = TRUE) +
  #ggtitle(paste("Lexical dispersion of <",paste0(term_newman, collapse = "|"),"> in Newman's sermons")) +
  ggtitle("") +
  ylab("")

# save plot
ggsave("plot_dispersion.tiff", 
       plot = example_dispersion, 
       path = here("gen/images"), 
       width = 6,
       height = 4,
       dpi = 600)

ggsave("plot_dispersion-s.tiff", 
       plot = example_dispersion, 
       path = here("gen/images"), 
       width = 6,
       height = 4,
       dpi = 100)

example_dispersion_color <- example_dispersion + 
  aes(color = keyword) +
  scale_color_manual(values = c("#E66100","#5D3A9B"))

ggsave("plot_dispersion_color.tiff", 
       plot = example_dispersion_color, 
       path = here("gen/images"), 
       width = 6,
       height = 4,
       dpi = 600)


# calculate and plot keyness

# tokenize balanced corpus 

sermons_tokens <- tokens(balanced_sample, 
                         include_docvars = TRUE, 
                         remove_punct = TRUE, 
                         remove_numbers = TRUE, 
                         remove_symbols = TRUE)

keyness_tokens <- sermons_tokens

# combine tokens that re-occur in fixed patterns
keyness_tokens <- tokens_compound(keyness_tokens, 
                                  pattern = phrase(c("i am","jesus christ",
                                                     "state of", "st *")))

# create document feature matrix
keyness_dfm <- dfm_group(dfm(keyness_tokens), groups = preacher)

# calculate keyness
keyness_newman <- keyness_dfm %>% 
  textstat_keyness(target = "newman",
                   measure = "chi2")

# plot keyness
example_keyness <- keyness_newman %>% 
  textplot_keyness(n = 20,
                   color = c("#808080","#D3D3D3")) +
  ggplot2::labs(x = "keyness (chi2)") +
  theme(
    legend.position = c(.25, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

example_keyness

# save plot
ggsave("plot_keyness.tiff", 
       plot = example_keyness, 
       path = here("gen/images"), 
       width = 6,
       height = 7,
       dpi = 600)

ggsave("plot_keyness-s.tiff", 
       plot = example_keyness, 
       path = here("gen/images"), 
       width = 6,
       height = 7,
       dpi = 100)

# plot keyness color
example_keyness_color <- keyness_newman %>% 
  textplot_keyness(n = 20, color = c("#5D3A9B", "#E66100")) +
  ggplot2::labs(x = "keyness (chi2)") +
  theme(
    legend.position = c(.25, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

example_keyness_color

# save plot
ggsave("plot_keyness-color.tiff", 
       plot = example_keyness_color, 
       path = here("gen/images"), 
       width = 6,
       height = 7,
       dpi = 600)
