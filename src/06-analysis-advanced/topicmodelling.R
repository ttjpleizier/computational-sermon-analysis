# topic-modelling.R
# Theo Pleizier, 7-12-2023
# belongs to project computational sermon analysis

# load packages
library(here)
library(quanteda)
library(seededlda)
library(stringr)

# test if corpus exists and load corpus
if(!file.exists(here("gen","spurgeon_corpus"))) source(here("src/04-corpus","spurgeon_corpus.R"))
load(here("gen","spurgeon_corpus"))

# add document variable 'chapter' to the corpus
spurgeon_corpus$scripture_chapter <- str_remove(spurgeon_corpus$scripture, ":\\d{1,3}.*$")

# find most preached chapters
most_preached_chapters <- names(tail(sort(table(spurgeon_corpus$scripture_chapter)),10))
most_preached_chapters <- most_preached_chapters[c(10,9,8,5,3,2)] # keep only unique chapters

# sample sermons from most preached chapters
sample_sermons <- corpus_subset(spurgeon_corpus, scripture_chapter %in% most_preached_chapters)
sample_sermons <- corpus_sample(sample_sermons,
                                replace = FALSE,
                                by = scripture_book,
                                size = 20)

# tokenize and document-term-matrix
toks_books <- tokens(sample_sermons,
                     remove_punct = TRUE, 
                     remove_symbols = TRUE, 
                     remove_numbers = TRUE)
toks_books <- tokens_tolower(toks_books)

toks_books <- tokens_remove(toks_books, 
                            pattern = c(stopwords("en")))

dfmat_books <- dfm(toks_books) %>% 
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile", 
           max_docfreq = 0.1, docfreq_type = "prop") 

# fit model
set.seed(231117)
tmod_lda_books <- textmodel_lda(dfmat_books, k = 13) # from quanteda package

# explore topics
terms(tmod_lda_books, 10)

dfmat_books$topic <- topics(tmod_lda_books) # add topics to documents as docvar
table(dfmat_books$scripture_chapter,dfmat_books$topic)

# save model with topics to a file
save(tmod_lda_books, dfmat_books, file = here("gen","topicmodel"))


