# calculate word frequencies, compounds, collocations, kwics
# dataset: Spurgeon/Newman
# Theo Pleizier, 29 November 2023
# script belongs to the bookchapter 'computational sermon analysis'
# section 2: digital theology & sermon analysis


# load libraries
library(here)
library(stringr)
library(dplyr)
library(quanteda) # the package for quantitative linguistics
library(quanteda.textstats)

# check existence of corpus files (previous pipeline)
if(!file.exists(here("gen","newman_corpus"))) source(here("src/04-corpus","newman_corpus.R"))
if(!file.exists(here("gen","balanced_texts_corpus"))) source(here("src/04-corpus","balanced_corpus.R"))

# load corpora
load(here("gen","balanced_texts_corpus"))
load(here("gen","balanced_sample_corpus"))
load(here("gen","newman_corpus"))

balanced_corpus <- balanced_texts

# create token and dfm objects from balanced corpus
balanced_tokens <- tokens(balanced_corpus, 
                          remove_punct = TRUE, remove_numbers = TRUE)

balanced_dfm <- dfm(balanced_tokens)

set.seed(20231130) 
example_dfm <- balanced_dfm[sample(1:length(balanced_corpus),5),10:19]


# calculate top_nr features, multiple combinations
top_nr <- 20 
balanced_topfeatures <- topfeatures(balanced_dfm, 
                                    n = top_nr)
balanced_nostop_topfeatures <- topfeatures(dfm_remove(balanced_dfm, stopwords("en")), 
                                           n = top_nr)
group_topfeatures <- topfeatures(dfm_remove(balanced_dfm, 
                                            c(stopwords("en"),
                                              names(balanced_nostop_topfeatures))),
                                            n = top_nr, groups = preacher)
# create dataframe with 6 columns
top_words <- data.frame(all = names(balanced_topfeatures),
                        no_stop = names(balanced_nostop_topfeatures),
                        spurgeon_only = names(group_topfeatures$spurgeon),
                        spurgeon_freq = group_topfeatures$spurgeon,
                        newman_only = names(group_topfeatures$newman),
                        newman_freq = group_topfeatures$newman,
                        row.names = NULL)

top_words

# Newman often uses the prefix 'st' in his sermons to refer to St. Paul etc. 
# create table with compounds with prefix 'st'

st_tokens <- tokens_compound(tokens_subset(balanced_tokens, preacher == "newman"), 
                             pattern = phrase(c("st *")))
st_s <- featnames(dfm_select(dfm(st_tokens), 
                             pattern = "st_.*'s$", 
                             valuetype = "regex"))
st_no.s <- str_remove(st_s, "'s$") 
st_tokens <- tokens_replace(st_tokens, pattern = st_s, replacement = st_no.s)
st_freq <- textstat_frequency(dfm_keep(dfm(st_tokens), pattern = "st_*"))

example_st <- head(st_freq, n = 5)

# create collocations from Newman's sermons: combinations with "god's"

newman_tokens <- tokens(newman_corpus,
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        remove_symbols = TRUE) 

newman_coll <- textstat_collocations(newman_tokens, size = 2, min_count = 2)
newman_coll_str <- newman_coll[str_starts(newman_coll$collocation, "god's"),] 
example_coll <- head(newman_coll_str[order(newman_coll_str$count, decreasing = TRUE),], 6)

example_coll

# example of keyword in context (concordance)

keyphrase <- "God's grace"

set.seed(20231130)
example_kwic <- newman_tokens %>% 
  kwic(pattern = phrase(keyphrase)) %>% # see https://quanteda.io/articles/pkgdown/examples/phrase.html
  sample_n(5)

example_kwic

