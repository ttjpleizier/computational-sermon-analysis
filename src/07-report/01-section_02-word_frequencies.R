# calculate word frequencies, compounds, collocations, kwics
# dataset: Spurgeon/Newman
# Theo Pleizier, 29 November 2023
# script belongs to the bookchapter 'computational sermon analysis'
# section 2: digital theology & sermon analysis


library(here)
library(stringr)
library(dplyr)
library(quanteda)
library(quanteda.textstats)

# load balanced corpus
# check existence of file (pipeline)
load(here("gen","balanced_texts_corpus"))
load(here("gen","balanced_sample_corpus"))

balanced_corpus <- balanced_texts

# create token and dfm objects
balanced_tokens <- tokens(balanced_corpus, remove_punct = TRUE, remove_numbers = TRUE)
balanced_dfm <- dfm(balanced_tokens)

set.seed(20231130)
example_dfm <- balanced_dfm[sample(1:length(balanced_corpus),5),10:19]


# calculate top_nr features, multiple combinations
top_nr <- 15
balanced_topfeatures <- topfeatures(balanced_dfm, n = top_nr)
balanced_nostop_topfeatures <- topfeatures(dfm_remove(balanced_dfm, stopwords("en")), n = top_nr)
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



# compounds 'st'

st_tokens <- tokens_compound(tokens_subset(balanced_tokens, preacher == "newman"), pattern = phrase(c("st *")))
st_freq <- textstat_frequency(dfm_keep(dfm(st_tokens), pattern = "st_*"))

example_st <- head(st_freq, n = 5)


# collocations god's

newman_coll <- textstat_collocations(tokens_subset(balanced_tokens, preacher == "newman"), size = 2, min_count = 2)
newman_coll_str <- newman_coll[str_starts(newman_coll$collocation, "god's"),] 
example_coll <- head(newman_coll_str[order(newman_coll_str$count, decreasing = TRUE),], 6)

# keyword in context

set.seed(20231130)
example_kwic <- tokens_subset(balanced_tokens, preacher == "newman") %>% 
  kwic(pattern = phrase("God's grace")) %>% 
  sample_n(5)

flextable::flextable(as.data.frame(example_kwic)) %>% 
  flextable::delete_columns(j="pattern")

library(flextable)


