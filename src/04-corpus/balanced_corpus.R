# create balanced corpora spurgeon-newman
# Theo Pleizier, (April-May 20223) (4-12-2023)
# script belongs to project computational sermon analysis

# the script creates two balanced corpora
# number of sermons: random sample of 150 sermons by newman and spurgeon
# biblical texts: 71 sermons of each preacher, based upon the same biblical texts (chapters)

library(here)
library(stringr)
library(readtext)
library(quanteda)

# settings
inputnewman <- "gen/newman/sermons"
inputspurgeon <- "gen/spurgeon/sermons/body"

if(!file.exists(here("gen", "newman_corpus"))) source(here("src/04-corpus","newman_corpus.R"))
if(!file.exists(here("gen", "spurgeon_corpus"))) source(here("src/04-corpus","spurgeon_corpus.R"))

load(here("gen","newman_corpus"))
load(here("gen","spurgeon_corpus"))

spurgeon_corpus$preacher <- "spurgeon"
newman_corpus$preacher <- "newman"


# balanced corpus: random sample
sample_sermons <- 150
newman_sample <- corpus_sample(newman_corpus, size = sample_sermons, replace = FALSE)
spurgeon_sample <- corpus_sample(spurgeon_corpus, size = sample_sermons, replace = FALSE)
spurgeon_sample$read <- NULL

balanced_sample <- c(newman_sample,spurgeon_sample)
save(balanced_sample, file = here("gen","balanced_sample_corpus"))

# balanced corpus: biblical chapters and biblical texts

spurgeon_texts <- unique(spurgeon_corpus$scripture)
newman_texts <- unique(newman_corpus$scripture)
shared_texts <- newman_texts[newman_texts %in% spurgeon_texts]

spurgeon_corpus$chapter <- str_remove(spurgeon_corpus$scripture, ":.*$")
newman_corpus$chapter <- str_remove(newman_corpus$scripture, ":.*$")

spurgeon_chapters <- unique(spurgeon_corpus$chapter)
newman_chapters <- unique(newman_corpus$chapter)
shared_chapters <- newman_chapters[newman_chapters %in% spurgeon_chapters]

for(b in seq_along(shared_chapters)){
  newman_b <- corpus_sample(corpus_subset(newman_corpus, newman_corpus$chapter ==  shared_chapters[b]), 1)
  spurgeon_b <- corpus_sample(corpus_subset(spurgeon_corpus, spurgeon_corpus$chapter == shared_chapters[b]), 1)
  if(b == 1){
    balanced_chapters <- c(newman_b,spurgeon_b)
  }  else {
    balanced_chapters <- c(balanced_chapters,newman_b,spurgeon_b)
  }
}


save(balanced_chapters, file = here("gen","balanced_chapters_corpus"))

for(t in seq_along(shared_texts)){
  newman_t <- corpus_sample(corpus_subset(newman_corpus, newman_corpus$scripture == shared_texts[t]), 1)
  spurgeon_t <- corpus_sample(corpus_subset(spurgeon_corpus, spurgeon_corpus$scripture == shared_texts[t]), 1)
  if(t == 1){
    balanced_texts <- c(newman_t,spurgeon_t)
  }  else {
    balanced_texts <- c(balanced_texts,newman_t,spurgeon_t)
  }
}

save(balanced_texts, file = here("gen","balanced_texts_corpus"))
