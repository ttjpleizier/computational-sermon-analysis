# classification.R
# goal: predict preacher from corpus of sermons
# Theo Pleizier, 8-12-2023
# script belongs to project computational sermon analysis

library(here)
library(stringr)
library(quanteda)
library(quanteda.textmodels)
library(caret)

if(!file.exists(here("gen","newman_corpus"))) source(here("src/04-corpus","newman_corpus.R"))
if(!file.exists(here("gen","spurgeon_corpus"))) source(here("src/04-corpus","spurgeon_corpus.R"))

load(file = here("gen", "newman_corpus"))
load(file = here("gen","spurgeon_corpus"))

sample_per_preacher <- 100 # sermons per preacher
chunk_size <- 200 # tokens per chunk
window <- 10 # overlap between chunks

source(here("src/06-analysis-advanced","function_features_preacher.R"))
