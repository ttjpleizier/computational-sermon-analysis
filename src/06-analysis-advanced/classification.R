# classification.R
# goal: predict preacher from corpus of sermons
# Theo Pleizier, 8-12-2023
# script belongs to project computational sermon analysis

# load packages
library(here)
library(stringr)
library(quanteda)
library(quanteda.textmodels)
library(caret)

# test if corpora exist, otherwise previous step in the pipeline
if(!file.exists(here("gen","newman_corpus"))) source(here("src/04-corpus","newman_corpus.R"))
if(!file.exists(here("gen","spurgeon_corpus"))) source(here("src/04-corpus","spurgeon_corpus.R"))

# load corpora
load(file = here("gen", "newman_corpus"))
load(file = here("gen","spurgeon_corpus"))

# settings for classification model 
sample_per_preacher <- 100 # sermons per preacher
chunk_size <- 200 # tokens per chunk
window <- 10 # overlap between chunks

# load special function for tokenisation
source(here("src/06-analysis-advanced","function_features_preacher.R"))

# draw sample from both corpora 
newman_sample <- corpus_sample(newman_corpus, size = sample_per_preacher)
spurgeon_sample <- corpus_subset(spurgeon_corpus, year < 1891) # only sermons before his death in 1892
spurgeon_sample <- corpus_sample(spurgeon_sample, size = sample_per_preacher) 

# merge both samples into one dataset (corpus)
docvars(newman_sample, field = "preacher") <- "newman"
docvars(newman_sample, field = "id") <- paste0(newman_sample$nr,"_",newman_sample$volume)
docvars(spurgeon_sample, field = "preacher") <- "spurgeon"
docvars(spurgeon_sample, field = "id") <- paste0(spurgeon_sample$nr,"_",spurgeon_sample$volume)

docvars(newman_sample)[c(1:5)] <- NULL
docvars(spurgeon_sample)[c(1:10)] <- NULL

corpus_sermons <- c(newman_sample,spurgeon_sample)

# tokenize combined corpus
tokens_chunks_sermons <- NULL
for (s in 1:ndoc(corpus_sermons)){
  selected_sermon <- corpus_sermons[corpus_sermons$id==corpus_sermons$id[s]]
  tokens_chunks_sermon <- tokenize_sermon(selected_sermon, 
                                          chunksize = chunk_size,
                                          chunkoverlap = window)
  tokens_chunks_sermon$preacher <- selected_sermon$preacher
  if (s > 1){
    tokens_chunks_sermons <- c(tokens_chunks_sermons, tokens_chunks_sermon)
  } else {
    tokens_chunks_sermons <- tokens_chunks_sermon
  }
}

# create balanced (50/50) dataset for machine learning
prop_preachers <- prop.table(table(tokens_chunks_sermons$preacher)) 
min_preacher <- min(names(prop_preachers))
min_sample <- sum(tokens_chunks_sermons$preacher == min_preacher)
min_chunks <- tokens_chunks_sermons[tokens_chunks_sermons$preacher == min(names(prop_preachers))]
max_chunks <- tokens_chunks_sermons[tokens_chunks_sermons$preacher == max(names(prop_preachers))]
balanced_chunks1 <- sample(max_chunks, length(min_chunks), replace = FALSE)
balanced_chunks <- c(balanced_chunks1,min_chunks)

save(balanced_chunks, file = here("gen", "balanced_chunks"))

# create dfm
balanced_chunks$id_numeric <- 1:ndoc(balanced_chunks)

dfm_sermons <- dfm(balanced_chunks)

id_train <- sample(1:ndoc(balanced_chunks), .8*ndoc(balanced_chunks))
prop.table(table(balanced_chunks$preacher[id_train])) # check balance newman/spurgeon

# split training en test sets
dfm_sermons_train <- dfm_subset(dfm_sermons, id_numeric %in% id_train)
dfm_sermons_test <- dfm_subset(dfm_sermons, !id_numeric %in% id_train)

# build classifier: naive bayes from quanteda textmodels
sermons_model <- textmodel_nb(dfm_sermons_train, dfm_sermons_train$preacher)

# explore the model
summary(sermons_model)
sermons_model$x # dfm used for the model
unique(docvars(sermons_model$x)$id) #sermon id-s newman/spurgeon

# test model; match features test/training set
dfm_sermons_match <- dfm_match(dfm_sermons_test, features = featnames(dfm_sermons_train))

actual_class <- dfm_sermons_match$preacher
predicted_class <- predict(sermons_model, newdata = dfm_sermons_match)
tab_class <- table(actual_class,predicted_class)
tab_class

# test performance
confusionmatrix <- confusionMatrix(tab_class, mode = "everything", positive = "newman")
confusionmatrix
confusionmatrix$overall[["Accuracy"]]

# results: 
# 100 sermons per preacher, balanced chunks, 200 chunksize, 10 overlap
# Accuracy 0.97

# save model for later predictions
save(sermons_model, file = here("gen",
                                paste0("preacher-model_chunksize",chunk_size,
                                       "_sermons",2*sample_per_preacher)))
