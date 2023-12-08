# sentiments
# script belongs to project computational sermon analysis
# Theo Pleizier, 7 december 2023

# script uses sentiment.ai: https://benwiseman.github.io/sentiment.ai/

library(here)
library(quanteda)
library(sentiment.ai)

# init conda environment
init_sentiment.ai(method = "conda")

# test if corpus exists, otherwise run previous pipeline: create corpus
if(!file.exists(here("gen","balanced_texts_corpus"))) source(here("src/04-corpus", "balanced_corpus.R"))
load(here("gen","balanced_texts_corpus"))

# reshape corpus: sentence being the unit of analysis
sentences <- corpus_reshape(balanced_texts, "sentences")



# determine sentiment score for each sentence
sentimentsai_score <- sentiment_score(sentences)

# store sentences and sentiments in dataframe
sentiments_df <- data.frame(line = 1:length(sentences), 
                            sentence = as.character(sentences),
                            doc_id = docid(sentences),
                            seg_id = segid(sentences),
                            bible = docvars(sentences,"scripture"),
                            preacher = docvars(sentences, "preacher"),
                            sentimentai = sentimentsai_score,
                            row.names = NULL)

save(sentiments_df, file = here("gen", "sentiments_balanced-texts"))



