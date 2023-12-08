# function features_predict_preacher
# called from: classification.R
# Theo Pleizier, (Oct 2023) 8-12-2023
# script belongs to project computational sermon analysis


# feature engineering

# stopwords
stopwords_english <- stopwords("en") 
includewords <- stopwords_english[c(1:29,58:80)] #pronouns
stopwords_special <- stopwords_english[!stopwords_english %in% includewords]


# function to tokenize and create chunks of data
tokenize_sermon <- function(x = "this is a test sermon", 
                            chunksize = 100,
                            chunkoverlap = 20){ 
  
  # tokenize sermon with feature engineering:
  # no punctuation, no numbers, no symbols
  # lowercase
  # compound tokens based upon keyness analysis newman/spurgeon
  # stopwords: pronouns included
  
  tokens_sermon <- tokens(x,
                          remove_punct = TRUE, 
                          remove_numbers = TRUE, 
                          remove_symbols = TRUE)
  
  tokens_sermon <- tokens_tolower(tokens_sermon)
  tokens_sermon <- tokens_compound(tokens_sermon, 
                                   pattern = phrase(c("i am",
                                                      "jesus christ", 
                                                      "state of", 
                                                      "st *"))) #keyness in eda.R
  
  tokens_sermon <- tokens_remove(tokens_sermon, stopwords_special)
  
  # divide in chunks
  tokens_sermon <- tokens_chunk(tokens_sermon, size = chunksize, overlap = chunkoverlap)
  
  # remove short chunks
  tokens_sermon <- tokens_sermon[ntoken(tokens_sermon) == chunksize]
  
  return(tokens_sermon)
}

