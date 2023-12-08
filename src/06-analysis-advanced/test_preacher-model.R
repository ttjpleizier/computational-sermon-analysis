# test_model-preacher.R
# Theo Pleizier, Nov/Dec 2023
# script belongs to project computational sermon analysis


library(here)
library(quanteda)
library(quanteda.textmodels)
library(stringr)
library(ggplot2)

source(here("src","function_features_preacher.R"))

model_name <- "preacher-model_chunksize200_sermons200"

if(!file.exists(here("gen",model_name))) source(here("src/06-analysis-advanced","classification.R"))
load(file = here("gen",model_name))
chunk_size <- as.numeric(str_remove(str_extract(model_name,"chunksize\\d{2,3}"),"chunksize"))


# laat andere preken
lambeth_corpus <- readtext::readtext(here("data/testset_lambeth/","*"))
lambeth_corpus <- corpus(lambeth_corpus)

save(lambeth_corpus, file = here("gen","lambeth-corpus"))

head(summary(lambeth_corpus))

chunk_overlap <- 175

toks_lambeth <- tokenize_sermon(x = lambeth_corpus, 
                                chunksize = chunk_size, 
                                chunkoverlap = chunk_overlap)


dfm_lambeth <- dfm(toks_lambeth)

predicted_class <- predict(sermons_model, 
                        newdata = dfm_match(dfm_lambeth, 
                                            features = featnames(sermons_model$x)))

predicted_class # voorspelt per chunk of het spurgeon/newman is

predict_df <- as.data.frame(predicted_class)

predict_df$sermon <- rownames(predict_df)
predict_df$preacher <- str_extract(predict_df$sermon, "^\\w+")
#predict_df[c("preacher","occasion","year")] <- str_split_fixed(predict_df$preacher, "_",3)
predict_df$time <- as.integer(str_extract(predict_df$sermon, pattern = "\\d{1,3}$"))
rownames(predict_df) <- NULL

# plot spurgeon/newman dispersion through the sermons

# info on archbischops: https://en.wikipedia.org/wiki/List_of_archbishops_of_Canterbury
predict_df$preacher_info[str_starts(predict_df$preacher, "ramsey")] <- "1961-1974 Michael Ramsey"
predict_df$preacher_info[str_starts(predict_df$preacher, "carey")] <- "1991-2002 George Carey"
predict_df$preacher_info[str_starts(predict_df$preacher, "williams")] <- "2002-2012 Rowan Williams"
predict_df$preacher_info[str_starts(predict_df$preacher, "welby")] <- "2013- Justin Welby"


predict_lambeth <- predict_df
save(predict_lambeth, file = here("gen","predict_lambeth"))


plot_lambeth <- ggplot(predict_lambeth) +
  geom_tile(aes(y = preacher, 
                x = time, 
                fill = predicted_class, 
                height = 0.5),
            color = "white") +
  facet_wrap(~ preacher_info, scales = "free_y", ncol = 1) +
  xlab(paste0("time (x ",chunk_size-chunk_overlap," tokens)")) + 
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "grey80", linetype = "dotted"), 
        legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  #scale_fill_manual(values = c(kleurja, kleurnee)) +
  scale_x_continuous(breaks = c(1, seq(10,max(predict_lambeth$time),10))) +
  ylab("")

plot_lambeth
