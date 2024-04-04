# medians_plot-sentiments.R
# input: determine-sentiments.R
# Theo Pleizier, 7-12-2023
# belongs to project computational sermon analysis

library(here)
library(tidyverse)
library(quanteda)

if(!file.exists(here("gen","balanced_texts_corpus"))) source(here("src/04-corpus", "balanced_corpus.R"))
load(here("gen","balanced_texts_corpus"))

if(!file.exists(here("gen","sentiments_balanced-texts"))) source(here("src/05-analysis-basic","determine-sentiments.R"))
load(here("gen","sentiments_balanced-texts")) 

df <- sentiments_df

as_tibble(df) %>%  
  select(doc_id,line,sentence,sentimentai) %>% 
  sample_n(10) 

# for word vectors, glove: https://emilhvitfeldt.github.io/textdata/reference/embedding_glove.html
# library(textdata) # textdata package Hvitfeldt/Silge https://github.com/EmilHvitfeldt/textdata

# median-values per sermon
sermons <- unique(df$doc_id)

sentiment_medians <- sapply(sermons, function(x){
  median(df$sentimentai[df$doc_id == x])
}, simplify = TRUE)

median_per_sermon <- data.frame(doc_id = sermons,
                                median = sentiment_medians,
                                preacher = balanced_texts$preacher[docid(balanced_texts) == sermons])

median_per_sermon <- median_per_sermon %>%
  left_join(df[,c("doc_id","bible")], 
            by = "doc_id",
            multiple = "first") %>% 
  #mutate(preacher = ifelse(preacher == "newman", "J.H. Newman", "C.H. Spurgeon")) %>% 
  as_tibble()

samplesermons <- sample(1:nrow(median_per_sermon), 5)
samplesermon <- median_per_sermon$doc_id[samplesermons[1]]
print(as.character(samplesermon))
summary(df$sentimentai[df$doc_id == samplesermon])

# compare general tendencies

medians_wide <- median_per_sermon %>%
  select(-doc_id) %>% 
  tidyr::pivot_wider(names_from = "preacher", values_from = "median") 

medians_wide$difference <- apply(medians_wide[,c(2:3)],1,max) - apply(medians_wide[,c(2:3)],1,min)

medians_wide$newton_target <- medians_wide[[2]] - medians_wide[[3]]

sample_medians <- medians_wide[medians_wide$bible %in% sample(medians_wide$bible, 5, replace = FALSE),1:3]
sample_medians

# top and tail of median

medians_wide <- medians_wide %>% arrange(difference) %>% mutate(sermon = row_number())

toptail_medians <- rbind(slice_min(medians_wide, order_by = difference, n = 3),
                         slice_max(medians_wide, order_by = difference, n = 3)) %>% 
  arrange(difference) %>% 
  select(sermon, bible, newman, spurgeon, difference)
  
toptail_medians


# find the most and the least different sermons
least_similar <- medians_wide$bible[medians_wide$difference == max(medians_wide$difference)]
most_similar <- medians_wide$bible[medians_wide$difference == min(medians_wide$difference)]

similar_sermons <- medians_wide[medians_wide$bible %in% c(least_similar,most_similar),1:4]

least_similar_sermon <- medians_wide[medians_wide$bible == least_similar,]
least_similar_sermon

file_newman <- docid(balanced_texts)[balanced_texts$scripture %in% least_similar & balanced_texts$preacher == "newman"]
file_spurgeon <- docid(balanced_texts)[balanced_texts$scripture %in% least_similar & balanced_texts$preacher == "spurgeon"]



# plot sentiment similarity for all sermons
plot_similarity <- medians_wide %>% 
  tidyr::pivot_longer(c(2:3),
                      names_to = "preacher", 
                      values_to = "median") %>% 
  arrange(difference) %>% 
  ggplot(aes(x = reorder(bible,difference), 
             y = newton_target)) +
  geom_bar(position = "identity", 
           stat = "identity", 
           alpha = .25) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1)) +
  ggtitle("Newton's sermons are more / less positive than Spurgeon's sermons") +
  labs(x = "", y = "Sentiment similarity")

plot_least_similar <- df %>% 
  #filter(bible %in% c(least_similar,most_similar)) %>% 
  filter(bible %in% least_similar) %>% 
  ggplot(aes(y = sentimentai, x = seg_id, 
             group = bible, fill = preacher)) +
  geom_col() +
  #scale_fill_manual(values = c("#606060","#606060")) +
  #facet_grid(vars(bible), vars(preacher), scales = "free_x") +
  facet_wrap(~ preacher, scales = "free_x", ncol = 1) +
  labs(y = "sentiment.ai score", 
       x = "lines of the sermon") +
  theme(legend.position = "none",
        panel.background = element_blank())

plot_least_similar 

plot_least_similar_gray <- plot_least_similar +
  scale_fill_manual(values = c("#606060","#606060"))

ggsave("plot_sentiments.tiff", 
       plot = plot_least_similar_gray, 
       path = here("gen/images"), 
       width = 6,
       height = 4,
       dpi = 600)

ggsave("plot_sentiments-s.tiff", 
       plot = plot_least_similar_gray, 
       path = here("gen/images"), 
       width = 6,
       height = 4,
       dpi = 100)

plot_least_similar_color <- plot_least_similar +
  scale_fill_manual(values = c("#5D3A9B","#E66100"))

ggsave("plot_sentiments_color.tiff", 
       plot = plot_least_similar_color, 
       path = here("gen/images"), 
       width = 6,
       height = 4,
       dpi = 600)

