# create table with shared texts/chapters
# Theo Pleizier, 11 december 2023
# script belongs to project computational sermon analysis

library(here)
library(quanteda)


load(here::here("gen","balanced_chapters_corpus"))
load(here::here("gen","balanced_texts_corpus"))


overview_scripture <- data.frame(text = balanced_chapters$chapter,
           preacher = balanced_chapters$preacher,
           volume = balanced_chapters$volume,
           nr = balanced_chapters$nr,
           corpus = "chapters")

overview_scripture <- rbind(overview_scripture,
      data.frame(text = balanced_texts$scripture,
                 preacher = balanced_texts$preacher,
                 volume = balanced_texts$volume,
                 nr = balanced_texts$nr,
                 corpus = "texts"))

overview_scripture$volume_nr <- paste0(overview_scripture$volume,"_",overview_scripture$nr)

overview_table <- tidyr::pivot_wider(overview_scripture,
                   id_cols = c(corpus,text),
                   names_from = preacher, values_from = volume_nr)

write.csv(overview_table, here("data","list_chapters-texts.csv"), row.names = FALSE)
