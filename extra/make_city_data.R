source("../funs/funs.R")

obj <- dsst_wiki_load("List_of_United_States_cities_by_population")
links <- dsst_wiki_get_links_table(obj, table_num = 5L, column_num = 1L)
docs <- dsst_wiki_make_data(links[seq_len(150),])

library(cleanNLP)
cnlp_init_udpipe("english")

docs <- filter(docs, stringi::stri_length(text) > 0)
anno <- cnlp_annotate(docs)$token

write_csv(docs, file.path("..", "data", "wiki_cities.csv"))
write_csv(anno, file.path("..", "data", "wiki_cities_anno.csv.gz"))
