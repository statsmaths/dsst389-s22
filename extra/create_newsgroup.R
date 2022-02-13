library(tidyverse)
library(stringi)

# http://qwone.com/~jason/20Newsgroups/20news-bydate.tar.gz
# https://archive.ics.uci.edu/ml/machine-learning-databases/20newsgroups-mld/mini_newsgroups.tar.gz

fin <- dir("mini_newsgroups", recursive = TRUE)

x <- rep("", length(fin))
for (i in seq_along(fin))
{
  y <- read_lines(file.path("mini_newsgroups", fin[i]))
  y <- stri_enc_toutf8(y, validate = TRUE)
  y <- y[!stri_detect(y, fixed = ":")]
  y <- y[!stri_detect(y, fixed = ">")]
  y <- y[stri_count(y, regex = "\\W") * 2 < stri_length(y)]
  y <- stri_paste(y, collapse = " ")
  y <- stri_replace_all(y, " ", regex = "[ ]+")
  y <- stri_split_boundaries(y)[[1]]
  y <- y[!stri_detect(y, regex = "[^\\w '\\.,\\?\\-\"\']")]
  x[i] <- stri_paste(y, collapse = "")
}

docs <- tibble(
  doc_id = sprintf("doc%04d", seq_along(x)),
  label = dirname(fin),
  text = x
)

library(cleanNLP)
library(reticulate)
use_virtualenv("~/gh/data-amazon/env", required = TRUE)
cnlp_init_spacy("en_core_web_lg")

anno <- cnlp_annotate(docs)$token

write_csv(docs, "newsgroups.csv.bz2")
write_csv(anno, "newsgroups_token.csv.bz2")
