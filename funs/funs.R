#############################################################################
# Settings and functions for working with the class notes and assignements
# Note that this file will be overwritten. Do not modify directly! Instead,
# create changes in your own funs_custom.R file.
#
# Date: 09 January 2022

#############################################################################
# load a few required packages; others will be referenced with :: and :::
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(stringi))

#############################################################################
# some standard settings
theme_set(theme_minimal())
Sys.setlocale(locale = "en_US.UTF-8")
Sys.setenv(LANG = "en")
options(width = 76L)
options(pillar.min_character_chars = 15)
options(dplyr.summarise.inform = FALSE)
options(readr.show_col_types = FALSE)
options(ggrepel.max.overlaps = Inf)
options(sparse.colnames = TRUE)

#############################################################################
# S3 print methods for model classes
print.enet <- function(x, ...)
{
  cat("\nNumber of lambda: ", length(x$model$lambda), "\n\n")
}

print.knnm <- function(x, ...)
{
  cat("\nk: ", x$k, "\n\n")
}

print.gbmm <- function(x, ...)
{
  cat("\nk: ", x$ntree, "\n\n")
}

print.ldam <- function(x, ...)
{
  cat("\nntopics: ", max(x$docs$topic), "\n\n")
}

#############################################################################
# hidden (start with ".") helper functions
.assert <- function (statement, msg = "")
{
    if (!statement) {
        stop(msg, call. = (msg == ""))
    }
}

.tidy_matrix <- function(
  x, rows_to = "document", cols_to = "term", values_to = "count"
)
{
  if (is.null(rownames(x)))
      stop("input must have row names")
  if (is.null(colnames(x)))
      stop("input must have column names")
  x <- as.matrix(x)
  out <- tibble::tibble(var1 = rownames(x)[row(x)], var2 = colnames(x)[col(x)],
      var3 = as.numeric(x))
  names(out) <- c(rows_to, cols_to, values_to)
  out
}

#############################################################################
# build predictive classification models: elastic net, knn, boosted trees

dsst_enet_build <- function(
  anno, docs,
  min_df = 0.001,
  max_df = 1.0,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  train_var = "train_id",
  label_var = "label",
  alpha = 0.9,
  nfolds = 3,
  trace_it = getOption("dsst.traceit", TRUE),
  lambda_min_ratio = 0.05,
  nlambda = 100,
  seed = 1
)
{
  if (!is.na(seed)) { set.seed(seed) }

  .assert(doc_var %in% names(docs),
          sprintf("doc_var '%s' not found in docs", doc_var))
  .assert(train_var %in% names(docs),
          sprintf("train_var '%s' not found in docs", train_var))
  .assert(label_var %in% names(docs),
          sprintf("label_var '%s' not found in docs", label_var))
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in docs", token_var))

  # create term frequency matrix
  X <- cleanNLP::cnlp_utils_tf(
    anno,
    doc_set = docs[[doc_var]],
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )

  # build model from the training set
  X_train <- X[docs[[train_var]] == "train", ]
  y_train <- docs[[label_var]][docs[[train_var]] == "train"]

  model <- glmnet::cv.glmnet(
    X_train,
    y_train,
    alpha = alpha,
    family = "multinomial",
    nfolds = nfolds,
    trace.it = trace_it,
    lambda.min.ratio = lambda_min_ratio,
    nlambda = nlambda
  )

  # create the predictions
  docs$pred_label <- as.vector(
    glmnet:::predict.cv.glmnet(model, newx = X, type = "class")
  )
  docs$pred_value <- apply(
    glmnet:::predict.cv.glmnet(model, newx = X, type = "response")[,,1], 1, max
  )
  docs$pred_index <- docs[[train_var]]
  docs$real_label <- docs[[label_var]]

  # create the output and return the values
  output <- structure(list(
    model = model,
    docs = ungroup(docs)
  ), class = c('enet'))

  output
}

dsst_knn_build <- function(
  anno,
  docs,
  min_df = 0.001,
  max_df = 1.0,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  train_var = "train_id",
  label_var = "label",
  k = NA,
  seed = 1
)
{
  if (!is.na(seed)) { set.seed(seed) }

  .assert(doc_var %in% names(docs),
          sprintf("doc_var '%s' not found in docs", doc_var))
  .assert(train_var %in% names(docs),
          sprintf("train_var '%s' not found in docs", train_var))
  .assert(label_var %in% names(docs),
          sprintf("label_var '%s' not found in docs", label_var))
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in docs", token_var))

  # create term frequency matrix
  X <- cleanNLP::cnlp_utils_tfidf(
    anno,
    doc_set = docs[[doc_var]],
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )
  y <- docs[[label_var]]

  # build model from the training set
  X_train <- X[docs[[train_var]] == "train", ]
  y_train <- y[docs[[train_var]] == "train"]

  this_k <- ifelse(is.na(k), 20, k)
  model <- FNN::knn(X_train, X, y_train, k = this_k)
  index <- attr(model, "nn.index")

  mat <- matrix(y_train[index], nrow = nrow(index))
  emat <- t(apply(mat != y, 1, cummean))
  cv_train <- apply(emat[docs[[train_var]] == "train",], 2, mean)
  cv_valid <- apply(emat[docs[[train_var]] == "valid",], 2, mean)

  if (is.na(k)) {
    best_k <- which.min(cv_valid)
  } else {
    best_k <- k
  }

  # create the predictions
  docs$pred_label <- mat[,best_k]
  docs$pred_value <- apply(
    mat[,seq(1, best_k),drop = FALSE] == mat[,best_k], 1, mean
  )
  docs$pred_index <- docs[[train_var]]
  docs$real_label <- docs[[label_var]]

  # create the output and return the values
  output <- structure(list(
    index = index,
    docs = ungroup(docs),
    k = best_k,
    cv_curve = tibble(
      k = seq_along(cv_train), cv_train = cv_train, cv_valid = cv_valid
    )
  ), class = c('knnm'))

  output
}

dsst_gbm_build <- function(
  anno, docs,
  min_df = 0.001,
  max_df = 1.0,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  train_var = "train_id",
  label_var = "label",
  trace_it = getOption("dsst.traceit", TRUE),
  max_depth = 3,
  eta = 0.05,
  nrounds = 10,
  seed = 1
)
{
  if (!is.na(seed)) { set.seed(seed) }

  .assert(doc_var %in% names(docs),
          sprintf("doc_var '%s' not found in docs", doc_var))
  .assert(train_var %in% names(docs),
          sprintf("train_var '%s' not found in docs", train_var))
  .assert(label_var %in% names(docs),
          sprintf("label_var '%s' not found in docs", label_var))
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in docs", token_var))

  # create term frequency matrix
  X <- cleanNLP::cnlp_utils_tf(
    anno,
    doc_set = docs[[doc_var]],
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )

  # need to integer encode the classes
  y_set <- unique(docs[[label_var]])
  y <- (match(docs[[label_var]], y_set) - 1L)

  # build model from the training set
  X_train <- X[docs[[train_var]] == "train", ]
  y_train <- y[docs[[train_var]] == "train"]
  X_valid <- X[docs[[train_var]] == "valid", ]
  y_valid <- y[docs[[train_var]] == "valid"]

  data_train <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  data_valid <- xgboost::xgb.DMatrix(data = X_valid, label = y_valid)
  watchlist <- list(train=data_train, valid=data_valid)

  model <- xgboost::xgb.train(data = data_train,
                     max_depth = max_depth,
                     eta = eta,
                     nrounds = nrounds,
                     nthread = 2,
                     objective = "multi:softmax",
                     eval_metric = "merror",
                     watchlist = watchlist,
                     verbose = trace_it,
                     print_every_n = 25,
                     num_class = length(y_set))

  y_hat <- y_set[xgboost:::predict.xgb.Booster(model, newdata = X) + 1]

  # create the predictions
  docs$pred_label <- y_hat
  docs$pred_value <- 1
  docs$pred_index <- docs[[train_var]]
  docs$real_label <- docs[[label_var]]

  # create the output and return the values
  output <- structure(list(
    model = model,
    docs = ungroup(docs)
  ), class = c('gbmm'))

  output
}

#############################################################################
# functions to evaluate performance of the model; these should work on all
# models

dsst_erate <- function(model, segmented = FALSE)
{
  if (segmented)
  {
    res <- model$docs %>%
      group_by(train_id, real_label) %>%
      summarize(
        class_rate = mean(pred_label != real_label), .groups = "drop"
      ) %>%
      pivot_wider(values_from = class_rate, names_from = train_id)
  } else {
    res <- model$docs %>%
      group_by(train_id) %>%
      summarize(
        class_rate = mean(pred_label != real_label), .groups = "drop"
      ) %>%
      pivot_wider(values_from = class_rate, names_from = train_id)
  }

  return(res)
}

dsst_confusion_matrix <- function(model, colnames = TRUE)
{
  tab <- table(
    model$docs$real_label[model$docs$train_id == "valid"],
    model$docs$pred_label[model$docs$train_id == "valid"]
  )

  if (!colnames) { colnames(tab) <- NULL }

  return(tab)
}


dsst_neg_examples <- function(model, n = 10L, real_label = NULL, seed = 1)
{
  if (!is.na(seed)) { set.seed(seed) }

  if (is.null(real_label))
  {
    res <- model$docs %>%
      filter(train_id == "valid") %>%
      filter(real_label != pred_label) %>%
      slice_sample(n = n)
  } else if (real_label == "ALL") {
    res <- model$docs %>%
      filter(train_id == "valid") %>%
      group_by(real_label) %>%
      filter(real_label != pred_label) %>%
      slice_sample(n = n)
  } else {
    res <- model$docs %>%
      filter(train_id == "valid") %>%
      filter(.data$real_label == .env$real_label) %>%
      filter(real_label != pred_label) %>%
      slice_sample(n = n)
  }

  return(res)
}

dsst_max_prob <- function(model, n = 10L, real_label = NULL, seed = 1)
{
  if (!is.na(seed)) { set.seed(seed) }

  if (is.null(real_label))
  {
    res <- model$docs %>%
      filter(train_id == "valid") %>%
      arrange(desc(pred_value)) %>%
      slice_head(n = n)
  } else if (real_label == "ALL") {
    res <- model$docs %>%
      filter(train_id == "valid") %>%
      group_by(real_label) %>%
      arrange(desc(pred_value)) %>%
      slice_head(n = n)
  } else {
    res <- model$docs %>%
      filter(train_id == "valid") %>%
      filter(.data$real_label == .env$real_label) %>%
      arrange(desc(pred_value)) %>%
      slice_head(n = n)
  }

  return(res)
}

#############################################################################
# helper function to print text; pairs with the function dsst_max_prob,
# dsst_neg_examples, etc.

dsst_print_text <- function(res, max_chars = 500L)
{
  if ("real_label" %in% names(res))
  {
    res %>%
      mutate(text = stringi::stri_sub(text, 1L, max_chars)) %>%
      mutate(response = sprintf("%s => %s (%f) \n %s\n",
                                real_label, pred_label, pred_value, text)) %>%
      magrittr::use_series(response) %>%
      stringi::stri_split(fixed = "\n") %>%
      unlist() %>%
      stringi::stri_wrap(width = options()$width - 1L) %>%
      cat(sep = "\n")
  } else {
    res %>%
      mutate(text = stringi::stri_sub(text, 1L, max_chars)) %>%
      mutate(response = sprintf("%s \n %s\n",
                                doc_id, text)) %>%
      magrittr::use_series(response) %>%
      stringi::stri_split(fixed = "\n") %>%
      unlist() %>%
      stringi::stri_wrap(width = 79) %>%
      cat(sep = "\n")
  }
}

#############################################################################
# functions to evaluate the coefficents from the elastic net model

dsst_coef <- function(model, nlambda = -1)
{
  lambda <- ifelse(
    nlambda > 0, model$model$lambda[nlambda], model$model[['lambda.1se']]
  )
  temp <- coef(model$model, s = lambda)
  beta <- Reduce(cbind, temp)
  beta <- beta[apply(beta != 0, 1, any),]
  if (is.null(nrow(beta)))
  {
    beta <- matrix(beta, nrow = 1)
    rownames(beta) <- "(Intercept)"
  }
  colnames(beta) <- names(temp)

  return(beta)
}

dsst_coef_positive <- function(model, nlambda = -1)
{
  beta <- dsst_coef(model, nlambda = nlambda)
  beta_df <- tibble(
    cname = colnames(beta)[as.integer(col(beta))],
    rname = rownames(beta)[as.integer(row(beta))],
    value = as.numeric(beta)
  )
  beta_df <- filter(beta_df, value > 0)
  beta_df <- filter(beta_df, rname != "(Intercept)")
  beta_df <- group_by(beta_df, cname)
  terms <- summarize(beta_df, terms = paste(rname, collapse = "; "))

  nmax <- max(stringi::stri_length(terms$cname))
  smax <- sprintf("%%%ds : %%s", nmax)
  terms <- sprintf(smax, terms$cname, terms$terms)

  cat(terms, sep = "\n")

  invisible(NULL)
}

#############################################################################
# function to evaluate variable importance for gradiant boosted trees

dsst_gbm_imp <- function(model)
{
  importance_matrix <- xgboost::xgb.importance(model = model$model)
  df <- tibble(
    feature = importance_matrix$Feature,
    gain = importance_matrix$Gain,
    cover = importance_matrix$Cover,
    frequency = importance_matrix$Frequency
  )
  df
}

#############################################################################
# functions to create new sets of features from the annotations

dsst_ngram <- function (
  object, n_min = 1, n = 3, doc_var = "doc_id", token_var = "lemma"
)
{
  .assert(doc_var %in% names(object),
          sprintf("doc_var '%s' not found in object", doc_var))
  .assert(token_var %in% names(object),
          sprintf("token_var '%s' not found in object", token_var))

  words <- split(object[[token_var]], object[[doc_var]])
  ngrams <- tokenizers:::generate_ngrams_batch(words, ngram_min = n_min,
    ngram_max = n, stopwords = character(), ngram_delim = " ")
  out <- tibble::tibble(
    doc_id = rep(names(words), sapply(ngrams, length)),
    lemma = unlist(ngrams)
  )
  out <- out[!is.na(out$lemma), ]
  out
}

dsst_skip_gram <- function (
  object, n_min = 1, n = 3, k = 1, doc_var = "doc_id", token_var = "lemma"
)
{
  .assert(doc_var %in% names(object),
          sprintf("doc_var '%s' not found in object", doc_var))
  .assert(token_var %in% names(object),
          sprintf("token_var '%s' not found in object", token_var))

  words <- split(object[[token_var]], object[[doc_var]])
  skips <- unique(unlist(lapply(n_min:n, tokenizers:::get_valid_skips,
      k), recursive = FALSE, use.names = FALSE))
  ngrams <- tokenizers:::skip_ngrams_vectorised(words, skips,
      character())
  out <- tibble::tibble(doc_id = rep(names(words), sapply(ngrams,
      length)), lemma = unlist(ngrams))
  out <- out[!is.na(out$lemma), ]
  out
}

#############################################################################
# functions to perform corpus linguistic operations

dsst_kwic <- function(
  term, docs, n = 20, ignore_case = TRUE, width = 20L, seed = 1
)
{
  if (!is.na(seed)) { set.seed(seed) }

  pre <- ifelse(ignore_case, "(?i)", "")
  rex <- sprintf("%s(.{0,%d})(\\W%s\\W)(.{0,%d})", pre, width, term, width)
  these <- stringi::stri_match(docs$text, regex = rex)

  doc_ids <- docs$doc_id[!is.na(these[, 1])]
  nmax <- max(nchar(doc_ids))
  doc_ids <- sprintf(paste0("%", nmax, "s"), doc_ids)

  these <- these[!is.na(these[, 1]), , drop = FALSE]
  rex <- paste0("[%s] %", width, "s|%s|%s")
  these <- sprintf(rex, doc_ids, these[, 2], these[, 3], these[, 4])
  if (n < length(these)) { these <- sample(these, n) }

  cat(these, sep = "\n")
  invisible(these)
}

dsst_tfidf <- function(
  anno,
  n_terms = 5,
  min_df = 0.1,
  max_df = 0.9,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma"
)
{
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in anno", token_var))

  # create temporary data
  doc_id <- token <- tf <- NULL
  x <- data.frame(doc_id = anno[[doc_var]], token = anno[[token_var]],
      stringsAsFactors = FALSE)
  N <- length(unique(x$doc_id))

  # build the vocabulary
  possible_vocab <- table(x[!duplicated(x), ]$token)/N
  possible_vocab <- possible_vocab[possible_vocab >= min_df &
      possible_vocab <= max_df]
  possible_vocab <- sort(possible_vocab, decreasing = TRUE)
  vocabulary <- names(possible_vocab[seq(1, min(max_features,
      length(possible_vocab)))])

  # build term frequency data
  .assert(length(vocabulary) >= 1, "vocabulary length is too small to continue")
  x <- x[x$token %in% vocabulary, ]
  tf_tibble <- dplyr::group_by(x, doc_id, token)
  tf_tibble <- dplyr::summarize(tf_tibble, tf = dplyr::n(), .groups = "drop")
  tf_tibble <- dplyr::group_by(tf_tibble, token)
  tf_tibble <- dplyr::mutate(tf_tibble, tfidf = (1 + log2(tf)) *
      log2(N/dplyr::n()))
  tf_tibble <- dplyr::ungroup(tf_tibble)

  # summarize
  res <- tf_tibble %>%
    group_by(doc_id) %>%
    arrange(desc(tfidf)) %>%
    slice_head(n = n_terms) %>%
    summarize(tokens = paste(token, collapse = "; "), .groups = "drop")

  return(res)
}

#############################################################################
# functions to perform clustering

dsst_angle_dist <- function(
  anno,
  n_docs = 1,
  type = c("each", "overall"),
  min_df = 0.1,
  max_df = 0.9,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  item_name = "document"
)
{
  type <- match.arg(type)
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in anno", token_var))

  x <- cleanNLP::cnlp_utils_tfidf(
    anno,
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )

  x <- as.matrix(x)
  sim <- x/sqrt(rowSums(x * x))
  sim <- sim %*% t(sim)
  out <- .tidy_matrix(sim, sprintf("%s1", item_name), sprintf("%s2",
      item_name), "distance")
  out$distance[out$distance > 1] <- 1
  out$distance[out$distance < -1] <- -1
  out$distance <- acos(out$distance)/pi

  if (type == "each")
  {
    res <- out %>%
      filter(document1 < document2) %>%
      group_by(document1) %>%
      arrange(distance) %>%
      slice_head(n = n_docs) %>%
      mutate(min_dist = min(distance)) %>%
      ungroup() %>%
      arrange(min_dist) %>%
      select(-min_dist)
  } else {
    res <- out %>%
      filter(document1 < document2) %>%
      arrange(distance) %>%
      slice_head(n = n_docs) %>%
      arrange(distance)
  }

  res
}


dsst_clusters <- function(
  anno,
  n_dims = 2,
  n_clusters = 5L,
  output_type = c("vector", "df"),
  invert = FALSE,
  min_df = 0.1,
  max_df = 0.9,
  max_features = ifelse(invert, 100, 10000),
  doc_var = "doc_id",
  token_var = "lemma",
  seed = 1
)
{
  output_type <- match.arg(output_type)

  df <- dsst_pca(
    anno = anno,
    n_dims = n_dims,
    invert = invert,
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var,
    seed = seed
  )

  X <- as.matrix(df[, -1L, drop = FALSE])
  df$cluster <- as.numeric(stats::kmeans(
    X, centers = n_clusters, nstart = 3L
  )$cluster)

  if (output_type == "vector")
  {
    names(df)[1] <- "document"
    df <- df %>%
      group_by(cluster) %>%
      summarize(docs = paste(document, collapse = "; "))
    df <- df$docs
  }
  return(df)
}

#############################################################################
# functions to perform dimensionality reduction

dsst_pca <- function(
  anno,
  n_dims = 2,
  invert = FALSE,
  min_df = 0.1,
  max_df = 0.9,
  max_features = ifelse(invert, 100, 10000),
  doc_var = "doc_id",
  token_var = "lemma",
  seed = 1
)
{
  if (!is.na(seed)) { set.seed(seed) }
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in anno", token_var))

  x <- cleanNLP::cnlp_utils_tfidf(
    anno,
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )

  x <- as.matrix(x)
  if (invert) { x <- t(x) }

  # determine if we will need any missing values due to zero variation
  sigma <- apply(x, 1, sd)
  keep_these <- which(sigma != 0)
  z <- x[keep_these, , drop=FALSE]

  # compute the rotation
  rot <- irlba::prcomp_irlba(t(z), n = n_dims, scale. = TRUE)$rotation
  colnames(rot) <- sprintf("v%d", seq_len(ncol(rot)))

  # build a tibble of the results
  df <- matrix(NA_real_, nrow = nrow(x), ncol = n_dims)
  colnames(df) <- sprintf("v%d", seq_len(ncol(df)))
  df <- tibble::as_tibble(df)
  df[keep_these,] <- tibble::as_tibble(rot)

  # create and return the output
  df <- dplyr::bind_cols(tibble::tibble(rownames(x)), df)
  names(df)[1] <- doc_var
  df
}

dsst_umap <- function(
  anno,
  type = c("each", "overall"),
  min_df = 0.1,
  max_df = 0.9,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  seed = 1
)
{
  if (!is.na(seed)) { set.seed(seed) }

  # produce the features
  x <- cleanNLP::cnlp_utils_tfidf(
    anno,
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )

  # fit the UMAP model
  x <- as.matrix(x)
  df <- umap::umap(x, n_components = 2L, random_state = seed)$layout

  # create and return the output
  colnames(df) <- sprintf("v%d", seq_len(ncol(df)))
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))
  df <- dplyr::bind_cols(tibble::tibble(rownames(x)), df)
  names(df)[1] <- doc_var

  df
}

#############################################################################
# functions to plot dimensionality reduction

dsst_plot_dred <- function(object, label_flag = (nrow(object) <= 100L))
{
  object$document <- object[[1L]]
  p <- object %>%
    ggplot(aes(x = v1, y = v2)) +
      geom_point(color = "grey90") +
      theme_void()

  if (label_flag) { p <- p + ggrepel::geom_text_repel(aes(label = document)) }

  p
}

dsst_json_drep <- function(
  object,
  docs,
  path = file.path("..", "output", "dim_reduction.json"),
  nchar = 500L,
  color = "#fe8019"
)
{
  doc_var <- names(object)[1]
  names(object)[1] <- "doc_id"

  .assert(doc_var %in% names(docs),
          sprintf("doc_var '%s' not found in docs", doc_var))
  .assert('v1' %in% names(object),
          "variable 'v1' not found in object")
  .assert('v2' %in% names(object),
          "variable 'v2' not found in object")

  cdata <- docs
  if (any(duplicated(docs[[doc_var]])))
  {
    cdata$doc_id <- cdata[[doc_var]]
    cdata <- cdata %>%
      group_by(doc_id) %>%
      summarise(text = stringi::stri_paste(text, collapse = "\n\n"))
  }
  cdata$text <- stringi::stri_sub(cdata$text, 1, nchar)
  cdata <- inner_join(object, cdata, by = c("doc_id" = "doc_id"))
  cdata <- filter(cdata, !is.na(v1), !is.na(v2))

  if (!('color' %in% names(cdata))) { cdata$color <- color }
  cdata$v1 <- (cdata$v1 - min(cdata$v1)) / (max(cdata$v1) - min(cdata$v1))
  cdata$v2 <- (cdata$v2 - min(cdata$v2)) / (max(cdata$v2) - min(cdata$v2))

  cdata <- nest(cdata, meta = -c(color, text, v1, v2))
  res <- as.character(jsonlite::toJSON(cdata, pretty = TRUE))
  write_lines(res, path)
}

#############################################################################
# functions to build and work with topic models

dsst_lda_build <- function(
  anno,
  num_topics = 16,
  min_df = 0.1,
  max_df = 0.9,
  max_features = 10000,
  doc_var = "doc_id",
  token_var = "lemma",
  trace_it = getOption("dsst.traceit", TRUE),
  seed = 1
)
{
  if (!is.na(seed)) { set.seed(seed) }
  .assert(doc_var %in% names(anno),
          sprintf("doc_var '%s' not found in anno", doc_var))
  .assert(token_var %in% names(anno),
          sprintf("token_var '%s' not found in anno", token_var))

  x <- cleanNLP::cnlp_utils_tf(
    anno,
    min_df = min_df,
    max_df = max_df,
    max_features = max_features,
    doc_var = doc_var,
    token_var = token_var
  )

  lda_model <- topicmodels::LDA(
    x = x, k = num_topics, control = list(seed = seed, verbose = trace_it)
  )

  docs <- tibble::tibble(
    doc_id = rep(rownames(x), num_topics),
    topic = rep(seq_len(num_topics), each = nrow(x)),
    prob = as.numeric(lda_model@gamma)
  )
  terms <- tibble::tibble(
    token = rep(colnames(x), each = num_topics),
    topic = rep(seq_len(num_topics), ncol(x)),
    beta = as.numeric(lda_model@beta)
  )

  # create the output and return the values
  output <- structure(list(
    docs = docs,
    terms = terms
  ), class = c('ldam'))

  return(output)
}

dsst_json_lda <- function(
  object,
  docs,
  path = file.path("..", "output", "lda_model.json"),
  truncate = -1
)
{
  tnames <- object$terms %>%
    arrange(topic, desc(beta)) %>%
    group_by(topic) %>%
    slice_head(n = 5) %>%
    group_by(topic) %>%
    summarize(name = paste(token, collapse = "; "))

  topic_words <- object$terms %>%
    arrange(topic, desc(beta)) %>%
    group_by(topic) %>%
    slice_head(n = 100) %>%
    mutate(weight = round(100 * exp(beta) / max(exp(beta)))) %>%
    filter(weight > 0) %>%
    ungroup()

  topic_weights <- object$docs %>%
    group_by(topic) %>%
    summarize(proportion = sum(prob)) %>%
    mutate(proportion = proportion / sum(proportion) * 100)

  top_docs <- object$docs %>%
    arrange(topic, desc(prob)) %>%
    group_by(topic) %>%
    filter(prob > 0) %>%
    mutate(prob = round(prob * 100)) %>%
    filter(prob > 0) %>%
    ungroup()

  dset <- sort(unique(object$docs$doc_id))
  top_docs$id <- match(top_docs$doc_id, dset) - 1L

  tset <- sort(unique(object$terms$topic))
  topics <- vector("list", length(tset))
  for (j in seq_along(topics))
  {
    topics[[j]] <- list(
      "short" = jsonlite::unbox(sprintf("Cluster %d", j)),
      "long" = jsonlite::unbox(sprintf("Cluster %d: %s", j, tnames$name[tnames$topic == tset[j]])),
      "proportion" = jsonlite::unbox(round(topic_weights$proportion[topic_weights$topic == tset[j]])),
      "top_docs_ids" = top_docs$id[top_docs$topic == tset[j]],
      "doc_perc" = top_docs$prob[top_docs$topic == tset[j]],
      "top_word" = topic_words$token[topic_words$topic == tset[j]],
      "word_wgt" = topic_words$weight[topic_words$topic == tset[j]]
    )
  }

  top_topics <- object$docs %>%
    arrange(doc_id, desc(prob)) %>%
    group_by(doc_id) %>%
    filter(prob > 0) %>%
    mutate(prob = round(prob * 100)) %>%
    filter(prob > 0) %>%
    ungroup()

  top_topics$id <- match(top_topics$topic, tset) - 1L

  lout <- vector("list", length(dset))
  if (truncate > 0) { docs$text <- stringi::stri_sub(docs$text, 1, truncate) }
  for (j in seq_along(lout))
  {
    lout[[j]] <- list(
      "top_topics_ids" = top_topics$id[top_topics$doc_id == dset[j]],
      "topic_weights" = top_topics$prob[top_topics$doc_id == dset[j]],
      "title" = jsonlite::unbox(dset[j]),
      "text" = docs$text[docs$doc_id == dset[j]],
      "meta" = list()
    )
  }

  res <- list(topics = topics, docs = lout)
  jsonlite::write_json(res, path)
}

#############################################################################
# functions to create wikipedia data

.http_cache_get <- function(url, cache_dir, page)
{
  # create cache directory if it does not yet exist
  dir.create(cache_dir, showWarnings = FALSE)

  # create a cache of the query
  cache_file <- file.path(cache_dir, paste0(rlang::hash(url), ".rds"))

  # check if file exists and either load or query and save
  if (file.exists(cache_file))
  {
    res <- readRDS(cache_file)
  } else {
    res <- httr::GET(url)
    saveRDS(res, cache_file)
    message(sprintf("Downloading %s", page))
  }

  return(res)
}

dsst_wiki_load <- function(
  page, cache_dir = file.path("..", "output", "cache")
)
{
  # get the page
  url <- httr::modify_url("https://en.wikipedia.org/w/api.php",
    query = list(
      action = "parse",
      format = "json",
      redirects = TRUE,
      page = utils::URLdecode(page)
    )
  )
  res <- .http_cache_get(url, cache_dir = cache_dir, page = page)
  httr::stop_for_status(res)
  obj <- httr::content(res, type = "application/json")

  # return the entire object
  return(obj)
}

dsst_wiki_get_links <- function(
  obj,
  xpath = ".//p//a",
  table_num = NA_integer_,
  column_num = NA_integer_
)
{
  tree <- xml2::read_html(obj$parse$text[[1]])
  links <- xml2::xml_find_all(tree, xpath = xpath)
  links <- xml2::xml_attr(links, "href")
  links <- links[stringi::stri_sub(links, 1L, 6L) == "/wiki/"]
  links <- links[stringi::stri_sub(links, 1L, 16L) != "/wiki/Wikipedia:"]
  links <- stringi::stri_sub(links, 7L, -1L)
  links <- links[!stringi::stri_detect(links, fixed = "#")]
  links <- unique(links)
  links
}

dsst_wiki_get_links_table <- function(
  obj,
  table_num = 1L,
  column_num = NULL,
  print_first_rows = FALSE
)
{
  tree <- xml2::read_html(obj$parse$text[[1]])
  tables <- xml2::xml_find_all(tree, xpath = ".//table")

  .assert(length(tables) > table_num,
          sprintf("Asking for table %d of %d", table_num, length(tables)))

  this_table <- tables[[table_num]]
  rows <- xml2::xml_find_all(this_table, ".//tr")

  if (print_first_rows)
  {
    fr <- map_chr(tables, ~ xml2::xml_text(xml2::xml_find_first(..1, ".//tr")))
    fr <- stringi::stri_sub(fr, 1L, options()$width - 5L)
    print(fr)
  }

  # grab links for all columns or specified column
  if (is.null(column_num))
  {
    links <- xml2::xml_find_all(rows, xpath = ".//a")
    links <- xml2::xml_attr(links, "href")
  } else {
    links <- map(rows, function(u) {
      td <- xml2::xml_find_all(u, ".//td")
      res <- ifelse(length(td) < column_num, "",
                    xml2::xml_attr(xml2::xml_find_all(td[[column_num]], ".//a"),
                                   "href"))
      res
    })
    links <- flatten_chr(links)
  }

  # process the links
  links <- links[!is.na(links)]
  links <- links[stringi::stri_sub(links, 1L, 6L) == "/wiki/"]
  links <- links[stringi::stri_sub(links, 1L, 16L) != "/wiki/Wikipedia:"]
  links <- stringi::stri_sub(links, 7L, -1L)
  links <- links[!stringi::stri_detect(links, fixed = "#")]
  links <- unique(links)
  links
}

dsst_wiki_make_data <- function(
  links, cache_dir = file.path("..", "output", "cache")
)
{
  nl <- length(links)
  res <- tibble(doc_id = rep(NA_character_, nl), text = rep(NA_character_, nl))
  for (j in seq_len(nl))
  {
    obj_json <- dsst_wiki_load(links[j], cache_dir = cache_dir)
    obj_html <- xml2::read_html(obj_json$parse$text[[1]])

    refs <- xml2::xml_find_all(obj_html, ".//sup/a")
    xml2::xml_text(refs) <- ""
    probs <- xml2::xml_find_all(obj_html, ".//span[not(@class)]")
    xml2::xml_text(probs) <- ""

    inst <- xml2::xml_find_all(obj_html, ".//p[not(@class)]")
    text <- stringi::stri_replace_all(
      xml2::xml_text(inst), "", regex = "\\[[\\W\\w]+\\]"
    )
    text <- text[stringi::stri_length(text) > 50]
    text <- text[stringi::stri_sub(text, 1L, 1L) %in% LETTERS]
    text <- stringi::stri_replace_all(text, "", regex = "\\([^\\)]+\\)")
    text <- stringi::stri_replace_all(text, '"', regex = "'")
    text <- stringi::stri_replace_all(text, "", regex = "[^\\w\\.;\\-\\' ]")
    text <- stringi::stri_replace_all(text, " ", regex = "[ ]+")
    text <- stringi::stri_paste(text, collapse = "\n\n")
    res$doc_id[j] <- obj_json$parse$displaytitle
    res$text[j] <- text
  }

  return(res)
}
