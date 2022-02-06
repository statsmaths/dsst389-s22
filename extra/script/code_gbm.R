library(tidyverse)

theme_set(theme_minimal())

make_df <- function(sigma) {
  set.seed(1)

  df <- tibble(
    class = rep(c(0, 1), each = 60),
    color = rep(c("blue", "orange"), each = 60),
    mode = rep(seq_len(4), each = 30),
    x = rnorm(120, sd = sigma),
    y = rnorm(120, sd = sigma),
    train_id = rep(c("train", "valid"), 60)
  )

  df$x[df$mode == 1] <- df$x[df$mode == 1] + 0.3
  df$y[df$mode == 1] <- df$y[df$mode == 1] + 0.2
  df$x[df$mode == 2] <- df$x[df$mode == 2] + 0.2
  df$y[df$mode == 2] <- df$y[df$mode == 2] + 0.3
  df$x[df$mode == 3] <- df$x[df$mode == 3] + 0.3
  df$y[df$mode == 3] <- df$y[df$mode == 3] + 0.5
  df$x[df$mode == 4] <- df$x[df$mode == 4] + 0.5
  df$y[df$mode == 4] <- df$y[df$mode == 4] + 0.3

  df
}

df <- make_df(0.05)
dft <- df %>%
  filter(train_id == "train")

# image of just the points
p <- dft %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    scale_fill_identity() +
    scale_color_identity()

ggsave("../img/gbm_fig01.jpg", p, height = 6, width = 6, dpi = 400)


# image with one line
p <- dft %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    scale_fill_identity() +
    geom_vline(xintercept = 0.4, linetype = "dashed") +
    scale_color_identity()

ggsave("../img/gbm_fig02.jpg", p, height = 6, width = 6, dpi = 400)

# image with one line and probs
mean(dft$color[dft$x < 0.4] == "orange")
p <- dft %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    scale_fill_identity() +
    annotate("text", x = 0.5, y = 0.15, label = "p[orange] = 1.00", size = 6) +
    annotate("text", x = 0.2, y = 0.15, label = "p[orange] = 0.32", size = 6) +
    geom_vline(xintercept = 0.4, linetype = "dashed") +
    scale_color_identity()

ggsave("../img/gbm_fig03.jpg", p, height = 6, width = 6, dpi = 400)

# image with two lines
p <- dft %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    scale_fill_identity() +
    geom_vline(xintercept = 0.4, linetype = "dashed") +
    annotate("segment", x = 0.132, xend = 0.4, y = 0.43, yend = 0.43, linetype = "dashed") +
    scale_color_identity()

ggsave("../img/gbm_fig04.jpg", p, height = 6, width = 6, dpi = 400)

# image with two lines
mean(dft$color[dft$x < 0.4 & dft$y < 0.43] == "orange")

p <- dft %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    scale_fill_identity() +
    geom_vline(xintercept = 0.4, linetype = "dashed") +
    annotate("segment", x = 0.132, xend = 0.4, y = 0.43, yend = 0.43, linetype = "dashed") +
    annotate("text", x = 0.5, y = 0.15, label = "p[orange] = 1.00", size = 6) +
    annotate("text", x = 0.2, y = 0.15, label = "p[orange] = 0.03", size = 6) +
    annotate("text", x = 0.2, y = 0.55, label = "p[orange] = 1.00", size = 6) +
    scale_color_identity()

ggsave("../img/gbm_fig05.jpg", p, height = 6, width = 6, dpi = 400)

########################################
make_df <- function(sigma) {
  set.seed(1)

  df <- tibble(
    class = rep(c(0, 0, 1, 2), each = 30),
    color = rep(c("blue", "blue", "yellowgreen", "orange"), each = 30),
    mode = rep(seq_len(4), each = 30),
    x = rnorm(120, sd = sigma),
    y = rnorm(120, sd = sigma),
    train_id = rep(c("train", "valid"), 60)
  )

  df$x[df$mode == 1] <- df$x[df$mode == 1] + 0.3
  df$y[df$mode == 1] <- df$y[df$mode == 1] + 0.2
  df$x[df$mode == 2] <- df$x[df$mode == 2] + 0.2
  df$y[df$mode == 2] <- df$y[df$mode == 2] + 0.3
  df$x[df$mode == 3] <- df$x[df$mode == 3] + 0.3
  df$y[df$mode == 3] <- df$y[df$mode == 3] + 0.5
  df$x[df$mode == 4] <- df$x[df$mode == 4] + 0.5
  df$y[df$mode == 4] <- df$y[df$mode == 4] + 0.3

  df
}

df <- make_df(0.2)

to_fit <- as_tibble(expand.grid(
  x = seq(min(df$x), max(df$x), length.out = 100),
  y = seq(min(df$y), max(df$y), length.out = 100)
))

# need to integer encode the classes
y_set <- unique(df$color)
y <- (match(df$color, y_set) - 1L)

# build model from the training set
X <- as.matrix(select(df, x, y))
X_train <- X[df$train_id == "train", ]
y_train <- y[df$train_id == "train"]
X_valid <- as.matrix(select(to_fit, x, y))
y_valid <- rep(0, nrow(X_valid))

data_train <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
data_valid <- xgboost::xgb.DMatrix(data = X_valid, label = y_valid)
watchlist <- list(train=data_train, valid=data_valid)

model <- xgboost::xgb.train(data = data_train,
                   max_depth = 3,
                   eta = 0.05,
                   nrounds = 10,
                   nthread = 2,
                   objective = "multi:softmax",
                   eval_metric = "merror",
                   watchlist = watchlist,
                   verbose = TRUE,
                   print_every_n = 25,
                   num_class = length(y_set))

to_fit$color <- y_set[xgboost:::predict.xgb.Booster(model, newdata = X_valid, iterationrange = c(1, 2)) + 1]

p <- df %>%
  ggplot(aes(x, y)) +
    geom_point(color = "white", data = to_fit) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 2) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL)

ggsave("../img/gbm_fig09.jpg", p, height = 6, width = 6, dpi = 400)

p <- df %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 0.5, data = to_fit) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 2) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL)

ggsave("../img/gbm_fig06.jpg", p, height = 6, width = 6, dpi = 400)

to_fit$color <- y_set[xgboost:::predict.xgb.Booster(model, newdata = X_valid, iterationrange = c(4, 5)) + 1]

p <- df %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 0.5, data = to_fit) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 2) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL)

ggsave("../img/gbm_fig07.jpg", p, height = 6, width = 6, dpi = 400)

to_fit$color <- y_set[xgboost:::predict.xgb.Booster(model, newdata = X_valid, iterationrange = c(8, 9)) + 1]

p <- df %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 0.5, data = to_fit) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 2) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL)

ggsave("../img/gbm_fig08.jpg", p, height = 6, width = 6, dpi = 400)


xgboost:::xgb.plot.tree(model = model)
