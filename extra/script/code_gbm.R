library(tidyverse)
library(xgboost)

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

create_fit <- function(model, to_fit, k = 1)
{
  mat <- as.matrix(select(to_fit, x, y))
  pred <- matrix(predict(model, mat, iterationrange = c(1, k)), ncol = 3, byrow = TRUE)
  to_fit$c1 <- pred[,1]
  to_fit$c2 <- pred[,2]
  to_fit$c3 <- pred[,3]

  these <- apply(pred, 1, which.max)
  # to_fit$c1[these == 2] <- 0
  # to_fit$c1[these == 3] <- 0
  # to_fit$c2[these == 1] <- 0
  # to_fit$c2[these == 3] <- 0
  # to_fit$c3[these == 1] <- 0
  # to_fit$c3[these == 2] <- 0
  to_fit
}

create_plot <- function(to_fit)
{
  p1 <- df %>%
    ggplot(aes(x, y)) +
      geom_point(aes(alpha = c1), color = "blue", data = to_fit, show.legend = FALSE) +
      scale_fill_identity() +
      scale_color_identity() +
      scale_x_continuous(labels = NULL) +
      scale_y_continuous(labels = NULL) +
      scale_alpha_identity()

  p2 <- df %>%
    ggplot(aes(x, y)) +
      geom_point(aes(alpha = c2), color = "yellowgreen", data = to_fit, show.legend = FALSE) +
      scale_fill_identity() +
      scale_color_identity() +
      scale_x_continuous(labels = NULL) +
      scale_y_continuous(labels = NULL) +
      scale_alpha_identity()

  p3 <- df %>%
    ggplot(aes(x, y)) +
      geom_point(aes(alpha = c3), color = "orange", data = to_fit, show.legend = FALSE) +
      scale_fill_identity() +
      scale_color_identity() +
      scale_x_continuous(labels = NULL) +
      scale_y_continuous(labels = NULL) +
      scale_alpha_identity()

  p <- ggpubr::ggarrange(p1, p2, p3, nrow = 1)
  return(p)
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
                   nrounds = 100,
                   nthread = 2,
                   objective = "multi:softprob",
                   eval_metric = "merror",
                   watchlist = watchlist,
                   verbose = TRUE,
                   print_every_n = 25,
                   num_class = length(y_set))

to_fit <- create_fit(model, to_fit, k = 2)
p <- create_plot(to_fit)
ggsave("../img/gbm_fig06.jpg", p, height = 6, width = 16, dpi = 400)

to_fit <- create_fit(model, to_fit, k = 3)
p <- create_plot(to_fit)
ggsave("../img/gbm_fig07.jpg", p, height = 6, width = 16, dpi = 400)

to_fit <- create_fit(model, to_fit, k = 5)
p <- create_plot(to_fit)
ggsave("../img/gbm_fig08.jpg", p, height = 6, width = 16, dpi = 400)

to_fit <- create_fit(model, to_fit, k = 10)
p <- create_plot(to_fit)
ggsave("../img/gbm_fig09.jpg", p, height = 6, width = 16, dpi = 400)

to_fit <- create_fit(model, to_fit, k = 25)
p <- create_plot(to_fit)
ggsave("../img/gbm_fig10.jpg", p, height = 6, width = 16, dpi = 400)

to_fit <- create_fit(model, to_fit, k = 50)
p <- create_plot(to_fit)
ggsave("../img/gbm_fig11.jpg", p, height = 6, width = 16, dpi = 400)

to_fit <- create_fit(model, to_fit, k = 100)
p <- create_plot(to_fit)
ggsave("../img/gbm_fig12.jpg", p, height = 6, width = 16, dpi = 400)
