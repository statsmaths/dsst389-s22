library(tidyverse)

theme_set(theme_minimal())

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

line_param <- function(df, class_main)
{
  # phi(P) = a + b X + c Y
  # Y = -(a/c) - (b/c) X + phi(P) / c
  df$class <- as.numeric(df$class == class_main)

  model <- glm(
    class ~ x + y, data = df[df$train_id == "train", ], family = binomial()
  )

  co <- as.numeric(coef(model))
  list(m = -(co[2]/co[3]), b = -(co[1]/co[3]))
}

df <- make_df(0.05)


# image of just the points
p <- df %>%
  filter(train_id == "train") %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL)

ggsave("../img/ml_fig13.jpg", p, height = 6, width = 6, dpi = 400)

# line for blue class
lp <- line_param(df, 0)

p <- df %>%
  filter(train_id == "train") %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    geom_abline(slope = lp$m, intercept = lp$b, linetype = "dashed") +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL)

ggsave("../img/ml_fig14.jpg", p, height = 6, width = 6, dpi = 400)

# line for green class
lp <- line_param(df, 1)

p <- df %>%
  filter(train_id == "train") %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    geom_abline(slope = lp$m, intercept = lp$b, linetype = "dashed") +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL)

ggsave("../img/ml_fig15.jpg", p, height = 6, width = 6, dpi = 400)

# line for orange class
lp <- line_param(df, 2)

p <- df %>%
  filter(train_id == "train") %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    geom_abline(slope = lp$m, intercept = lp$b, linetype = "dashed") +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL)

ggsave("../img/ml_fig16.jpg", p, height = 6, width = 6, dpi = 400)
