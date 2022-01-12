library(tidyverse)

theme_set(theme_minimal())

make_df <- function(sigma) {
  set.seed(1)

  df <- tibble(
    class = rep(c(0, 1), each = 20),
    color = rep(c("blue", "orange"), each = 20),
    shape = rep(c(19L, 6L), each = 20),
    mode = rep(seq_len(4), each = 10),
    x = rnorm(40, sd = sigma),
    y = rnorm(40, sd = sigma),
    train_id = rep(c("train", "valid"), 20)
  )

  df$x[df$mode == 1] <- df$x[df$mode == 1] + 0.3
  df$y[df$mode == 1] <- df$y[df$mode == 1] + 0.2
  df$x[df$mode == 2] <- df$x[df$mode == 2] + 0.2
  df$y[df$mode == 2] <- df$y[df$mode == 2] + 0.3
  df$x[df$mode == 3] <- df$x[df$mode == 3] + 0.3
  df$y[df$mode == 3] <- df$y[df$mode == 3] + 0.5
  df$x[df$mode == 4] <- df$x[df$mode == 4] + 0.5
  df$y[df$mode == 4] <- df$y[df$mode == 4] + 0.3

  df$x <- (df$x - min(df$x)) / (max(df$x) - min(df$x)) * 5
  df$y <- (df$y - min(df$y)) / (max(df$y) - min(df$y)) * 5

  df
}

df <- make_df(0.1)

p <- df %>%
  ggplot(aes(x, y)) +
    geom_point(aes(shape = shape), size = 3) +
    scale_shape_identity() +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(n.breaks = 10) +
    facet_wrap(~train_id, nrow = 1L)

ggsave("../img/ml1_hout_fig01.jpg", p, height = 5, width = 10, dpi = 400)
