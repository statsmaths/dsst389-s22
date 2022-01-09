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

line_param <- function(df)
{
  # phi(P) = a + b X + c Y
  # Y = -(a/c) - (b/c) X + phi(P) / c

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
    scale_y_continuous(labels = NULL) +
    labs(x = "H.S. GPA", y = "SAT SCORE")

ggsave("../img/ml_fig01.jpg", p, height = 6, width = 6, dpi = 400)

# image with four easy unknowns
set.seed(1)
dfv <- df %>%
  filter(train_id == "valid") %>%
  group_by(mode) %>%
  slice_sample(n = 1)

p <- df %>%
  filter(train_id == "train") %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    geom_point(data = dfv, pch = 21, size = 3, color = "black", fill = "white") +
    geom_text(aes(label = mode), data = dfv, size = 5, nudge_x = -0.01) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    labs(x = "H.S. GPA", y = "SAT SCORE")

ggsave("../img/ml_fig02.jpg", p, height = 6, width = 6, dpi = 400)

# image with harder unknowns
lp <- line_param(df)

set.seed(1)
dfv <- tibble(
  x = c(0.2, 0.3, 0.4),
  y = lp$m * x + lp$b + rnorm(3, sd = 0.04),
  mode = seq_len(3L)
)

p <- df %>%
  filter(train_id == "train") %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    geom_point(data = dfv, pch = 21, size = 3, color = "black", fill = "white") +
    geom_text(aes(label = mode), data = dfv, size = 5, nudge_x = -0.01) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    labs(x = "H.S. GPA", y = "SAT SCORE")

ggsave("../img/ml_fig03.jpg", p, height = 6, width = 6, dpi = 400)

# draw a line?
lp <- line_param(df)

p <- df %>%
  filter(train_id == "train") %>%
  mutate(color = if_else(train_id == "valid", "white", color)) %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    geom_abline(slope = lp$m, intercept = lp$b, linetype = "dashed") +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    labs(x = "H.S. GPA", y = "SAT SCORE")

ggsave("../img/ml_fig04.jpg", p, height = 6, width = 6, dpi = 400)

# draw a line with points
lp <- line_param(df)

p <- df %>%
  filter(train_id == "train") %>%
  mutate(color = if_else(train_id == "valid", "white", color)) %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    geom_abline(slope = lp$m, intercept = lp$b, linetype = "dashed") +
    geom_point(data = dfv, pch = 21, size = 3, color = "black", fill = "white") +
    geom_text(aes(label = mode), data = dfv, size = 5, nudge_x = -0.01) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    labs(x = "H.S. GPA", y = "SAT SCORE")

ggsave("../img/ml_fig05.jpg", p, height = 6, width = 6, dpi = 400)

# how to evaluate?
dfv <- df %>%
  filter(train_id == "valid") %>%
  group_by(mode) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  mutate(color = if_else(mode == 1, "orange", color)) %>%
  mutate(x = if_else(mode == 1, 0.45, x))

lp <- line_param(df)

p <- df %>%
  filter(train_id == "train") %>%
  mutate(color = if_else(train_id == "valid", "white", color)) %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    geom_abline(slope = lp$m, intercept = lp$b, linetype = "dashed") +
    geom_point(fill = "white", data = dfv, pch = 21, size = 3, color = "black") +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    labs(x = "H.S. GPA", y = "SAT SCORE")

ggsave("../img/ml_fig06.jpg", p, height = 6, width = 6, dpi = 400)

p <- df %>%
  filter(train_id == "train") %>%
  mutate(color = if_else(train_id == "valid", "white", color)) %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3, alpha = 0.4) +
    geom_abline(slope = lp$m, intercept = lp$b, linetype = "dashed") +
    geom_point(aes(fill = color), data = dfv, pch = 21, size = 3, color = "black") +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    labs(x = "H.S. GPA", y = "SAT SCORE")

ggsave("../img/ml_fig07.jpg", p, height = 6, width = 6, dpi = 400)

# multiple lines work to perfectly seperate the data here

lp <- line_param(df)

p <- df %>%
  filter(train_id == "train") %>%
  mutate(color = if_else(train_id == "valid", "white", color)) %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color, color = color), pch = 21, size = 3) +
    geom_abline(slope = lp$m + 0.09, intercept = lp$b - 0.02, linetype = "dotted") +
    geom_abline(slope = lp$m - 0.3, intercept = lp$b + 0.08, linetype = "dotted") +
    geom_abline(slope = lp$m, intercept = lp$b, linetype = "dotted") +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    labs(x = "H.S. GPA", y = "SAT SCORE")

ggsave("../img/ml_fig08.jpg", p, height = 6, width = 6, dpi = 400)

# in other cases, there are no lines that seperate the data perfectly, and many
# that make the same number of errors

df <- make_df(0.1)
lp <- line_param(df)

p <- df %>%
  filter(train_id == "train") %>%
  mutate(color = if_else(train_id == "valid", "white", color)) %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color), pch = 21, size = 3, color = "black") +
    scale_fill_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL)

ggsave("../img/ml_fig09.jpg", p, height = 6, width = 6, dpi = 400)

# noisy case with line

df <- make_df(0.1)
lp <- line_param(df)

p <- df %>%
  filter(train_id == "train") %>%
  mutate(color = if_else(train_id == "valid", "white", color)) %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color), pch = 21, size = 3, color = "black") +
    geom_abline(slope = lp$m, intercept = lp$b, linetype = "dotted") +
    scale_fill_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL)

ggsave("../img/ml_fig10.jpg", p, height = 6, width = 6, dpi = 400)

# plot of logistic function; maps R -> [0, 1]

df <- tibble(
  x = seq(-10, 10, by = 0.1),
  y = exp(x) / (exp(x) + 1)
)

p <- df %>%
  ggplot(aes(x, y)) +
    geom_line() +
    scale_x_continuous(breaks = c(-10, -5, 0, 5, 10)) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    labs(x = "X", y = "logistic(X)")

ggsave("../img/ml_fig11.jpg", p, height = 5, width = 9, dpi = 400)

# look at the contours
df <- make_df(0.1)
lp <- line_param(df)

model <- glm(
  class ~ x + y, data = df[df$train_id == "train", ], family = binomial()
)
pred_df <- tibble(
  x = (0.1 - lp$b + c(0.1, 0.2, 0, -0.1, -0.2)) / lp$m,
  y = 0.1
)
pred_df$prob <- as.character(round(predict(model, pred_df, type = "response"), 2))


p <- df %>%
  filter(train_id == "train") %>%
  mutate(color = if_else(train_id == "valid", "white", color)) %>%
  ggplot(aes(x, y)) +
    geom_point(aes(fill = color), pch = 21, size = 3, color = "black", alpha = 0.4) +
    geom_abline(slope = lp$m, intercept = lp$b, linetype = "dashed") +
    geom_abline(slope = lp$m, intercept = lp$b - 0.1, linetype = "dotdash") +
    geom_abline(slope = lp$m, intercept = lp$b + 0.1, linetype = "dotdash") +
    geom_abline(slope = lp$m, intercept = lp$b - 0.2, linetype = "dotted") +
    geom_abline(slope = lp$m, intercept = lp$b + 0.2, linetype = "dotted") +
    geom_label(aes(label = prob), data = pred_df) +
    scale_fill_identity() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL)

ggsave("../img/ml_fig12.jpg", p, height = 6, width = 6, dpi = 400)
