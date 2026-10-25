
library(ggplot2)
library(uwot)
library(showtext)
library(sysfonts)

set.seed(42)

if (.Platform$OS.type == "windows") {
  font_add(
    "Times New Roman",
    regular = "C:/Windows/Fonts/times.ttf",
    bold = "C:/Windows/Fonts/timesbd.ttf",
    italic = "C:/Windows/Fonts/timesi.ttf",
    bolditalic = "C:/Windows/Fonts/timesbi.ttf"
  )
} else {
  font_add(
    "Times New Roman",
    regular = "fonts/TimesNewRoman/times.ttf",
    bold = "fonts/TimesNewRoman/timesbd.ttf",
    italic = "fonts/TimesNewRoman/timesi.ttf",
    bolditalic = "fonts/TimesNewRoman/timesbi.ttf"
  )
}

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

font_nba <- "Times New Roman"

position_colors <- c(
  PG = "#376A93",
  SG = "#6B9A9C",
  SF = "#87A978",
  PF = "#D4A35D",
  C = "#BD746B"
)

col_main <- position_colors[["PG"]]
col_accent <- position_colors[["C"]]

theme_nba <- function() {
  theme_minimal(base_size = 18, base_family = font_nba) +
    theme(
      text = element_text(family = font_nba, colour = "black"),
      plot.title = element_text(
        size = 22,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 12)
      ),
      plot.subtitle = element_text(size = 17),
      axis.title = element_text(size = 19),
      axis.text = element_text(size = 17),
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 17),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        color = "#E8E8E8",
        linewidth = 0.3
      ),
      panel.border = element_blank(),
      axis.line = element_blank(),
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.margin = margin(15, 20, 15, 15)
    )
}

theme_set(theme_nba())

figure_dir <- "nba_eda_figures"
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

save_plot <- function(plot, filename, width = 8, height = 5) {
  ggsave(
    filename = file.path(figure_dir, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    device = "png",
    bg = "white"
  )
}

nba <- read.csv(
  "2023-2025-nba-stats(in).csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

nba <- nba[
  ,
  !(names(nba) %in% c("", "Unnamed: 0")),
  drop = FALSE
]

missing_table <- data.frame(
  Variable = names(nba),
  Missing = colSums(is.na(nba))
)

missing_plot_data <- subset(missing_table, Missing > 0)

if (nrow(missing_plot_data) > 0) {
  p_missing <- ggplot(
    missing_plot_data,
    aes(x = reorder(Variable, Missing), y = Missing)
  ) +
    geom_col(
      fill = col_main,
      color = "white",
      linewidth = 0.4,
      width = 0.7
    ) +
    geom_text(
      aes(label = Missing),
      hjust = -0.2,
      size = 5,
      family = font_nba
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.18))
    ) +
    coord_flip() +
    labs(
      title = "Missing Values by Variable",
      x = NULL,
      y = "Number of missing observations"
    )

  save_plot(p_missing, "01_missing_values.png")
}

nba_clean <- nba[
  !is.na(nba$Team) &
    !grepl("^[0-9]+TM$", nba$Team),
  ,
  drop = FALSE
]

thresholds <- c(
  0, 50, 100, 150, 200,
  250, 300, 400, 500, 750, 1000
)

threshold_results <- data.frame(
  Minutes = thresholds,
  Retained_Percent = sapply(thresholds, function(t) {
    100 * sum(
      !is.na(nba_clean$MP) & nba_clean$MP >= t
    ) / nrow(nba_clean)
  })
)

p_retained <- ggplot(
  threshold_results,
  aes(x = Minutes, y = Retained_Percent)
) +
  geom_line(color = col_main, linewidth = 1) +
  geom_point(color = col_main, size = 3) +
  geom_vline(
    xintercept = 250,
    color = col_accent,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  scale_x_continuous(
    breaks = thresholds,
    guide = guide_axis(check.overlap = TRUE)
  ) +
  labs(
    title = "Observations Retained by Minutes Threshold",
    x = "Minimum minutes played",
    y = "Observations retained (%)"
  )

save_plot(p_retained, "03_threshold_retained.png")

nba_filtered <- nba_clean[
  !is.na(nba_clean$MP) &
    nba_clean$MP >= 250,
  ,
  drop = FALSE
]

plot_histogram <- function(variable, title, filename) {
  median_value <- median(
    nba_filtered[[variable]],
    na.rm = TRUE
  )

  p <- ggplot(
    nba_filtered,
    aes(x = .data[[variable]])
  ) +
    geom_histogram(
      bins = 35,
      fill = col_main,
      color = "white",
      linewidth = 0.3
    ) +
    geom_vline(
      xintercept = median_value,
      color = col_accent,
      linetype = "dashed",
      linewidth = 0.9
    ) +
    labs(
      title = title,
      x = paste0(variable, " per 36 minutes"),
      y = "Number of observations"
    )

  save_plot(p, filename)
}

plot_histogram(
  "PTS",
  "Distribution of Points",
  "07_points.png"
)

plot_histogram(
  "3PA",
  "Distribution of Three-Point Attempts",
  "08_three_point_attempts.png"
)

plot_histogram(
  "AST",
  "Distribution of Assists",
  "09_assists.png"
)

plot_histogram(
  "TRB",
  "Distribution of Rebounds",
  "10_rebounds.png"
)

umap_features <- c(
  "FGA", "3PA", "FTA",
  "ORB", "DRB", "AST",
  "STL", "BLK", "TOV"
)

X <- nba_filtered[
  ,
  umap_features,
  drop = FALSE
]

umap_rows <- complete.cases(X)

X <- X[
  umap_rows,
  ,
  drop = FALSE
]

nba_umap <- nba_filtered[
  umap_rows,
  ,
  drop = FALSE
]

X_scaled <- scale(X)

set.seed(42)

umap_result <- uwot::umap(
  X_scaled,
  n_neighbors = 15,
  min_dist = 0.01,
  metric = "euclidean",
  n_components = 2,
  n_threads = 1
)

nba_umap$UMAP1 <- umap_result[, 1]
nba_umap$UMAP2 <- umap_result[, 2]

positions <- c("PG", "SG", "SF", "PF", "C")

nba_umap_positions <- nba_umap[
  !is.na(nba_umap$Pos) &
    nba_umap$Pos %in% positions,
  ,
  drop = FALSE
]

nba_umap_positions$Pos <- factor(
  nba_umap_positions$Pos,
  levels = positions
)

p_umap_position <- ggplot(
  nba_umap_positions,
  aes(x = UMAP1, y = UMAP2, color = Pos)
) +
  geom_point(
    size = 1.6,
    alpha = 0.75
  ) +
  scale_color_manual(
    values = position_colors,
    limits = positions,
    drop = FALSE
  ) +
  coord_equal() +
  labs(
    title = "UMAP Projection by Traditional Position",
    x = "UMAP 1",
    y = "UMAP 2",
    color = "Position"
  ) +
  theme(panel.grid = element_blank())

save_plot(
  p_umap_position,
  "14_umap_position.png",
  8,
  6
)

nba_positions <- nba_filtered[
  !is.na(nba_filtered$Pos) &
    nba_filtered$Pos %in% positions,
  ,
  drop = FALSE
]

nba_positions$Pos <- factor(
  nba_positions$Pos,
  levels = positions
)

plot_position_boxplot <- function(variable, title, filename) {
  p <- ggplot(
    nba_positions,
    aes(
      x = Pos,
      y = .data[[variable]],
      fill = Pos
    )
  ) +
    geom_boxplot(
      width = 0.6,
      alpha = 0.85,
      color = "#555555",
      outlier.alpha = 0.35,
      outlier.size = 1.2,
      linewidth = 0.4
    ) +
    scale_fill_manual(
      values = position_colors,
      limits = positions,
      drop = FALSE
    ) +
    labs(
      title = title,
      x = "Traditional position",
      y = paste0(variable, " per 36 minutes")
    ) +
    theme(legend.position = "none")

  save_plot(p, filename)
}

plot_position_boxplot(
  "3PA",
  "Three-Point Attempts by Position",
  "15_boxplot_three_point_attempts.png"
)

plot_position_boxplot(
  "AST",
  "Assists by Position",
  "16_boxplot_assists.png"
)

plot_position_boxplot(
  "TRB",
  "Rebounds by Position",
  "17_boxplot_rebounds.png"
)

plot_position_boxplot(
  "PTS",
  "Points by Position",
  "18_boxplot_points.png"
)
