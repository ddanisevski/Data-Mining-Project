
# ============================================================
# NBA INITIAL EXPLORATORY DATA ANALYSIS
# ============================================================


# ============================================================
# 1. LIBRARIES
# ============================================================

library(ggplot2)
library(uwot)
library(scales)
library(showtext)
library(sysfonts)

set.seed(42)


# ============================================================
# 2. FONT REGISTRATION AND GRAPH STYLE
# ============================================================

# Register Times New Roman before creating any plots.
# The Windows paths below correspond to standard Windows
# Times New Roman font files.

if (.Platform$OS.type == "windows") {

  font_add(
    family = "Times New Roman",
    regular = "C:/Windows/Fonts/times.ttf",
    bold = "C:/Windows/Fonts/timesbd.ttf",
    italic = "C:/Windows/Fonts/timesi.ttf",
    bolditalic = "C:/Windows/Fonts/timesbi.ttf"
  )

} else {

  # Change these paths if your font files are elsewhere.
  font_add(
    family = "Times New Roman",
    regular = "fonts/TimesNewRoman/times.ttf",
    bold = "fonts/TimesNewRoman/timesbd.ttf",
    italic = "fonts/TimesNewRoman/timesi.ttf",
    bolditalic = "fonts/TimesNewRoman/timesbi.ttf"
  )

}

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

font_nba <- "Times New Roman"

# Verify that the font was registered.
stopifnot(font_nba %in% sysfonts::font_families())

cat("Plot font:", font_nba, "\n")


# Colour palette from UMAP by traditional position

position_colors <- c(
  "PG" = "#376A93",
  "SG" = "#6B9A9C",
  "SF" = "#87A978",
  "PF" = "#D4A35D",
  "C"  = "#BD746B"
)

col_main <- unname(position_colors["PG"])
col_light <- unname(position_colors["SG"])
col_accent <- unname(position_colors["C"])
col_green <- unname(position_colors["SF"])
col_gold <- unname(position_colors["PF"])


# Academic theme with Times New Roman

theme_nba <- function() {

  theme_minimal(
    base_size = 18,
    base_family = font_nba
  ) +
    theme(
      text = element_text(
        family = font_nba,
        colour = "black"
      ),

      plot.title = element_text(
        family = font_nba,
        size = 22,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 12)
      ),

      plot.subtitle = element_text(
        family = font_nba,
        size = 17
      ),

      axis.title = element_text(
        family = font_nba,
        size = 19
      ),

      axis.text = element_text(
        family = font_nba,
        size = 17
      ),

      legend.title = element_text(
        family = font_nba,
        size = 18,
        face = "bold"
      ),

      legend.text = element_text(
        family = font_nba,
        size = 17
      ),

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

# Set the global theme after font registration.
theme_set(theme_nba())


# ============================================================
# 3. SAVE FIGURES IN NEW FOLDER
# ============================================================

figure_dir <- "nba_eda_figures_new"

dir.create(
  figure_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


# Save all figures with an explicit Times New Roman theme.
# Using a fresh PNG device helps ensure consistent rendering.

save_plot <- function(
    plot,
    filename,
    width = 8,
    height = 5
) {

  new_filename <- sub(
    "\\.png$",
    "_new.png",
    filename,
    ignore.case = TRUE
  )

  plot_to_save <- plot +
    theme(
      text = element_text(family = font_nba),
      plot.title = element_text(family = font_nba),
      plot.subtitle = element_text(family = font_nba),
      axis.title = element_text(family = font_nba),
      axis.text = element_text(family = font_nba),
      legend.title = element_text(family = font_nba),
      legend.text = element_text(family = font_nba)
    )

  ggsave(
    filename = file.path(
      figure_dir,
      new_filename
    ),
    plot = plot_to_save,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    device = "png",
    bg = "white"
  )
}


# ============================================================
# 4. LOAD DATA
# ============================================================

nba <- read.csv(
  "2023-2025-nba-stats(in).csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Remove index columns.

nba <- nba[
  ,
  !(names(nba) %in% c("", "Unnamed: 0")),
  drop = FALSE
]

cat("Dataset dimensions:", dim(nba), "\n")

print(names(nba))
print(head(nba))
str(nba)

print(table(nba$Season))


# ============================================================
# 5. MISSING VALUES
# ============================================================

cat(
  "Total missing values:",
  sum(is.na(nba)),
  "\n"
)

cat(
  "Missing percentage:",
  round(100 * mean(is.na(nba)), 2),
  "%\n"
)

missing_table <- data.frame(
  Variable = names(nba),

  Missing = colSums(is.na(nba)),

  Missing_Percent = round(
    100 * colMeans(is.na(nba)),
    2
  )
)

missing_table <- missing_table[
  order(-missing_table$Missing),
  ,
  drop = FALSE
]

print(missing_table)

missing_plot_data <- subset(
  missing_table,
  Missing > 0
)

if (nrow(missing_plot_data) > 0) {

  p_missing <- ggplot(
    missing_plot_data,
    aes(
      x = reorder(Variable, Missing),
      y = Missing
    )
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
      family = font_nba,
      color = "black"
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

  print(p_missing)

  save_plot(
    p_missing,
    "01_missing_values.png"
  )
}


# ============================================================
# 6. REMOVE MULTI-TEAM AGGREGATES
# ============================================================

nba_clean <- nba[
  !is.na(nba$Team) &
    !grepl("^[0-9]+TM$", nba$Team),
  ,
  drop = FALSE
]

cat(
  "Original observations:",
  nrow(nba),
  "\n"
)

cat(
  "Observations after removing aggregates:",
  nrow(nba_clean),
  "\n"
)

duplicate_count <- sum(
  duplicated(
    nba_clean[
      ,
      c("Player", "Team", "Season")
    ]
  )
)

cat(
  "Duplicate player-team-season records:",
  duplicate_count,
  "\n"
)


# ============================================================
# 7. DESCRIPTIVE STATISTICS
# ============================================================

numeric_cols <- which(
  vapply(
    nba_clean,
    is.numeric,
    logical(1)
  )
)

numeric_data <- nba_clean[
  ,
  numeric_cols,
  drop = FALSE
]

safe_stat <- function(x, fun) {

  if (all(is.na(x))) {
    return(NA_real_)
  }

  fun(x, na.rm = TRUE)
}

descriptive_stats <- data.frame(
  Variable = names(numeric_data),

  N = sapply(
    numeric_data,
    function(x) sum(!is.na(x))
  ),

  Mean = sapply(
    numeric_data,
    safe_stat,
    fun = mean
  ),

  Median = sapply(
    numeric_data,
    safe_stat,
    fun = median
  ),

  SD = sapply(
    numeric_data,
    safe_stat,
    fun = sd
  ),

  Min = sapply(
    numeric_data,
    safe_stat,
    fun = min
  ),

  Max = sapply(
    numeric_data,
    safe_stat,
    fun = max
  )
)

descriptive_stats[-1] <- lapply(
  descriptive_stats[-1],
  function(x) round(x, 2)
)

print(descriptive_stats)


# ============================================================
# 8. PLAYING TIME DISTRIBUTION
# ============================================================

print(summary(nba_clean$MP))

p_minutes <- ggplot(
  nba_clean,
  aes(x = MP)
) +
  geom_histogram(
    bins = 35,
    fill = col_main,
    color = "white",
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = 250,
    color = col_accent,
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_x_continuous(
    labels = label_comma()
  ) +
  labs(
    title = "Distribution of Playing Time",
    x = "Total minutes played",
    y = "Number of observations"
  )

print(p_minutes)

save_plot(
  p_minutes,
  "02_playing_time.png"
)


# ============================================================
# 9. COMPARE MINUTE THRESHOLDS
# ============================================================

thresholds <- c(
  0, 50, 100, 150, 200,
  250, 300, 400, 500, 750, 1000
)

# All numeric basketball statistics.

stat_cols <- which(
  vapply(
    nba_clean,
    is.numeric,
    logical(1)
  ) &
    !(names(nba_clean) %in% c("Age", "MP"))
)

# Shooting percentages.

shooting_cols <- which(
  names(nba_clean) %in% c(
    "FG%",
    "3P%",
    "2P%",
    "eFG%",
    "FT%"
  )
)

threshold_results <- data.frame()

for (t in thresholds) {

  temp <- nba_clean[
    !is.na(nba_clean$MP) &
      nba_clean$MP >= t,
    ,
    drop = FALSE
  ]

  stats_temp <- temp[
    ,
    stat_cols,
    drop = FALSE
  ]

  shooting_temp <- temp[
    ,
    shooting_cols,
    drop = FALSE
  ]

  missing_count <- sum(
    is.na(stats_temp)
  )

  missing_pct <- if (
    length(as.matrix(stats_temp)) > 0
  ) {
    100 * mean(is.na(stats_temp))
  } else {
    NA_real_
  }

  shooting_missing_pct <- if (
    length(as.matrix(shooting_temp)) > 0
  ) {
    100 * mean(is.na(shooting_temp))
  } else {
    NA_real_
  }

  incomplete_count <- sum(
    !complete.cases(stats_temp)
  )

  complete_count <- nrow(temp) -
    incomplete_count

  complete_pct <- if (nrow(temp) > 0) {
    100 * complete_count / nrow(temp)
  } else {
    NA_real_
  }

  threshold_results <- rbind(
    threshold_results,

    data.frame(
      Minutes = t,

      Observations = nrow(temp),

      Retained_Percent = round(
        100 * nrow(temp) / nrow(nba_clean),
        2
      ),

      Missing_Cells = missing_count,

      Missing_Percent = round(
        missing_pct,
        2
      ),

      Shooting_Missing_Percent = round(
        shooting_missing_pct,
        2
      ),

      Incomplete_Observations =
        incomplete_count,

      Complete_Observations =
        complete_count,

      Complete_Percent = round(
        complete_pct,
        2
      )
    )
  )
}

print(threshold_results)


# ============================================================
# 10. THRESHOLD VISUALIZATIONS
# ============================================================

plot_threshold <- function(
    data,
    variable,
    title,
    y_label,
    filename
) {

  p <- ggplot(
    data,
    aes(
      x = Minutes,
      y = .data[[variable]]
    )
  ) +
    geom_line(
      color = col_main,
      linewidth = 1
    ) +
    geom_point(
      color = col_main,
      size = 3
    ) +
    geom_vline(
      xintercept = 250,
      color = col_accent,
      linetype = "dashed",
      linewidth = 0.9
    ) +
    scale_x_continuous(
      breaks = thresholds,
      guide = guide_axis(
        check.overlap = TRUE
      )
    ) +
    labs(
      title = title,
      x = "Minimum minutes played",
      y = y_label
    ) +
    theme_nba() +
    theme(
      text = element_text(
        family = font_nba
      ),
      plot.title = element_text(
        family = font_nba,
        size = 22,
        face = "bold",
        hjust = 0.5
      ),
      axis.title = element_text(
        family = font_nba,
        size = 19
      ),
      axis.text = element_text(
        family = font_nba,
        size = 17
      )
    )

  print(p)

  save_plot(
    p,
    filename
  )
}


plot_threshold(
  threshold_results,
  "Retained_Percent",
  "Observations Retained by Minutes Threshold",
  "Observations retained (%)",
  "03_threshold_retained.png"
)

plot_threshold(
  threshold_results,
  "Missing_Percent",
  "Missing Values by Minutes Threshold",
  "Missing cells (%)",
  "04_threshold_missing.png"
)

plot_threshold(
  threshold_results,
  "Shooting_Missing_Percent",
  "Missing Shooting Percentages by Minutes Threshold",
  "Missing shooting percentages (%)",
  "05_threshold_shooting_missing.png"
)

plot_threshold(
  threshold_results,
  "Complete_Percent",
  "Complete Observations by Minutes Threshold",
  "Complete observations (%)",
  "06_threshold_complete.png"
)


# ============================================================
# 11. APPLY 250-MINUTE THRESHOLD
# ============================================================

nba_filtered <- nba_clean[
  !is.na(nba_clean$MP) &
    nba_clean$MP >= 250,
  ,
  drop = FALSE
]

cat(
  "Observations after filtering:",
  nrow(nba_filtered),
  "\n"
)

cat(
  "Remaining missing values:",
  sum(is.na(nba_filtered)),
  "\n"
)

print(
  sort(
    colSums(is.na(nba_filtered)),
    decreasing = TRUE
  )
)

print(table(nba_filtered$Season))


# ============================================================
# 12. HISTOGRAMS
# ============================================================

plot_histogram <- function(
    data,
    variable,
    title,
    filename
) {

  median_value <- median(
    data[[variable]],
    na.rm = TRUE
  )

  p <- ggplot(
    data,
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
      x = paste0(
        variable,
        " per 36 minutes"
      ),
      y = "Number of observations"
    )

  print(p)

  save_plot(
    p,
    filename
  )
}


plot_histogram(
  nba_filtered,
  "PTS",
  "Distribution of Points",
  "07_points.png"
)

plot_histogram(
  nba_filtered,
  "3PA",
  "Distribution of Three-Point Attempts",
  "08_three_point_attempts.png"
)

plot_histogram(
  nba_filtered,
  "AST",
  "Distribution of Assists",
  "09_assists.png"
)

plot_histogram(
  nba_filtered,
  "TRB",
  "Distribution of Rebounds",
  "10_rebounds.png"
)


# ============================================================
# 13. CORRELATION ANALYSIS
# ============================================================

correlation_features <- c(
  "PTS",
  "FGA",
  "3PA",
  "FTA",
  "ORB",
  "DRB",
  "TRB",
  "AST",
  "STL",
  "BLK",
  "TOV"
)

correlation_data <- nba_filtered[
  ,
  match(
    correlation_features,
    names(nba_filtered)
  ),
  drop = FALSE
]

names(correlation_data) <-
  correlation_features

correlation_matrix <- cor(
  correlation_data,
  use = "pairwise.complete.obs"
)

print(round(correlation_matrix, 2))

correlation_plot <- as.data.frame(
  as.table(correlation_matrix)
)

names(correlation_plot) <- c(
  "Variable1",
  "Variable2",
  "Correlation"
)

p_corr <- ggplot(
  correlation_plot,
  aes(
    x = Variable1,
    y = Variable2,
    fill = Correlation
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.6
  ) +
  geom_text(
    aes(
      label = sprintf(
        "%.2f",
        Correlation
      )
    ),
    size = 5,
    family = font_nba,
    color = "black"
  ) +
  scale_fill_gradient2(
    low = col_main,
    mid = "white",
    high = col_accent,
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  coord_equal() +
  labs(
    title = "Correlation Between Player Statistics",
    x = NULL,
    y = NULL,
    fill = "Correlation"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      family = font_nba
    ),
    panel.grid = element_blank()
  )

print(p_corr)

save_plot(
  p_corr,
  "11_correlation_heatmap.png",
  9,
  7
)


# ============================================================
# 14. PREPARE DATA FOR UMAP
# ============================================================

umap_features <- c(
  "FGA",
  "3PA",
  "FTA",
  "ORB",
  "DRB",
  "AST",
  "STL",
  "BLK",
  "TOV"
)

stopifnot(
  all(
    umap_features %in%
      names(nba_filtered)
  )
)

X <- nba_filtered[
  ,
  match(
    umap_features,
    names(nba_filtered)
  ),
  drop = FALSE
]

names(X) <- umap_features

print(colSums(is.na(X)))

# Keep complete observations.

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

cat(
  "Observations used for UMAP:",
  nrow(X),
  "\n"
)

X_scaled <- scale(X)

stopifnot(
  all(is.finite(X_scaled))
)


# ============================================================
# 15. UMAP
# ============================================================

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


# ============================================================
# 16. UMAP VISUALIZATIONS
# ============================================================

# Overall UMAP

p_umap <- ggplot(
  nba_umap,
  aes(
    x = UMAP1,
    y = UMAP2
  )
) +
  geom_point(
    color = col_main,
    size = 1.5,
    alpha = 0.6
  ) +
  coord_equal() +
  labs(
    title = "UMAP Projection of NBA Players",
    x = "UMAP 1",
    y = "UMAP 2"
  ) +
  theme(
    panel.grid = element_blank()
  )

print(p_umap)

save_plot(
  p_umap,
  "12_umap_overall.png",
  8,
  6
)


# UMAP by season

p_umap_season <- ggplot(
  nba_umap,
  aes(
    x = UMAP1,
    y = UMAP2,
    color = as.factor(Season)
  )
) +
  geom_point(
    size = 1.6,
    alpha = 0.75
  ) +
  scale_color_manual(
    values = c(
      col_main,
      col_green,
      col_accent
    )
  ) +
  coord_equal() +
  labs(
    title = "UMAP Projection by Season",
    x = "UMAP 1",
    y = "UMAP 2",
    color = "Season"
  ) +
  theme(
    panel.grid = element_blank()
  )

print(p_umap_season)

save_plot(
  p_umap_season,
  "13_umap_season.png",
  8,
  6
)


# Five traditional positions

nba_umap_positions <- nba_umap[
  !is.na(nba_umap$Pos) &
    nba_umap$Pos %in% c(
      "PG",
      "SG",
      "SF",
      "PF",
      "C"
    ),
  ,
  drop = FALSE
]

nba_umap_positions$Pos <- factor(
  nba_umap_positions$Pos,
  levels = c(
    "PG",
    "SG",
    "SF",
    "PF",
    "C"
  )
)


# UMAP by traditional position

p_umap_position <- ggplot(
  nba_umap_positions,
  aes(
    x = UMAP1,
    y = UMAP2,
    color = Pos
  )
) +
  geom_point(
    size = 1.6,
    alpha = 0.75
  ) +
  scale_color_manual(
    values = position_colors,
    limits = c(
      "PG",
      "SG",
      "SF",
      "PF",
      "C"
    ),
    drop = FALSE
  ) +
  coord_equal() +
  labs(
    title = "UMAP Projection by Traditional Position",
    x = "UMAP 1",
    y = "UMAP 2",
    color = "Position"
  ) +
  theme(
    panel.grid = element_blank()
  )

print(p_umap_position)

save_plot(
  p_umap_position,
  "14_umap_position.png",
  8,
  6
)


# ============================================================
# 17. BOXPLOTS BY TRADITIONAL POSITION
# ============================================================

nba_positions <- nba_filtered[
  !is.na(nba_filtered$Pos) &
    nba_filtered$Pos %in% c(
      "PG",
      "SG",
      "SF",
      "PF",
      "C"
    ),
  ,
  drop = FALSE
]

nba_positions$Pos <- factor(
  nba_positions$Pos,
  levels = c(
    "PG",
    "SG",
    "SF",
    "PF",
    "C"
  )
)


plot_position_boxplot <- function(
    data,
    variable,
    title,
    filename
) {

  p <- ggplot(
    data,
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
      limits = c(
        "PG",
        "SG",
        "SF",
        "PF",
        "C"
      ),
      drop = FALSE
    ) +
    labs(
      title = title,
      x = "Traditional position",
      y = paste0(
        variable,
        " per 36 minutes"
      )
    ) +
    theme(
      legend.position = "none"
    )

  print(p)

  save_plot(
    p,
    filename
  )
}


plot_position_boxplot(
  nba_positions,
  "3PA",
  "Three-Point Attempts by Position",
  "15_boxplot_three_point_attempts.png"
)

plot_position_boxplot(
  nba_positions,
  "AST",
  "Assists by Position",
  "16_boxplot_assists.png"
)

plot_position_boxplot(
  nba_positions,
  "TRB",
  "Rebounds by Position",
  "17_boxplot_rebounds.png"
)

plot_position_boxplot(
  nba_positions,
  "PTS",
  "Points by Position",
  "18_boxplot_points.png"
)
