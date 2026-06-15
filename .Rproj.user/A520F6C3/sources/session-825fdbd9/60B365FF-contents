library(tidyverse)
library(jsonlite)
library(lme4)
library(lmerTest)
library(emmeans)
library(knitr)
library(kableExtra)
library(svglite)
library(psych)

data_raw_dir <- "../data/raw"
data_derived_dir <- "../data/derived"
fig_dir <- "../figures"
output_dir <- "../outputs"

GROUP_LEVELS <- c("CC", "CP", "PC", "PP")

GROUP_COLORS <- c(
  CC = "#CC79A7",
  CP = "#0072B2",
  PC = "#D55E00",
  PP = "#009E73"
)

theme_plotwise <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      legend.position = "top",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray40"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text = element_text(color = "black")
    )
}

save_plot_all <- function(plot, filename, width = 8, height = 5, dpi = 300) {
  ggsave(file.path(fig_dir, paste0(filename, ".png")), plot, width = width, height = height, dpi = dpi)
  ggsave(file.path(fig_dir, paste0(filename, ".svg")), plot, width = width, height = height, device = svglite::svglite)
  ggsave(file.path(fig_dir, paste0(filename, ".pdf")), plot, width = width, height = height, device = cairo_pdf)
}