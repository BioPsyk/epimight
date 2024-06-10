#!/usr/bin/env Rscript

library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
library(readr, quietly = TRUE, warn.conflicts = FALSE)

args <- commandArgs(trailingOnly = TRUE)

show_usage <- function() {
  cat("usage: plot-benchmark-sumstats.R [INPUT PATH] [TITLE]\n")
}

if (length(args) < 1) {
  show_usage()
  stop("Input path was not given")
}

if (length(args) < 2) {
  show_usage()
  stop("Title was not given")
}

input_path <- args[1]
title      <- args[2]
sumstats   <- read_csv(input_path)
plot       <- ggplot(sumstats, aes(x = n, y = mean, color = expr)) +
  geom_line() +
  labs(
    title = title,
    x     = "Samples",
    y     = "Mean runtime (seconds)",
    color = "Implementation"
  )

output_path <- sprintf("%s.png", input_path)
ggsave(output_path)
