source("src/math_tools.R")

library(dplyr)
library(tidyr)
library(tibble)   

# ---- Settings ------------------------------------------------
# How the var are named by default in Gallup data
YEAR <- "YEAR_CALENDAR"
COUNTRY <- "WP5"
WELLBEING <- "WP16"
INCOME    <- "WP2319"
BASE_YEAR <- 2010

# Categories for each variable
CATS <- list(
  WP16   = 0:10,
  WP2319 = 0:3
)

# ---- Load and clean ------------------------------------------
gallup <- read.csv("data/gwp_preliminary_subset.csv")

# Remove Invalid Rows
gallup_clean <- gallup %>%
  filter(.data[[WELLBEING]] %in% CATS[[WELLBEING]],
         .data[[INCOME]]    %in% CATS[[INCOME]])

# ---- Build one panel for any set of variables ----------------
# vars = one name (univariate) or several names (joint)
build_panel <- function(df, vars, prefix) {
  
  cats <- CATS[vars]   # named list, one entry per variable
  
  # Summary statistics for each country-year
  stats <- df %>%
    group_by(across(all_of(c(COUNTRY, YEAR)))) %>%
    group_modify(function(d, key) {
      s <- distributional_summary(d[vars], cats)
      as_tibble_row(unlist(s))
    }) %>%
    ungroup()
  
  # Transvariation vs. base year for each country-year
  tv <- df %>%
    group_by(across(all_of(COUNTRY))) %>%
    group_modify(function(d, key) {
      base  <- d[d[[YEAR]] == BASE_YEAR, vars, drop = FALSE]
      years <- sort(unique(d[[YEAR]]))
      
      tvs <- vapply(years, function(yr) {
        if (nrow(base) == 0) return(NA_real_)
        transvariation(
          base,
          d[d[[YEAR]] == yr, vars, drop = FALSE],
          categories = cats
        )$Transvariation
      }, numeric(1))
      
      tibble(!!YEAR := years, TV = tvs)
    }) %>%
    ungroup()
  
  left_join(stats, tv, by = c(COUNTRY, YEAR)) %>%
    rename_with(~ paste0(prefix, .x), -c(COUNTRY, YEAR))
}

# ---- Full panel ----------------------------------------------
panel <- build_panel(gallup_clean, WELLBEING, "WB_") %>%
  left_join(
    build_panel(gallup_clean, INCOME, "INCOME_"),
    by = c(COUNTRY, YEAR)
  ) %>%
  left_join(
    build_panel(gallup_clean, c(WELLBEING, INCOME), "JOINT_"),
    by = c(COUNTRY, YEAR)
  )

print(head(panel))


## Tests
test <- distributional_summary(gallup_clean[["WP16"]], 1:10)
m_test <- mode_tools(gallup_clean[["WP16"]], 1:10)
