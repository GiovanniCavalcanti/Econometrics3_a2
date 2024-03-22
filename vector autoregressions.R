rm(list = ls())
gc()

library(tidyverse)
library(xts)
library(vars)

data_monthly_original <- readRDS(file = "datasets/data_monthly_original.rds")
data_monthly_expanded <- readRDS(file = "datasets/data_monthly_expanded.rds")


# original ----------------------------------------------------------------


# expanded ----------------------------------------------------------------

data_monthly_expanded$mtgdate <- as.Date(as.yearmon(data_monthly_expanded$mtgdate))

# Filter dataset
filtered_data <- data_monthly_expanded %>%
  filter(mtgdate >= as.Date("1969-03-01") & mtgdate <= as.Date("2007-12-31"))

# Convert to xts for VAR analysis
VAR_data <- xts(dplyr::select(filtered_data, lnipnsa, lnppinsa, sumshck), order.by=filtered_data$mtgdate)

VAR_est <- VAR(VAR_data, p = 36, type = "const")

irf_results <- irf(VAR_est, n.ahead = 48, ci = 0.67, boot = TRUE, ortho = FALSE, cumulative = TRUE, runs = 500)

plot(irf_results, impulse = "sumshck", response)

teste <- irf(VAR_est, impulse = "sumshck", response = "sumshck", n.ahead = 48, ortho = FALSE,
               cumulative = TRUE, boot = TRUE, ci = 0.9, runs = 100)
#png("figs/irf_asy_quarterly.png", width = 700, height = 500)
plot(teste)

teste <- irf(VAR_est, impulse = "sumshck", response = "lnipnsa", n.ahead = 48, ortho = FALSE,
             cumulative = TRUE, boot = TRUE, ci = 0.9, runs = 100)
#png("figs/irf_asy_quarterly.png", width = 700, height = 500)
plot(teste)

teste <- irf(VAR_est, impulse = "sumshck", response = "lnppinsa", n.ahead = 48, ortho = FALSE,
             cumulative = TRUE, boot = TRUE, ci = 0.9, runs = 100)
#png("figs/irf_asy_quarterly.png", width = 700, height = 500)
plot(teste)










