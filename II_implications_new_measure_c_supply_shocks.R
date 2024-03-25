rm(list = ls())
cat('\014')
gc()  # free unused RAM

library(tidyverse)
library(stargazer)
# library(lubridate)
library(patchwork)

# read data
data_monthly_original <- readRDS(file = "datasets/data_monthly_original.rds") %>% 
  mutate(month = month(date), year = year(date))
print(colnames(data_monthly_original))

# ---
# Output ----
# ---

# 36 lags behind => so we exclude the first year
data_monthly_original_output <- data_monthly_original %>% 
  filter(year>1966)

# function to run output regresions
reg_output <- function(output_var = 'pcipnsa', indep_var = 'resid', verbose = F){
  # formula output
  lag_output_formula <- str_flatten(sapply(1:24, function (x) str_c('lag(', output_var, ', ', x, ') + ')))
  lag_monetary_formula <- str_flatten(sapply(1:36, function (x) str_c('lag(', indep_var, ', ', x, ') + ')))
  formula_output <- as.formula(str_c(output_var, ' ~ ', lag_output_formula, lag_monetary_formula, 'factor(month)'))
  # run the model
  model_output <- lm(formula_output, data = data_monthly_original_output)
  if (verbose) {
    print(summary(model_output))
  }
  
  return(model_output)
}

# function to run output regresions with commodities
reg_output_com <- function(output_var = 'pcipnsa', indep_var = 'resid', verbose = F){
  # formula output
  lag_output_formula <- str_flatten(sapply(1:24, function (x) str_c('lag(', output_var, ', ', x, ') + ')))
  lag_monetary_formula <- str_flatten(sapply(1:36, function (x) str_c('lag(', indep_var, ', ', x, ') + ')))
  lag_commodity_formula <- str_flatten(sapply(1:12, function (x) str_c('lag(', 'pcwcp', ', ', x, ') + ')))
  formula_output <- as.formula(str_c(output_var, ' ~ ', lag_output_formula, lag_monetary_formula, lag_commodity_formula,  'factor(month)'))
  # run the model
  model_output <- lm(formula_output, data = data_monthly_original_output)
  if (verbose) {
    print(summary(model_output))
  }
  
  return(model_output)
}

# function to calculate monetary shocks
calculate_mon_shock <- function(model_coef = model_actual$coefficients, output_var='pcipnsa', indep_var = "dff") {
  # select coefficients
  model_coef_dep <- c(model_coef[str_subset(names(model_coef), str_c(output_var, ','))], rep(0, 24))
  model_coef_mon <- c(model_coef[str_subset(names(model_coef), str_c(indep_var, ','))], rep(0, 12))
  
  #
  n <- length(model_coef_mon)
  mon_shock <- numeric(n)
  mon_shock[1] <- model_coef_mon[1]
  
  for (i in 2:n) {
    mon_shock[i] <- model_coef_mon[i] + sum(model_coef_dep[1:(i-1)] * mon_shock[(i-1):1])
  }
  
  return(mon_shock)
}


model_industrial <- reg_output(verbose = T)
model_industrial_com <- reg_output_com(verbose = T)
# alternative measures
model_actual <- reg_output(output_var = 'pcipnsa', indep_var = 'dff')
model_actual_com <- reg_output_com(output_var = 'pcipnsa', indep_var = 'dff')

# calculate monetary shocks
shock_industrial <- cumsum(calculate_mon_shock(model_coef = model_industrial$coefficients, output_var =  'pcipnsa', indep_var = "resid"))
shock_industrial_com <- cumsum(calculate_mon_shock(model_coef = model_industrial_com$coefficients, output_var =  'pcipnsa', indep_var = "resid"))
#
shock_actual <- cumsum(calculate_mon_shock(model_coef = model_actual$coefficients, output_var='pcipnsa', indep_var = "dff"))
shock_actual_com <- cumsum(calculate_mon_shock(model_coef = model_actual_com$coefficients, output_var='pcipnsa', indep_var = "dff"))



# ---
## Graphs ----
# ---

# Industrial 
plot_industrial <- ggplot(mapping = aes(x = 1:48)) +
  geom_line(mapping = aes(y = shock_industrial_com), linetype = "dotdash", color = "black") +
  geom_line(mapping = aes(y = shock_industrial), linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  labs(y = 'Percent', x = 'Months After Shock', title = "a. Using the New Measure of Monetary Policy Shocks") +
  scale_y_continuous(limits = c(-0.07, 0.02), breaks = seq(-0.07, 0.02, by = 0.01)) + 
  scale_x_continuous(breaks = seq(0, 48, by = 3), limits = c(0, 48), expand = c(0, 0)) +
  annotate("text", x = 15, y = -0.015, label = "With Commodity Prices", hjust = 0, size = 3.5) +
  annotate("text", x = 24, y = -0.05, label = "Without Commodity Prices", hjust = 0, size = 3.5)

# Alternative measures

# Actual Funds rate 
plot_actual <- ggplot(mapping = aes(x = 1:48)) +
  geom_line(mapping = aes(y = shock_actual_com), linetype = "dotdash", color = "black") +
  geom_line(mapping = aes(y = shock_actual), linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  annotate("text", x = 27, y = -0.01, label = "With Commodity Prices", hjust = 0, size = 3.5) +
  annotate("text", x = 27, y = -0.035, label = "Without Commodity Prices", hjust = 0, size = 3.5) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(y = 'Percent', x = 'Months After Shock', title = "b. Using the Change in the Actual Federal Funds Rate") +
  scale_y_continuous(limits = c(-0.07, 0.02), breaks = seq(-0.07, 0.02, by = 0.01)) +  
  scale_x_continuous(breaks = seq(0, 48, by = 3), limits = c(0, 48), expand = c(0, 0))

# Combine the two plots into one panel
figure_7 <- plot_industrial / plot_actual + 
  plot_layout(guides = 'collect') +
  plot_annotation(title = 'Figure 7. The Effect of Monetary Policy on Output With and Without Commodity Prices')

# Print the combined plot
print(figure_7)

rm(data_monthly_original_output, model_actual, model_actual_com, model_industrial, model_industrial_com,
   plot_actual, plot_industrial, shock_actual, shock_actual_com, shock_industrial, shock_industrial_com,
   calculate_mon_shock, reg_output, reg_output_com)

# ---
# Prices ----
# ---

# 48 lags behind => so we don't exclude the first year
data_monthly_original_price <- data_monthly_original

# function to run output regresions
reg_output <- function(output_var = 'pcppinsa', indep_var = 'resid', verbose = F){
  # formula output
  lag_output_formula <- str_flatten(sapply(1:24, function (x) str_c('lag(', output_var, ', ', x, ') + ')))
  lag_monetary_formula <- str_flatten(sapply(1:48, function (x) str_c('lag(', indep_var, ', ', x, ') + ')))
  formula_output <- as.formula(str_c(output_var, ' ~ ', lag_output_formula, lag_monetary_formula, 'factor(month)'))
  # run the model
  model_output <- lm(formula_output, data = data_monthly_original_price)
  if (verbose) {
    print(summary(model_output))
  }
  
  return(model_output)
}

# function to run output regresions with commodities
reg_output_com <- function(output_var = 'pcppinsa', indep_var = 'resid', verbose = F){
  # formula output
  lag_output_formula <- str_flatten(sapply(1:24, function (x) str_c('lag(', output_var, ', ', x, ') + ')))
  lag_monetary_formula <- str_flatten(sapply(1:48, function (x) str_c('lag(', indep_var, ', ', x, ') + ')))
  lag_commodity_formula <- str_flatten(sapply(1:12, function (x) str_c('lag(', 'pcwcp', ', ', x, ') + ')))
  formula_output <- as.formula(str_c(output_var, ' ~ ', lag_output_formula, lag_monetary_formula, lag_commodity_formula,  'factor(month)'))
  # run the model
  model_output <- lm(formula_output, data = data_monthly_original_price)
  if (verbose) {
    print(summary(model_output))
  }
  
  return(model_output)
}

# function to calculate monetary shocks
calculate_mon_shock <- function(model_coef = model_price$coefficients, output_var='pcppinsa', indep_var = "resid") {
  # select coefficients
  model_coef_dep <- c(model_coef[str_subset(names(model_coef), str_c(output_var, ','))], rep(0, 24))
  model_coef_mon <- c(model_coef[str_subset(names(model_coef), str_c(indep_var, ','))])
  
  #
  n <- length(model_coef_mon)
  mon_shock <- numeric(n)
  mon_shock[1] <- model_coef_mon[1]
  
  for (i in 2:n) {
    mon_shock[i] <- model_coef_mon[i] + sum(model_coef_dep[1:(i-1)] * mon_shock[(i-1):1])
  }
  
  return(mon_shock)
}

model_price <- reg_output(verbose = T)
model_price_com <- reg_output_com(verbose = T)
# alternative measures
model_actual <- reg_output(output_var = 'pcppinsa', indep_var = 'dff')
model_actual_com <- reg_output_com(output_var = 'pcppinsa', indep_var = 'dff')

# calculate monetary shocks
shock_price<- cumsum(calculate_mon_shock(model_coef = model_price$coefficients, output_var =  'pcppinsa', indep_var = "resid"))
shock_price_com <- cumsum(calculate_mon_shock(model_coef = model_price_com$coefficients, output_var =  'pcppinsa', indep_var = "resid"))
#
shock_actual <- cumsum(calculate_mon_shock(model_coef = model_actual$coefficients, output_var='pcppinsa', indep_var = "dff"))
shock_actual_com <- cumsum(calculate_mon_shock(model_coef = model_actual_com$coefficients, output_var='pcppinsa', indep_var = "dff"))

# ---
## Graphs ----
# ---

# Figure 8a
plot_price <- ggplot(mapping = aes(x = 1:48)) +
  geom_line(mapping = aes(y = shock_price), linetype = "solid", color = "black") +
  geom_line(mapping = aes(y = shock_price_com), linetype = "dotdash", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(y = 'Percent', x = 'Months After Shock', title = "a. Using the New Measure of Monetary Policy Shocks") +
  scale_y_continuous(limits = c(-0.07, 0.03), breaks = seq(-0.07, 0.03, by = 0.01)) + 
  scale_x_continuous(breaks = seq(0, 48, by = 3), limits = c(0, 48), expand = c(0, 0)) +
  annotate("text", x = 30, y = -0.01, label = "With Commodity Prices", hjust = 0, size = 3.5) +
  annotate("text", x = 24, y = -0.03, label = "Without Commodity Prices", hjust = 0, size = 3.5)

# Figure 8b
plot_actual <- ggplot(mapping = aes(x = 1:48)) +
  geom_line(mapping = aes(y = shock_actual_com), linetype = "dotdash", color = "black") +
  geom_line(mapping = aes(y = shock_actual), linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  annotate("text", x = 3, y = 0.01, label = "Without Commodity Prices", hjust = 0, size = 3.5) +
  annotate("text", x = 9, y = -0.005, label = "With Commodity Prices", hjust = 0, size = 3.5) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(y = 'Percent', x = 'Months After Shock', title = "b. Using the Change in the Actual Federal Funds Rate") +
  scale_y_continuous(limits = c(-0.07, 0.02), breaks = seq(-0.07, 0.02, by = 0.01)) +  
  scale_x_continuous(breaks = seq(0, 48, by = 3), limits = c(0, 48), expand = c(0, 0))

# Combine the two plots into one panel
figure_8 <- plot_price / plot_actual + 
  plot_layout(guides = 'collect') +
  plot_annotation(title = 'Figure 8. The Effect of Monetary Policy on The Price Level With and Without Commodity Prices')

# Print the combined plot
print(figure_8)

rm(model_actual, model_actual_com, model_price, model_price_com, plot_actual, plot_price, 
   shock_actual, shock_actual_com, shock_price, shock_price_com, calculate_mon_shock, reg_output,
   reg_output_com, data_monthly_original_price)
