rm(list = ls())
cat('\014')
gc()  # free unused RAM

library(tidyverse)
library(stargazer)
# library(lubridate)
# library(patchwork)

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

# function to get bootstrap values
bootstrap_se <- function(model, output_var='pcipnsa', indep_var = "resid", look_path = F){
  # select model coefficients 
  model_coef <- model$coefficients
  
  # generate bootstrap values
  model_coef_bootstrap <- MASS::mvrnorm(500, model_coef, vcov(model))
  
  # loop
  model_bootstrap <- matrix(NA, 500, 48)
  for (i in 1:500) {
    model_coef <- model_coef_bootstrap[i,]
    model_bootstrap[i, ] <- cumsum(calculate_mon_shock(model_coef = model_coef, output_var =  output_var, indep_var = indep_var))
  }
  
  if (look_path) {
    # plot boots
    plot(1:48, model_bootstrap[1, ], type = "l", ylim = range(model_bootstrap), 
         xlab = "Months After Shock", ylab = "Percent", 
         main = str_c("Different Paths Under Bootstrap (", output_var, ")"))
    for (i in 2:500) {
      lines(1:ncol(model_bootstrap), model_bootstrap[i, ], col = rainbow(nrow(model_bootstrap))[i])
    }
  }
  
  std_errors_bootstrap <- c()
  for (i in 1:48) {
    std_errors_bootstrap[i] <- sd(model_bootstrap[,i])
  }
  
  return(std_errors_bootstrap)
} 

model_industrial <- reg_output(verbose = T)
# alternative measures
model_actual <- reg_output(output_var = 'pcipnsa', indep_var = 'dff')
model_intended <- reg_output(indep_var = 'dtarg')
model_actual_forecasts <- reg_output(indep_var = 'residf')

# calculate monetary shocks
shock_industrial <- cumsum(calculate_mon_shock(model_coef = model_industrial$coefficients, output_var =  'pcipnsa', indep_var = "resid"))
#
shock_actual <- cumsum(calculate_mon_shock(model_coef = model_actual$coefficients, output_var='pcipnsa', indep_var = "dff"))
shock_intended <- cumsum(calculate_mon_shock(model_coef = model_intended$coefficients, output_var = 'pcipnsa', indep_var = "dtarg"))
shock_actual_forecasts <- cumsum(calculate_mon_shock(model_coef = model_actual_forecasts$coefficients, 'pcipnsa', indep_var = "residf"))

# calculating bootstrap errors
bootstrap_se_industrial <- bootstrap_se(model = model_industrial, output_var='pcipnsa', indep_var = "resid", look_path = F)
#
bootstrap_se_actual <- bootstrap_se(model = model_actual, output_var = 'pcipnsa', indep_var = 'dff', look_path = F)
bootstrap_se_intended <- bootstrap_se(model = model_intended, output_var = 'pcipnsa', indep_var = 'dtarg', look_path = F)
bootstrap_se_actual_forecasts <- bootstrap_se(model = model_actual_forecasts, output_var = 'pcipnsa', indep_var = 'residf', look_path = F)

# ---
## Table 2 ----
# ---


# ---
## Graphs ----
# ---

# Industrial 
plot_industrial <- ggplot(mapping=aes(x=1:48)) +
  geom_line(mapping=aes(y=shock_industrial)) + 
  geom_line(mapping=aes(y=shock_industrial-bootstrap_se_industrial), linetype=2) +
  geom_line(mapping=aes(y=shock_industrial+bootstrap_se_industrial), linetype=2) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 2)) +
  labs(y = 'Percent', x = 'Months After Shock') +
  scale_y_continuous(n.breaks = 10) + 
  scale_x_continuous(n.breaks=16, limits = c(0, 48), expand = c(0, 0))

print(plot_industrial)

# Alternative measures

# Actual Funds rate 
plot_actual <- ggplot(mapping=aes(x=1:48)) +
  geom_line(mapping=aes(y=shock_actual)) + 
  geom_line(mapping=aes(y=shock_actual-bootstrap_se_actual), linetype=2) +
  geom_line(mapping=aes(y=shock_actual+bootstrap_se_actual), linetype=2) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 2)) +
  labs(y = 'Percent', x = 'Months After Shock') +
  scale_y_continuous(n.breaks = 10) +  
  ylim(-0.07, 0.02) +
  scale_x_continuous(n.breaks=16, limits = c(0, 48), expand = c(0, 0))

print(plot_actual)

# Intermediate Broader measures
plot_intermediate <- ggplot(mapping=aes(x=1:48))  +
  geom_line(mapping = aes(y = shock_actual_forecasts, linetype = "Actual Forecasts")) + 
  geom_line(mapping = aes(y = shock_intended, linetype = "Intended")) +
  scale_linetype_manual(values = c("Actual Forecasts" = "dotdash", "Intended" = "solid")) + 
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 2)) +
  labs(y = 'Percent', x = 'Months After Shock', linetype = "Series") +
  scale_y_continuous(n.breaks = 10) + 
  ylim(-0.07, 0.02) +
  scale_x_continuous(n.breaks=16, limits = c(0, 48), expand = c(0, 0))

print(plot_intermediate)

# ---
# Price ----
# ---

reg_price <- function(price_var = 'pcppinsa', verbose = F){
  # formula price
  lag_price_formula <- str_flatten(sapply(1:24, function (x) str_c('lag(', price_var, ', ', x, ') + ')))
  lag_monetary_formula <- str_flatten(sapply(1:48, function (x) str_c('lag(sumshck, ', x, ') + ')))
  formula_price <- as.formula(str_c(price_var, ' ~ ', lag_price_formula, lag_monetary_formula, 'factor(month)'))
  # run the model
  model_price <- lm(formula_price, data = data_monthly_original)
  if (verbose) {
    print(summary(model_price))
  }
  
  return(model_price)
}

model_ppp <- reg_price('pcppinsa', T)
# alternative measures
model_pce <- reg_price('pccpinsa')
model_cpi <- reg_price('pcpcegsa')



# ---
# Tests ----
# ---

model <- model_actual_forecasts
dep_var <- 'residf'

# select model coefficients 
model_coef <- model$coefficients

# generate bootstrap values
model_coef_bootstrap <- MASS::mvrnorm(500, model_coef, vcov(model))

a <- cumsum(calculate_mon_shock(model_coef_mon, model_coef_dep))
length(model_coef_mon)

# loop
model_bootstrap <- matrix(NA, 500, 48)
for (i in 1:500) {
  model_coef <- model_coef_bootstrap[i,]
  model_coef_dep <- c(model_coef[str_subset(names(model_coef), str_c(dep_var, ','))], rep(0, 24))
  model_coef_mon <- c(model_coef[str_subset(names(model_coef), 'resid,')], rep(0, 12))
  
  model_bootstrap[i,] <- cumsum(calculate_mon_shock(model_coef_mon, model_coef_dep))
}

if (look_path) {
  # plot boots
  plot(1:48, model_bootstrap[1, ], type = "l", ylim = range(model_bootstrap), 
       xlab = "Months After Shock", ylab = "Percent", 
       main = str_c("Different Paths Under Bootstrap (", dep_var, ")"))
  for (i in 2:500) {
    lines(1:ncol(model_bootstrap), model_bootstrap[i, ], col = rainbow(nrow(model_bootstrap))[i])
  }
}

std_errors_bootstrap <- c()
for (i in 1:48) {
  std_errors_bootstrap[i] <- sd(model_bootstrap[,i])
}

return(std_errors_bootstrap)

  