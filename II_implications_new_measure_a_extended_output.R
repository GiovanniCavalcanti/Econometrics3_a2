rm(list = ls())
cat('\014')
gc()  # free unused RAM

library(tidyverse)
library(stargazer)
# library(lubridate)
library(patchwork)

# read data
data_monthly_extended <- readRDS(file = "datasets/data_monthly_extended.rds") %>% 
  mutate(month = month(date), year = year(date))
print(colnames(data_monthly_extended))

# ---
# Output ----
# ---

# 36 lags behind => so we exclude the first year
data_monthly_extended_output <- data_monthly_extended %>% 
  filter(year > 1966, year < 2008) %>% 
  mutate(across(everything(), function(x) replace_na(x, 0)))

# function to run output regresions
reg_output <- function(output_var = 'pcipnsa', indep_var = 'resid', model_data=data_monthly_extended_output, verbose = F){
  # formula output
  lag_output_formula <- str_flatten(sapply(1:24, function (x) str_c('lag(', output_var, ', ', x, ') + ')))
  lag_monetary_formula <- str_flatten(sapply(1:36, function (x) str_c('lag(', indep_var, ', ', x, ') + ')))
  formula_output <- as.formula(str_c(output_var, ' ~ ', lag_output_formula, lag_monetary_formula, 'factor(month)'))
  # run the model
  model_output <- lm(formula_output, data = data_monthly_extended_output)
  if (verbose) {
    print(summary(model_output))
  }
  
  return(model_output)
}

# function to calculate monetary shocks
calculate_mon_shock <- function(model_coef, output_var='pcipnsa', indep_var = "dff") {
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

model_industrial <- reg_output(verbose = F)
# alternative measures
model_actual <- reg_output(output_var = 'pcipnsa', indep_var = 'dff')
# model_intended <- reg_output(indep_var = 'dtarg')
# model_actual_forecasts <- reg_output(indep_var = 'residf')

# calculate monetary shocks
shock_industrial <- cumsum(calculate_mon_shock(model_coef = model_industrial$coefficients, output_var =  'pcipnsa', indep_var = "resid"))
#
shock_actual <- cumsum(calculate_mon_shock(model_coef = model_actual$coefficients, output_var='pcipnsa', indep_var = "dff"))
# shock_intended <- cumsum(calculate_mon_shock(model_coef = model_intended$coefficients, output_var = 'pcipnsa', indep_var = "dtarg"))
# shock_actual_forecasts <- cumsum(calculate_mon_shock(model_coef = model_actual_forecasts$coefficients, 'pcipnsa', indep_var = "residf"))

# calculating bootstrap errors
bootstrap_se_industrial <- bootstrap_se(model = model_industrial, 
                                        output_var='pcipnsa', 
                                        indep_var = "resid", 
                                        look_path = T)
#
bootstrap_se_actual <- bootstrap_se(model = model_actual, output_var = 'pcipnsa', indep_var = 'dff', look_path = F)
# bootstrap_se_intended <- bootstrap_se(model = model_intended, output_var = 'pcipnsa', indep_var = 'dtarg', look_path = F)
# bootstrap_se_actual_forecasts <- bootstrap_se(model = model_actual_forecasts, output_var = 'pcipnsa', indep_var = 'residf', look_path = F)

# ---
## Table 3 ----
# ---

# table_3e <- stargazer(model_industrial, type = "text", 
#           column.labels = c("Monetary policy shock", "Change in industrial production"),
#           omit = c("Constant", "month"),
#           title = "Table 3—The Impact of Monetary Policy Shocks on Industrial Production",
#           notes = c("R² = 0.86; D.W. = 2.01; s.e.e. = 0.009; N = 324. The sample period is 1970:1-1996:12.",
#                     "Coefficients and standard errors for the constant term and monthly dummies are not reported."),
#           notes.align = "l",
#           intercept.bottom = FALSE, 
#           omit.stat = c("adj.rsq", "f", "ser"), 
#           digits = 4)

# organizing to make like the paper table
table3_aspaper <- matrix(NA, 36, 6)
output_coef <- model_industrial$coefficients
output_se <- sqrt(diag(vcov(model_industrial)))
# lags columns
table3_aspaper[,1] <- 1:36
table3_aspaper[1:24,4] <- 1:24
# coefficient columns
table3_aspaper[,2] <- output_coef[str_subset(names(output_coef), 'resid')]
table3_aspaper[1:24,5] <- output_coef[str_subset(names(output_coef), 'pcipnsa')]
# se errors column
table3_aspaper[,3] <- output_se[str_subset(names(output_se), 'resid')]
table3_aspaper[1:24,6] <- output_se[str_subset(names(output_se), 'pcipnsa')]

# stargazer(round(table3_aspaper, 4), out='output/table3e.tex', summary = F, header=F, digits=4)


# ---
## Graphs ----
# ---

# Industrial 
figure_2e <- ggplot(mapping=aes(x=1:48)) +
  geom_line(mapping=aes(y=shock_industrial), color="black") + 
  geom_ribbon(aes(ymin=shock_industrial-bootstrap_se_industrial, ymax=shock_industrial+bootstrap_se_industrial), alpha=0.2) +
  geom_hline(yintercept = 0, linetype="dotted") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(y = 'Percent', x = 'Months After Shock', title="Figure 2. The Effect of Monetary Policy on Output") +
  # scale_y_continuous(limits = c(-1, 0.02), breaks = seq(-1, 0.02, by = 0.1)) + 
  scale_x_continuous(breaks=seq(0, 48, by = 3), limits = c(0, 48), expand = c(0, 0))


print(figure_2e)

# Alternative measures

# Actual Funds rate 
figure_3e_a <- ggplot(mapping=aes(x=1:48)) +
  geom_line(mapping=aes(y=shock_actual), color="black") + 
  geom_line(mapping=aes(y=shock_actual-bootstrap_se_actual), linetype="dotted") +
  geom_line(mapping=aes(y=shock_actual+bootstrap_se_actual), linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(y = 'Percent', x = 'Months After Shock') +
  ggtitle("a. Using the Change in the Actual Federal Funds Rate") +
  # scale_y_continuous(limits = c(-0.07, 0.02), breaks = seq(-0.07, 0.02, by = 0.01)) +  
  scale_x_continuous(breaks=seq(0, 48, by = 3), limits = c(0, 48), expand = c(0, 0))

print(figure_3e_a)


# # Intermediate Broader measures
# plot_intermediate <- ggplot(mapping=aes(x=1:48))  +
#   geom_line(mapping = aes(y = shock_actual_forecasts), linetype = "dotdash") + 
#   geom_line(mapping = aes(y = shock_intended), linetype = "solid") +
#   geom_hline(yintercept = 0, linetype="dotted") +
#   annotate("text", x = 10, y = 0.005, label = "Change in the Intended Funds Rate", hjust = 0, size = 3) +
#   annotate("text", x = 6, y = -0.02, label = "Change in the Actual Funds Rate Controlling for Forecasts", hjust = 0, size = 3) +
#   theme_minimal() +
#   theme(
#     panel.border = element_rect(color = "black", fill = NA, size = 2),
#     legend.position = "none",
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   labs(y = 'Percent', x = 'Months After Shock') +
#   scale_y_continuous(limits = c(-0.07, 0.02), breaks = seq(-0.07, 0.02, 0.01)) + 
#   scale_x_continuous(breaks=seq(0, 48, 3), limits = c(0, 48), expand = c(0, 0)) +
#   ggtitle("b. Using the Intermediate Broader Measures")
# 
# figure_3 <- plot_actual / plot_intermediate + 
#   plot_layout(guides = 'collect') +
#   plot_annotation(title = 'FIGURE 3. THE EFFECT OF BROADER MEASURES OF MONETARY POLICY ON OUTPUT')

# Print the combined plot
ggsave(plot=figure_2e, filename='output/figure2e.png', scale=2)
ggsave(plot=figure_3e_a, filename = 'output/figure3e_a.png', scale = 2)

# ---
# removing stuff
# ---

# List all objects in the environment
all_objects <- ls()

# Specify the objects you want to keep
objects_to_keep <- c("table_3", "figure_2", "figure_3")

# Identify objects to remove (those not in the objects_to_keep list)
objects_to_remove <- setdiff(all_objects, objects_to_keep)

# Remove the objects_to_remove
if (length(objects_to_remove) > 0) {
  rm(list = objects_to_remove)
}