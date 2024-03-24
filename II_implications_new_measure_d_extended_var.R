rm(list = ls())
gc()

library(tidyverse)
library(xts)
library(vars)
library(patchwork)

data_monthly_extended <- readRDS(file = "datasets/data_monthly_extended.rds") %>%
  filter(mtgdate > "1969-02-01", mtgdate < "2008-01-01")

# original ----------------------------------------------------------------
# Convert to xts for VAR analysis
VAR_data <- xts(dplyr::select(data_monthly_extended, lnipnsa, lnppinsa, sumshck), order.by=data_monthly_extended$mtgdate)

# Building VAR

VAR_model <- VAR(VAR_data[,1:3], p = 36, type = "const", season = NULL)
summary(VAR_model)

response_1 <- irf(VAR_model, impulse = "sumshck", response = c("sumshck"), n.ahead = 48, boot = FALSE)
response_2 <- irf(VAR_model, impulse = "sumshck", response = c("lnipnsa", "lnppinsa"), n.ahead = 48, boot = FALSE)


calculate_mon_shock <- function(model_coef = model_actual$coefficients, output_var='pcipnsa', indep_var = "dff") {
  # select coefficients
  model_coef_dep <- c(model_coef[str_subset(names(model_coef), str_c(output_var, '.'))], rep(0, 12))
  model_coef_mon <- c(model_coef[str_subset(names(model_coef), str_c(indep_var, '.'))], rep(0, 12))
  
  #
  n <- length(model_coef_mon)
  mon_shock <- numeric(n)
  mon_shock[1] <- model_coef_mon[1]
  
  for (i in 2:n) {
    mon_shock[i] <- model_coef_mon[i] + sum(model_coef_dep[1:(i-1)] * mon_shock[(i-1):1])
  }
  
  return(mon_shock)
}
# Bootstrap the ci's
# function to get bootstrap values
bootstrap_se <- function(model, output_var='lnipnsa', indep_var = "sumshck", look_path = F){
  # select model coefficients dynamically based on output_var
  model_coef <- model$varresult[[output_var]]$coefficients
  
  # generate bootstrap values, using dynamic access to the vcov matrix
  model_coef_bootstrap <- MASS::mvrnorm(500, model_coef, vcov(model$varresult[[output_var]]))
  
  # loop
  model_bootstrap <- matrix(NA, 500, 48)
  for (i in 1:500) {
    model_coef <- model_coef_bootstrap[i,]
    model_bootstrap[i, ] <- cumsum(calculate_mon_shock(model_coef = model_coef, output_var =  output_var, indep_var = indep_var))
  }
  
  if (look_path) {
    # plot boots, adjusted for dynamic output_var
    plot(1:48, model_bootstrap[1, ], type = "l", ylim = range(model_bootstrap), 
         xlab = "Months After Shock", ylab = "Percent", 
         main = paste("Different Paths Under Bootstrap (", output_var, ")", sep=""))
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

# calculating bootstrap errors
bootstrap_se_sumshck <- bootstrap_se(model = VAR_model, output_var='sumshck', indep_var = "sumshck", look_path = F)
bootstrap_se_lnipnsa <- bootstrap_se(model = VAR_model, output_var='lnipnsa', indep_var = "sumshck", look_path = F)
bootstrap_se_lnppinsa <- bootstrap_se(model = VAR_model, output_var='lnppinsa', indep_var = "sumshck", look_path = F)

plot_sumshck <- ggplot(mapping=aes(x=1:48)) +
  geom_line(mapping=aes(y=response_1[["irf"]][["sumshck"]][1:48,1]), color="black") + 
  geom_line(mapping=aes(y=response_1[["irf"]][["sumshck"]][1:48,1]-bootstrap_se_sumshck/100), linetype="dotted") +
  geom_line(mapping=aes(y=response_1[["irf"]][["sumshck"]][1:48,1]+bootstrap_se_sumshck/100), linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(y = 'Percentage Points', x = 'Months After Shock', title = "a. Effect on the Cumulated Shock") +  
  scale_x_continuous(breaks = seq(0, 48, by = 3), limits = c(0, 48), expand = c(0, 0))

plot_lnipnsa <- ggplot(mapping=aes(x=1:48)) +
  geom_line(mapping=aes(y=response_2[["irf"]][["sumshck"]][1:48,1]), color="black") + 
  geom_line(mapping=aes(y=response_2[["irf"]][["sumshck"]][1:48,1]-bootstrap_se_lnipnsa/10), linetype="dotted") +
  geom_line(mapping=aes(y=response_2[["irf"]][["sumshck"]][1:48,1]+bootstrap_se_lnipnsa/10), linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(y = 'Percent', x = 'Months After Shock', title = "b. Effect on Output") +  
  scale_x_continuous(breaks = seq(0, 48, by = 3), limits = c(0, 48), expand = c(0, 0))

plot_lnppinsa <- ggplot(mapping=aes(x=1:48)) +
  geom_line(mapping=aes(y=response_2[["irf"]][["sumshck"]][1:48,2]), color="black") + 
  geom_line(mapping=aes(y=response_2[["irf"]][["sumshck"]][1:48,2]-bootstrap_se_lnppinsa/10), linetype="dotted") +
  geom_line(mapping=aes(y=response_2[["irf"]][["sumshck"]][1:48,2]+bootstrap_se_lnppinsa/10), linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 2),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(y = 'Percent', x = 'Months After Shock', title = "c. Effect on the Price Level") +  
  scale_x_continuous(breaks = seq(0, 48, by = 3), limits = c(0, 48), expand = c(0, 0))

# Combine the plots
figure_9e <- plot_sumshck / plot_lnipnsa / plot_lnppinsa +
  plot_layout(guides = "collect") +
  plot_annotation(title = "FIGURE 9e. THE EFFECT OF MONETARY POLICY IN A VAR USING THE NEW MEASURE OF MONETARY POLICY")

# Print the combined plot
print(figure_9e)
