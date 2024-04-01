rm(list = ls())
cat('\014')
gc()

library(tidyverse)
library(stargazer)
library(lubridate)
library(patchwork)

data_meeting_original <- readRDS(file = "datasets/data_meeting_original.rds")
data_monthly_original <- readRDS(file = "datasets/data_monthly_original.rds")

model_1 <- lm(formula = dtarg ~ oldtarg + 
                (graym + gray0 + gray1 + gray2) + 
                (igrym + igry0 + igry1 + igry2) +
                (gradm + grad0 + grad1 + grad2) +
                (igrdm + igrd0 + igrd1 + igrd2) +
                grau0,
              data = data_meeting_original
              )
 

# table 1 -----------------------------------------------------------------

coef_dic <- c('Old Target', 
              'Forecasted Output Growth (-1)',
              'Forecasted Output Growth (0)',
              'Forecasted Output Growth (+1)',
              'Forecasted Output Growth (+2)',
              'Change in Forecast Output (-1)',
              'Change in Forecast Output (0)',
              'Change in Forecast Output (+1)',
              'Change in Forecast Output (+2)',
              'Forecasted Inflation (-1)',
              'Forecasted Inflation (0)',
              'Forecasted Inflation (+1)',
              'Forecasted Inflation (+2)',
              'Change in Forecasted Inflation (-1)',
              'Change in Forecasted Inflation (0)',
              'Change in Forecasted Inflation (+1)',
              'Change in Forecasted Inflation (+2)',
              'Forecasted Unemployment Rate (0)')

table_1 <- stargazer(model_1, 
          type = "latex",
          title = "Determinants of the Change in the Intended Federal Funds Rate",
          column.labels = c("Coefficient", "Standard Error"),
          omit.stat = c("f", "ser", "adj.rsq"),
          digits = 3,
          out = 'output/table1.tex', 
          covariate.labels = coef_dic,
          dep.var.caption = 'Change in Intended Federal Funds Rate',
          header=F)

# list(c("Notes:", "R² = 0.28; D.W. = 1.84; s.e.e. = 0.39; N = 263."),
#      c("The sample is FOMC meetings over the period 1969:3–1996:12."))

# table 2 -----------------------------------------------------------------

data_meeting_original$residuals <- NA

na_action <- model_1[["na.action"]]

data_meeting_original$residuals[-na_action] <- residuals(model_1)

attr(data_meeting_original$residuals, "label") <- "residuals of equation (1)"

all.equal(data_meeting_original$residuals, data_meeting_original$resid)

rm(na_action)

table_2_data <- select(data_meeting_original, mtgdate, residuals, dffmtg) %>%
  mutate(month_year = format(.$mtgdate, "%m-%Y")) %>%
  group_by(month_year) %>%
  summarise(residuals = sum(residuals),
            dffmtg = sum(dffmtg)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste0("01-", month_year), format="%d-%m-%Y")) %>%
  arrange(date) %>%
  select(-date) %>%
  mutate(across(everything(), ~replace_na(., 0)))

# Convert `month_year` to date to make sure it's ordered properly
table_2_data$mtgdate <- as.Date(paste0("01-", table_2_data$month_year), format="%d-%m-%Y")

table_2_data_wide <- table_2_data %>%
  mutate(year = year(mtgdate),
         month = month(mtgdate, label = TRUE)) %>%
  select(-month_year, -mtgdate, -dffmtg)  %>%
  pivot_wider(names_from = month, values_from = residuals) %>%
  mutate(across(everything(), ~replace_na(., 0)))

table_2_data <- mutate(table_2_data, qrtdate = zoo::as.yearqtr(mtgdate)) %>%
  group_by(qrtdate) %>%
  summarise(residuals = sum(residuals),
            dffmtg = sum(dffmtg))

# to use in the plot breaks
years <- year(data_monthly_original$date) %>% unique()

p1 <- ggplot(table_2_data, aes(x = qrtdate, y = residuals)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-5, 3) +
  theme_minimal() +
  labs(x = "", y = "Percentage Points", 
       title = "a. New Measure of Monetary Policy Shocks") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_continuous(breaks = seq(from=min(years), to=max(years), by=2))

data_monthly_original <- select(data_monthly_original, date, dff) %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  group_by(date) %>%
  summarise(dff = sum(dff))

p2 <- ggplot(data_monthly_original, aes(x = date, y = dff)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-9, 9) +
  theme_minimal() +
  labs(x = "", y = "Percentage Points", 
       title = "b. Change in the Actual Federal Funds Rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_continuous(breaks = seq(from=min(years), to=max(years), by=2))

# Use the patchwork package to combine the plots
figure_1 <- p1 / p2

# Display the combined plot
# to run those tables, change type above where it's created to 'text' and remove the _file_ option (in order for stargazer not to save it)
# print(table_1)
# stargazer(round(table_2_data_wide, 3), out = 'output/table2.tex', summary = F)
# print(figure_1)

# saving results
ggsave(filename = 'output/figure1.png', figure_1, scale = 2)

rm(data_meeting_original, data_monthly_original, model_1, p1, p2, table_2_data)





