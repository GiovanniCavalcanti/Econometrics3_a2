
# environment -------------------------------------------------------------

rm(list = ls())

gc()

# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)

# original data -----------------------------------------------------------

data_meeting_original <- read_xls(path = "replication_kit/RomerandRomerDataAppendix.xls",
                          sheet = "DATA BY MEETING") %>%
  rename_with(tolower) %>%
  mutate(
    mtgdate = as.character(mtgdate),
    mtgdate = if_else(nchar(mtgdate) == 5, paste0("0", mtgdate), mtgdate),
    mtgdate = paste0(substr(mtgdate, 1, 4), "19", substr(mtgdate, 5, 6)),
    mtgdate = as.Date(mtgdate, format = "%m%d%Y")
  ) %>%
  # Make all columns except the first one numeric
  mutate(across(-1, as.numeric))

labels <- c(
  mtgdate = "Date of the FOMC meeting",
  dtarg = "Δff in equation (1)",
  oldtarg = "ffb in equation (1)",
  gradm = "π~_{-1} in equation (1)",
  grad0 = "Same as before, for the current quarter.",
  grad1 = "Same as before, for the quarter ahead.",
  grad2 = "Same as before, for two quarters ahead.",
  igrdm = "π~_{m,–1}–π~_{m-1,–1} in equation (1)",
  igrd0 = "Same as before, for the current quarter.",
  igrd1 = "Same as before, for the quarter ahead.",
  igrd2 = "Same as before, for two quarters ahead.",
  graym = "Δy~_{–1} in equation (1)",
  gray0 = "Same as before, for the current quarter.",
  gray1 = "Same as before, for the quarter ahead.",
  gray2 = "Same as before, for two quarters ahead.",
  igrym = "Δy~_{m,–1}–Δy~{m-1,–1} in equation (1)",
  igry0 = "Same as before, for the current quarter.",
  igry1 = "Same as before, for the quarter ahead.",
  igry2 = "Same as before, for two quarters ahead.",
  grau0 = "u~_0 in equation (1)",
  resid = "residuals of equation (1)",
  dffmtg = "change in the weekly average of the actual funds rate",
  residf = "residuals of equation (1) estimated using DFFMTG as dependent variable"
)
for (col_name in names(labels)) {
  attr(data_meeting_original[[col_name]], "label") <- labels[col_name]
}

rm(col_name, labels)

data_monthly_original <- read_xls(path = "replication_kit/RomerandRomerDataAppendix.xls",
                                  sheet = "DATA BY MONTH") %>%
  rename_with(tolower) %>%
  mutate(across(-1, as.numeric))

monthly_labels <- c(
  resid = "Our new shock series, converted to monthly.",
  dff = "Change in the actual federal funds rate.",
  dtarg = "Change in the intended federal funds rate decided at FOMC meetings, converted to monthly.",
  residf = "Change in the actual federal funds rate controlling for forecasts, converted to monthly.",
  pcipnsa = "Change in the log of the non-seasonally-adjusted index of industrial production.",
  pcwcp = "Change in the log of the index of world commodity prices",
  pcppinsa = "Change in the log of the non-seasonally-adjusted producer price index.",
  pccpinsa = "Change in the log of the non-seasonally-adjusted consumer price index.",
  pcpcegsa = "Change in the log of the seasonally-adjusted chain-type price index for personal consumption expenditures.",
  lnipnsa = "Log of the non-seasonally-adjusted index of industrial production (used in the VARs).",
  lnppinsa = "Log of the non-seasonally-adjusted producer price index (used in the VARs).",
  lnwcp = "Log of the index of world commodity prices (used in some of the VARs).",
  sumshck = "Our new measure of monetary shocks, cumulated to be in levels (used in some of the VARs).",
  ff = "Level of the federal funds rate (used in some of the VARs).",
  sumdtarg = "The change in the intended funds rate, cumulated to be in levels (used in some of the VARs).",
  sumshckf = "RESIDF, cumulated to be in levels (used in some of the VARs)."
)

# Loop through the labels vector and assign each label to the corresponding column
for (col_name in names(monthly_labels)) {
  attr(data_monthly_original[[col_name]], "label") <- monthly_labels[col_name]
}
  
rm(col_name, monthly_labels)

write_rds(data_meeting_original, file = "datasets/data_meeting_original.rds")
write_rds(data_monthly_original, file = "datasets/data_monthly_original.rds")

# extended data -----------------------------------------------------------

data_monthly_extended <- read_xlsx(path = "Ramey_HOM_monetary/Monetarydat.xlsx",
                                   sheet = "Monthly") %>%
  rename_with(tolower) %>%
  mutate(dff = c(NA, diff(ffr)),
         pcipnsa = c(NA, diff(lip)),
         pcwcp = c(NA, diff(lpcom)),
         pccpinsa = c(NA, diff(lcpi))
  ) %>%
  dplyr::select(resid = rrshock,
                dff,
                pcipnsa,
                pcwcp =,
                pccpinsa,
                lnipnsa = lip, 
                lnppinsa = lcpi, 
                sumshck = cumrrshock)

date_sequence <- seq.Date(from = as.Date("1959-01-01"), by = "month", length.out = nrow(data_monthly_extended))

# Assign this date sequence to a new column in your dataframe
data_monthly_extended$date <- date_sequence

rm(date_sequence)

monthly_labels <- c(
  resid = "Our new shock series, converted to monthly.",
  dff = "Change in the actual federal funds rate.",
  pcipnsa = "Change in the log of the non-seasonally-adjusted index of industrial production.",
  pcwcp = "Change in the log of the index of world commodity prices",
  pccpinsa = "Change in the log of the non-seasonally-adjusted consumer price index.",
  lnipnsa = "Log of the non-seasonally-adjusted index of industrial production (used in the VARs).",
  lnppinsa = "Log of the non-seasonally-adjusted producer price index (used in the VARs).",
  sumshck = "Our new measure of monetary shocks, cumulated to be in levels (used in some of the VARs).",
  date = ""
)
for (col_name in names(monthly_labels)) {
  attr(data_monthly_extended[[col_name]], "label") <- monthly_labels[col_name]
}

rm(col_name, labels)

data_monthly_extended <- filter(data_monthly_extended, date > "1965-12-01", date < "2008-01-01") %>%
  mutate(sumshck = if_else(is.na(sumshck), 0, sumshck))

write_rds(data_monthly_extended, file = "datasets/data_monthly_extended.rds")

