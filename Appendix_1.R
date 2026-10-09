library(dplyr)
library(tidyr)
library(survival)
library(readr)
library(tidyverse)
library(GGally)

df <- read_csv("~/my-r-project/my_final_dataset.csv")

m1 <- coxph(
  Surv(tstart, tstop, event) ~ sanction
    + polity2
    + tt(polity2)
    + Llog_gdppc 
    + Llog_pop 
    + Llog_imports_gdp_pc
    + Llog_oil_gas 
    + cold_war 
    + relfrac 
    + gmlmidongoing
    + cluster(ccode),
  data = df |> filter(demobin == 0),
  tt = function(x, t, ...) x * log(t + 1)
)

m2 <- coxph(
  Surv(tstart, tstop, event) ~ sanction_fin 
    + sanction_trade
    + polity2 
    + tt(polity2)
    + Llog_gdppc 
    + Llog_pop 
    + Llog_imports_gdp_pc
    + Llog_oil_gas 
    + cold_war 
    + relfrac 
    + gmlmidongoing
    + cluster(ccode),
  data = df |> filter(demobin == 0),
  tt = function(x, t, ...) x * log(t + 1)
)

m3 <- coxph(
  Surv(tstart, tstop, event) ~ sanction * anocracy
    + Llog_gdppc 
    + Llog_pop 
    + Llog_imports_gdp_pc
    + Llog_oil_gas 
    + cold_war 
    + gmlmidongoing 
    + relfrac
    + cluster(ccode),
  data = df |> filter(demobin == 0)
)

m4 <- coxph(
  Surv(tstart, tstop, event) ~ sanction_fin * gwf_personal
    + polity2 
    + tt(polity2)
    + Llog_gdppc 
    + Llog_pop 
    + Llog_imports_gdp_pc
    + Llog_oil_gas 
    + cold_war
    + gmlmidongoing
    + cluster(ccode),
  data = df |> filter(demobin == 0),
  tt = function(x, t, ...) x * log(t + 1)
)

ggcoef_compare(
  list(`Model 1` = m1, `Model 2` = m2, `Model 3` = m3, `Model 4` = m4), 
  exponentiate = TRUE,
  point_size = 1,
  interaction_sep = " × ",
  variable_labels = c(
    "sanction" = "Sanctions",
    "polity2" = "Regime type (Polity2)",
    "tt(polity2)" = "Regime type (Polity2) × log(incumbent tenure)",
    "Llog_gdppc" = "log(GDP per capita), t-1",
    "Llog_pop" = "log(population), t-1",
    "Llog_imports_gdp_pc" = "log(economic openness), t-1",
    "Llog_oil_gas" = "log(oil and gas rents), t-1",
    "cold_war" = "Cold War",
    "relfrac" = "Religious fractionalization",
    "gmlmidongoing" = "Interstate conflict",
    "sanction_fin" = "Financial sanctions",
    "sanction_trade" = "Trade sanctions",
    "anocracy" = "Anocracy",
    "sanction × anocracy" = "Sanctions × Anocracy",
    "gwf_personal" = "Personalist regime",
    "sanction_fin × gwf_personal" = "Financial sanctions × Personalist regime"
  )
) +
  labs(
    x = "Hazard Ratio"
  ) +
  theme(
    axis.text.y = element_text(hjust = 1) 
  )