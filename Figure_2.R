library(readr)
library(dplyr)
library(stringr)
library(survival)
library(ggplot2)

df <- read_csv("~/my-r-project/my_final_dataset.csv")

df2 <- df |>
  filter(demobin == 0) |>
  mutate(
    sanction_type = case_when(
      sanction == 0 ~ "No sanctions",
      sanction_fin == 1 & sanction_trade == 0 ~ "Financial only",
      sanction_fin == 0 & sanction_trade == 1 ~ "Trade only",
      sanction_fin == 1 & sanction_trade == 1 ~ "Financial and Trade",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sanction_type))

m <- coxph(
  Surv(tstart, tstop, event) ~
    strata(sanction_type) +
    polity2 +
    Llog_gdppc +
    Llog_pop +
    Llog_imports_gdp_pc +
    Llog_oil_gas +
    cold_war +
    relfrac +
    gmlmidongoing +
    cluster(ccode),
  data = df2
)

s <- summary(survfit(m))

pl <- data.frame(
  years = s$time / 365.25,
  surv = s$surv,
  sanction_type = str_remove(s$strata, "sanction_type=")
)

ggplot(
  pl,
  aes(
    x = years,
    y = surv,
    color = sanction_type,
    linetype = sanction_type
  )
) +
  geom_step(linewidth = 0.8) +
  geom_hline(
    yintercept = 0.5,
    color = "#dadada",
    linewidth = 0.5
  ) +
  scale_color_manual(
    values = c(
      "No sanctions" = "black",
      "Financial only" = "#9c9e9e",
      "Trade only" = "#696b6b",
      "Financial and Trade" = "#434645"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "No sanctions" = "solid",
      "Financial only" = "longdash",
      "Trade only" = "dotted",
      "Financial and Trade" = "dotdash"
    )
  ) +
  labs(
    x = "Tenure in office (years)",
    y = "Survival probability",
    color = "",
    linetype = ""
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14), 
    legend.position = "bottom",
    legend.text = element_text(size = 14), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )