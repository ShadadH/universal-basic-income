# Load libraries
library(dplyr)
library(DescTools)
library(writexl)
library(ggplot2)


# Load your processed data
hbasicinc_base <- readRDS("hbasicinc_tax.rds")

# 1. Define Welfare
hbasicinc_base <- hbasicinc_base %>%
  mutate(
    welfare = incweitc / hn_csunit
  )

# 2. Define Final Weights (sampling weight * number of persons)
hbasicinc_base <- hbasicinc_base %>%
  mutate(
    final_weight = hsup_wgt * h_numper
  )

# 3. Create Welfare Percentiles (1 to 100)
hbasicinc_base <- hbasicinc_base %>%
  arrange(welfare) %>%
  mutate(
    percentile = ntile(welfare, 100)
  )

# 4. Group into Analysis Bands
hbasicinc_base <- hbasicinc_base %>%
  mutate(
    group = case_when(
      percentile <= 20 ~ "Bottom 20%",
      percentile > 20 & percentile <= 40 ~ "20-40%",
      percentile > 40 & percentile <= 60 ~ "40-60%",
      percentile > 60 & percentile <= 80 ~ "60-80%",
      percentile > 80 & percentile <= 90 ~ "80-90%",
      percentile > 90 & percentile <= 95 ~ "90-95%",
      percentile > 95 & percentile <= 99 ~ "96-99%",
      percentile == 100 ~ "Top 1%"
    )
  )

# 5. Summarize Mean Welfare and Share per Group
group_summary <- hbasicinc_base %>%
  group_by(group) %>%
  summarise(
    mean_welfare = weighted.mean(welfare, final_weight),
    total_welfare = sum(welfare * final_weight),
    population = sum(final_weight),
    .groups = "drop"
  )

# Calculate overall total welfare to compute shares
overall_total_welfare <- sum(group_summary$total_welfare)

group_summary <- group_summary %>%
  mutate(
    welfare_share = total_welfare / overall_total_welfare * 100
  )

# 6. Calculate Overall Mean Welfare and Gini
overall_mean_welfare <- weighted.mean(hbasicinc_base$welfare, hbasicinc_base$final_weight)

gini_welfare <- Gini(hbasicinc_base$welfare, weights = hbasicinc_base$final_weight)

# Display Results
print(group_summary)
cat("\nOverall Mean Welfare:", round(overall_mean_welfare, 2))
cat("\nGini Coefficient (Welfare):", round(gini_welfare, 4))


# Lorenz curve points
lorenz <- Lc(hbasicinc_base$welfare, weights = hbasicinc_base$final_weight)

# Convert to a data frame
lorenz_df <- data.frame(
  p = lorenz$p,  # cumulative population
  L = lorenz$L   # cumulative welfare
)

# Plot
ggplot(lorenz_df, aes(x = p, y = L)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "Lorenz Curve of Welfare Distribution",
    x = "Cumulative Share of Population",
    y = "Cumulative Share of Welfare"
  ) +
  theme_minimal(base_size = 14)

library(gt)

# Create a polished table
group_summary %>%
  mutate(
    mean_welfare = round(mean_welfare, 2),
    welfare_share = round(welfare_share, 2)
  ) %>%
  gt() %>%
  tab_header(
    title = "Welfare Distribution by Income Group",
    subtitle = "Based on US Redistribution System (Post-Tax, Transfers, and EITC)"
  ) %>%
  cols_label(
    group = "Income Group",
    mean_welfare = "Mean Welfare ($)",
    total_welfare = "Total Welfare",
    population = "Population (Weighted)",
    welfare_share = "Share of Total Welfare (%)"
  ) %>%
  fmt_number(
    columns = c(mean_welfare, total_welfare, population),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_number(
    columns = welfare_share,
    decimals = 1
  )



