# -------------------------------
# Part C: Simulate Budget-Neutral Progressive Reform
# -------------------------------

# Load Libraries
library(dplyr)
library(DescTools)
library(ggplot2)

# Load original Part A dataset
hbasicinc_base <- readRDS("hbasicinc_tax.rds")

# -------------------------------
# Step 1. Update Tax Calculation (Top Bracket to 45%)
# -------------------------------

# Adjust taxable income 
hbasicinc_base <- hbasicinc_base %>%
  mutate(
    income_tax_prog = case_when(
      taxableinc <= 9700 ~ 0.10 * taxableinc,
      taxableinc <= 39475 ~ 970 + 0.12 * (taxableinc - 9700),
      taxableinc <= 84200 ~ 4543 + 0.22 * (taxableinc - 39475),
      taxableinc <= 160725 ~ 14382.5 + 0.24 * (taxableinc - 84200),
      taxableinc <= 204100 ~ 32748.5 + 0.32 * (taxableinc - 160725),
      taxableinc <= 510300 ~ 46628.5 + 0.35 * (taxableinc - 204100),
      TRUE ~ 153798.5 + 0.45 * (taxableinc - 510300)  # Top bracket now 45%
    ),
    
    # New disposable income before EITC
    disp_inc_prog = htotval - (income_tax_prog * numfiler)
  )

# -------------------------------
# Step 2. Expand EITC Ceilings
# -------------------------------

eitc_multiplier <- 1.2

hbasicinc_base <- hbasicinc_base %>%
  mutate(
    inc_eit = ifelse(hprimaryval > 0, hprimaryval, 0),
    
    # Scaled max credits
    max_eitc_0 = 519 * eitc_multiplier,
    max_eitc_1 = 3481 * eitc_multiplier,
    max_eitc_2 = 5716 * eitc_multiplier,
    max_eitc_3 = 6431 * eitc_multiplier,
    
    # Scaled phase-in rates
    rate_0 = 0.0765 * eitc_multiplier,
    rate_1 = 0.34 * eitc_multiplier,
    rate_2 = 0.40 * eitc_multiplier,
    rate_3 = 0.45 * eitc_multiplier,
    
    # Compute progressive EITC
    eitc_prog = case_when(
      hunder18 == 0 & inc_eit <= (6780 * eitc_multiplier) ~ pmin(max_eitc_0, rate_0 * inc_eit),
      hunder18 == 1 & inc_eit <= (10680 * eitc_multiplier) ~ pmin(max_eitc_1, rate_1 * inc_eit),
      hunder18 == 2 & inc_eit <= (14290 * eitc_multiplier) ~ pmin(max_eitc_2, rate_2 * inc_eit),
      hunder18 >= 3 & inc_eit <= (14290 * eitc_multiplier) ~ pmin(max_eitc_3, rate_3 * inc_eit),
      TRUE ~ 0
    )
  )


# -------------------------------
# Step 3. Calculate New Welfare
# -------------------------------

hbasicinc_base <- hbasicinc_base %>%
  mutate(
    incweitc_prog = disp_inc_prog + eitc_prog,
    welfare_prog = incweitc_prog / hn_csunit,
    final_weight = hsup_wgt * h_numper  # Final weight
  )

# -------------------------------
# Step 4. Create Percentiles and Groupings
# -------------------------------

hbasicinc_base <- hbasicinc_base %>%
  arrange(welfare_prog) %>%
  mutate(
    percentile_prog = ntile(welfare_prog, 100),
    
    group_prog = case_when(
      percentile_prog <= 20 ~ "Bottom 20%",
      percentile_prog <= 40 ~ "20-40%",
      percentile_prog <= 60 ~ "40-60%",
      percentile_prog <= 80 ~ "60-80%",
      percentile_prog <= 90 ~ "80-90%",
      percentile_prog <= 95 ~ "90-95%",
      percentile_prog <= 99 ~ "96-99%",
      percentile_prog == 100 ~ "Top 1%"
    )
  )

# -------------------------------
# Step 5. Gini Coefficient and Overall Welfare
# -------------------------------

overall_mean_welfare_prog <- weighted.mean(hbasicinc_base$welfare_prog, hbasicinc_base$final_weight)

gini_prog <- Gini(hbasicinc_base$welfare_prog, weights = hbasicinc_base$final_weight)

# Display Results
cat("\nOverall Mean Welfare (Progressive Reform):", round(overall_mean_welfare_prog, 2))
cat("\nGini Coefficient (Progressive Reform):", round(gini_prog, 4))

# -------------------------------
# Step 6. Plot Lorenz Curve
# -------------------------------

# Lorenz curve points
lorenz_prog <- Lc(hbasicinc_base$welfare_prog, weights = hbasicinc_base$final_weight)

lorenz_df_prog <- data.frame(
  p = lorenz_prog$p,
  L = lorenz_prog$L
)

# Plot
ggplot(lorenz_df_prog, aes(x = p, y = L)) +
  geom_line(color = "green", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  labs(
    title = "Lorenz Curve of Welfare Distribution (Progressive Reform)",
    x = "Cumulative Share of Population",
    y = "Cumulative Share of Welfare"
  ) +
  theme_minimal(base_size = 14)

# -------------------------------
# Step 7. Group Summary Table 
# -------------------------------
library(gt)

group_summary_prog %>%
  mutate(
    mean_welfare = round(mean_welfare, 2),
    welfare_share = welfare_share / 100  # Convert to proportion
  ) %>%
  gt() %>%
  tab_header(
    title = "Welfare Distribution by Income Group (Progressive Reform)",
    subtitle = "Top Bracket: 45% | Expanded EITC (x1.2 ceilings)"
  ) %>%
  cols_label(
    group_prog = "Income Group",
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
  fmt_percent(
    columns = welfare_share,
    decimals = 1
  )

