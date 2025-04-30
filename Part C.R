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

# EITC expansion multiplier
eitc_multiplier <- 1.3

# Redefine EITC based on expanded ceilings
hbasicinc_base <- hbasicinc_base %>%
  mutate(
    inc_eit = ifelse(hprimaryval > 0, hprimaryval, 0),
    eitc_prog = 0,
    
    # EITC with expanded ceilings
    eitc_prog = case_when(
      inc_eit <= (6780 * eitc_multiplier) & hunder18 == 0 ~ pmin(519, 0.0765 * inc_eit),
      inc_eit <= (10680 * eitc_multiplier) & hunder18 == 1 ~ pmin(3481, 0.34 * inc_eit),
      inc_eit <= (14290 * eitc_multiplier) & hunder18 == 2 ~ pmin(5716, 0.40 * inc_eit),
      inc_eit <= (14290 * eitc_multiplier) & hunder18 >= 3 ~ pmin(6431, 0.45 * inc_eit),
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

group_summary_prog <- hbasicinc_base %>%
  group_by(group_prog) %>%
  summarise(
    mean_welfare = weighted.mean(welfare_prog, final_weight),
    total_welfare = sum(welfare_prog * final_weight),
    population = sum(final_weight),
    .groups = "drop"
  )

# Calculate share of total welfare
total_welfare_prog <- sum(group_summary_prog$total_welfare)

group_summary_prog <- group_summary_prog %>%
  mutate(
    welfare_share = total_welfare / total_welfare_prog * 100
  )

# View the table
print(group_summary_prog)

# (Optional) Save to Excel
# library(writexl)
# write_xlsx(group_summary_prog, "group_summary_progressive.xlsx")


# -------------------------------
# (Optional) Save to Excel or Image
# -------------------------------

# library(writexl)
# write_xlsx(hbasicinc_base, "progressive_reform_summary.xlsx")
# ggsave("lorenz_curve_progressive_reform.png")

# -------------------------------
# Done!
# -------------------------------
