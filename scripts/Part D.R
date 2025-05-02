# -------------------------------
# Part D: Simulate Basic Income + Flat Tax + Super Tax
# -------------------------------

# Load Libraries
library(dplyr)
library(DescTools)
library(ggplot2)
library(gt)


# Load original Part A dataset
hbasicinc_base <- readRDS("hbasicinc_tax.rds")

# -------------------------------
# Step 1. Define Parameters
# -------------------------------

# Flat tax rate (t)
flat_tax_rate <- 0.245

# Super-tax rate (T)
super_tax_rate <- 0.30

# Super-tax threshold per consumption unit (L)
super_tax_threshold <- 220000

# Initial guess for Basic Income (B)
basic_income_B <- 10000

# -------------------------------
# Step 2. Calculate Components
# -------------------------------

# Calculate primary income per consumption unit
hbasicinc_base <- hbasicinc_base %>%
  mutate(
    primary_income_pcu = hprimaryval / hn_csunit
  )

# Calculate flat tax payment
hbasicinc_base <- hbasicinc_base %>%
  mutate(
    flat_tax_payment = flat_tax_rate * hprimaryval
  )

# Calculate super-tax payment
hbasicinc_base <- hbasicinc_base %>%
  mutate(
    super_tax_payment = super_tax_rate * pmax(0, (primary_income_pcu - super_tax_threshold)) * hn_csunit
  )

# Calculate final disposable income under Basic Income system
hbasicinc_base <- hbasicinc_base %>%
  mutate(
    disp_inc_basic = basic_income_B + (hprimaryval - flat_tax_payment) - super_tax_payment
  )

# -------------------------------
# Step 3. Calculate Welfare and Weights
# -------------------------------

hbasicinc_base <- hbasicinc_base %>%
  mutate(
    welfare_basic = disp_inc_basic / hn_csunit,
    final_weight = hsup_wgt * h_numper
  )

# -------------------------------
# Step 4. Calculate Overall Mean Welfare and Gini
# -------------------------------

overall_mean_welfare_basic <- weighted.mean(hbasicinc_base$welfare_basic, hbasicinc_base$final_weight)

gini_basic <- Gini(hbasicinc_base$welfare_basic, weights = hbasicinc_base$final_weight)

# Display Results
cat("\nOverall Mean Welfare (Basic Income System):", round(overall_mean_welfare_basic, 2))
cat("\nGini Coefficient (Basic Income System):", round(gini_basic, 4))

# -------------------------------
# Step 5. Lorenz Curve Plot
# -------------------------------

lorenz_basic <- Lc(hbasicinc_base$welfare_basic, weights = hbasicinc_base$final_weight)

lorenz_df_basic <- data.frame(
  p = lorenz_basic$p,
  L = lorenz_basic$L
)

ggplot(lorenz_df_basic, aes(x = p, y = L)) +
  geom_line(color = "orange", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  labs(
    title = "Lorenz Curve of Welfare Distribution (Basic Income + Flat Tax System)",
    x = "Cumulative Share of Population",
    y = "Cumulative Share of Welfare"
  ) +
  theme_minimal(base_size = 14)

# -------------------------------
# Step 6. Group Summary Table
# -------------------------------

hbasicinc_base <- hbasicinc_base %>%
  arrange(welfare_basic) %>%
  mutate(
    percentile_basic = ntile(welfare_basic, 100),
    
    group_basic = case_when(
      percentile_basic <= 20 ~ "Bottom 20%",
      percentile_basic <= 40 ~ "20-40%",
      percentile_basic <= 60 ~ "40-60%",
      percentile_basic <= 80 ~ "60-80%",
      percentile_basic <= 90 ~ "80-90%",
      percentile_basic <= 95 ~ "90-95%",
      percentile_basic <= 99 ~ "96-99%",
      percentile_basic == 100 ~ "Top 1%"
    )
  )


group_summary_basic <- hbasicinc_base %>%
  group_by(group_basic) %>%
  summarise(
    mean_welfare = weighted.mean(welfare_basic, final_weight),
    total_welfare = sum(welfare_basic * final_weight),
    population = sum(final_weight),
    .groups = "drop"
  )

total_welfare_basic <- sum(group_summary_basic$total_welfare)

group_summary_basic <- group_summary_basic %>%
  mutate(
    welfare_share = total_welfare / total_welfare_basic * 100
  )

group_summary_basic %>%
  gt() %>%
  fmt_number(
    columns = c(mean_welfare, total_welfare, population),
    decimals = 2,
    use_seps = TRUE
  ) %>%
  fmt_percent(
    columns = welfare_share,
    decimals = 1,
    scale_values = FALSE
  ) %>%
  cols_label(
    group_basic = "Group",
    mean_welfare = "Mean Welfare",
    total_welfare = "Total Welfare",
    population = "Population",
    welfare_share = "Welfare Share (%)"
  ) %>%
  tab_header(
    title = "Welfare Distribution Summary by Group",
    subtitle = "t = 24.5% | T= 30% | L = $220,000 | B = $10,000"
    
  )

