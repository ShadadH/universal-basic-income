# -------------------------------
# Part B: Simulate Pre-Trump 2017 Tax System
# -------------------------------

# Load Libraries
library(dplyr)
library(DescTools)
library(gt)
library(ggplot2)
library(ineq)

# Load your saved Part A dataset
hbasicinc_base <- readRDS("hbasicinc_tax.rds")

# -------------------------------
# Step 1. Update Tax Calculation (Pre-2017 Brackets)
# -------------------------------

hbasicinc_base <- hbasicinc_base %>%
  mutate(
    # Tax per filer under 2017 system
    income_tax_pre2017 = case_when(
      taxableinc <= 9325 ~ 0.10 * taxableinc,
      taxableinc <= 37950 ~ 932.5 + 0.15 * (taxableinc - 9325),
      taxableinc <= 91900 ~ 5226.25 + 0.25 * (taxableinc - 37950),
      taxableinc <= 191650 ~ 18713.75 + 0.28 * (taxableinc - 91900),
      taxableinc <= 416700 ~ 46643.75 + 0.33 * (taxableinc - 191650),
      taxableinc <= 418400 ~ 120910.25 + 0.35 * (taxableinc - 416700),
      TRUE ~ 121505.25 + 0.396 * (taxableinc - 418400)
    ),
    
    # Disposable income under old tax brackets
    disp_inc_pre2017 = htotval - (income_tax_pre2017 * numfiler)
  )

# -------------------------------
# Step 2. Recalculate Welfare Pre-Trump
# -------------------------------

hbasicinc_base <- hbasicinc_base %>%
  mutate(
    incweitc_pre2017 = disp_inc_pre2017 + eitc,
    welfare_pre2017 = incweitc_pre2017 / hn_csunit,
    final_weight = hsup_wgt * h_numper  # redo final weight just to be safe
  )

# -------------------------------
# Step 3. Create Percentiles and Groupings
# -------------------------------

hbasicinc_base <- hbasicinc_base %>%
  arrange(welfare_pre2017) %>%
  mutate(
    percentile_pre2017 = ntile(welfare_pre2017, 100),
    
    group_pre2017 = case_when(
      percentile_pre2017 <= 20 ~ "Bottom 20%",
      percentile_pre2017 <= 40 ~ "20-40%",
      percentile_pre2017 <= 60 ~ "40-60%",
      percentile_pre2017 <= 80 ~ "60-80%",
      percentile_pre2017 <= 90 ~ "80-90%",
      percentile_pre2017 <= 95 ~ "90-95%",
      percentile_pre2017 <= 99 ~ "96-99%",
      percentile_pre2017 == 100 ~ "Top 1%"
    )
  )

# Summarize
group_summary_pre2017 <- hbasicinc_base %>%
  group_by(group_pre2017) %>%
  summarise(
    mean_welfare = weighted.mean(welfare_pre2017, final_weight),
    total_welfare = sum(welfare_pre2017 * final_weight),
    population = sum(final_weight),
    .groups = "drop"
  )

# -------------------------------
# Step 4. Gini Coefficient and Overall Welfare
# -------------------------------

overall_mean_welfare_pre2017 <- weighted.mean(hbasicinc_base$welfare_pre2017, hbasicinc_base$final_weight)

gini_pre2017 <- Gini(hbasicinc_base$welfare_pre2017, weights = hbasicinc_base$final_weight)

# Display Results
print(group_summary_pre2017)
cat("\nOverall Mean Welfare (Pre-2017 Tax):", round(overall_mean_welfare_pre2017, 2))
cat("\nGini Coefficient (Pre-2017 Tax):", round(gini_pre2017, 4))

# -------------------------------
# Step 5. Plot Lorenz Curve Pre-2017
# -------------------------------

# Lorenz Curve
lorenz_pre2017 <- Lc(hbasicinc_base$welfare_pre2017, weights = hbasicinc_base$final_weight)

lorenz_df_pre2017 <- data.frame(
  p = lorenz_pre2017$p,
  L = lorenz_pre2017$L
)

# Plot
ggplot(lorenz_df_pre2017, aes(x = p, y = L)) +
  geom_line(color = "red", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "Lorenz Curve of Welfare Distribution (Pre-Trump Tax System)",
    x = "Cumulative Share of Population",
    y = "Cumulative Share of Welfare"
  ) +
  theme_minimal(base_size = 14)

# -------------------------------
# Step 6. Save if Needed
# -------------------------------

# Optionally save tables
library(writexl)
write_xlsx(group_summary_pre2017, "group_summary_pre2017.xlsx")

# Optionally save Lorenz Curve
# ggsave("lorenz_pre2017.png")

# -------------------------------
# Done!
# -------------------------------


# -------------------------------
# Combined Lorenz Curve Plot
# -------------------------------

# First, make sure you have both Lorenz curve objects:
# lorenz_post = Part A Lorenz (Post-Trump)
# lorenz_pre = Part B Lorenz (Pre-Trump)


# Create combined data frame
lorenz_combined <- data.frame(
  p = c(lorenz_df$p, lorenz_pre2017$p),
  L = c(lorenz_df$L, lorenz_pre2017$L),
  system = c(rep("Post-Trump (2019)", length(lorenz_df$p)), rep("Pre-Trump (2017)", length(lorenz_pre2017$p)))
)

# Plot
library(ggplot2)

ggplot(lorenz_combined, aes(x = p, y = L, color = system)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("blue", "red")) +
  labs(
    title = "Lorenz Curves of Welfare Distribution: Pre- vs Post-Trump Tax Systems",
    x = "Cumulative Share of Population",
    y = "Cumulative Share of Welfare",
    color = "Tax System"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
