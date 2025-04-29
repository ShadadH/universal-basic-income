# Load required packages
library(haven)       # To read Stata files
library(dplyr)       # For data manipulation
library(here)        # For file path management

# Step 1: Import the base dataset
hbasicinc_base <- read_dta(here("hbasicinc_base.dta"))

# Step 2: Run the tax and EITC calculations
source(here("datagen.R"))  # This will run the datagen script you already have

# Step 3: Load the percentile analysis function
source(here("percentile.R"))

# Step 4: Run the percentile analysis on three income measures
# Create y and w variables for primary income
hbasicinc_base$y <- hbasicinc_base$hprimaryval / hbasicinc_base$hn_csunit
hbasicinc_base$w <- hbasicinc_base$hsup_wgt
primary_results <- percentile_analysis(hbasicinc_base, "y", "w")
primary_gini <- primary_results$gini

# Create y and w variables for disposable income
hbasicinc_base$y <- hbasicinc_base$disp_inc / hbasicinc_base$hn_csunit
disposable_results <- percentile_analysis(hbasicinc_base, "y", "w")
disposable_gini <- disposable_results$gini

# Create y and w variables for final income with EITC
hbasicinc_base$y <- hbasicinc_base$incweitc / hbasicinc_base$hn_csunit
final_results <- percentile_analysis(hbasicinc_base, "y", "w")
final_gini <- final_results$gini

# Step 5: Print the key results
cat("Gini Coefficients:\n")
cat("Primary Income:", primary_gini, "\n")
cat("Disposable Income (after tax):", disposable_gini, "\n")
cat("Final Income (with EITC):", final_gini, "\n")

# Optional: Export the percentiles data for review
write.csv(primary_results$percentiles, here("primary_income_percentiles.csv"))
write.csv(disposable_results$percentiles, here("disposable_income_percentiles.csv"))
write.csv(final_results$percentiles, here("final_income_percentiles.csv"))