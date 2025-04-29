# R conversion of datagen.do
# Original Stata code comments are preserved with ##

## /*
## *creating hbasicinc_base from original data
## use hinc_tax_cps17
## keep hh_num htotval
## merge 1:1 hh_num using h_inc_cps17
## drop if _merge<3
## drop _merge
## save hinc_wf, replace
## gen hprimaryval=hearnval+hinc_pen+hinc_fin+hinc_uc
## gen htransval=htotval-hprimaryval
## keep hh_num h_numper hsup_wgt hunder18 hh5to18 hn_csunit hhage hhsex hhrace hhwkstat htotval hearnval hinc_pen hinc_fin hinc_uc hprimaryval htransval 
## save hbasicinc_base, replace
## */
library(here)
library(haven)       # To read Stata files
library(dplyr)       # For data manipulation
library(ggplot2)     # For visualization
library(readr)       # For exporting CSV files
# computing taxes and eitc in hbasicinc_base and storing in hbasicinc_tax.dta
#
# Load data
hbasicinc_base <- read_dta(here("data", "hbasicinc_base.dta"))

# Generate new variables
hbasicinc_base$hnumadult <- hbasicinc_base$h_numper - hbasicinc_base$hunder18
hbasicinc_base$numfiler <- 1
hbasicinc_base$numfiler[hbasicinc_base$hnumadult > 1] <- 2
hbasicinc_base$deduction <- 1200 + 2000*(hbasicinc_base$hunder18/hbasicinc_base$numfiler) + 
  500*(hbasicinc_base$hnumadult - hbasicinc_base$numfiler)/hbasicinc_base$numfiler
hbasicinc_base$taxableinc <- hbasicinc_base$hprimaryval/hbasicinc_base$numfiler - hbasicinc_base$deduction

# Compute tax1
hbasicinc_base$tax1 <- 0
hbasicinc_base$tax1[hbasicinc_base$taxableinc < 9525] <- 0.10 * hbasicinc_base$taxableinc[hbasicinc_base$taxableinc < 9525]
hbasicinc_base$tax1[hbasicinc_base$taxableinc >= 9525 & hbasicinc_base$taxableinc < 38700] <- 
  952.5 + 0.12 * (hbasicinc_base$taxableinc[hbasicinc_base$taxableinc >= 9525 & hbasicinc_base$taxableinc < 38700] - 9525)
hbasicinc_base$tax1[hbasicinc_base$taxableinc >= 38700 & hbasicinc_base$taxableinc < 82500] <- 
  4453.5 + 0.22 * (hbasicinc_base$taxableinc[hbasicinc_base$taxableinc >= 38700 & hbasicinc_base$taxableinc < 82500] - 38700)
hbasicinc_base$tax1[hbasicinc_base$taxableinc >= 82500 & hbasicinc_base$taxableinc < 157500] <- 
  14089.5 + 0.24 * (hbasicinc_base$taxableinc[hbasicinc_base$taxableinc >= 82500 & hbasicinc_base$taxableinc < 157500] - 82500)
hbasicinc_base$tax1[hbasicinc_base$taxableinc >= 157500 & hbasicinc_base$taxableinc < 200000] <- 
  32089.5 + 0.32 * (hbasicinc_base$taxableinc[hbasicinc_base$taxableinc >= 157500 & hbasicinc_base$taxableinc < 200000] - 157500)
hbasicinc_base$tax1[hbasicinc_base$taxableinc >= 200000 & hbasicinc_base$taxableinc < 500000] <- 
  45689.5 + 0.35 * (hbasicinc_base$taxableinc[hbasicinc_base$taxableinc >= 200000 & hbasicinc_base$taxableinc < 500000] - 200000)
hbasicinc_base$tax1[hbasicinc_base$taxableinc >= 500000] <- 
  150689.5 + 0.37 * (hbasicinc_base$taxableinc[hbasicinc_base$taxableinc >= 500000] - 500000)

# Compute alternative minimum tax
hbasicinc_base$tax2 <- 0
hbasicinc_base$tax2[hbasicinc_base$taxableinc >= 109000 & hbasicinc_base$taxableinc < 191000] <- 
  0.26 * hbasicinc_base$taxableinc[hbasicinc_base$taxableinc >= 109000 & hbasicinc_base$taxableinc < 191000]
hbasicinc_base$tax2[hbasicinc_base$taxableinc >= 191000 & hbasicinc_base$taxableinc < 1000000] <- 
  0.28 * hbasicinc_base$taxableinc[hbasicinc_base$taxableinc >= 191000 & hbasicinc_base$taxableinc < 1000000]
hbasicinc_base$tax2[hbasicinc_base$taxableinc >= 1000000 & hbasicinc_base$taxableinc < 1500000] <- 
  0.35 * hbasicinc_base$taxableinc[hbasicinc_base$taxableinc >= 1000000 & hbasicinc_base$taxableinc < 1500000]
hbasicinc_base$tax2[hbasicinc_base$taxableinc >= 1500000] <- 
  0.28 * hbasicinc_base$taxableinc[hbasicinc_base$taxableinc >= 1500000]

# Final tax per filer
hbasicinc_base$tax <- hbasicinc_base$tax1
hbasicinc_base$tax[hbasicinc_base$tax2 > hbasicinc_base$tax1] <- hbasicinc_base$tax2[hbasicinc_base$tax2 > hbasicinc_base$tax1]

# Go back to actual number of filers
hbasicinc_base$inctax <- hbasicinc_base$tax * hbasicinc_base$numfiler

# Disposable income
hbasicinc_base$disp_inc <- hbasicinc_base$htotval - hbasicinc_base$inctax


# EITC computation
hbasicinc_base$inc_eit <- 0
hbasicinc_base$inc_eit[hbasicinc_base$hprimaryval > 0] <- hbasicinc_base$hprimaryval[hbasicinc_base$hprimaryval > 0]
hbasicinc_base$eitc <- 0

# Initial EITC calculations based on income and number of children
hbasicinc_base$eitc[hbasicinc_base$inc_eit <= 6780 & hbasicinc_base$hunder18 == 0] <- 
  pmin(519, 0.0765 * hbasicinc_base$inc_eit[hbasicinc_base$inc_eit <= 6780 & hbasicinc_base$hunder18 == 0])
hbasicinc_base$eitc[hbasicinc_base$inc_eit <= 10680 & hbasicinc_base$hunder18 == 1] <- 
  pmin(3481, 0.34 * hbasicinc_base$inc_eit[hbasicinc_base$inc_eit <= 10680 & hbasicinc_base$hunder18 == 1])
hbasicinc_base$eitc[hbasicinc_base$inc_eit <= 14290 & hbasicinc_base$hunder18 == 2] <- 
  pmin(5716, 0.40 * hbasicinc_base$inc_eit[hbasicinc_base$inc_eit <= 14290 & hbasicinc_base$hunder18 == 2])
hbasicinc_base$eitc[hbasicinc_base$inc_eit <= 14290 & hbasicinc_base$hunder18 >= 3] <- 
  pmin(6431, 0.45 * hbasicinc_base$inc_eit[hbasicinc_base$inc_eit <= 14290 & hbasicinc_base$hunder18 >= 3])

# EITC ceiling for numfiler=1
hbasicinc_base$eitc[hbasicinc_base$inc_eit >= 6780 & hbasicinc_base$inc_eit <= 8490 & 
                      hbasicinc_base$hunder18 == 0 & hbasicinc_base$numfiler == 1] <- 519
hbasicinc_base$eitc[hbasicinc_base$inc_eit >= 10680 & hbasicinc_base$inc_eit <= 18660 & 
                      hbasicinc_base$hunder18 == 1 & hbasicinc_base$numfiler == 1] <- 3481
hbasicinc_base$eitc[hbasicinc_base$inc_eit >= 14290 & hbasicinc_base$inc_eit <= 18660 & 
                      hbasicinc_base$hunder18 == 2 & hbasicinc_base$numfiler == 1] <- 5716
hbasicinc_base$eitc[hbasicinc_base$inc_eit >= 14290 & hbasicinc_base$inc_eit <= 18660 & 
                      hbasicinc_base$hunder18 >= 3 & hbasicinc_base$numfiler == 1] <- 6431

# EITC phasing out for numfiler=1
phaseout_condition1 <- hbasicinc_base$inc_eit >= 8490 & hbasicinc_base$hunder18 == 0 & hbasicinc_base$numfiler == 1
hbasicinc_base$eitc[phaseout_condition1] <- 
  pmax(0, 519 - 0.0765 * (hbasicinc_base$inc_eit[phaseout_condition1] - 8490))

phaseout_condition2 <- hbasicinc_base$inc_eit >= 18660 & hbasicinc_base$hunder18 == 1 & hbasicinc_base$numfiler == 1
hbasicinc_base$eitc[phaseout_condition2] <- 
  pmax(0, 3481 - 0.1598 * (hbasicinc_base$inc_eit[phaseout_condition2] - 18660))

phaseout_condition3 <- hbasicinc_base$inc_eit >= 18660 & hbasicinc_base$hunder18 == 2 & hbasicinc_base$numfiler == 1
hbasicinc_base$eitc[phaseout_condition3] <- 
  pmax(0, 5716 - 0.2106 * (hbasicinc_base$inc_eit[phaseout_condition3] - 18660))

phaseout_condition4 <- hbasicinc_base$inc_eit >= 18660 & hbasicinc_base$hunder18 >= 3 & hbasicinc_base$numfiler == 1
hbasicinc_base$eitc[phaseout_condition4] <- 
  pmax(0, 6431 - 0.2106 * (hbasicinc_base$inc_eit[phaseout_condition4] - 18660))

# EITC ceiling for numfiler=2
hbasicinc_base$eitc[hbasicinc_base$inc_eit >= 6780 & hbasicinc_base$inc_eit <= 14170 & 
                      hbasicinc_base$hunder18 == 0 & hbasicinc_base$numfiler == 2] <- 519
hbasicinc_base$eitc[hbasicinc_base$inc_eit >= 10680 & hbasicinc_base$inc_eit <= 24350 & 
                      hbasicinc_base$hunder18 == 1 & hbasicinc_base$numfiler == 2] <- 3481
hbasicinc_base$eitc[hbasicinc_base$inc_eit >= 14290 & hbasicinc_base$inc_eit <= 24350 & 
                      hbasicinc_base$hunder18 == 2 & hbasicinc_base$numfiler == 2] <- 5716
hbasicinc_base$eitc[hbasicinc_base$inc_eit >= 14290 & hbasicinc_base$inc_eit <= 24350 & 
                      hbasicinc_base$hunder18 >= 3 & hbasicinc_base$numfiler == 2] <- 6431

# EITC phasing out for numfiler=2
phaseout_condition5 <- hbasicinc_base$inc_eit >= 14170 & hbasicinc_base$hunder18 == 0 & hbasicinc_base$numfiler == 2
hbasicinc_base$eitc[phaseout_condition5] <- 
  pmax(0, 519 - 0.0765 * (hbasicinc_base$inc_eit[phaseout_condition5] - 14170))

phaseout_condition6 <- hbasicinc_base$inc_eit >= 24350 & hbasicinc_base$hunder18 == 1 & hbasicinc_base$numfiler == 2
hbasicinc_base$eitc[phaseout_condition6] <- 
  pmax(0, 3481 - 0.1598 * (hbasicinc_base$inc_eit[phaseout_condition6] - 24350))

phaseout_condition7 <- hbasicinc_base$inc_eit >= 24350 & hbasicinc_base$hunder18 == 2 & hbasicinc_base$numfiler == 2
hbasicinc_base$eitc[phaseout_condition7] <- 
  pmax(0, 5716 - 0.2106 * (hbasicinc_base$inc_eit[phaseout_condition7] - 24350))

phaseout_condition8 <- hbasicinc_base$inc_eit >= 24350 & hbasicinc_base$hunder18 >= 3 & hbasicinc_base$numfiler == 2
hbasicinc_base$eitc[phaseout_condition8] <- 
  pmax(0, 6431 - 0.2106 * (hbasicinc_base$inc_eit[phaseout_condition8] - 24350))

# Final calculations
hbasicinc_base$incweitc <- hbasicinc_base$disp_inc + hbasicinc_base$eitc
hbasicinc_base$incweitc_pcu <- hbasicinc_base$incweitc / hbasicinc_base$hn_csunit

# Drop rows where incweitc_pcu < 6000
hbasicinc_base <- hbasicinc_base[hbasicinc_base$incweitc_pcu >= 6000, ]

# Final variables
hbasicinc_base$disp_inc_pcu <- hbasicinc_base$disp_inc / hbasicinc_base$hn_csunit
# Note: There seems to be a typo in the original Stata code (hprimary vs hprimaryval)
# Using hprimaryval as it's defined earlier
hbasicinc_base$hprimaryval_pcu <- hbasicinc_base$hprimaryval / hbasicinc_base$hn_csunit

# Save the data
saveRDS(hbasicinc_base, "hbasicinc_tax.rds")

