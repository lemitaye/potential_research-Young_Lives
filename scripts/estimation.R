
# Date created: March 29, 2022
# DEPENDS ON: "extract_clean.R"

# This script runs the main regressions using the data generated in "extract_clean.R"

# Start by estimating a simple model for both OLS and IV

# OLS and IV on all sample

## First stage regression (no_covariates)
sfrst <- lm(IMTI ~ E_is, data = joined)
stargazer(sfrst, type = "text")

# 2SLS on the whole sample (no covariates)
smod_ols <- lm(wage_employ ~ IMTI, data = joined)
smod_iv <- ivreg(wage_employ ~ IMTI | E_is, data = joined)
stargazer(smod_ols, smod_iv, type = "text")

# The same regressions as above, but excluding the Addis Ababa sample:
  
sfrst_noaa <- lm(IMTI ~ E_is, data = joined, subset = region_r3 != "Addis Ababa")

smod_ols_noaa <- lm(wage_employ ~ IMTI, data = joined, subset = region_r3 != "Addis Ababa")
smod_iv_noaa <- ivreg(wage_employ ~ IMTI | E_is, data = joined, subset = region_r3 != "Addis Ababa")

stargazer(sfrst_noaa, type = "text")
stargazer(smod_ols_noaa, smod_iv_noaa, type = "text")


# Validation sample (including only AA subjects)
aa_sample <- joined %>% 
  filter(region_r3 == "Addis Ababa") %>% 
  mutate(
    ethnic_D = if_else(chethnic == "Amhara", 0.75, 0),
    IMIT = if_else(chethnic == "Amhara",6, 0)
  )

# Regression using the validation sample
ols_aa <- lm(wage_employ ~ ethnic_D, data = aa_sample)
iv_aa <- ivreg(wage_employ ~ IMTI | ethnic_D, data = aa_sample)
stargazer(ols_aa, iv_aa, type = "text")





















