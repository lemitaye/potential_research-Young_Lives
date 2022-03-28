
# Date created: March 29, 2022
# DEPENDS ON: "extract_clean.R"

# This script runs the main regressions using the data generated in "extract_clean.R"

# Start by estimating a simple model for both OLS and IV

smod_ols <- lm(wage_employ ~ IMTI, data = joined)
smod_iv <- ivreg(wage_employ ~ IMTI | E_is, data = joined)

stargazer(smod_ols, smod_iv, type = "text")


# First stage regression
sfrst <- lm(IMTI ~ E_is, data = joined)
stargazer(sfrst, type = "text")