
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
# aa_sample <- joined %>% 
#   filter(region_r3 == "Addis Ababa") %>% 
#   mutate(
#     ethnic_D = if_else(chethnic == "Amhara", 0.75, 0),
#     IMIT = if_else(chethnic == "Amhara", 6, 0)
#   )

# Regression using the validation sample
ols_aa <- lm(wage_employ ~ ethnic_D, data = aa_sample)
iv_aa <- ivreg(wage_employ ~ IMTI | ethnic_D, data = aa_sample)
stargazer(ols_aa, iv_aa, type = "text")

# Simple model for test scores from round 4:
smod_iv_test__noaa <- ivreg(raw_maths ~ IMTI | E_is, data = joined, subset = region != "Addis Ababa")

smod_iv_test_noaa <- ivreg(raw_lang ~ IMTI | E_is, data = joined, subset = region != "Addis Ababa")


stargazer(smod_iv_test__noaa, smod_iv_test_noaa, type = "text")


# Regression with covariates: ####

make_formula_frst_stg <- function(dep_var, instrument, clus = FALSE, added = NULL) {
  # Define a vector of covariates
  covar1 <- c(
    "zbfa", "stunting", "caredu_r1", "careage_r1", "factor(caresex_r1)", 
    "hhsize", "wi_new", "hq_new", "cd_new", "elecq_new", 
    "ownlandhse_r1", "chsex"
    )

  covar <- paste(
    paste(covar1, collapse = "+")
  )
  
  if (clus) {
    f <- as.formula(paste(
      dep_var, " ~ ", instrument, " + ", added, covar,
      " | 0 | 0 | region"
    ))
  } else {
    f <- as.formula(paste(
      dep_var, " ~ ", instrument, " + ", added, covar
      ))
  }
  
  return(f)
}

f1 <- make_formula_frst_stg("IMTI", "E_is", added = "factor(typesite_r3) +", clus = TRUE)

m1 <- felm(f1, data = non_aa_samp)

stargazer(
  m1, 
  keep = c("E_is", "region", "chsex"),
  type = "text",
  keep.stat = c("n","rsq")
  )

waldtest(m1, ~ E_is)[["F"]]


# Reduced form for the AA sample:

f_rf1 <- make_formula_frst_stg("wage_employ", "E_is")
f_rf2 <- make_formula_frst_stg("raw_maths", "E_is")
f_rf3 <- make_formula_frst_stg("raw_lang", "E_is")
f_rf4 <- make_formula_frst_stg("hghgrade_final_num", "E_is")
f_rf5 <- make_formula_frst_stg("wage_employII", "E_is")


# can't run these regressions because all E_is values are 0 for AA!
rf1aa <- lm(f_rf1, data = aa_samp)
rf2aa <- lm(f_rf2, data = aa_samp)
rf3aa <- lm(f_rf3, data = aa_samp)
rf4aa <- lm(f_rf4, data = aa_samp)
rf5aa <- lm(f_rf5, data = aa_samp)

stargazer(
  rf1aa, rf2aa, rf3aa, rf4aa, rf5aa,
  keep = c("E_is"),
  type = "text",
  keep.stat = c("n","rsq")
)

# IV estimation:
make_formula_iv <- function(
  dep_var, instrument = 0, added = NULL
  ) {
  # Define a vector of covariates
  covar1 <- c(
    "chsex", "zbfa", "stunting", "caredu_r1",
    "careage_r1", "factor(caresex_r1)", "hhsize", 
    "wi_new", "hq_new", "cd_new", "elecq_new", 
    "ownlandhse_r1"
  )
  covar2 <- 0
  
  covar <- paste(
    paste(covar1, collapse = "+"), "|",
    paste(covar2, collapse = "+")
  )
  
  if (instrument == 0) {
    f <- as.formula(paste(dep_var, " ~ IMTI + ", added, covar))
  } else {
    f <- as.formula(paste(
      dep_var, " ~ ", added, covar, 
      " | (IMTI ~ ", instrument, ")"
    ))
  }
  
  return(f)
}


added_1 <- "hghgrade_final_num + factor(typesite_r3) +"
added_2 <- "hghgrade_4_num + factor(typesite_r3) +"


# fols1 <- make_formula_iv("wage_employ", added = added_1)
# fols2 <- make_formula_iv("raw_maths", added = added_2)
# fols3 <- make_formula_iv("raw_lang", added = added_2)

# ols1 <- felm(fols1, data = non_aa_samp)
# ols2 <- felm(fols2, data = non_aa_samp)
# ols3 <- felm(fols3, data = non_aa_samp)

# stargazer(
#   ols1, ols2, ols3,
#   keep = c("IMTI", "region"),
#   keep.stat = c("n","rsq"),
#   type = "text"
# )

fiv1 <- make_formula_iv("wage_employ", "E_is", added = NULL)
fiv2 <- make_formula_iv("self_employ", "E_is", added = NULL)
fiv3 <- make_formula_iv("raw_maths", "E_is", added = NULL)
fiv4 <- make_formula_iv("raw_lang", "E_is", added = NULL)
fiv5 <- make_formula_iv("hghgrade_final_num", "E_is", added = NULL)
fiv6 <- make_formula_iv("wage_employII", "E_is", added = NULL)
fiv7 <- make_formula_iv("self_employII", "E_is", added = NULL)

iv1 <- felm(fiv1, data = non_aa_samp)
iv2 <- felm(fiv2, data = non_aa_samp)
iv3 <- felm(fiv3, data = non_aa_samp)
iv4 <- felm(fiv4, data = non_aa_samp)
iv5 <- felm(fiv5, data = non_aa_samp)
iv6 <- felm(fiv6, data = non_aa_samp)
iv7 <- felm(fiv7, data = non_aa_samp)

iv1aa <- felm(fiv1, data = aa_samp)
iv2aa <- felm(fiv2, data = aa_samp)
iv3aa <- felm(fiv3, data = aa_samp)
iv4aa <- felm(fiv4, data = aa_samp)
iv5aa <- felm(fiv5, data = aa_samp)
iv6aa <- felm(fiv6, data = aa_samp)
iv7aa <- felm(fiv7, data = aa_samp)


stargazer(
  # iv1, iv2,
  iv3, iv4, iv5,
  iv6, iv1,
  keep = c("IMTI"),
  keep.stat = c("n","rsq"),
  type = "text"
)

stargazer(
  # iv1aa, iv2aa, 
  iv3aa, iv4aa, iv5aa,
  iv6aa, iv1aa,
  keep = c("IMTI"),
  keep.stat = c("n","rsq"),
  type = "text"
)






ivss1 <- felm(fiv1, data = non_aa_samp, subset = region %in% c("SNNP"))
ivss2 <- felm(fiv2, data = non_aa_samp, subset = region %in% c("SNNP"))
ivss3 <- felm(fiv3, data = non_aa_samp, subset = region %in% c("SNNP"))

stargazer(
  ivss1, ivss2, ivss3,
  keep = c("IMTI"),
  keep.stat = c("n","rsq"),
  type = "text"
)

# Robustness checks ####

non_aa_samp %>% 
  count(chethnic, region, sort = TRUE) %>% 
  filter(chethnic == "Amhara")

# sample size issue
# * only 39 Amharas not in Amhara
# The others evan less.

# Focus on Amharas

non_aa_samp %>% 
  filter( 
    chethnic == "Amhara" & region != "Amhara"
    )
  







