
# Date created: February 24, 2022

# preparation
rm(list = ls())

library(tidyverse)
library(haven)

# Read data 
child_hh_r2 <- read_dta("raw_data/Round_2_2006/etchildlevel12yrold.dta")

child_hh_r2 <- child_hh_r2 %>% 
  select(
    child_id = CHILDID,  # id
    sex = SEX,      # sex of child
    urban = TYPESITE, # Urban/Rural
    region = REGION,   # Region of residence.
    lang_local = EDOMLANG, # most commonly used language in the locality
    ethnic_orig = CHLDETH,  # child's ethnic origin
    lang_frst = CHLNG1ST, # the first language the child learned at home
    speak_local = CHSPKLNG, # does child speak the most commonly used language 
    address_born = CHILDLIV, # Where did YL child live when she/he was born?
    yrs_lived = LONGLIV,  # How long has YL child lived here (in years)? 
    left = LEFT      # Has the YL child ever left this locality? 
  ) 

child_hh_r2 %>% 
  count(yrs_lived)

child_hh_r2 %>% 
  count(address_born)

child_hh_r2 %>% 
  count(region, ethnic_orig)

child_hh_r2 %>% 
  count(region, lang_local, lang_frst) %>% 
  print(n = Inf)




















