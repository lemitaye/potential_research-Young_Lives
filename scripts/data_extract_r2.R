
# Date created: February 24, 2022

# preparation
rm(list = ls())

library(tidyverse)
library(haven)
library(janitor)

# Read data 
child_hh <- read_dta("raw_data/Round_2_2006/etchildlevel12yrold.dta")

child_hh <- child_hh %>% 
  select(
    CHILDID,  # id
    SEX,      # sex of child
    TYPESITE, # Urban/Rural
    REGION,   # Region of residence.
    EDOMLANG, # most commonly used language in the locality
    CHLDETH,  # child's ethnic origin
    CHLNG1ST, # the first language the child learned at home
    CHSPKLNG, # does child speak the most commonly used language 
    CHILDLIV, # Where did YL child live when she/he was born?
    LONGLIV,  # How long has YL child lived here (in years)? 
    LEFT      # Has the YL child ever left this locality? 
  ) %>% 
  clean_names()




















