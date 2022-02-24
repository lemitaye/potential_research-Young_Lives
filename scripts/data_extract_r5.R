
# Date created: February 24, 2022

# preparation
rm(list = ls())

library(tidyverse)
library(haven)
library(janitor)

# Read data 
child <- read_dta("raw_data/Round_5_2016/et_r5_occh_olderchild.dta")
activity <- read_dta("raw_data/Round_5_2016/et_r5_occh_activity.dta")

# cleaning

child <- child %>% 
  select(
    CHILDCODE, # id
    NEWRGCR5, # Region of residence – OBSERVE
    HGHQULR5, # highest completed qualification/certificate
    ENRSCHR5, # Are you currently in full‐time education?
    MRTSTSR5, # What is your current marital status?
    ENRSCHR5, # Are you currently in full‐time education?
  ) %>% 
  clean_names()
  
activity <- activity %>% 
  select(
    CHILDCODE, # id
    ACTIDR5,   # No. of Paid Activities
    ACTR5,     # Type of activity
    ACTOWNR5,  # Who do you do this activity for?
    PYMRECR5,  # form of payment 
    ERNCSHR5,  # In cash (value in Birr)
    ERNKNDR5,  # In kind (cash equivalent) (value in Birr)
    HWPAIDR5,  # What period of time did this payment cover?
    PRFACTR5  # Do you currently perform this activity?  
  )
  
  
  
  
  
  
  
  
  

