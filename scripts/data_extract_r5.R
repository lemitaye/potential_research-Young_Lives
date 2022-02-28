
# Date created: February 24, 2022

# preparation
rm(list = ls())

library(tidyverse)
library(haven)
library(janitor)

# Read data 
child_r5 <- read_dta("raw_data/Round_5_2016/et_r5_occh_olderchild.dta")
activity_r5 <- read_dta("raw_data/Round_5_2016/et_r5_occh_activity.dta")

# cleaning

child_r5 <- child_r5 %>% 
  select(
    child_id = CHILDCODE, # id
    region = NEWRGCR5, # Region of residence – OBSERVE
    educ_high = HGHQULR5, # highest completed qualification/certificate
    now_in_educ = ENRSCHR5, # Are you currently in full‐time education?
    marital = MRTSTSR5 # What is your current marital status?
  )
  
activity_r5 <- activity_r5 %>% 
  select(
    child_id = CHILDCODE, # id
    no_activ = ACTIDR5,   # No. of Paid Activities
    type_activ = ACTR5,     # Type of activity_r5
    owner_activ = ACTOWNR5,  # Who do you do this activity_r5 for?
    pay_form = PYMRECR5,     # form of payment 
    in_cash = ERNCSHR5,      # In cash (value in Birr)
    in_kind_value = ERNKNDR5, # In kind (cash equivalent) (value in Birr)
    pay_period = HWPAIDR5,  # What period of time did this payment cover?
    current_perform = PRFACTR5 # Do you currently perform this activity_r5?  
  )


activity_r5 %>% 
  count(no_activ)

activity_r5 %>% 
  count(type_activ)

activity_r5 %>% 
  count(pay_form)
  
activity_r5 %>% 
  count(type_activ, pay_form) %>% 
  # filter(pay_form == 0) %>% 
  print(n = Inf)

activity_r5 %>% 
  filter(type_activ %in% 1:7) 
  
child_r5 %>% 
  count(educ_high)

# After joining with r3: type of activity by region
# (think of doing this by sector)
activity_r5 %>% 
  left_join(child_hh_r3, by = "child_id") %>% 
  filter(!is.na(type_activ)) %>% 
  mutate( agg_activity = case_when(
    type_activ %in% c(1:4, 8:11) ~ "Self Employed",
    type_activ %in% c(5, 12, 13) ~ "Wage employment (salaried & unsalaried)",
    type_activ == NA ~ "Unknown",
    TRUE ~ "Other"
  )) %>% 
  count(region, agg_activity) %>% print(n = Inf)


# anti_join(x, y) drops all observations in x that have a match in y.

activity_r5 %>% 
  anti_join(child_hh_r3, by = "child_id")

child_r5 %>% 
  anti_join(child_hh_r3, by = "child_id")

# earnings:

activity_r5 %>% 
  filter(is.na(in_kind_value), is.na(in_cash)) %>% 
  count(type_activ, sort = TRUE)

activity_r5 %>% 
  filter(!is.na(in_kind_value) | !is.na(in_cash)) %>% 
  count(pay_period, sort = TRUE)

earning_month <- activity_r5 %>% 
  left_join(child_hh_r3, by = "child_id") %>% 
  filter( !is.na(in_kind_value) | !is.na(in_cash) ) %>% 
  mutate(
    in_cash = replace_na(in_cash, 0),
    in_kind_value = replace_na(in_kind_value, 0),
    raw_earning = in_cash + in_kind_value,
    std_earning = case_when(
      pay_period == 2 ~ raw_earning * 30,
      pay_period == 3 ~ raw_earning * 4,
      pay_period == 5 ~ raw_earning / 12,
      TRUE ~ raw_earning       
    )
      ) %>% 
  mutate(
    region = case_when(
      region ==  11 ~ "Addis Ababa",
      region ==  12 ~ "Amhara",
      region ==  13 ~ "Oromia",
      region ==  14 ~ "SNNP",
      region ==  15 ~ "Tigray"
    ) %>% factor() ) %>% 
  filter(in_kind_value >= 0, !is.na(region)) 
  

earning_month %>% 
  filter(std_earning < 1000) %>% 
  ggplot(aes(region, std_earning)) +
  geom_boxplot()

# should still be very careful since we have removed those who are
# self-employed (half of the data), which might lead to systematic
# bias

# Educational attainment in round 5

educ_high <- child_r5 %>% 
  select(-region) %>% 
  inner_join(
    child_hh_r3 %>% select(child_id, region), 
    by = "child_id") %>% 
  filter(educ_high %in% c(0:3, 6, 7)) %>% 
  mutate(
      educ_high = case_when(
      educ_high ==  0 ~ "No certificate",
      educ_high ==  1 ~ "Grade 8 completion",
      educ_high ==  2 ~ "Ethiopian General secondary education",
      educ_high ==  3 ~ "Ethiopian higher education entrance certificate",
      educ_high ==  6 ~ "Completion TVET certificate",
      educ_high ==  7 ~ "University Degree"
    ) %>% factor( levels = c(
      "No certificate",
      "Grade 8 completion",
      "Ethiopian General secondary education",
      "Completion TVET certificate",
      "Ethiopian higher education entrance certificate",
      "University Degree"
    ) ) 
    )

educ_high %>% 
  count(region, educ_high) %>% 
  ggplot(aes(educ_high, n)) +
  geom_col(mapping = aes(fill = region), 
           position = "dodge") +
  coord_flip() + 
  labs(
    title = "Highest completed qualification by region",
    x = "",
    y = "Count",
    fill = "Region"
  )
  















