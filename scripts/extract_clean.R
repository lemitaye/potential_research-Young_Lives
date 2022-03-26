
# Date created: March 25, 2022
# 
# Description: This script loads the necessary data from several rounds of the
#   Young Lives Data sets, selects relevant variables, joins separate files, 
#   cleans, merges, constructs new variables (both outcomes and covariates) and
#   produces file(s) ready for analyses.

# Note: load the necessary packages in "master.R" before running this script


# Load data: ####

cons_all <- read_dta("raw_data/Rounds_1-5_Constructed_Files/ethiopia_constructed.dta")

child_r3 <- read_dta("raw_data/Round_3_2009/et_oc_childlevel.dta")

cognitive_r4 <- read_dta("raw_data/Round_4_2013-14/et_r4_occog_olderchild.dta")

occh_activity <- read_dta("raw_data/Round_5_2016/et_r5_occh_activity.dta")

# clean data: ####

cons_oc <- cons_all %>% 
  filter(yc == 0) %>%  # filter older cohort 
  filter(is.na(deceased)) %>% # filter non-deceased
  mutate(child_id = str_remove(childid, "ET") %>% as.numeric()) %>% 
  select(
    child_id, yc:cookingq_new,
    -c(deceased, chsmoke, childid)
  )

test_r4 <- cognitive_r4 %>% 
  select(
    child_id = CHILDCODE,
    lang_instr = RDITLANR4,
    raw_lang = lang_raw,
    perco_lang = lang_perco,
    raw_maths = maths_raw,
    perco_maths = maths_perco
  ) %>% 
  mutate(
    lang_instr = case_when(
      lang_instr == 1 ~ "English",
      lang_instr == 2 ~ "Amharic",
      lang_instr == 3 ~ "Hadiya",
      lang_instr == 4 ~ "Afaan Oromo",
      lang_instr == 5 ~ "Wolayta",
      lang_instr == 6 ~ "Sidamigna",
      lang_instr == 7 ~ "Tigrigna"
    )
  )

test_r3 <- child_r3 %>% 
  select(
    child_id = CHILDID, # id
    lang_math_test = MTHLNGTS,
    verbal_score = ppvt,
    math_score = math,
  ) %>% 
  mutate( 
    child_id = str_remove(child_id, "ET") %>% as.numeric(),
    
    lang_math_test = case_when(
      lang_math_test == 2 ~ "Amharic",
      lang_math_test == 7 ~ "Hadiya",
      lang_math_test == 11 ~ "Afaan Oromo",
      lang_math_test == 12 ~ "Sidamigna",
      lang_math_test == 15 ~ "Tigrigna",
      lang_math_test == 16 ~ "Wolayta",
      lang_math_test %in% c(88, NA) ~ NA_character_,
    )
    )

# Extract time invariant characteristics from "constructed" file: 
time_invar <- cons_oc %>% 
  filter(round == 5) %>% 
  select(
    child_id, # id
    chsex, # sex
    chethnic, # ethnicity
    chlang, # first language
    chldrel, # religion
    preprim, # attended pre-primary school
    agegr1 # age at start of grade 1
    ) 


activity_r5 <- occh_activity %>% 
  select(
    child_id = CHILDCODE, # id
    no_activ = ACTIDR5,   # No. of Paid Activities
    type_activ = ACTR5,     # Type of activity_r5
    owner_activ = ACTOWNR5,  # Who do you do this activity_r5 for?
    pay_form = PYMRECR5,     # form of payment 
    in_cash = ERNCSHR5,      # In cash (value in Birr)
    in_kind_value = ERNKNDR5, # In kind (cash equivalent) (value in Birr)
    pay_period = HWPAIDR5  # What period of time did this payment cover?
  ) %>% 
  # Filter the main activity (child-activity combination)
  filter(no_activ ==  1) %>% 
  mutate(
    wage_employ = case_when(
      # The following get 1:
      #    5: Wage Employment (Agriculture)
      #   12: Wage Employment (Unsalaried; Non-agriculture)
      #   13: Regular Salaried Employment
      type_activ %in% c(5, 12, 13) ~ 1,
      
      # Otherwise, 0
      TRUE ~ 0
    )
  )


# Variables from round 5

vars_r5 <- cons_oc %>% 
  filter(round == 5) %>% 
  select(
    child_id, # id
    hghgrade_r5 = hghgrade, # Highest grade achieved at time of interview
    region_r5 = region, # region of residence
    hwork_r5 = hwork  # Hours/day spent in paid activity
  ) 

# Variables from round 4

vars_r4 <- cons_oc %>% 
  filter(round == 4) %>% 
  select(
    child_id, # id
    region_r4 = region, # region of residence
    hghgrade_r4 = hghgrade, # Highest grade achieved at time of interview
  ) 


# Variables from round 3

# Here we collect a lot of covariates:

vars_r3 <- cons_oc %>% 
  filter(round == 3) %>% 
  select(
    child_id, # id
    region_r3 = region, # region of residence
    hghgrade_r3 = hghgrade # Highest grade achieved at time of interview
  ) 


# Construction of variables ####

# Construct the (local) language of instruction in primary schools

language_primary <- vars_r4 %>% 
  select(child_id, hghgrade_r4) %>% 
  left_join(test_r3, by = "child_id") %>% 
  left_join(test_r4, by = "child_id") %>% 
  mutate(
    # 
    lang_primary =
      case_when(
        # If child is in primary school and reported a local language of 
        # instruction, assign that as the language of primary school
        (hghgrade_r4 <= 8 & lang_instr != "English") ~ lang_instr,
        
        # Otherwise, use the language used during the math test in round 3 as
        # a proxy
        TRUE ~ lang_math_test
      ) %>% factor()
  ) %>% 
  select(child_id, lang_primary)


# join with region in round 3 and time invariant variables

joined <- time_invar %>% 
  left_join(language_primary, by = "child_id") %>% 
  left_join(vars_r3, by = "child_id") %>% 
  left_join(vars_r5, by = "child_id") %>% 
  left_join(activity_r5, by = "child_id")

# Being active is highly correlated with reporting activity ("type_activ)
joined %>% 
  mutate(active = case_when(
    hwork_r5 == 0 ~ 0, 
    hwork_r5 > 0 ~ 1
  )) %>% 
  count(active, type_activ) %>% 
  print(n = Inf)













