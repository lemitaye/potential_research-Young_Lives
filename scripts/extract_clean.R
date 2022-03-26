
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
    child_id, everything(),
    -c(deceased, chsmoke, childid)
  ) %>% 
  mutate(
    region = case_when(
      region ==  1 ~ "Tigray",
      region ==  2 ~ "Afar",
      region ==  3 ~ "Amhara",
      region ==  4 ~ "Oromia",
      region ==  5 ~ "Somali",
      region ==  7 ~ "SNNP",
      region ==  14 ~ "Addis Ababa"
    ) %>% factor()
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
    ) %>% 
  mutate(
    chlang = case_when(
      chlang == 1 ~  "Afar",
      chlang == 2 ~  "Amharic",
      chlang == 6 ~  "Gurage",
      chlang == 7 ~  "Hadiya",
      chlang == 11 ~ "Afaan Oromo",
      chlang == 12 ~ "Sidamigna",
      chlang == 13 ~ "Siltigna",
      chlang == 15 ~ "Tigrigna",
      chlang == 16 ~ "Wolayta",
      chlang == 20 ~ "Other",
      chlang == NA ~ NA_character_
    ) %>% factor(),
    
    chethnic = case_when(
      chethnic == 10 ~ "Other",
      chethnic == 12 ~ "Amhara",
      chethnic == 13 ~ "Gurage",
      chethnic == 14 ~ "Hadiya",
      chethnic == 16 ~ "Oromo",
      chethnic == 17 ~ "Sidama",
      chethnic == 18 ~ "Tigrian",
      chethnic == 19 ~ "Wolayta"
    ) %>% factor()
  )


activity_r5 <- occh_activity %>% 
  select(
    child_id = CHILDCODE, # id
    no_activ = ACTIDR5,   # No. of Paid Activities
    type_activ = ACTR5     # Type of activity_r5
  ) %>% 
  # Filter the main activity (child-activity combination)
  filter(no_activ ==  1) 


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

vars_r3 <- cons_oc %>% 
  filter(round == 3) %>% 
  select(
    # Here we collect a lot of covariates:
    child_id, # id
    region_r3 = region, # region of residence
    hghgrade_r3 = hghgrade, # Highest grade achieved at time of interview
    dadedu, # Father's level of education
    dadage, # Father's age
    dadcantread, # Father cannot read
    momedu, # Mother's level of education
    momage, # Mother's age
    momcantread , # Mother cannot read
    hhsize, # household size
    wi_new, # Wealth index
    hq_new, # Housing quality index
    sv_new, # Access to services index
    cd_new, # Consumer durables index
    drwaterq_new, # Access to safe drinking water
    toiletq_new, # Access to sanitation
    elecq_new, # Access to electricity
    cookingq_new, # Access to adequate fuels for cooking
    aniany, # Household owned any livestock in the past 12 months
    ownlandhse, #  Household owns land where house is on
    ownhouse # Household owns the house
  ) 


# Variables from round 2

vars_r2 <- cons_oc %>% 
  filter(round == 2) %>% 
  select(
    child_id, # id
    region_r2 = region, # region of residence
    hghgrade_r2 = hghgrade # Highest grade achieved at time of interview
  ) 

# Variables from round 1

vars_r1 <- cons_oc %>% 
  filter(round == 1) %>% 
  select(
    child_id, # id
    region_r1 = region, # region of residence
    hghgrade_r1 = hghgrade, # Highest grade achieved at time of interview
    zwfa, # Weight-for-age z-score
    zhfa, # Height-for-age z-score
    zbfa, # BMI-for-age z-score
    underweight, # Low weight for age
    stunting, # Short height for age
    thinness # Low BMI for age
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
  left_join(vars_r1, by = "child_id") %>% 
  left_join(vars_r2, by = "child_id") %>% 
  left_join(vars_r3, by = "child_id") %>% 
  left_join(vars_r4, by = "child_id") %>% 
  left_join(vars_r5, by = "child_id") %>% 
  left_join(activity_r5, by = "child_id") 

# Function to generate intensity of treatment variable:

top_code <- function(var, ceil) {
  
  out <- if_else( var > ceil, ceil, var )
  
  return(out)
}


joined <- joined %>% 
  # Generate main outcome variable and instruments
  mutate(
    
    active = case_when(
      hwork_r5 == 0 ~ 0, 
      hwork_r5 > 0 ~ 1
    ),
    
    wage_employ = case_when(
      # The following get 1:
      #    5: Wage Employment (Agriculture)
      #   12: Wage Employment (Unsalaried; Non-agriculture)
      #   13: Regular Salaried Employment
      type_activ %in% c(5, 12, 13) ~ 1,
      
      # If both "type_activ" and "active" are missing, they get NA.
      is.na(type_activ) & is.na(active) ~ NA_real_,
      
      # Otherwise, 0.
      TRUE ~ 0
    ),
    
    hghgrade = case_when(
      
      !is.na(hghgrade_r5) ~ hghgrade_r5,
      
      is.na(hghgrade_r5) & !is.na(hghgrade_r4) ~ hghgrade_r4,
      
      is.na(hghgrade_r5) & is.na(hghgrade_r4) & 
        !is.na(hghgrade_r3) ~ hghgrade_r3,
      
      is.na(hghgrade_r5) & is.na(hghgrade_r4) & 
        is.na(hghgrade_r3) & !is.na(hghgrade_r2) ~ hghgrade_r2,
      
      is.na(hghgrade_r5) & is.na(hghgrade_r4) & 
        is.na(hghgrade_r3) & is.na(hghgrade_r2) ~ hghgrade_r1
      
    ),
    
    hghgrade_num = case_when(
      hghgrade <= 14 ~ as.numeric(hghgrade),
      # Recode "Religious education" and "Other" to 0
      hghgrade %in% c(29, 30) ~ 0
    ),

    IMTI = case_when(

      region_r3 == "Tigray" & chlang == "Tigrigna" & lang_primary == "Tigrigna"
      ~ top_code(hghgrade_num, 8),

      region_r3 == "Oromia" & chlang == "Afaan Oromo" & lang_primary == "Afaan Oromo"
      ~ top_code(hghgrade_num, 8),

      region_r3 == "Oromia" & chlang == "Amaharic" & lang_primary == "Amharic"
      ~ top_code(hghgrade_num, 8),

      region_r3 == "Amhara" & chlang == "Amharic" & lang_primary == "Amharic"
      ~ top_code(hghgrade_num, 6),

      region_r3 == "Amhara" & chlang == "Afaan Oromo" & lang_primary == "Afaan Oromo"
      ~ top_code(hghgrade_num, 8),

      region_r3 == "SNNP" & chlang == "Sidamigna" & lang_primary == "Sidamigna"
      ~ top_code(hghgrade_num, 4),

      region_r3 == "SNNP" & chlang == "Wolayta" & lang_primary == "Wolayta"
      ~ top_code(hghgrade_num, 4),

      region_r3 == "SNNP" & chlang == "Hadiya" & lang_primary == "Hadiya"
      ~ top_code(hghgrade_num, 4),

      region_r3 == "SNNP" & chlang == "Amharic" & lang_primary == "Amharic"
      ~ top_code(hghgrade_num, 4),

      region_r3 == "Addis Ababa" & chlang == "Amharic" & lang_primary == "Amharic"
      ~ top_code(hghgrade_num, 6),

      TRUE ~ 0
    ),
    
    # omit child language "other" 
    # region_r3 == "SNNP" & chlang == "Afaan Oromo" & lang_primary == "Afaan Oromo" 
    # ~ top_code(hghgrade_num, 4), ?
    
    E_is = case_when(
      
      region_r3 == "Oromia" & chethnic == "Oromo" ~ 1,
      
      region_r3 == "Tigray" & chethnic == "Tigrian" ~ 1,
      
      region_r3 == "Oromia" & chethnic == "Amhara" & lang_primary == "Amharic" ~ 1,
      
      region_r3 == "Amhara" & chethnic == "Oromo" & lang_primary == "Afaan Oromo" ~ 1,
      
      region_r3 == "Amhara" & chethnic == "Amhara" ~ 0.75,
      
      region_r3 == "SNNP" & chethnic == "Hadiya" & lang_primary == "Hadiya" ~ 0.5,
      
      region_r3 == "SNNP" & chethnic == "Sidama" & lang_primary == "Sidamigna" ~ 0.5,
      
      region_r3 == "SNNP" & chethnic == "Wolayta" & lang_primary == "Wolayta" ~ 0.5,
      
      region_r3 == "SNNP" & chethnic == "Amhara" & lang_primary == "Amharic" ~ 0.5,
      
      TRUE ~ 0
    )
    
  )

# Next steps:
#  Add some covariates from round 3
#  Start some preliminary analysis









