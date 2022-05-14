
# Date created: April 30, 2022
# Depends on: extract_clean.R

# Figures and Descriptive Statistics

# Some descriptives:

non_aa_samp %>% 
  ggplot(aes(IMTI, raw_maths)) +
  geom_point() +
  facet_wrap(~ region)

non_aa_samp %>% 
  filter(!is.na(wage_employ)) %>% 
  ggplot(aes(factor(wage_employII))) +
  geom_histogram(stat = "count") +
  facet_wrap(~ region)

non_aa_samp %>% 
  count(region, wage_employII)

non_aa_samp %>% 
  ggplot(aes(IMTI, wage_employII)) +
  geom_point(position = "jitter") +
  geom_hline(yintercept = 0.5, color = "blue") +
  facet_wrap(~ region)

non_aa_samp %>% 
  ggplot(aes(factor(region), hghgrade_final_num)) +
  geom_boxplot() 

non_aa_samp %>% 
  group_by(region) %>% 
  summarise(mean_wage = mean(wage_employII, na.rm = TRUE)) %>% 
  ggplot(aes(fct_reorder(region, mean_wage), mean_wage)) +
  geom_col() +
  coord_flip() 

plot_by_region <- function(var) {
  non_aa_descr %>% 
    group_by(region) %>% 
    summarise(mean_wage = mean({{var}}, na.rm = TRUE)) %>% 
    ggplot(aes(fct_reorder(region, mean), mean_wage)) +
    geom_col() +
    coord_flip()
}
 

plot_by_region(wage_employII)
plot_by_region(hghgrade_final_num)

non_aa_samp %>% 
  ggplot(aes(region, hghgrade_final_num)) +
  geom_boxplot() +
  coord_flip()

plot_by_region(raw_maths)
plot_by_region(raw_lang)

non_aa_descr %>% 
  group_by(region) %>% 
  summarise(mean_lang = mean(raw_lang, na.rm = TRUE)) %>% 
  ggplot(aes(fct_reorder(region, mean_lang), mean_lang)) +
  geom_col() +
  coord_flip()


# Table of Summary Statistics (Table I) ####
non_aa_descr <- non_aa_samp %>% 
  mutate(
    male = (chsex == "Male"),
    care_female = (caresex_r1 == 2),
    stunted = (stunting == 1)
  ) %>% 
  select(
    male, zbfa, stunted, caredu_r1, careage_r1, care_female, hhsize, 
    wi_new, hq_new, cd_new, elecq_new, ownlandhse_r1, region, raw_maths,
    raw_lang, hghgrade_final_num, wage_employ, wage_employII
  ) %>% 
  # filter(complete.cases(.)) %>%
  dummy_cols(c("region")) %>% 
  select(-c(region))


aa_descr <- aa_samp %>% 
  mutate(
    male = (chsex == "Male"),
    care_female = (caresex_r1 == 2),
    stunted = (stunting == 1)
  ) %>% 
  select(
    male, zbfa, stunted, caredu_r1, careage_r1, care_female, hhsize, 
    wi_new, hq_new, cd_new, elecq_new, ownlandhse_r1, raw_maths,
    raw_lang, hghgrade_final_num, wage_employ, wage_employII
  ) #%>% 
  # filter(complete.cases(.)) 
# %>% 
#   dummy_cols(c("region")) %>% 
#   select(-c(region))


sd_rm_bin <- function(x) {
  require(purrr)
  
  if ( is_logical(x) ) {
    res <- NA
  } else {
    res <- sd(x, na.rm = TRUE)
  }
  
  res
}

obs_nonaa <- non_aa_descr %>% 
  map_dbl(~sum(!is.na(.)))

mean_nonaa <- non_aa_descr %>% 
  map_dbl(mean, na.rm = TRUE) %>% 
  round(3)

sd_nonaa <- non_aa_descr %>% 
  map_dbl(sd_rm_bin) %>% 
  round(3)

obs_aa <- aa_descr %>% 
  map_dbl(~sum(!is.na(.)))

mean_aa <- aa_descr %>% 
  map_dbl(mean, na.rm = TRUE) %>% 
  round(3)

sd_aa <- aa_descr %>% 
  map_dbl(sd_rm_bin) %>% 
  round(3)

x <- cbind(obs_nonaa, mean_nonaa, sd_nonaa) %>% 
  data.frame() %>% 
  rownames_to_column("variable")

y <- cbind(obs_aa, mean_aa, sd_aa) %>% 
  data.frame() %>% 
  rownames_to_column("variable")

z <- full_join(x, y, by = "variable") 
z$empty0 <- NA
z$empty1 <- NA
z <- select(z, empty0, variable, obs_nonaa, mean_nonaa, sd_nonaa, empty1, 
            obs_aa, mean_aa, sd_aa)

xtab <- xtable(
  z, display = c("s", "s", "s", "g", "g", "g", "s", "g", "g", "g"),
  digits = 4, caption = "Summary Statistics", label = "tab:01"
) 

comm <- paste0(" \n \\\\[-1.8ex] \\multicolumn{6}{l}",
               "{\\footnotesize{\\textit{Note:} The standard deviations for proportions is 
               not presented.}} \n")


addtorow <- list()
addtorow$pos <- list(0, 0, 0, 0, 0, 21, 21)
addtorow$command <- c(
  " \\\\[-1.8ex]",
  " & & \\multicolumn{3}{c}{Non AA Sample} & & \\multicolumn{3}{c}{AA Sample}  \\\\[0.2ex]",
  "\\cline{3-5}  \\cline{7-9}  \\\\[-1.2ex]" ,
  " & & \\multicolumn{1}{c}{N} & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{sd} & & \\multicolumn{1}{c}{N} & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{sd} \\\\",
  " \\\\[-1.8ex] &  & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} & & 
  \\multicolumn{1}{c}{(4)} & \\multicolumn{1}{c}{(5)} & \\multicolumn{1}{c}{(6)} \\\\ ",
  # " \\\\[-1.8ex] \\hline \\\\[-1.8ex] ",
  # obs_descr,
  " \\bottomrule ",
  comm
)


print(
  xtab, 
  add.to.row = addtorow,
  include.rownames = FALSE, include.colnames = FALSE, 
  booktabs = TRUE, caption.placement = "top", 
  hline.after = c(-1, 0),
  file = "tex/tables/tableI.tex" 
)







