
# Date created: April 30, 2022
# Depends on: extract_clean.R; estimation.R

# A script to send results to .tex files

# Main Regression Table (Non-AA Sample) ####

first <- stargazer(
  # iv1, iv2,
  iv8$stage1, iv9$stage1, #iv5$stage1,
  iv6$stage1, iv1$stage1,
  keep = c("E_is"),
  keep.stat = c("n"),
  column.labels = c("Maths", "Language", "Wage", "Salary"),
  dep.var.labels = "",
  covariate.labels = "$E_{is}$",
  title = "Main Results",
  label = "tab:mainreg"
  # ,type = "text"
)

iv <- stargazer(
  # iv1, iv2,
  iv8, iv9, #iv5,
  iv6, iv1,
  keep = c("IMTI"),
  keep.stat = c("n"),
  covariate.labels = "IMTI"
  # ,type = "text"
)

tabII <- star_panel(
  first, iv,
  same.summary.stats = TRUE, 
  panel.label.fontface = "bold",
  panel.names = c("First Stage", "2SLS")
) %>% star_insert_row(
  c(
    " & Z-score & Z-score & Employed & Employed \\\\"
  ),
  insert.after = c(13)
) %>% star_notes_tex(
  note.type = "threeparttable",
  note = "Standard errors are in parentheses. *** Significant at 1\\%, ** Significant at 5\\%, * Significant at 10\\%."
)

star_tex_write(tabII,
  file = "Young-Lives---Collaboration/tables/tableII.tex"
)


# Results for the AA sample ####

## First stages and IV for the AA sample ####

first_aa <- stargazer(
  rf6aa, rf7aa, rf5aa, rf1aa, iv5aa_yc_rf, iv6aa_yc_rf,
  keep = c("E_is"),
  keep.stat = c("n"),
  column.labels = c("Maths", "Language", "Wage", "Salary", "Maths", "Language"),
  dep.var.labels.include = FALSE,
  dep.var.caption = "",
  model.names = FALSE,
  covariate.labels = "$E_{is}$",
  title = "Estimates for Addis Ababa",
  label = "tab:aareg"
  # ,type = "text"
)

iv_aa <- stargazer(
  iv8aa, iv9aa, #iv5aa,
  iv6aa, iv1aa, iv5aa_yc, iv6aa_yc,
  keep = c("IMTI"),
  keep.stat = c("n"),
  covariate.labels = "IMTI"
  # ,type = "text"
)

tabIII <- star_panel(
  first_aa, iv_aa,
  same.summary.stats = TRUE, 
  panel.label.fontface = "bold",
  panel.names = c("Reduced Form", "2SLS")
) %>% star_insert_row(
  c(
    " & \\multicolumn{4}{c}{\\textbf{Older Chohort}} & \\multicolumn{2}{c}{\\textbf{Younger Cohort}} \\\\ ",
    "\\cline{2-5} \\cline{6-7} ",
    "\\\\[-1.8ex]",
    " & Z-score & Z-score & Employed & Employed & Z-score & Z-score \\\\ "
  ),
  insert.after = c(9, 9, 9, 10)
) %>% star_notes_tex(
  note.type = "threeparttable",
  note = "Standard errors are in parentheses. *** Significant at 1\\%, ** Significant at 5\\%, * Significant at 10\\%."
)

star_tex_write(tabIII, 
               file = "Young-Lives---Collaboration/tables/tableIII.tex")


## Reduced form & 2SLS for the younger cohort AA sample ####

# yc_rf_aa <- stargazer(
#   iv5aa_yc_rf, iv6aa_yc_rf,
#   keep = c("E_is"),
#   keep.stat = c("n"),
#   dep.var.labels = c("Maths", "Language"),
#   covariate.labels = "$E_{is}$",
#   title = "Estimates for Addis Ababa: Younger Cohort",
#   label = "tab:ycaa"
#   # ,type = "text"
# )
# 
# yc_iv_aa <- stargazer(
#   iv5aa_yc, iv6aa_yc,
#   keep = c("IMTI"),
#   keep.stat = c("n"),
#   covariate.labels = "IMTI"
#   # ,type = "text"
# )
# 
# tabIV <- star_panel(
#   yc_rf_aa, yc_iv_aa,
#   same.summary.stats = TRUE, 
#   panel.label.fontface = "bold",
#   panel.names = c("Reduced Form", "2SLS")
# ) %>% star_insert_row(
#   c(
#     " & Z-score & Z-score \\\\"
#   ),
#   insert.after = c(12)
# ) %>% star_notes_tex(
#   note.type = "threeparttable",
#   note = "Standard errors are in parentheses. *** Significant at 1\\%, ** Significant at 5\\%, * Significant at 10\\%."
# )
# 
# star_tex_write(tabIV, 
#                file = "Young-Lives---Collaboration/tables/tableIV.tex")



# Robustness Check (Oromia vs. Tigray) ####

ot_rob <- stargazer(
  rf6ot, rf7ot, rf5ot ,rf1ot, 
  keep = c("Tigray"),
  keep.stat = c("n","rsq"),
  dep.var.labels = c("Maths", "Language", "Wage", "Salary"),
  title = "Comparing Estimate for Tigray and Oromia",
  label = "tab:oromtig"
)

tabV <- star_insert_row(ot_rob,
                         c(
                           " & Z-score & Z-score & Employed & Employed \\\\"
                         ),
                         insert.after = c(12)
) %>% star_notes_tex(
  note.type = "threeparttable",
  note = "Standard errors are in parentheses. *** Significant at 1\\%, ** Significant at 5\\%, * Significant at 10\\%."
)

star_tex_write(tabV, 
               file = "Young-Lives---Collaboration/tables/tableV.tex")


# Regression for the non-Amhara sample (Non-AA Sample) ####

first_nonAm <- stargazer(
  # iv1, iv2,
  iv8_nonAm$stage1, iv9_nonAm$stage1, #iv5$stage1,
  iv6_nonAm$stage1, iv1_nonAm$stage1,
  keep = c("E_is"),
  keep.stat = c("n"),
  column.labels = c("Maths", "Language", "Wage", "Salary"),
  dep.var.labels = "",
  covariate.labels = "$E_{is}$",
  title = "Estimates Excluding the Amhara",
  label = "tab:nonAm"
  # ,type = "text"
)

iv_nonAm <- stargazer(
  # iv1, iv2,
  iv8_nonAm, iv9_nonAm, #iv5,
  iv6_nonAm, iv1_nonAm,
  keep = c("IMTI"),
  keep.stat = c("n"),
  covariate.labels = "IMTI"
  # ,type = "text"
)

tabVI <- star_panel(
  first_nonAm, iv_nonAm,
  same.summary.stats = TRUE, 
  panel.label.fontface = "bold",
  panel.names = c("First Stage", "2SLS")
) %>% star_insert_row(
  c(
    " & Z-score & Z-score & Employed & Employed \\\\"
  ),
  insert.after = c(13)
) %>% star_notes_tex(
  note.type = "threeparttable",
  note = "Standard errors are in parentheses. *** Significant at 1\\%, ** Significant at 5\\%, * Significant at 10\\%."
)

star_tex_write(tabVI,
               file = "Young-Lives---Collaboration/tables/tableVI.tex"
)






