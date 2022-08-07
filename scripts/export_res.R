
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
  title = "Estimates Excluding the Addis Ababa Sample",
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
  panel.names = c("First Stage$^{\\dag}$", "2SLS$^{\\ddag}$")
) %>% star_insert_row(
  c(
    " & Z-core & Z-core & Employed & Employed \\\\"
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
  # iv1, iv2,
  iv3aa$stage1, iv4aa$stage1, #iv5aa$stage1,
  iv6aa$stage1, iv1aa$stage1,
  keep = c("E_is"),
  keep.stat = c("n"),
  column.labels = c("Maths", "Language", "Wage", "Salary"),
  dep.var.labels = "",
  covariate.labels = "$E_{is}$",
  title = "Estimates for Addis Ababa Sample",
  label = "tab:aareg"
  # ,type = "text"
)

iv_aa <- stargazer(
  # iv1, iv2,
  iv8aa, iv9aa, #iv5aa,
  iv6aa, iv1aa,
  keep = c("IMTI"),
  keep.stat = c("n"),
  covariate.labels = "IMTI"
  # ,type = "text"
)

tabIII <- star_panel(
  first_aa, iv_aa,
  same.summary.stats = TRUE, 
  panel.label.fontface = "bold",
  panel.names = c("First Stage$^{\\dag}$", "2SLS$^{\\ddag}$")
) %>% star_insert_row(
  c(
    " & Z-score & Z-score & Employed & Employed \\\\"
  ),
  insert.after = c(13)
) %>% star_notes_tex(
  note.type = "threeparttable",
  note = "Standard errors are in parentheses. *** Significant at 1\\%, ** Significant at 5\\%, * Significant at 10\\%."
)

star_tex_write(tabIII, 
               file = "Young-Lives---Collaboration/tables/tableIII.tex")


## Reduced form for the AA sample ####

rfaa <- stargazer(
  rf6aa, rf7aa, rf5aa, rf1aa,
  keep = c("E_is"),
  # type = "text",
  keep.stat = c("n","rsq"),
  # column.labels = c("Maths", "Language", "Highest", "Wage", "Wage"),
  dep.var.labels = c("Maths", "Language", "Wage", "Salary"),
  covariate.labels = "$E_{is}$",
  title = "Reduced Form Estimates for Addis Ababa Sample",
  label = "tab:aared"
)

tabIV <- star_insert_row(rfaa,
  c(
    " & Z-score & Z-score & Employed & Employed \\\\"
  ),
  insert.after = c(12)
) %>% star_notes_tex(
  note.type = "threeparttable",
  note = "Standard errors are in parentheses. *** Significant at 1\\%, ** Significant at 5\\%, * Significant at 10\\%."
)

star_tex_write(tabIV, 
               file = "Young-Lives---Collaboration/tables/tableIV.tex")


# Robustness Check (Oromia vs. Tigray) ####

ot_rob <- stargazer(
  rf6ot, rf7ot, rf5ot ,rf1ot, 
  keep = c("T"),
  keep.stat = c("n","rsq"),
  dep.var.labels = c("Maths", "Language", "Wage", "Salary"),
  title = "Robustness Check: Oromia vs. Tigray",
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









