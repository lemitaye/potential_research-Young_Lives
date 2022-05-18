
# Date created: April 30, 2022
# Depends on: extract_clean.R; estimation.R

# A script to send results to .tex files

# Main Regression Table (Non-AA Sample) ####

first <- stargazer(
  # iv1, iv2,
  iv3$stage1, iv4$stage1, iv5$stage1,
  iv6$stage1, iv1$stage1,
  keep = c("E_is"),
  keep.stat = c("n"),
  column.labels = c("Maths", "Language", "Highest", "Wage", "Wage"),
  dep.var.labels = "",
  covariate.labels = "$E_{is}$",
  title = "Estimates Excluding the Addis Ababa Sample"
  # ,type = "text"
)

iv <- stargazer(
  # iv1, iv2,
  iv3, iv4, iv5,
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
    " & Score & Score & Grade & Employ I & Employ II \\\\"
  ),
  insert.after = c(13)
)

star_tex_write(tabII,
  file = "tex/tables/tableII.tex"
)


# Results for the AA sample ####

## First stages and IV for the AA sample ####

first_aa <- stargazer(
  # iv1, iv2,
  iv3aa$stage1, iv4aa$stage1, iv5aa$stage1,
  iv6aa$stage1, iv1aa$stage1,
  keep = c("E_is"),
  keep.stat = c("n"),
  column.labels = c("Maths", "Language", "Highest", "Wage", "Wage"),
  dep.var.labels = "",
  covariate.labels = "$E_{is}$",
  title = "Estimates for Addis Ababa Sample"
  # ,type = "text"
)

iv_aa <- stargazer(
  # iv1, iv2,
  iv3aa, iv4aa, iv5aa,
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
    " & Score & Score & Grade & Employ I & Employ II \\\\"
  ),
  insert.after = c(13)
)

star_tex_write(tabIII, 
               file = "tex/tables/tableIII.tex")


## Reduced form for the AA sample ####

rfaa <- stargazer(
  rf2aa, rf3aa, rf4aa, rf5aa, rf1aa,
  keep = c("E_is"),
  # type = "text",
  keep.stat = c("n","rsq"),
  # column.labels = c("Maths", "Language", "Highest", "Wage", "Wage"),
  dep.var.labels = c("Maths", "Language", "Highest", "Wage", "Wage"),
  covariate.labels = "$E_{is}$",
  title = "Reduced Form Estimates for Addis Ababa Sample"
)

tabIV <- star_insert_row(rfaa,
  c(
    " & Score & Score & Grade & Employ I & Employ II \\\\"
  ),
  insert.after = c(12)
)

star_tex_write(tabIV, 
               file = "tex/tables/tableIV.tex")












