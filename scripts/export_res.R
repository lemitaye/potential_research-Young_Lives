
# Date created: April 30, 2022
# Depends on: extract_clean.R; estimation.R

# A script to send results to .tex files

stargazer(
  # iv1, iv2,
  iv3, iv4, iv5,
  iv6, iv1,
  keep = c("IMTI"),
  keep.stat = c("n","rsq"),
  type = "text"
)