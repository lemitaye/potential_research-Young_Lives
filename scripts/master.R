########################################################################~
#
# Project: Mother Tongue Instruction, Learning and Employment (Ethiopia)
# Owners : Teferi Mergo (PhD) and Lemi Daba
#
# MASTER SCRIPT — reproduces the analysis behind the manuscript in
# ../MTI__learning_and_job_outcomes/.
#
# HOW TO RUN
#   Set the working directory to the PROJECT ROOT (the folder containing this
#   `scripts/` directory), then:  source("scripts/master.R")
#   or from a shell:              Rscript scripts/master.R
#
# All paths below are relative to the project root.
########################################################################~

# ---- 0. Preliminaries -------------------------------------------------------
rm(list = ls())

# Packages used across the pipeline. (starpolishr is a GitHub package:
#   remotes::install_github("ChandlerLutz/starpolishr")  )
library(tidyverse)     # data wrangling + ggplot
library(haven)         # read Stata .dta
library(scales)        # axis/label formatting (descriptives)
library(stargazer)     # regression tables
library(AER)           # IV / diagnostics
library(lfe)           # felm() 2SLS
library(starpolishr)   # star_panel(), star_tex_write() (export_res.R)
library(fastDummies)   # dummy_cols() (descriptives)
library(xtable)        # summary-stats table (descriptives)

theme_set(theme_light())

# NOTE ON WORKSPACE: descriptives.R and estimation.R each begin with
# rm(list = ls()); that is intentional (each reloads its data from data/*.csv).
# export_res.R has NO rm() and CONSUMES the model objects left in memory by
# estimation.R — so it must be sourced immediately after it, in this session.

# ---- 1. Build analysis data (raw .dta -> data/*.csv) ------------------------
source("scripts/extract_clean.R", echo = TRUE, max = 1000)  # -> data/aa_samp.csv, data/non_aa_samp.csv  (older cohort)
source("scripts/cleaning_yc.R",   echo = TRUE, max = 1000)  # -> data/ylsample_yc.csv, data/schsur_yl_joined.csv (younger cohort)

# ---- 2. Descriptives: Table 1 + Figure 1 ------------------------------------
source("scripts/descriptives.R",  echo = TRUE, max = 1000)  # -> output/tables/tableI.tex, output/figures/employ_gr.pdf

# ---- 3. Estimation + regression tables --------------------------------------
source("scripts/estimation.R",    echo = TRUE, max = 1000)  # fits all models into the workspace
source("scripts/export_res.R",    echo = TRUE, max = 1000)  # -> output/tables/tableII..VI.tex  (uses step-3 models)

# ---- 4. Table 3: "Likely Channels" (writes straight to the manuscript) ------
source("scripts/table_rev.R",     echo = TRUE, max = 1000)  # -> MTI__learning_and_job_outcomes/tables/table_rev.tex

# ---- Done -------------------------------------------------------------------
# Generated tables/figures land in output/. Review them, then copy the ones the
# manuscript uses into MTI__learning_and_job_outcomes/tables|figures and compile
# main.tex. See README.md for the table-number -> file mapping and the
# reproduction-status caveats (only table_rev.R is byte-verified against the
# current manuscript; export_res.R is an earlier vintage — see README).
