########################################################################~
# 
# Project Title: " ..."
# 
# Project Owners: Teferi Mergo (PhD) and Lemi Daba
# R Script "master"
# Created on: March 25, 2022
########################################################################~

# Some preparations: ####

rm(list = ls())

# packages
library(tidyverse)
library(haven)
library(scales)
library(stargazer)
library(AER)
library(lfe)
library(starpolishr)
library(fastDummies)
library(xtable)

theme_set(theme_light())


# Call R scripts ####
source("scripts/extract_clean.R", echo=TRUE, max=1000) # Data import and cleaning
# source("scripts/descriptives.R",echo=TRUE,max=1000) # Descriptive statistics
# source("scripts/estimation.R"  ,echo=TRUE,max=1000) # Estimation of model 
# source("scripts/results.R"     ,echo=TRUE,max=1000) # Tables and Figures
