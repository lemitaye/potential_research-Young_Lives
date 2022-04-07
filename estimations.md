Preliminary Estimations
================
Lemi Daba
4/7/2022

Preliminaries:

``` r
rm(list = ls())

# packages
library(tidyverse)
library(haven)
library(scales)
library(stargazer)
library(AER)

theme_set(theme_light())
```

Need to run data cleaning first

``` r
source("scripts/extract_clean.R")
```

## OLS and IV on all sample

Start by estimating a simple model for both OLS and IV

First stage regression (no covariates):

``` r
sfrst <- lm(IMTI ~ E_is, data = joined)
stargazer(sfrst, type = "text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                IMTI            
    ## -----------------------------------------------
    ## E_is                         2.869***          
    ##                               (0.183)          
    ##                                                
    ## Constant                     3.303***          
    ##                               (0.130)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                    989            
    ## R2                             0.199           
    ## Adjusted R2                    0.198           
    ## Residual Std. Error      2.407 (df = 987)      
    ## F Statistic          245.460*** (df = 1; 987)  
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

2SLS on the whole sample:

``` r
smod_ols <- lm(wage_employ ~ IMTI, data = joined)
smod_iv <- ivreg(wage_employ ~ IMTI | E_is, data = joined)
stargazer(smod_ols, smod_iv, type = "text")
```

    ## 
    ## =================================================================
    ##                                       Dependent variable:        
    ##                                ----------------------------------
    ##                                           wage_employ            
    ##                                         OLS          instrumental
    ##                                                        variable  
    ##                                         (1)              (2)     
    ## -----------------------------------------------------------------
    ## IMTI                                  0.014**          -0.026*   
    ##                                       (0.006)          (0.014)   
    ##                                                                  
    ## Constant                             0.210***          0.410***  
    ##                                       (0.034)          (0.073)   
    ##                                                                  
    ## -----------------------------------------------------------------
    ## Observations                            813              813     
    ## R2                                     0.006            -0.047   
    ## Adjusted R2                            0.005            -0.048   
    ## Residual Std. Error (df = 811)         0.447            0.459    
    ## F Statistic                    5.013** (df = 1; 811)             
    ## =================================================================
    ## Note:                                 *p<0.1; **p<0.05; ***p<0.01
