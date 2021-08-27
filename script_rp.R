library(tidyverse)
library(Formula)
library(broom)
library(lfe)
library(sf)
library(knitr)
library(plm)
library(splm)
library(spatialreg)

rm(list = ls())


#### Functions ####

add_stars <- function(x){
  y <- character(length(x))
  y[x < 0.1] <- "*"
  y[x < 0.05] <- "**"
  y[x < 0.01] <- "***"
  
  return(y)
}

get_sem <- function(sform, rdata = rdata_rp, spw = obce_ww){
  spml(
    sform,
    data = rdata,
    index = c("KOD_OBEC","KOLO"),
    listw = spw,
    effect = "individual",
    model = "within",
    lag = FALSE,
    spatial.error="kkp"
  )
}

get_sem_pooling <- function(sform, rdata = rdata_rp, spw = obce_ww){
  spml(
    sform,
    data = rdata,
    index = c("KOD_OBEC","KOLO"),
    listw = spw,
    effect = "individual",
    model = "pooling",
    lag = FALSE
  )
}

get_sem_column <- function(esem){
  
  esem_summary <- summary(esem)
  
  mstat <- tibble(
    term = c("R2","Observation"),
    value = c(
      format(esem_summary$rsqr, digits = 1, nsmall = 3, scientific = FALSE, trim = TRUE),
      format(as.integer(length(esem$residuals)), trim = TRUE, big.mark = ",")
    )
  ) %>% 
    mutate(
      stat = "xstat"
    )
  
  esem_summary$CoefTable %>%
    as_tibble(rownames = "term") %>% 
    magrittr::set_names(c("term","estimate","std.err","tstat","pval")) %>% 
    select(-tstat) %>% 
    mutate(
      pval = add_stars(pval)
    ) %>% 
    rowwise() %>% 
    mutate(
      across(
        where(is.double),
        format,
        digits = 1,
        nsmall = 3,
        trim = TRUE,
        scientific = FALSE
      )
    ) %>% 
    mutate(
      estimate = str_c(estimate,pval),
      std.err = str_c("(",std.err,")")
    ) %>% 
    ungroup() %>% 
    select(-pval) %>% 
    pivot_longer(-term, names_to = "stat") %>% 
    bind_rows(.,mstat)
  
}

get_depvar <- function(modell){
  modell %>% 
    map_chr(
      function(x) as.character(x[2])
    )
}

#### Formulas ####

# Baseline (columns 1-3)
model_base <- X ~ I(KOLO == 2) + I((KOLO == 2)*100*voters_at_risk)

modell_base <- list(
  update(model_base, SSchwarzenberg ~ .),
  update(model_base, SZeman ~ .),
  update(model_base, turnout ~ .)
)

# Alternative specification (columns 4-6)
model_1st <- X ~ I(KOLO == 2) + I((KOLO == 2)*100*voters_at_risk) + I(100*voters_at_risk) + HLASY_01 + HLASY_02 + HLASY_03 + HLASY_04 + HLASY_05 + HLASY_07 + HLASY_08

modell_1st <- list(
  update(model_1st, SSchwarzenberg ~ .),
  update(model_1st, SZeman ~ .),
  update(model_1st, turnout ~ .)
)

#### Data ####

load("data_rp.RData")

## Table: rdata_rp
# KOD_OBEC...municipality ID
# KOLO...round no.
# SSchwarzenberg...support for Schwarzenberg (%)
# SZeman...support for Zeman (%)
# turnout...voter turnout (%)
# voters_at_risk...share of voters at risk
# VOL_SEZNAM...no. of eligible voters
# HLASY_*...support for other candidates in the first round

## Variable: obce_ww
# "Matrix" W from the Eq. (3)

# To create "obce_ww" you need to donwload map data ArcČR 500 in version 3.3,
# accept the license agreement, and run the following code:
# obce <- st_read("...","ObcePolygony") # Add path to unpacked data
# obce_nb <- poly2nb(obce, row.names = obce$KOD_OBEC, queen = TRUE)
# obce_ww <- nb2listw(obce_nb)

# Data are available here: https://www.arcdata.cz/produkty/geograficka-data/arccr-4-0
# Please, note, that a new version (4.0) was just released. However, we use the older one
# (3.3) that was released in 2016.

# Table 1, columns 1-3

baseline_est <- modell_base %>% map(get_sem)

baseline_est %>% 
  map(get_sem_column) %>% 
  reduce(full_join, by = c("term","stat")) %>% 
  magrittr::set_names(c("term","stat",get_depvar(modell_base))) %>% 
  select(-stat) 

# Table 1, columns 4-6

modell_1st  %>% 
  map(get_sem_pooling) %>% 
  map(get_sem_column) %>% 
  reduce(full_join, by = c("term","stat")) %>% 
  magrittr::set_names(c("term","stat",get_depvar(modell_1st))) %>% 
  select(-stat)
