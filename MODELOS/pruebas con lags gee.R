library(tidyverse)
library(geepack)

gee_LAG_m0_sexo_fac <- readRDS("MODELOS/gee_LAG_m0_sexo_fac.RDS")
gee_LAG_s12_l <- readRDS("MODELOS/gee_LAG_s12_l.RDS")
gee_LAG_m01_fac <- readRDS("MODELOS/gee_LAG_m01_fac.RDS")
gee_LAG_s_imc_lc <- readRDS("MODELOS/gee_LAG_s_imc_lc.RDS")
gee_LAG_s04_l <- readRDS("MODELOS/gee_LAG_s04_l.RDS")

gee_LAG_m0_sexo_fac%>%
  summary()

gee_LAG_s12_l%>%
  summary()

gee_LAG_m01_fac%>%
  summary()

gee_LAG_s_imc_lc%>%
  summary()

gee_LAG_s04_l%>%
  summary()

list(gee_LAG_m0_sexo_fac,gee_LAG_s12_l,gee_LAG_m01_fac,gee_LAG_s_imc_lc,gee_LAG_s04_l)%>%
  texreg::knitreg(single.row=TRUE)