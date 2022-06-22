library(tidyverse)
library(gee)

gee_inter_m0_sexo_fac <- readRDS("MODELOS/gee_inter_m0_sexo_fac.RDS")
gee_inter_m01_fac <- readRDS("MODELOS/gee_inter_m01_fac.RDS")
gee_inter_s_imc_lc <- readRDS("MODELOS/gee_inter_s_imc_lc.RDS")
gee_inter_s04_l <- readRDS("MODELOS/gee_inter_s04_l.RDS")
gee_inter_s12_l <- readRDS("MODELOS/gee_inter_s12_l.RDS")




gee_inter_m0_sexo_fac%>%
  summary()

  gee_inter_m01_fac%>%
    summary()

gee_inter_s04_l%>%
  summary()

gee_inter_s12_l%>%
  summary()

gee_inter_s_imc_lc%>%
  summary()