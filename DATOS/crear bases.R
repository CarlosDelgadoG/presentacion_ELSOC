library(tidyverse)
muestra_1 <- readRDS("DATOS/muestra_1.RDS")


bm_desc <- muestra_1 %>%
           select(idencuesta,s11_phq9,s11_phq9_bin,ola,m0_sexo_fac,m01_fac,s_imc_fac,s04_fac,s12_fac,ponderador02)
write_csv(bm_desc, file = 'DATOS/bm_desc.csv',)

muestra_1 %>%
  select(idencuesta,s11_phq9_bin,ola_num,m0_sexo_fac,m01_fac,s_imc_l,s04_l,s12_l,ponderador02)%>%
  group_by(idencuesta) %>%
  mutate(lag_1= dplyr::lag(s11_phq9_bin, n = 1, default = NA))


bm_desc%>%
  group_by(ola,m0_sexo_fac)%>%
  summarise(w_avg=weighted.mean(s11_phq9_bin,ponderador02,na.rm=TRUE),
            avg=mean(s11_phq9_bin,na.rm=TRUE))