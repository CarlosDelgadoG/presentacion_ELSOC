library(tidyverse)
library(gee)
library(geepack)
muestra_1 <- readRDS("DATOS/muestra_1.RDS")


bmod <- muestra_1 %>%
  select(idencuesta,s11_phq9_bin,ola_num,m0_sexo_fac,m01_fac,s_imc_l,s04_l,s12_l)%>%
  group_by(idencuesta) %>%
  mutate(lag_1= dplyr::lag(s11_phq9_bin, n = 1, default = NA))


bmod$s_imc_lc <- bmod$s_imc_l- mean(bmod$s_imc_l,na.rm = TRUE)


# TIEMPO ------------------------------------------------------------------

gee_t_fac <-gee(s11_phq9_bin~factor(ola),
                family = binomial,
                id = idencuesta,
                corstr = 'AR-M',
                data=bmod)

gee_t_num <-gee(s11_phq9_bin~ola_num,
                family = binomial,
                id = idencuesta,
                corstr = 'AR-M',
                data=bmod)


# TIEMPO Y PREDICTORES ----------------------------------------------------

gee_tn_preds <-gee(s11_phq9_bin~ola_num+m0_sexo_fac+m01_fac+s_imc_lc+s04_l+s12_l,
                   family = binomial,
                   id = idencuesta,
                   corstr = 'AR-M',
                   data=bmod)

# INTERACCIONES -----------------------------------------------------------
covariables <- c('ola_num',
                 'm0_sexo_fac',
                 'm01_fac',
                 's_imc_lc',
                 's04_l',
                 's12_l')


est_GEE <- function(var_inter=FALSE,preds,guardar=FALSE){
  
  if(var_inter){
    ecuacion <- reformulate(response ='s11_phq9_bin',
                            termlabels = c(preds,
                                           paste('ola_num',var_inter,sep=':')))}
  else{
    var_inter <- 'preds'
    #Estimar por defecto sólo los predictores
    ecuacion <- reformulate(response ='s11_phq9_bin',
                            termlabels = c(preds))
  }
  
  
  
  modelo<-   gee(formula = ecuacion,
                 family = binomial,
                 id = idencuesta,
                 corstr = 'AR-M',
                 data=bmod)  
  if(guardar){
    saveRDS(modelo, file = paste0('MODELOS/gee_inter_',var_inter,'.RDS'))
  }
  return(modelo)
}


#HACER UN FOR LOOP PARA TODOS LOS PREDICTORES
for(i in covariables){
  est_GEE(var_inter = i,preds=covariables,guardar = TRUE)
}

