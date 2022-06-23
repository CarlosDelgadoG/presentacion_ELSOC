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

gee_t_fac <-geeglm(s11_phq9_bin~factor(ola),
                family = binomial,
                id = idencuesta,
                corstr = 'ar1',
                data=na.omit(select(muestra_1, idencuesta,ola,s11_phq9_bin)))

saveRDS(gee_t_fac, file = 'MODELOS/gee_t_fac.RDS')

gee_t_num <-geeglm(s11_phq9_bin~ola_num,
                   family = binomial,
                   id = idencuesta,
                   corstr = 'ar1',
                   data=na.omit(select(muestra_1, idencuesta,ola_num,s11_phq9_bin)))
saveRDS(gee_t_num, file = 'MODELOS/gee_t_num.RDS')

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

bmod <- muestra_1 %>%
  select(idencuesta,s11_phq9_bin,ola_num,m0_sexo_fac,m01_fac,s_imc_l,s04_l,s12_l)%>%
  group_by(idencuesta) %>%
  mutate(lag_1= dplyr::lag(s11_phq9_bin, n = 1, default = NA))
bmod$s_imc_lc <- bmod$s_imc_l- mean(bmod$s_imc_l,na.rm = TRUE)


est_GEE <- function(var_inter=FALSE,preds,guardar=FALSE,lag=FALSE){
  
  if(is.character(var_inter)){
    if(lag){
      ecuacion <- reformulate(response ='s11_phq9_bin',
                              termlabels = c(preds,
                                             'lag_1',
                                             paste('lag_1',var_inter,sep=':')))
    }else{
      ecuacion <- reformulate(response ='s11_phq9_bin',
                              termlabels = c(preds,
                                             paste('ola_num',var_inter,sep=':')))}  
  }
  
  else{
    var_inter <- 'preds'
    #Estimar por defecto sólo los predictores
    ecuacion <- reformulate(response ='s11_phq9_bin',
                            termlabels = c(preds))
  }
  
  if(lag){
    modelo  <-geepack::geeglm(ecuacion,
                              family = binomial,
                              id = idencuesta,
                              corstr = 'ar1',
                              data=na.omit(bmod))
  }else{
    modelo<-   gee(formula = ecuacion,
                   family = binomial,
                   id = idencuesta,
                   corstr = 'AR-M',
                   data=bmod)    
  }
  
  
  if(guardar){
    if(lag){
      saveRDS(modelo, file = paste0('MODELOS/gee_LAG_',var_inter,'.RDS'))
    }else{
      saveRDS(modelo, file = paste0('MODELOS/gee_inter_',var_inter,'.RDS'))  
    }
    
  }
  return(modelo)
}


#HACER UN FOR LOOP PARA TODOS LOS PREDICTORES
for(i in covariables){
  est_GEE(var_inter = i,preds=covariables,guardar = TRUE)
}


#HACER UN FOR LOOP PARA TODOS LOS PREDICTORES CON LAGS
for(i in covariables){
  est_GEE(var_inter = i,preds=covariables,guardar = TRUE,lag = TRUE)
}
# TABLAS ------------------------------------------------------------------

gee_inter_preds%>%
  texreg::htmlreg(single.row=TRUE,
                  custom.model.name='Modelo Aditivo',
                  custom.coef.names=c('Intercepto','Tiempo','Mujer',
                                      'Educ: Media','Educ: Técnica','Educ:Universitaria',
                                      'IMC','AF: Intenso','AF: Leve',
                                      'Apoyo: Algunas veces', 'Apoyo: Siempre o Casi siempre'))

