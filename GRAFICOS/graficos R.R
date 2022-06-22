library(plotly)
s10_ola<-select(muestra_1,idencuesta,ola_num,s11_phq9_bin)%>%
  pivot_wider(id_cols = idencuesta,names_from=ola_num,values_from = s11_phq9_bin)%>%
  drop_na()

cors<-round(sirt::tetrachoric2(select(s10_ola,-idencuesta))$rho,2)

cor_olas <-plot_ly(
                x = c("primera", "segunda", "tercera","cuarta"), 
                y = c("primera", "segunda", "tercera","cuarta"),
                z = cors, 
                type = "heatmap")

htmlwidgets::saveWidget(cor_olas, "GRAFICOS/cor_olas.html", selfcontained = F, libdir = "lib")



# Estimacion betas modelo aditivo -----------------------------------------
gee_inter_preds <- readRDS("MODELOS/gee_inter_preds.RDS")

covs_names<-c('Intercepto','Tiempo','Mujer',
              'Educ: Media','Educ: Técnica','Educ:Universitaria',
              'IMC','AF: Intenso','AF: Leve',
              'Apoyo: Algunas veces', 'Apoyo: Siempre o Casi siempre')


datos <-tibble('vars'=factor(covs_names,
                             levels = covs_names),
               'beta'=round(as.numeric(coefficients(gee_inter_preds)),2),
               'sd'=round(as.numeric(sqrt(diag(gee_inter_preds$robust.variance))),2))%>%
  mutate('es'=round(qnorm(.99)*sd,2),
         'sig'= factor(ifelse(abs(beta/sd) >qnorm(.99),'Significativo','No significativo')))

gf_betas_aditivo<-plotly::plot_ly(filter(datos, sig=='Significativo'),x=~vars,y=~beta,
                type = 'scatter',
                mode='markers',
                name = 'Significativo',
                error_y = ~list(array = es),
                hoverinfo='text',
                text=~paste(vars,'<br>',
                            'Beta:',beta,'<br>',
                            'Error:',es,'<br>',
                            'Chances:', round(exp(beta),2)))%>%
  plotly::add_trace(data=filter(datos, sig=='No significativo'), name='No significativo',color='red')%>%
  layout(xaxis=list(title='Covariables'),
         yaxis=list(title='Coeficiente Estimado'))

htmlwidgets::saveWidget(gf_betas_aditivo, "GRAFICOS/gf_betas_aditivo.html", selfcontained = F, libdir = "lib")



# GRAFICO INTERACCIONES ---------------------------------------------------


#FUNCION PARA EXTRAER INTERACCIONES
get_inter <-function(modelo){
  
  coefs <- coefficients(modelo)
  index <- grepl(x=names(coefs),pattern=':')
  se <-sqrt(diag(modelo$robust.variance))
  
  
  tabla<-  tibble('var'=gsub(x=names(coefs[index]),
                             pattern = 'ola_num:', 
                             replacement = ''),
                  'beta'=coefs[index],
                  'se'=se[index])
  return(tabla)
}



#CARGAR MODELOS
gee_inter_m0_sexo_fac <- readRDS("MODELOS/gee_inter_m0_sexo_fac.RDS")
gee_inter_m01_fac <- readRDS("MODELOS/gee_inter_m01_fac.RDS")
gee_inter_s_imc_lc <- readRDS("MODELOS/gee_inter_s_imc_lc.RDS")
gee_inter_s04_l <- readRDS("MODELOS/gee_inter_s04_l.RDS")
gee_inter_s12_l <- readRDS("MODELOS/gee_inter_s12_l.RDS")

# LISTAR LOS MODELOS
patron <-grep("gee_inter",names(.GlobalEnv),value=TRUE)
lista_mods <-do.call("list",mget(patron))
#EXTRAER INTERACCIONES DE CADA MODELO
tabla_inter <-lapply(1:length(lista_mods), function(i){get_inter(lista_mods[[i]])})%>%
  bind_rows()
#PASARLOS A TABLA Y CALCULAR SIGNIFICANCIA
tabla_inter <-tabla_inter%>%
  mutate(error=se*qnorm(0.9),
         sig=ifelse(abs(beta/se)> qnorm(0.9),'Significativo','No Significativo'))%>%
  mutate(var_names=c('Mujer','AF: Intensa','AF: Suave','Apoyo: Algunas','Apoyo: Siempre','IMC',
                     'Educ: Media','Educ: Técnica','Educ: Universitaria'))
#GRAFICAS
gf_inter <- plotly::plot_ly(filter(tabla_inter, sig=='Significativo'),x=~var_names,y=~beta,
                            type = 'scatter',
                            mode='markers',
                            name = 'Significativo',
                            error_y = ~list(array = error),
                            hoverinfo='text',
                            text=~paste(var_names,'<br>',
                                        'Beta:',round(beta,2),'<br>',
                                        'Error:',round(error,2),'<br>',
                                        'Chances:', round(exp(beta),2)))%>%
  plotly::add_trace(data=filter(tabla_inter, sig=='No Significativo'), name='No significativo',color='red')%>%
  plotly::layout(xaxis=list(title='Covariables'),
                 yaxis=list(title='Coeficiente Estimado'))

htmlwidgets::saveWidget(gf_inter, "GRAFICOS/gf_inter.html", selfcontained = F, libdir = "lib")
