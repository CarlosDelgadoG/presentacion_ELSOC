
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