# Ponderaciones:

# En primer lugar calcularemos los ponderadores iniciales de los hogares.
# Que se calculan como la el inverso de la probabilidad de salir en la muestra
# dado que la manzana quedó en la muestra, por el inverso de la probabilidad de
# que la manzana esté en la muestra.

# la probabilidad de inclusión de las manzanas están calculadas en el objeto pik
# la probabilidad de inclusión de los hogares será n/N dado que su manzana salió en la muestra


library(tidyverse)

source("muestra_circuito.R")

villa <- as.data.frame(zona1)

villa <- villa %>% select(
  c(CODCOMP_A ,CODCOMP, V_TOT, V_PAR, P_TOT, P_TOT_HOM, P_TOT_MUJ))

villa <- cbind(villa, pik,s)  


muestra_zonas_villa <- villa %>% filter(
  s==1
)


# muestra de zonas
muestra_zonas_villa <-  muestra_zonas_villa %>% mutate(
  pil2 = 6/V_PAR,  #probabilidad de inclusion de los hogares
  pond_orig = (1/pik)*(1/pil2)
  )



# totales para calibrar
totales_sexo_pob <- villa %>% summarize(
  tot_pob_H = sum(P_TOT_HOM),
  tot_pob_M = sum(P_TOT_MUJ)
)  #la suma de hombres y mujeres ponderados en la muestra debe dar esto


tot_hogares = sum(villa$V_PAR)
sum_pond_hogares=sum(muestra_zonas_villa$pond_orig)*6





