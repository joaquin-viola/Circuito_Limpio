# El Tobogan

datostobogan <- datos_identificados %>% filter(
  `Barrio y zona` != "Villa del Cerro"
)

# calculo de ponderadores
datostobogan1 <- datostobogan[c(1,6)]

respuestas_tobogan <- datostobogan %>% select(Manzana) %>% group_by(Manzana) %>% 
  summarise(Respuestas = n())

sum(respuestas_tobogan$Respuestas)


total_viviendas=c(NA,38,23,20,18,19,14,11,14,9,9)

respuestas_tobogan <- cbind(respuestas_tobogan, total_viviendas)


respuestas_tobogan <- respuestas_tobogan %>% mutate(
  w=total_viviendas/Respuestas
)


