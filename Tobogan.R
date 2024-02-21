# El Tobogan

datostobogan <- datos_identificados %>% filter(
  `Barrio y zona` != "Villa del Cerro"
)

# calculo de ponderadores
datostobogan1 <- datostobogan[c(1,6)]

respuestas_tobogan <- datostobogan %>% select(Manzana) %>% group_by(Manzana) %>% 
  summarise(Respuestas = n())

sum(respuestas_tobogan$Respuestas)


total_viviendas=c(38,23,20,18,19,14,11,14,9,9)
sum(total_viviendas)

respuestas_tobogan <- cbind(respuestas_tobogan, total_viviendas)


respuestas_tobogan <- respuestas_tobogan %>% mutate(
  w=total_viviendas/Respuestas
)

datostobogan1 <- datostobogan1 %>% left_join(respuestas_tobogan[c(1,4)], by="Manzana")



datostobogan_id = datostobogan[c(1,6,30,40)]

colnames(datostobogan_id) <- c("id_hogar","Manzana","Recicla","ConoceLaPaloma")


datostobogan_id <- datostobogan_id %>% mutate(
  Recicla = as.factor(Recicla),
  ConoceLaPaloma = as.factor(ConoceLaPaloma)
)


levels(datostobogan_id$Recicla)
levels(datostobogan_id$ConoceLaPaloma)

# hay que cambiar los nombres de las categorias, mergear con los hogares calibrados y estimar

datostobogan_id <- datostobogan_id %>% mutate(
  Recicla = recode_factor(Recicla,
                          "Sí, siempre" = "Si",
                          "A veces" = "Si",
                          "No, nunca (pasar a la pregunta 20)" = "No",
                          "No sabe/ No contesta (pasar a la pregunta 21)" = "NS/NC"),
  ConoceLaPaloma = recode_factor(ConoceLaPaloma,
                                 "Sí, la conozco bastante" = "Si",
                                 "Sí, pero tengo poco conocimiento de la cooperativa" = "Si",
                                 "No, nunca escuche hablar de la cooperativa" = "No")
)

datostobogan_id <- merge(datostobogan_id, datostobogan1[c(1,3)], by="id_hogar")


disenio_tobogan <- svydesign(ids =~ NULL,
                             strata =~ Manzana,
#                             fpc =~ 1/w,
                             weights =~ w,
                             data = datostobogan_id)

sum(datostobogan_id$w)
svymean(~Recicla,disenio_tobogan, deff=TRUE)
svymean(~ConoceLaPaloma, disenio_tobogan, deff=TRUE)

svytotal(~Recicla,disenio_tobogan, deff=TRUE)
svytotal(~ConoceLaPaloma, disenio_tobogan, deff=TRUE)
