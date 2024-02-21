# estimaciones de variables de interes

# Le agregamos la variable de id_hogar a los datos con la respuesta de las variables de interes para poder
# asignar a cada hogar calibrado una respuesta de la encuesta.

# primero filtraremos los datos de las respuesta a las variables que nos interesa junto con la variable de identificacion

datos_identificados_cerro <- datos_identificados %>% filter(`Barrio y zona` == "Villa del Cerro")

datos_identificados_cerro <- datos_identificados_cerro[c(1,6,30,40)]

colnames(datos_identificados_cerro) <- c("id_hogar","Manzana","Recicla","ConoceLaPaloma")


datos_identificados_cerro <- datos_identificados_cerro %>% mutate(
  Recicla = as.factor(Recicla),
  ConoceLaPaloma = as.factor(ConoceLaPaloma)
)


levels(datos_identificados_cerro$Recicla)
levels(datos_identificados_cerro$ConoceLaPaloma)

# hay que cambiar los nombres de las categorias, mergear con los hogares calibrados y estimar

datos_identificados_cerro <- datos_identificados_cerro %>% mutate(
Recicla = recode_factor(Recicla,
                        "Sí, siempre" = "Si",
                        "A veces" = "Si",
                        "No, nunca (pasar a la pregunta 20)" = "No"),
ConoceLaPaloma = recode_factor(ConoceLaPaloma,
                               "Sí, la conozco bastante" = "Si",
                               "Sí, pero tengo poco conocimiento de la cooperativa" = "Si",
                               "No, nunca escuche hablar de la cooperativa" = "No")
)



viviendas_calibradas <- merge(viviendas_calibradas, datos_identificados_cerro, by="id_hogar")

info_post_estrato <- muestra_zonas_villa %>% select(CODCOMP, post_estrato)

viviendas_calibradas <- merge(viviendas_calibradas, info_post_estrato, by.x="Manzana", by.y = "CODCOMP")



# disenio sin post-estrato
disenio_viviendas = svydesign(ids =~ Manzana,
                               weights =~ w_rake,
                               data = viviendas_calibradas)




svytotal(~Recicla, disenio_viviendas,deff=TRUE)

svytotal(~ConoceLaPaloma, disenio_viviendas,deff=TRUE)

svymean(~Recicla, disenio_viviendas,deff=TRUE)

svymean(~ConoceLaPaloma, disenio_viviendas,deff=TRUE)

# estimacion por post-estrato

svyby(formula = ~ Recicla,
      by = ~ post_estrato,
      FUN = svymean,
      na.rm = TRUE,
      design = disenio_viviendas,
      vartype = c("se"))

svyby(formula = ~ ConoceLaPaloma,
      by = ~ post_estrato,
      FUN = svymean,
      na.rm = TRUE,
      design = disenio_viviendas,
      vartype = c("se"))

