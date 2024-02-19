# estimaciones de variables de interes

# Le agregamos la variable de id_hogar a los datos con la respuesta de las variables de interes para poder
# asignar a cada hogar calibrado una respuesta de la encuesta.

# primero filtraremos los datos de las respuesta a las variables que nos interesa junto con la variable de identificacion

datos_identificados <- datos_identificados[c(1,30,40)]

colnames(datos_identificados) <- c("id_hogar","Recicla","ConoceLaPaloma")


datos_identificados <- datos_identificados %>% mutate(
  Recicla = as.factor(Recicla),
  ConoceLaPaloma = as.factor(ConoceLaPaloma)
)


levels(datos_identificados$Recicla)
levels(datos_identificados$ConoceLaPaloma)

# hay que cambiar los nombres de las categorias, mergear con los hogares calibrados y estimar


