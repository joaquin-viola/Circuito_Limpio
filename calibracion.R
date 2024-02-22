## calibracion

load("Personas/personas_cerro.RData")


personas_cerro %>% group_by(SEGM) %>% summarise(n_per=n())

npersonascenso = nrow(personas_cerro)

nuniverso = sum(zona1$P_TOT)

ajuste = nuniverso/npersonascenso

totales_edades = table(personas_cerro$grupo_edad)

totales_edades_ajustado = totales_edades*ajuste

totales_sexo = table(personas_cerro$SEXO)

totales_sexo_ajustado = totales_sexo*ajuste


# simulamos la edad y el sexo en las personas faltantes en el formulario
set.seed(100124)

#sexo
proporcion_sexo <- table(datoscerro$Sexo) / nrow(datoscerro)
indices_faltantes <- sample(which(is.na(datoscerro$Sexo)), sum(is.na(datoscerro$Sexo)))

# Asignar valores basados en la probabilidad proporcional
datoscerro$Sexo[indices_faltantes] <- sample(names(proporcion_sexo), 
                                             size = length(indices_faltantes), 
                                             replace = TRUE, 
                                             prob = proporcion_sexo)

# Verificar el resultado
table(datoscerro$Sexo)


#edad
proporcion_edad <- table(datoscerro$Edad) / nrow(datoscerro)
indices_faltantes2 <- sample(which(is.na(datoscerro$Edad)), sum(is.na(datoscerro$Edad)))

# Asignar valores basados en la probabilidad proporcional
datoscerro$Edad[indices_faltantes2] <- sample(names(proporcion_edad), 
                                             size = length(indices_faltantes2), 
                                             replace = TRUE, 
                                             prob = proporcion_edad)

# Verificar el resultado
table(datoscerro$Edad)


# agregamos las ponderadores por no respuesta de cada hogar

ponderadores <- muestra_zonas_villa %>% select(
  CODCOMP,
  pond_no_resp,
  pond_orig
)

ponderadores <- ponderadores %>% mutate(
  CODCOMP = as.character(CODCOMP)
)

datoscerro <- datoscerro %>% mutate(
  Manzana = as.character(Manzana)
)

datoscerro2 <- merge(datoscerro, ponderadores, by.x = "Manzana", by.y="CODCOMP")

names(totales_edades_ajustado) <- c("menos 5","5-12","13-18","19-24","25-40","41-60","mas 60")

mapeo_edades <- data.frame(
  edades_censo = names(totales_edades_ajustado),
  edades_encuesta = c("De 0 a 5 años", "De 6 a 12 años", "De 13 a 18 años", "De 19 a 24 años", "De 25 a 40 años", "De 41 a 60 años", "61 o más años")
)

datoscerro2 <- merge(datoscerro2, mapeo_edades, by.x = "Edad", by.y = "edades_encuesta")

totales_edades_ajustado = round(totales_edades_ajustado)

names(totales_sexo_ajustado) <- c("Hombre","Mujer")

totales_sexo_ajustado <- round(totales_sexo_ajustado)

# ponemos los totales en un frame con nombre de categoria Freq para poder usar calibrate

totales_edades_frame = as.data.frame(totales_edades_ajustado)

colnames(totales_edades_frame) <- c("edades_censo","Freq")


totales_sexo_frame = as.data.frame(totales_sexo_ajustado)

colnames(totales_sexo_frame) <- c("Sexo","Freq")

#creamos el diseño de la muestra separada por persona

library(survey)

dis_per <- svydesign(id=~id_hogar,
                     strata = NULL,
                     # fpc=~1/pond_no_resp,
                     weights =~ pond_no_resp,
                     data=datoscerro2)


#calibramos con la funcion rake que se tiene mas control de los totales poblacionales

rake_cluster <- rake(dis_per, sample.margins=list(~edades_censo,~Sexo),
                     population.margins=list(totales_edades_frame,totales_sexo_frame))


svytotal(~edades_censo, rake_cluster)  #ajusta mejor las edades que el calibrate

totales_edades_frame

datos_calibrados_rake <- datoscerro2 %>% mutate(w_rake = weights(rake_cluster))

datos_calibrados_rake <- datos_calibrados_rake %>% mutate(ajuste_rake = w_rake/pond_no_resp)

datos_calibrados_rake  %>% select(Manzana,id_hogar, pond_no_resp, w_rake) %>% group_by(Manzana, id_hogar) %>% 
  summarise(
    pond_no_resp = mean(pond_no_resp),
    w_rake = mean(w_rake),
    ajuste_rake = w_rake/pond_no_resp) %>% ggplot(aes(x=ajuste_rake)) + geom_histogram() + xlab("Factores de Ajuste de los hogares") + ylab("Cuantía") + theme_bw()

viviendas_calibradas <- datos_calibrados_rake %>% 
  group_by(id_hogar) %>% 
  summarise(
    pond_no_resp = mean(pond_no_resp),
    w_rake = mean(w_rake),
    ajuste_rake=w_rake/pond_no_resp)

summary(viviendas_calibradas$ajuste_rake)


sum(viviendas_calibradas$w_rake)

deffK(viviendas_calibradas$w_rake)

