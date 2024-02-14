## calibracion
library(foreign)

# se leen los datos del censo
personas_censo <- read_sav("Personas/Personas.sav")

# debemos ver los segmentos que forman nuestro universo elegible dentro de la seccion para filtrar
# a las personas pertenecientes a esa seccion

zona1frame <- as.data.frame(zona1)

segmentos <- unique(zona1frame$SEGMENTO)

censo_frame = as.data.frame(personas_censo)

personas_cerro <- censo_frame  %>% mutate(
  SEGM = as.numeric(SEGM)
)%>% filter(
  DPTO == "01",
  SECC == "13",
  SEGM %in% c(segmentos)
)

# tenemos los datos de todas las personas de nuestro universo de elegibles
# seleccionamos las variables de interes

personas_cerro <- personas_cerro %>% select(
  SECC,
  SEGM,
  VIVID,
  PERPH02,
  PERNA01
)

colnames(personas_cerro) <- c("SECC","SEGM","VIV","SEXO","EDAD")

personas_cerro <- personas_cerro %>% mutate(
  grupo_edad = cut(EDAD, breaks = c(-Inf,5,12,18,24,40,60,Inf))
)





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

dis_per <- svydesign(id =~ id_hogar,
                     strata = NULL, #no deja poner estrato =~ Manzana
                     # fpc=~1/pond_orig,
                     weights =~ pond_no_resp,
                     data=datoscerro2)

CONTEOS_POBL = c(nuniverso,
                 totales_edades_frame$Freq[-1],
                 totales_sexo_frame$Freq[-1])

rak_ind <- calibrate(design=dis_per,
                 formula =~ Edad + Sexo,
                 population = CONTEOS_POBL,
                 calfun = "raking")

rak_viv <- calibrate(design=dis_per,
                 formula =~ Edad + Sexo,
                 population = CONTEOS_POBL,
                 calfun = "raking",
                 aggregate.stage = 1)


datos_calibrados <- datoscerro2 %>% mutate(w_cali=weights(rak_viv))

datos_calibrados <- datos_calibrados %>% mutate(factores_ajuste = (pond_no_resp)/(w_cali))

datos_calibrados %>% group_by(id_hogar) %>% select(id_hogar, factores_ajuste) %>% ggplot(aes(x=factores_ajuste)) + geom_histogram()

# los factores de ajustes son muy grandes, vamos a truncarlos!

rak_viv2 <- calibrate(design=dis_per,
                     formula =~ Edad + Sexo,
                     population = CONTEOS_POBL,
                     calfun = "raking",
                     bounds = c(0.2, 4),
                     aggregate.stage = 1,
                     epsilon = 1)

datos_calibrados <- datos_calibrados %>% mutate(w_cali2=weights(rak_viv2))

datos_calibrados <- datos_calibrados %>% mutate(factores_ajuste2 = (pond_no_resp)/(w_cali2))

datos_calibrados %>% group_by(id_hogar) %>% select(id_hogar, factores_ajuste2) %>% ggplot(aes(x=factores_ajuste2)) + geom_histogram()

viviendas_calibradas <- datos_calibrados %>% select(Manzana,id_hogar, pond_no_resp, `Barrio y zona` ,w_cali,w_cali2) %>% group_by(id_hogar)

svytotal(~edades_censo,rak_viv2)
totales_edades_ajustado
svytotal(~edades_censo, rak_viv)

#probando con la funcion rake que se tiene mas control de los totales poblacionales

rake_cluster <- rake(dis_per, sample.margins=list(~edades_censo,~Sexo),
                     population.margins=list(totales_edades_frame,totales_sexo_frame))

svytotal(~edades_censo, rake_cluster)  #ajusta mejor las edades que el calibrate
totales_edades_frame

datos_calibrados_rake <- datos_calibrados %>% mutate(w_rake = weights(rake_cluster))

datos_calibrados_rake <- datos_calibrados_rake %>% mutate(ajuste_rake = w_rake/pond_no_resp)

datos_calibrados_rake %>% group_by(id_hogar) %>% select(id_hogar, ajuste_rake) %>% 
  ggplot(aes(x=ajuste_rake)) + geom_histogram() + xlab("Factores de Ajuste de los hogares") + ylab("Cuantía") + theme_bw()

viviendas_calibradas <- datos_calibrados_rake %>% 
  group_by(Manzana,id_hogar) %>% 
  summarise(
    pond_no_resp = mean(pond_no_resp),
    w_cali = mean(w_cali),
    factor_ajuste = mean(factores_ajuste),
    w_cali2 = mean(w_cali2),
    factor_ajuste2 = mean(factores_ajuste2),
    w_rake = mean(w_rake),
    ajuste_rake = mean(ajuste_rake)
  )

sum(viviendas_calibradas$w_rake)
