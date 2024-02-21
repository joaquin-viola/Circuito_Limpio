# Guardar los datos del .sav en un rdata

library(foreign)
library(haven)

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


carpeta <- "Personas"

if (!file.exists(carpeta)) {
  dir.create(carpeta)
}

# Guardar los datos en un archivo .RData dentro de la carpeta
save(personas_cerro, file = paste0(carpeta, "/personas_cerro.RData"))
