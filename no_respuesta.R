library(tidyverse)
library(readxl)
library(RColorBrewer)


source("ponderadores.R")

no_respondentes<-read_xlsx("Copia de Circuito Limpio Direcciones Villa del Cerro.xlsx")


## Para tener una sola variable con respuestas
no_respondentes <- no_respondentes %>%
  rowwise() %>%
  mutate(Respuesta = if ("H" %in% c(estado1, estado2, estado3)) 1 else 0) %>%
  ungroup()


tabla_respuestas <- no_respondentes %>% group_by(codcomp) %>% summarise(
  respuestas = sum(Respuesta)
) 

no_respondentes %>% group_by(codcomp) %>% summarise(
  respuestas = sum(Respuesta)
) %>% ggplot(aes(respuestas)) + geom_histogram()


tabla_respuestas <- tabla_respuestas %>% rename(
 CODCOMP= codcomp
)

# para que matchee manzanas que no matcehaban
tabla_respuestas$CODCOMP <- round(tabla_respuestas$CODCOMP, digits = 0)
m$CODCOMP <- round(m$CODCOMP, digits = 0)

mapR <- merge(m,tabla_respuestas, by="CODCOMP")

mapR$respfact <- as.factor(mapR$respuestas)


# mapeo de respustas
tmap_mode("view")
#tmap_options(check.and.fix = TRUE)


tm_shape(mapR)+
  tm_polygons(id="V_PAR",alpha = 0.5, col="respfact")

colores_dark2 <- brewer.pal(8, "Dark2")

tm_shape(mapR) +
  tm_borders("black") +  # Bordes en negro
  tm_fill(col = "respfact", alpha = 0.5, palette = "Reds") +
  tm_layout(legend.position = c("left", "bottom"))


# Viendo el mapa coloreado por cantidad de respuesta en cada zona sorteada haremos 3
# estratos para hacer el mismo ponderador en cada uno de esos estratos.

# Decidimos cortar el primer estrat las zonas que estan al norte de la calle Belgica
# el segundo estrato las que estan entre Belgica y Juan V. Viacaba, y el tercero las
# zonas que estan por debajo de Juan B. Viacaba
# en el primer estrato quedaran 11 manzanas
# en el segundo estrato quedaran 11 manzanas
# en el tercero quedaran las restantes 8 manzanas.


unique(mapR$CODCOMP)

post_estratoR <- c(rep(1,11),rep(2,11),rep(3,8))

CODCOMP_ord_NR <- c(113029002,113030001,113030601,113029008,113030004,113030007,113029010,113028004,113028007,113028002,113028600,113021017,113020003,113018011,113019001,113019006,113019003,113019008,113018014,113005001,113004002,113005009,113005003,113004004,113005012,113005005,113005006,113001001,113001003,113001006)

post_estratRESP <- as.data.frame(cbind(post_estratoR,CODCOMP_ord_NR))

colnames(post_estratRESP) <- c("post_estrato","CODCOMP")

mapR <- left_join(mapR,post_estratRESP,by="CODCOMP")

# calculamos la propension de respuestas en cada uno de estos estratos, esta propension será nuestra probabilidad
# estimada de responder y la usaremos para el ajuste por no respuesta de nuestros ponderadores.


prop_resp <- as.data.frame(mapR) %>% group_by(post_estrato) %>% 
  summarise(
    prob_resp = sum(respuestas)/(6*n())
  )

mapR <- left_join(mapR,prop_resp, by="post_estrato")


#agregamos el postestrato al objeto donde tenemos los ponderadores originales
muestra_zonas_villa$CODCOMP = round(muestra_zonas_villa$CODCOMP,0)


muestra_zonas_villa <- left_join(muestra_zonas_villa,post_estratRESP,by="CODCOMP")

muestra_zonas_villa <- left_join(muestra_zonas_villa,prop_resp, by="post_estrato")



muestra_zonas_villa <- muestra_zonas_villa %>% mutate(
  pond_no_resp = pond_orig*(1/prob_resp)
)



