library(sf)
library(haven)
library(sampling)
library(tmap)

zona1=st_read("villadelcerro.shp")
plot(st_geometry(zona1))


hist(zona1$V_TOT)

n=30
set.seed(756350)

pik=inclusionprobabilities(zona1$V_PAR,n)
s=UPsystematic(pik)
m=zona1[s==1,]

tmap_mode("view")
tmap_options(check.and.fix = TRUE)

tm_shape(m)+
  tm_polygons(id="V_PAR",alpha = 0.5)


st_write(m,"muestra_circuito_limpio.shp")




