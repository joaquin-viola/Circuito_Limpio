# Circuito_Limpio

## Resumen

En el marco de una actividad de extensión del APEX del Cerro y la Cooperativa de Reciclaje "La Paloma", junto a estudiantes de la licenciatura en estadística se realizó una encuesta en Villa del Cerro y el barrio El Tobogán sobre hábitos de reciclaje para la creación de un "Circuito Limpio".

En este trabajo se realiza el cálculo de los ponderadores de las viviendas de la muestra y se ajustan estos para lograr estimaciones de las variables de interés, en este caso si la vivienda Recicla y si Conoce a la Cooperativa de Trabajo La Paloma. Para más detalles sobre cada etapa se puede leer el archivo "Trabajo_Circuito_Limpio.PDF".

Para el correcto funcionamiento de los códigos se sugiere correrlos en el siguiente órden:

1- muestra_circuita.R
2- ponderadores.R
3- no_respuesta.R
4- EncuestaCircuitoLimpioDatos.R
5- calibracion.R (*)
6- estimaciones.R
7- Tobogan.R

(*) Este código  carga una base de datos que no está en este repositorio por pesar demasiado, pero se puede descargar libremente en el link https://www4.ine.gub.uy/Anda5/index.php/catalog/243/download/1010 se sugiere guardar el archivo "Personas.sav" en la carpeta que se esté trabajando dentro de otra carpeta llamada "Personas"
