# global.R ---------------------------------------------------------------------
# Description: Este script carga toda la información y paquetes necesarios
# para ejecutar el dashboard.
# Created by -------------------------------------------------------------------
# Name: CIM Data Team
# Created on: 2024-01-29
# Editorial --------------------------------------------------------------------
# Section for editorial changes or notes
# ______________________________________________________________________________

# Requerimientos ---------------------------------------------------------------
source("requirements.R")

pacman::p_load(plotly)

source("local_settings.R")

con <- DBI::dbConnect(RPostgres::Postgres(),
                      host = host,
                      dbname = dbname,
                      user = user,
                      password = password,
                      port = 5432) #Tenerle fe al número



dbListTables(con)

# Justificación -----------------------------------------------------------

rnve_online <- tbl(con, "rnve")
registro_civil_online <- tbl(con,"registro-civil")

#glimpse(rnve_online)
#count(rnve_online)
#colnames(rnve_online)

# Calcular la cobertura:

dosis <- rnve_online %>% 
  # Calculemos el año de cada evento de vacunación
  mutate(ano = lubridate::year(fecha_vac)) %>% 
  # Agrupemos por año y dosis
  group_by(ano, dosis) %>% 
  # Calculemos cuantas vacunas fueron aplicadas para cada año y dosis
  # NOTA: tally() es lo mismo que summarise(n = n()); o sea, contar cuántas
  #       filas hay en cada grupo
  # NOTA: Con el argumento name, cambiamos el nombre de la columna. En lugar
  #       de llamarse n, se llamará total_dosis.
  tally(name = "total_dosis") %>% 
  mutate(total_dosis = as.numeric(total_dosis))


pob <- registro_civil_online %>% 
  # Obtenemos los años de cada fecha de nacimiento
  mutate(ano = lubridate::year(fecha_nac)) %>% 
  # Para cada año, calculamos cuántas filas hay (o sea, cuántos nacimientos)
  group_by(ano) %>% 
  tally() %>%
  mutate(ano = ano + 1)%>%
  mutate(n = as.numeric(n))

cobertura <- dosis %>% 
  left_join(., pob, by = c("ano")) %>% #El ".," es para indicar a pipe que ahí irá la dosis
  mutate(cobertura = total_dosis / n  * 100)

#head(dosis)


susceptibles <- cobertura %>% 
  # Nos interesa obtener el número de susceptibles de los últimos 5 años. Este
  # número será importante, pues nos indica cuándo debemos implementar
  # una campaña de vacunación. Filtremos los años deseados.
  filter(ano <= 2023) %>% 
  # Utilizamos solo las primeras dosis
  filter(dosis == "Primera") %>% 
  # Definimos la falla primaria de la vacuna
  mutate(falla_primaria = 0.05) %>% 
  # Calculamos la cantidad de niños no vacunados ("n" es la población)
  mutate(no_vacunados = n - total_dosis) %>% 
  # Agregamos la cantidad de niños vacunados pero no inmunizados
  mutate(susceptibles = no_vacunados + (total_dosis * falla_primaria)) %>% 
  # Calculamos el acumulado de susceptibles conforme los años pasan
  #   1.  Usamos ungroup() en caso haya alguna agrupación previa que hayamos
  #       ingresado
  #   2.  Ordenamos la tabla de acuerdo al año, en orden ascendente
  #   3.  Calculamos el acumulado por medio de cumsum
  ungroup() %>% 
  #arrange(ano) %>% #ordenar la tabla según el año. MARCA ERROR Y USAREMOS WINDOW_ORDER()
  
  dbplyr::window_order(ano)%>% 
  
  mutate(susceptibles_acumulado = cumsum(susceptibles)) %>% 
  collect()

### Gráfica --------------------------------------------------------------------
# Visualicemos los datos que tenemos de manera gráfica.
ggplot(susceptibles, aes(x = ano)) +
  # Nombres de ejes
  labs(
    title = "Susceptibles acumulados en los últimos 5 años",
    x = "Año",
    y = "Susceptibles Acumulados"
  ) +
  # Cobertura en barras
  geom_bar(aes(y = cobertura * 400), position = "dodge", stat = "identity", fill = "#094775") +
  # Susceptibles acumulados en lineas
  geom_line(aes(y = susceptibles_acumulado), colour = "#ff671f", linewidth = 1) +
  # Ajustamos los dos ejes verticales
  scale_y_continuous(
    # Las dosis alcanzan cerca de 40 mil
    limits = c(0, 40e3),
    # Agregamos un segundo eje horizontal para cobertura (con el mismo factor
    # de conversion que en geom_bar)
    # NOTA: Aplicamos un factor de conversión de 400 para que el eje de 
    #       cobertura alcance 100% cuando el número de dosis alcance 40,000
    #       dosis.
    sec.axis = sec_axis( trans= ~./400, name = "Cobertura (%)")
  ) +
  # Ajuste de eje X
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  # Ajustes visuales
  theme_classic() +
  theme(text = element_text(size = 16))


# Datos para la tabla ya por municipio ------------------------------------


## Dosis Muni --------------------------------------------------------------


vacunados_rnve_muni <- rnve_online %>% 
  # Queremos a todos aquellos que tienen una primera dosis de la vacuna
  filter(dosis == "Primera") %>% 
  # Calculamos el año de la fecha de vacunación
  mutate(ano = lubridate::year(fecha_vac)) %>% 
  # Y agrupamos por año de vacunación y municipio de residencia de la madre
  # NOTA: En este caso, agrupamos por municipio porque quisiéramos calcular
  #       la cantidad de susceptibles a nivel municipal (ADM2). Esto facilitará
  #       la planificación y el seguimiento de la campaña, para priorizar
  #       municipios que lo requieran (por ejemplo).
  group_by(ano, municipio_res_mad) %>% 
  # Con los grupos hechos, calculamos cuántos niños/as han sido vacunados con
  # una primera dosis
  tally(name = "vacunados_primera")


# Población por municipio -------------------------------------------------


pob_muni <- registro_civil_online %>% 
  # Obtenemos los años de cada fecha de nacimiento
  mutate(ano = lubridate::year(fecha_nac)) %>% 
  # Para cada año, calculamos cuántas filas hay (o sea, cuántos nacimientos)
  group_by(ano, municipio_res_mad) %>% 
  tally(name = "poblacion") %>% 
  # Agregamos 1 a los años. Esto, porque queremos utilizar la población de 
  # un año previo para calcular la cobertura del año actual. En otras palabras,
  # la población del año 2018 será la población objetivo para los vacunados
  # en el año 2019.
  mutate(ano = ano + 1)

cobertura_muni <- vacunados_rnve_muni %>% 
  # Quisiéramos unir ambas bases a través del año y el municipio.
  left_join(., pob_muni, by = c("ano", "municipio_res_mad")) %>% 
  # Calculamos cobertura
  mutate(cobertura = as.numeric(vacunados_primera) / as.numeric(poblacion)  * 100) %>% 
  mutate(cobertura = round(cobertura, 2))

susceptibles_muni <- cobertura_muni %>% 
  # Nos interesa obtener el número de susceptibles de los últimos 5 años. Este
  # número será importante, pues nos indica cuándo debemos implementar
  # una campaña de vacunación. Filtremos los años deseados.
  filter(ano <= 2023) %>% 
  # # Utilizamos solo las primeras dosis
  # filter(dosis == "Primera") %>% 
  # Definimos la falla primaria de la vacuna
  mutate(falla_primaria = 0.05) %>% 
  group_by(ano, municipio_res_mad) %>% 
  # Calculamos la cantidad de niños no vacunados ("n" es la población)
  mutate(no_vacunados = poblacion - vacunados_primera) %>% 
  # Agregamos la cantidad de niños vacunados pero no inmunizados
  mutate(susceptibles = no_vacunados + (vacunados_primera * falla_primaria)) %>% 
  mutate(susceptibles = round(susceptibles, 0)) %>% 
  # Calculamos el acumulado de susceptibles conforme los años pasan
  #   1.  Usamos ungroup() en caso haya alguna agrupación previa que hayamos
  #       ingresado
  #   2.  Ordenamos la tabla de acuerdo al año, en orden ascendente
  #   3.  Calculamos el acumulado por medio de cumsum
  ungroup() %>% 
  #arrange(ano) %>% #ordenar la tabla según el año. MARCA ERROR Y USAREMOS WINDOW_ORDER()
  
  dbplyr::window_order(ano)%>% 
  
  mutate(susceptibles_acumulado_muni = cumsum(susceptibles)) %>% 
  collect()
  

head(susceptibles_muni)


# Avance de campaña -------------------------------------------------------

# 7. Avance de campaña ---------------------------------------------------------
# El correcto seguimiento de los avances de una campaña de vacunación es clave
# para su buena ejecución. Además, fomenta la transparencia y la mejora
# continua. Por lo tanto, en esta sección exploraremos cómo obtener gráficas
# que permitan visualizar el avance de la campaña de vacunación que estamos
# simulando.
#
# Para empezar, esta campaña de vacunación se está aplicando a niños y niñas
# con al menos 13 meses de edad y no más de 5 años de edad. Por lo tanto,
# dentro de nuestro registro civil y RNVe, necesitamos filtrar a todos aquellos
# niñ@s que cumplan con estas características.

## Fechas de campaña -----------------------------------------------------------
# Empezamos por definir la fecha de inicio de la campaña, que ya conocemos.
fecha_campana <- as.Date("2024-03-04", "%Y-%m-%d")
fecha_campana
# Calculamos la fecha de nacimiento que corresponde a la edad mínima (13 meses)
# NOTA: El operador %m-% es un operador especial de lubridate que permite
#       realizar la resta de dos fechas en términos de meses.
fecha_edad_minima <- fecha_campana %m-% months(12)
fecha_edad_minima
# Calculamos la fecha de nacimiento que corresponde a la edad máxima (menos
# de 5 años)
fecha_edad_maxima <- fecha_campana %m-% months(12 * 5) - 1 #cuando tengo que restar dias, si puedo usar el "-1", para restar mes si se usa "%m-%"
fecha_edad_maxima

## Población objetivo ----------------------------------------------------------
# Con esto listo, calculamos la población objetivo de la campaña
# Hagamoslo a nivel departamental (ADM1) también.
pop_campana_adm1 <- registro_civil_online %>% 
  # Filtramos la fecha de nacimiento de acuerdo a las edades calculadas
  # anteriormente.
  filter(fecha_nac <= fecha_edad_minima) %>% 
  filter(fecha_nac >= fecha_edad_maxima) %>% 
  # Agrupamos de acuerdo all departamento de nacimiento
  group_by(departamento_res_mad) %>% 
  # Calculamos la cantidad de personas nacidas en ese departamento, que son
  # elegibles para la campaña. Llamamos a esta columna "población".
  tally(name = "poblacion")


## Cobertura de campaña --------------------------------------------------------
# Ahora, quisiéramos calcular la cobertura de la campaña para cada departamento
# y cada día que ha pasado.
campana_departamento <- rnve_online %>% 
  # Puesto que estamos monitoreando solo la campaña, filtramos esas dosis
  filter(dosis == "Campaña") %>% 
  collect() %>% 
  # La columna municipio_res_mad contiene el municipio y el departamento
  # separados por un "-". Puesto que este patrón es válido para todas las filas,
  # podemos obtener el departamento utilizando la función separate de tidyr.
  tidyr::separate(
    # La columna que queremos separar
    municipio_res_mad,
    # Los nombres de las nuevas columnas que se crearán.
    #   1.  municipio contendrá todo lo que esté a la izquierda del
    #       separador
    #   2.  departamento_res_mad contendrá todo lo que esté a la derecha
    #       del separador
    c("municipio", "departamento_res_mad"),
    # El caracter que queremos usar como separador.
    sep = "-"
  ) %>% 
  # En caso haya quedado espacio en blanco extra, lo eliminamos con la
  # función trimws.
  mutate(departamento_res_mad = trimws(departamento_res_mad)) %>% 
  # Ahora ya podemos agrupar por día de vacunación y el departamento de
  # residencia
  group_by(fecha_vac, departamento_res_mad) %>% 
  # Y con la agrupación hecha, calcular la cantidad de vacunados para esos
  # grupos
  summarise(
    vacunados = n()
  ) %>%
  # Como últimos pasos, quisiéramos calcular la cobertura, y para ello
  # necesitamos la población. Esta información la tenemos en pop_campana_adm1.
  left_join(., collect(pop_campana_adm1), by = "departamento_res_mad") %>% 
  # Calculamos la cobertura
  mutate(cobertura = vacunados / poblacion * 100) %>% 
  # Y calculamos la cobertura acumulada para cada departamento (similar
  # al cálculo de susceptibles acumulados en la sección 6.)
  group_by(departamento_res_mad) %>% 
  arrange(fecha_vac) %>% 
  mutate(cobertura_acumulada = cumsum(cobertura))
#head(campana_departamento)

## Cobertura nacional ----------------------------------------------------------
# La tabla anterior nos da un resumen por departamento y día de la campaña.
# Para obtener una que nos resuma el avance por día a nivel nacional, podemos
# realizar los mismos pasos de antes, pero sin agrupar por departamento.
#
# Primero, obtenemos la población objetivo de la campaña.
pop_campana_nacional <- registro_civil_online %>% 
  # Filtramos la fecha de nacimiento de acuerdo a las edades calculadas
  # anteriormente.
  filter(fecha_nac <= fecha_edad_minima) %>% 
  filter(fecha_nac >= fecha_edad_maxima) %>% 
  # Calculamos la cantidad de personas nacidas que son elegibles para la
  # campaña. Llamamos a esta columna "población".
  tally(name = "poblacion") %>% #Conteo de personas elegibles con las condiciones que se especificaron
  # Puesto que para este punto tenemos una tabla con una sola fila,
  # sacamos el dato de población objetivo a nivel nacional
  pull(poblacion) #Sirve para jalar el número que se creó en tally
pop_campana_nacional
# Ahora, calculamos la cobertura
campana_nacional <- rnve_online %>% 
  # Puesto que estamos monitoreando solo la campaña, filtramos esas dosis
  filter(dosis == "Campaña") %>% 
  # Agrupamos por día solamente
  group_by(fecha_vac) %>% #Esto para dar seguimiento dia por dia
  # Y calculamos la cantidad de vacunados por dia
  summarise(
    vacunados = n()
  ) %>%
  # Calculamos la cobertura usando la poblacion objetivo a nivel nacional
  mutate(cobertura = as.numeric(vacunados) / as.numeric(pop_campana_nacional) * 100) %>% 
  # Desagrupamos, ordenamos por fecha de vacunacion y calculamos la cobertura
  # acumulada diaria
  ungroup %>% 
  dbplyr::window_order(fecha_vac) %>% #ordena las fechas
  mutate(cobertura_acumulada = cumsum(cobertura)) %>% 
  collect()
#head(campana_nacional)
### Gráfica --------------------------------------------------------------------















# Geo ---------------------------------------------------------------------

# Run requirements.R to load the needed libraries ------------------------------
source("requirements.R")

#Cargamos bases

# Run create_registro_civil_db.R -----------------------------------------------
#source("scripts/create_registro_civil_db.R")

# Run create_rnve_db.R ---------------------------------------------------------
#source("scripts/create_rnve_db.R")
#rnve <- import("data/rnve.csv")
#registro_civil <- read_csv("data/registro_civil.csv")

# Leer el archivo shapefile Uruguay
shp <- st_read("data/Anterior_URYMixed/URY_ADM2_Anterior.shp")

# Lee BD con info de puntos

rnve_campana <- rnve_online %>% 
  filter(dosis == "Campaña") %>% 
  #select(latitude, longitude, depart) %>% 
  select(-dosis,
         -nombre,
         -nombre_madre, 
         -apellido, 
         -apellido_madre, 
         -fecha_nac, 
         -fecha_nac_madre, 
         -pais_res_mad, 
         -nacionalidad, 
         -vacuna) %>% 
  collect() %>% 
  st_as_sf(coords = c("longitude", "latitude"), # Columnas 
                      crs = st_crs(shp) # sistema de coordeadas
)

# Revisar la calidad de las geometrias 

table(st_is_valid(rnve_campana)) #Revisa que las geometrias ingresadas sean validas y no se crucen 

table(st_is_valid(shp))

# Corregir las geometrias

shp <- st_make_valid(shp) # arregla el error  austando el shp en un elemnto temporal

# Realizar la intersección espacial y unión con el shapefile
interseccion <- st_intersects(rnve_campana, shp)

# Obtener los índices de la intersección

indices_interseccion <- lapply( #aplica la funcion deseada a cada elemento de una lista especificcada
  interseccion, #Elemento tipo lista   
  function(x) # define que se aplicara la funcion en el objeto X
    ifelse(length(x) > 0, # revisa que el numero de caracteres sea mayor a 0
           x, #si cumple guarda el elemento
           NA) # si no cumple regresa NA
)

# Asignar atributos manualmente
rnve_campana$departamento_res_mad <- sapply( #es una función en R que aplica una función a cada elemento de una lista y devuelve un vector o matriz con los resultados.
  indices_interseccion, #lista
  function(x) # define que se aplicara la funcion en el objeto X
    ifelse( #condiccional
      !is.na(x), #Busca todo lo que no sea NA
      shp$ADM1_ISON[x], # Asigna la variable de la base shp correspondiente 
      NA) # si no es NA
)


rnve_campana$municipio_res_mad <- sapply(indices_interseccion, function(x) ifelse(!is.na(x), shp$ADM2_ISON[x], NA))

rnve_campana$cod_municipio <- sapply(indices_interseccion, function(x) ifelse(!is.na(x), shp$GIS_CODE[x], NA))

rnve_campana$cod_departamento <- sapply(indices_interseccion, function(x) ifelse(!is.na(x), shp$ADM1_ISOC[x], NA))



# se agrega la informacion de Depto y Muni a base puntos

# rnve_campana <- rnve_campana %>% 
#   select(ID, departamento_res_mad, municipio_res_mad, cod_municipio, cod_departamento)

# puntos <- left_join(x = puntos, y = puntos_sf, by = "ID")

# rnve_campana <- rnve_campana %>% 
#   select(-c(geometry, departamento_res_mad, municipio_res_mad, cod_municipio, cod_departamento)) #Selecciono mis variables de interes
  # rename( # Renombro las variables
  #   departamento_res_mad = departamento_res_mad.y,
  #   municipio_res_mad = municipio_res_mad.y, 
  #   cod_municipio = cod_municipio.y, 
  #   cod_departamento = cod_departamento.y
  # ) #%>% 
#select(1:9, 19, 20, 18, 21, everything()) #selecciono mis columnas de interes

# registro_civil_ajust <- puntos
# Export table -----------------------------------------------------------------
#write_csv(registro_civil_ajust, "data/Registro civil - Uruguay_ajust.csv")

# Remove all unnecessary variables ---------------------------------------------
#rm(list=setdiff(ls(), c("registro_civil", "live_births")))



#--------------------------------

mun <- read_sf("data\\Anterior_URYMixed\\URY_ADM2_Anterior.shp")

### visualizacion general ####

# Al añadir la funcion plot no permite visualizar la estructura basica del SHP

#plot(mun)


# Visualizaciones con GGPLOT ----------------------------------------------


## Basica ####

# tambien se puede utilizar ggplot2 para generar la visualizacion del codigo shp

#ggplot()+
  #geom_sf(data = mun)

## Ajuste BD y union al shp ####

# Ademas podemos combinar el shp con bases de datos para generar visualizaciones simples y estaticas

# poblacion <- rnve_campana %>% 
#   select(cod_municipio, municipio_res_mad) %>% 
#   group_by(GIS_CODE = cod_municipio) %>% 
#   summarise(municipio_res_mad = first(municipio_res_mad), total_pob = n())

vacunados <- rnve_campana %>% 
  select(municipio_res_mad) %>% 
  group_by(municipio_res_mad) %>% 
  summarise(total_vac = n())

#datos_map <- vacunados

table(st_is_valid(vacunados))
mun <- st_make_valid(mun)

st_make_valid(vacunados)

## Creacion de variables en el shp ####
mun$municipio_res_mad <-  mun$ADM2_ISON

vacunados <- st_join(mun, vacunados) %>% #GIS_CODE: código de área
  # mutate(rango_pob = case_when(total_pob >= 1 & total_pob <= 474 ~ "1 - 474", # se pueden generar variables adicionales
  #                              total_pob > 474  & total_pob <= 964  ~ "474 - 964",
  #                              total_pob > 964 & total_pob <= 2648 ~ "964 - 2648",
  #                              total_pob > 2648 ~ "> 2648",
  #                              TRUE ~ "Sin Dato"),
         # rango_pob = factor(rango_pob, levels = c("1 - 474", #asignar ajuste de variables(tipo factor en este caso)
         #                                          "474 - 964",
         #                                          "964 - 2648",
         #                                          "> 2648",
         #                                          "Sin Dato")),
         mutate(rango_avance = case_when(total_vac >= 1 & total_vac <= 533 ~ "1 - 533",
                                  total_vac > 533 & total_vac <= 1080 ~ "533 - 1080",
                                  total_vac > 1080 & total_vac <= 2898 ~ "1080 - 2898",
                                  total_vac > 2898~ "> 2898",
                                  TRUE ~ "Sin Dato"),
         rango_avance = factor(rango_avance, levels = c("1 - 533",
                                                        "533 - 1080",
                                                        "1080 - 2898",
                                                        "> 2898",
                                                        "Sin Dato")))
  #)

## mapa Corropletas ####

# Se puede generar un mapa estatico de corropletas para el analisis de 
# distribucion espacial (Poblacion a intervenir) ggplot2

# corropletas <- ggplot()+ 
#   geom_sf(data = datos_map,
#           aes(fill = rango_pob,
#               geometry = geometry),
#           color = '#969696',
#           size = .9)+
#   scale_fill_manual("Numeró de habitantes", 
#                     values = c("white", "lightpink", "red", "darkred", "gray")) # ajuste manual de plaeta de color

#corropletas

## Creacion de centroides ####

# al generar uniones se pueden generar duplicidad de informacion o puede ser posible que existan valores duplicados en las bases previas
#datos_map <- st_make_valid(datos_map)


# Hacer un centroide para cada poligono

# coord_puntos <- datos_map %>% 
#   st_centroid()

## Mapa de frecuencia por clouster ####

# puntos <-  ggplot()+
#   geom_sf(data = datos_map,
#           color = 'black',
#           size = .1)+
#   geom_sf(data = coord_puntos,
#           aes(size = rango_avance),
#           color = 'red',   # Set the color to red
#           alpha = 0.5)     # Set the transparency (alpha) to 0.5
# 
# puntos


# Cambiar los mapas ####


# ggplot()+  # Inicia un objeto ggplot
#   geom_sf(data = datos_map,   # Agrega una capa al gráfico con datos de geometría espacial (geom_sf).
#           aes(fill = rango_pob), # para colorear las áreas según la variable rango_pob. 
#           color = '#636363',
#           size = .2)+
#   geom_sf(data = coord_puntos %>% # Agrega una capa al gráfico con datos de geometría espacial (geom_sf). 
#             filter(rango_avance != "Sin Dato"),
#           aes(size = rango_avance), # Usa el tamaño del punto (size) según la variable rango_avance
#           color = "black", alpha = 0.7)+
#   scale_fill_manual("Tasa de incidencia", #Define manualmente los colores para la escala de colores de la variable de relleno. 
#                     values = c("1 - 474" = "#ffffd4",
#                                "474 - 964"= "#fee391",
#                                "964 - 2648"= "#fe9929",
#                                "> 2648"= "#d95f0e"))+
#   labs(title = "Porcentaje de avance campaña vacunacion 2024",
#        caption = "Fuente : Elaboración propia con base en datos random")+
#   theme_void()+ # Personalización adicional del tema del gráfico. 
#   theme(title=element_text(face = "bold"), #Establece el estilo del título en negrita,  
#         legend.position= c(.9, .3), 
#         legend.justification='left', # la posición y la orientación de la leyenda,
#         legend.direction='vertical',
#         legend.text=element_text(size=14)) # y el tamaño del texto de la leyenda.

# Mapas Interactivos ####

#datos_map2 <- full_join(poblacion, sin_vac, by = "municipio_res_mad")

## Creacion de variables en el shp ####

# datos_map2 <- full_join(mun, datos_map2,by = "GIS_CODE")
# 
# set.seed(234) #Partir siempre de un parámetro específico (pasa de ser probabilistico a algo deterministico, debido a que la base es muy grande, se toma una muestra, que en este caso sería la misma)

# Extraer una muestra aleatoria del dataframe
#rnve <- rnve[sample(nrow(rnve), 10000), ]

# Create leaflet

# Manual breaks for color bins
breaks <- quantile(vacunados$total_vac, na.rm = T)

# para encontrar colores se puede utilizar paginas como https://r-charts.com/es/colores/

pal <- colorBin(c("#24693D","#8CCE7D", "orange" ,"#EACF65", "#BF233C"), reverse = T , domain = vacunados$total_vac, bins = breaks)

# se agregan labels a las capas agregadas
labels_cor <- sprintf("<b>%s", paste("Avance",vacunados$municipio_res_mad, vacunados$rango_avance)) %>%
  lapply(htmltools::HTML)

labels_punt <- sprintf(paste("ID caso", vacunados$ID))

mapa_vacuna <- leaflet(vacunados) %>% #leaflet es lo que se usa en lugar de ggplot
  setView(-55.5, -32.5, zoom = 6) %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addEasyButton(
    easyButton(
      icon = "fa-globe",
      title = "Zoom Inicial",
      onClick = JS("function(btn, map){ map.setZoom(6); }")
    )
  )
mapa_vacuna

mapa_vacuna <-mapa_vacuna %>%
  addPolygons(
    fillColor = ~pal(total_vac), #INCLUIR CAMPAÑA MUNICIPAL
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    label = labels_cor,
    group = "avance" )%>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~total_vac,
    na.label = "Sin Dato",
    title = "Número ")
# map <- map %>% 
#   addCircles(
#     data = rnve_online,
#     lng = ~longitude, #X
#     lat = ~latitude, #Y
#     group = "Puntos", #Cómo el mapa va identificar la capa
#     label = labels_punt,
#     fillOpacity = 0.4) 
mapa_vacuna <- mapa_vacuna %>% 
  addHeatmap(
    data = vacunados,
    lng = ~LONGITUDE,
    lat = ~LATITUDE,
    group = "calor", 
    intensity = 2,
    blur = 50) %>% #Para aumentar los valores de riesgo y visualizarlo. Es subjetivo
  addLayersControl(overlayGroups = c("avance", "Puntos", "calor") , #Es un filtro, prácticamente
                   options = layersControlOptions(collapsed = TRUE ))

mapa_vacuna


# Mapa con filtro ---------------------------------------------------------------------

# sexo <- unique(rnve$sexo)
# 
# sexo_list <- list()
# 
# for (i in 1:length(sexo)) {
#   
#   sexo_list[[i]] <- rnve %>% dplyr::filter(sexo == sexo[i]) 
# }
# 
# names(sexo_list) <- sexo
# 
# map2 <- leaflet() %>% addTiles()
# 
# colores <- c("pink", "blue")
# 
# for (i in 1:length(sexo)) {
#   map2 <- map2 %>% addCircles(data = sexo_list[[i]], 
#                               lat = ~latitude,
#                               lng = ~longitude,
#                               fillOpacity = 0.5, 
#                               label = ~ID,
#                               popup = ~paste("Edad de la madre", edad_madre), 
#                               group = sexo[i],
#                               color = colores[i])
# }
# 
# map2 <- map2 %>% 
#   addLayersControl(overlayGroups = sexo, 
#                    options = layersControlOptions(collapsed = TRUE ))
# map2



dbDisconnect(con)

