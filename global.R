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







dbDisconnect(con)

