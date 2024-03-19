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
  mutate(cobertura = as.numeric(vacunados_primera) / as.numeric(poblacion)  * 100)

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

dbDisconnect(con)
