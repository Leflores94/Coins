# requirements.R ---------------------------------------------------------------
# Description: Contiene el listado de paquetes necesarios para la correcta
# ejecución de este proyecto.
# Created by -------------------------------------------------------------------
# Name: CIM Data Team
# Created on: 2024-01-29
# Editorial --------------------------------------------------------------------
# Section for editorial changes or notes
# ______________________________________________________________________________

# cargar pacman si no lo está
if (!require("pacman")) install.packages("pacman")

# cargar paquetes necesarios
pacman::p_load(
  shiny,
  shinydashboard,
  ggplot2,
  DT,
  tidyr,
  dplyr,
  lubridate,
  stringr,
  rio,
  bench,
  DBI,
  RPostgres,
  plotly,
  scales,
  leaflet,
  leaflet.extras,
  sf
)
