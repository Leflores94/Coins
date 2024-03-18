# server.R ---------------------------------------------------------------------
# Description: Este script crea un servidor, que representa una sesión de R
# que corre código y devuelve resultados (p. ej., una gráfica).
# Created by -------------------------------------------------------------------
# Name: CIM Data Team
# Created on: 2024-01-29
# Editorial --------------------------------------------------------------------
# Section for editorial changes or notes
# ______________________________________________________________________________

# Inicializar el servidor ------------------------------------------------------
shinyServer(function(input, output) {
  ## Elementos del UI ----------------------------------------------------------
  ### Inicio -------------------------------------------------------------------
  # Cuadro informativo para seccion de Inicio
  output$inicio_textbox <- renderUI({
    HTML("El siguiente tablero es un trabajo honesto de Coins group elaborado en el marco del Taller avanzado de R 
         celebrado en la Ciudad de Guatemala del 18-22 de marzo de 2024."
      
    )
  })
  #imagen de equipo
  output$foto_grupo <- renderImage({
    list(src = "data/coins.png", #los ".." y / significa que debe regresar una carpeta, ya que este código es para la capeta de scripts
         contentType = "image/png",
         width = "300",
         height = "300"
    )
  }, deleteFile = FALSE)

  # Descripción del equipo.
  output$texto_equipo <- renderUI({
    HTML("Integrantes: <br> 
         Grethel Alvarado, Guatemala <br> 
         Paulina Muñoz, Chile  <br> 
         Luis Flores, El Salvador  <br>
         Alejandra Yepez, México")
  })
  ### Justificacion ------------------------------------------------------------
  # Cuadro informativo para seccion de Justificacion
  output$justificacion_textbox <- renderText({
    "Descripción"
  })
  ### Avance de campaña --------------------------------------------------------
  # Cuadro informativo para seccion de Avance de campaña
  output$avance_campana_textbox <- renderText({
    "Descripción"
  })
  ### Georreferenciación -------------------------------------------------------
  # Cuadro informativo para seccion de Georreferenciación
  output$georreferenciacion_textbox <- renderText({
    "Descripción"
  })
})