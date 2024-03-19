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
         width = "100%",
         height = "100%"
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
  
  ## Gráfica de justificación (aqui se hace el gráfico) ------------------------------------------------
  output$grafica_justificacion <- renderPlot({
    grafica <- # Visualicemos los datos que tenemos de manera gráfica.
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
    
    grafica
  })
  
  
  # Tabla de la justificación -----------------------------------------------
  output$tabla_justificacion <- renderDataTable({
    datatable(susceptibles, class = "compact")
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