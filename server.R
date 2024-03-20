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
    datatable(susceptibles_muni, class = "compact",
              options = list(
                dom = "Btp",
                paging = FALSE,
                scrollX = TRUE,
                scrollY = "500px"
              ))
  })
  
  ### Avance de campaña --------------------------------------------------------
  # Cuadro informativo para seccion de Avance de campaña
  output$grafica_avance <- renderPlotly({
    grafica_avance <- ggplot(
      campana_nacional,
      aes(x = fecha_vac)
    ) +
      # Nombramos los ejes y la leyenda
      labs(x = "Fecha", y = "Dosis", fill = "Dosis", linetype = "Cobertura") +
      # Mostramos el numero de dosis aplicadas cada mes
      geom_bar(aes(y = vacunados), stat = "identity", position = "stack") +
      # Mostramos la cobertura acumulada para cada mes.
      # NOTA: Los datos de n_dosis alcanzan aprox 7,500, mientras que las 
      #       coberturas son entre 0 y 100. Por lo tanto, se multiplica el valor de 
      #       coberturas por 75 para igualar los dos ejes.
      geom_line(aes(y = cobertura_acumulada * 75), linewidth = 1) +
      # Modificamos el eje vertical
      scale_y_continuous(
        # Ajustamos los limites entre 0 y 1,000 dosis
        limits = c(0, 7500),
        # Agregamos un segundo eje horizontal
        # NOTA: Aplicamos un factor de conversión de 75 para que el eje de cobertura
        #       alcance 100% cuando el número de dosis alcance 7,500 dosis.
        sec.axis = sec_axis( trans= ~./75, name = "Cobertura (%)")) + 
      scale_x_date(breaks = "3 day", labels = date_format("%m %d"))
      # Mejoramos la visualización
      theme_classic() +
      theme(text = element_text(size = 16))
    ggplotly(grafica_avance, tooltip = "text") %>% 
      config(
        locale = "es",
        displaylogo = FALSE,
        scrollZoom = TRUE,
        modeBarButtonsToAdd = c(
          "drawline", # dibujar líneas rectas
          "drawopenpath", # dibujar líneas libres
          "drawcircle", # dibujar círculos
          "drawrect", #dibujar rectángulos
          "eraseshape" # borrador
        ))
    ggplotly(grafica_avance) %>% 
      # Debemos agregar el segundo eje de nuevo, esta vez manualmente,
      # mediante la función add_lines de plotly
      add_lines(
        x = ~fecha_vac, y = ~cobertura_acumulada, data = campana_nacional,
        yaxis = "y2"
      ) %>% 
      # hacemos algunas configuraciones al eje y secundario y a los márgenes,
      # para que nuestra gráfica se vea bien
      layout(
        # configuraciones al nuevo eje vertical
        yaxis2 = list(
          tickfont = list(size = 16),
          titlefont = list(size = 18),
          overlaying = "y",
          nticks = 10,
          side = "right",
          title = "Cobertura (%)",
          # limitamos el eje entre 0 y 100%
          range = c(0,100),
          showline = TRUE
        ),
        # agregamos un poco de margen a la derecha para que quepa el nuevo eje
        # vertical
        margin = list(r = 100)
      )
    
  })
  departamento_reactive <- reactive({
    campana_departamento %>% 
      filter(departamento_res_mad == input$avance_input)
  })
  output$grafica_avance_dinamica<- renderPlotly({
    grafica_avance_dinamica <- ggplot(
      departamento_reactive(),
      aes(x = fecha_vac)
    ) +
      # Nombramos los ejes y la leyenda
      labs(x = "Fecha", y = "Dosis", fill = "Dosis", linetype = "Cobertura") +
      # Mostramos el numero de dosis aplicadas cada mes
      geom_bar(aes(y = vacunados), stat = "identity", position = "stack") +
      # Mostramos la cobertura acumulada para cada mes.
      # NOTA: Los datos de n_dosis alcanzan aprox 7,500, mientras que las 
      #       coberturas son entre 0 y 100. Por lo tanto, se multiplica el valor de 
      #       coberturas por 75 para igualar los dos ejes.
      geom_line(aes(y = cobertura_acumulada), linewidth = 1) +
      # Modificamos el eje vertical
      #scale_y_continuous(
        # Ajustamos los limites entre 0 y 1,000 dosis
        #limits = c(0, 1500),
        # Agregamos un segundo eje horizontal
        # NOTA: Aplicamos un factor de conversión de 75 para que el eje de cobertura
        #       alcance 100% cuando el número de dosis alcance 7,500 dosis.
        #sec.axis = sec_axis( trans= ~./75, name = "Cobertura (%)")
      #) +
      # Mejoramos la visualización
      theme_classic() +
      theme(text = element_text(size = 16))
    ggplotly(grafica_avance_dinamica, tooltip = "text") %>% 
      config(
        locale = "es",
        displaylogo = FALSE,
        scrollZoom = TRUE,
        modeBarButtonsToAdd = c(
          "drawline", # dibujar líneas rectas
          "drawopenpath", # dibujar líneas libres
          "drawcircle", # dibujar círculos
          "drawrect", #dibujar rectángulos
          "eraseshape" # borrador
        ))
    ggplotly(grafica_avance_dinamica) %>% 
      # Debemos agregar el segundo eje de nuevo, esta vez manualmente,
      # mediante la función add_lines de plotly
      add_lines(
        x = ~fecha_vac, y = ~cobertura_acumulada, data = campana_departamento,
        yaxis = "y2"
      ) %>% 
      # hacemos algunas configuraciones al eje y secundario y a los márgenes,
      # para que nuestra gráfica se vea bien
      layout(
        # configuraciones al nuevo eje vertical
        yaxis2 = list(
          tickfont = list(size = 16),
          titlefont = list(size = 18),
          overlaying = "y",
          nticks = 10,
          side = "right",
          title = "Cobertura (%)",
          # limitamos el eje entre 0 y 100%
          range = c(0,100),
          showline = TRUE
        ),
        # agregamos un poco de margen a la derecha para que quepa el nuevo eje
        # vertical
        margin = list(r = 100)
      )
  })
  
  ### Georreferenciación -------------------------------------------------------
  # Cuadro informativo para seccion de Georreferenciación
  output$georreferenciacion_textbox <- renderText({
    "Descripción"
  })
})