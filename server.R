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
    "Ante la alerta epidemiológica internacional brindada por la Organización
    Panamericana de la Salud de los brotes de sarampión en la Región de las 
    Américas es necesario realizar una verificación de las coberturas de 
    vacunación de SPR1 y SPR2 como estrategia de disminución de brechas de
    niños susceptibles que permitirán reducir el riesgo ante un caso importado
    en nuestras poblaciones e identificar la cantidad de susceptibles a 
    sarampión para realizar acciones como la implementación de una campaña 
    de seguimiento de alta calidad."
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
                scrollY = "374px"
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
    "Distribución geoespacial de las personas vacunadas en la campaña de SRP"
  })
  
  output$mapa_vacuna <- renderLeaflet({
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
    
    mapa_vacuna <-mapa_vacuna %>%
      addPolygons(
        fillColor = ~pal(total_vac), #INCLUIR CAMPAÑA MUNICIPAL
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels_cor,
        group = "avance")%>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~total_vac,
        na.label = "Sin Dato",
        title = "Número")
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
    
    
    
  })
  
})