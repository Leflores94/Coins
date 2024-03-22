# ui.R -------------------------------------------------------------------------
# Description: Este script crea la interfaz de usuario de la aplicación de
# Shiny.
# Created by -------------------------------------------------------------------
# Name: CIM Data Team
# Created on: 2024-01-29
# Editorial --------------------------------------------------------------------
# Section for editorial changes or notes
# ______________________________________________________________________________

# Inicializar la UI ------------------------------------------------------------
fluidPage(
  fluidRow(
    box(width = 12,
        background = "red",
        div(
          HTML(paste0(
            '<font color="white"><strong>',
            "Este tablero es producto del curso avanzado de R organizado por OPS en marzo de 2024.
    Los datos presentados en este tablero son completamente ficticios y han sido creados 
    únicamente con fines didácticos. Cualquier similitud con datos reales, personas o 
    eventos es pura coincidencia y no debe interpretarse como una representación exacta 
    de la realidad. Este contenido está diseñado para ilustrar conceptos y promover el 
    aprendizaje a través de ejemplos construidos para este fin.",
    '</strong></font>'
          )
          ))
    )
  ),
  ## CSS -------------------------------------------------------------------------
  includeCSS("style.scss"),
  ## Inicializar dashboard -------------------------------------------------------
  dashboardPage(
    ## Header dashboard ------------------------------------------------------------
    dashboardHeader(title = paste0(Sys.Date())),
    ## Sidebar dashboard -----------------------------------------------------------
    dashboardSidebar(
      sidebarMenu(
        # Modulo 1: Inicio
        menuItem(text = "Inicio",
                 tabName = "inicio",
                 icon = icon("heart"),
                 selected = TRUE
        ),
        # Modulo 2: Justificacion
        menuItem(text = "Justificación",
                 tabName = "justificacion",
                 icon = icon("shield-virus")
        ),
        # Modulo 3: Avance Campaña
        menuItem(text = "Avance Campaña",
                 tabName = "avance_campana",
                 icon = icon("syringe")
        ),
        # Modulo 4: Georreferenciacion
        menuItem(text = "Georreferenciación",
                 tabName = "georreferenciacion",
                 icon = icon("earth-americas")
        )
      )
    ),
    ## Cuerpo dashboard ------------------------------------------------------------
    dashboardBody(
      tabItems(
        ### Inicio -------------------------------------------------------------
        tabItem(tabName = "inicio",
                fluidRow(box(
                  width = 12,
                  title = "Inicio",
                  uiOutput(outputId = "inicio_textbox")
                )
                ),
                fluidRow(box(
                  width = 6,
                  title = "Teams Coins",
                  imageOutput(outputId = "foto_grupo")
                ),
                box(
                  width = 6,
                  icon = icon("people-group"),
                  title = "Descripción del equipo",
                  uiOutput(outputId = "texto_equipo")
                ))
                ),
        ### Justificacion ------------------------------------------------------
        tabItem(tabName = "justificacion",
                fluidRow(
                  box(
                    width = 12,
                    title = "Justificacion",
                    textOutput(outputId = "justificacion_textbox"))
                ),
                fluidRow(
                  box(
                    width = 6,
                    height = 500,
                    title = "Gráfica",
                    plotOutput(outputId = "grafica_justificacion")),
                  
                  box(
                    width = 6,
                    height = 500,
                    title = "Tabla",
                    dataTableOutput(outputId = "tabla_justificacion"))
                  )
                ),
        ### Avance de campaña --------------------------------------------------
        tabItem(tabName = "avance_campana",
                fluidRow(
                  box(
                    width = 12,
                    title = "Avance de Campaña",
                    textOutput(outputId = "avance_campana_textbox")
                  )
                ),
                fluidRow(
                  box(
                    width = 6,
                    size = 14,
                    title = "Gráfica de avance diario nacional",
                    #tags$h5("Este es un subtítulo 2", style = "margin-center: 20px; margin-top: 5px;"),
                    plotlyOutput(outputId = "grafica_avance"),
                    tags$h5("Los datos presentados en esta gráfica son
                            preliminares y están sujetos a cambios, según 
                          confirmación de establecimientos de salud.",
                            style = "margin-left: 20px; margin-down: 5px;"),
                  
                  tags$h5("Fuente: Ministerio de Salud, datos del registro 
                            nominal de vacunación. 2024.",
                          style = "margin-left: 20px; margin-down: 5px;"),
                  ),
                  box(
                    width = 6,
                    selectizeInput(
                      inputId = "avance_input",
                      label = "Departamento",
                      choices = unique(campana_departamento$departamento_res_mad)
                    ),
                    plotlyOutput(
                      outputId = "grafica_avance_dinamica"
                    )
                  )
                )),
        ### Georreferenciación -------------------------------------------------
        tabItem(tabName = "georreferenciacion",
                fluidRow(
                  box(
                    width = 12,
                    title = "Información geoespacial de la campaña de vacunación SRP",
                    textOutput(outputId = "georreferenciacion_textbox")
                  )
                ),
                fluidRow(
                  box(
                    width = 6,
                    height = 600,
                    title = "Mapa: Total de vacunados",
                    selectInput(
                      inputId = "municipio_input",
                      label = "Departamento",
                      choices = unique(vacunados$ADM1_ISON), 
                      # multiple = TRUE, 
                      # selected = unique(vacunados$municipio_res_mad.x)
                    ),
                    leafletOutput(outputId = "mapa_vacuna")
                  ),
                  box(
                    width = 6,
                    height = 600,
                    title = "Mapa de calor: Total de vacunados",
                    leafletOutput(outputId = "mapa_calor")
                  )
                )
                )
      )
    )
  )
)