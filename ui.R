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
                 icon = icon("home"),
                 selected = TRUE
        ),
        # Modulo 2: Justificacion
        menuItem(text = "Justificación",
                 tabName = "justificacion",
                 icon = icon("th-large")
        ),
        # Modulo 3: Avance Campaña
        menuItem(text = "Avance Campaña",
                 tabName = "avance_campana",
                 icon = icon("th-large")
        ),
        # Modulo 4: Georreferenciacion
        menuItem(text = "Georreferenciacion",
                 tabName = "georreferenciacion",
                 icon = icon("th-large")
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
                    title = "Gráfica",
                    plotOutput(outputId = "grafica_justificacion")),
                  
                  box(
                    width = 6,
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
                )),
        ### Georreferenciación -------------------------------------------------
        tabItem(tabName = "georreferenciacion",
                fluidRow(
                  box(
                    width = 12,
                    title = "Georreferenciación",
                    textOutput(outputId = "georreferenciacion_textbox")
                  )
                ))
      )
    )
  )
)