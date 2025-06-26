#### Librerías ####

library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(grid)

#### Carga y transformación de datos ####

Protestas <- read.csv("Argentina_ACLED.csv")

Protestas$fecha_date <-
  as.Date(Protestas$event_date, format = "%d %B %Y")

Protestas$dia_semana <- weekdays(Protestas$fecha_date)

Protestas$mes <- format(as.Date(Protestas$fecha_date), "%B")

dias <- c(
  "Monday" = "Lunes",
  "Tuesday" = "Martes",
  "Wednesday" = "Miércoles",
  "Thursday" = "Jueves",
  "Friday" = "Viernes",
  "Saturday" = "Sábado",
  "Sunday" = "Domingo"
)

meses <- c(
  "January" = "Enero",
  "February" = "Febrero",
  "March" = "Marzo",
  "April" = "Abril",
  "May" = "Mayo",
  "June" = "Junio",
  "July" = "Julio",
  "August" = "Agosto",
  "September" = "Septiembre",
  "October" = "Octubre",
  "November" = "Noviembre",
  "December" = "Diciembre"
)

Protestas <- Protestas %>%
  mutate(
    dia_semana = recode(dia_semana,!!!dias),
    dia_semana = factor(dia_semana),
    mes = recode(mes,!!!meses),
    mes = factor(mes)
  )

Actores <- Protestas %>%
  separate_rows(assoc_actor_1, sep = "; ") %>%
  mutate(
    assoc_actor_1 = trimws(assoc_actor_1),
    assoc_actor_1 = if_else(assoc_actor_1 == "", "Self-Organized Protesters", assoc_actor_1)
  ) %>%
  group_by(year, assoc_actor_1) %>%
  summarise(total_protests = n(), .groups = "drop") %>%
  mutate(percentage = (total_protests / sum(total_protests)) * 100) %>%
  arrange(desc(total_protests))

Protestas <- Protestas %>%
  mutate(event_date = as.Date(event_date, format = "%d %B %Y"),
         year = year(event_date))

Dias_Protestas <- Protestas %>%
  group_by(year, dia_semana) %>%
  summarise(cantidad_protestas = n()) %>%
  slice(which.max(cantidad_protestas))

Mes_Protestas <- Protestas %>%
  group_by(year, mes) %>%
  summarise(cantidad_protestas = n(), .groups = 'keep') %>%
  group_by(year) %>%
  slice(which.max(cantidad_protestas))

Resumen_Provincias <- Protestas %>%
  group_by(year, admin1) %>%
  summarize(protestas_pcia = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total_protestas = sum(protestas_pcia)) %>%
  mutate(porcentaje_pcia = (protestas_pcia / total_protestas) * 100) %>%
  mutate(admin1 = ifelse(admin1 == "Ciudad Autonoma de Buenos Aires", "CABA", admin1)) %>%
  rename(provincia = admin1)

#### Interfaz de usuario ####

ui <- fluidPage(
  titlePanel("🪧 Mapa de la protesta social en Argentina"),
  tabsetPanel(
    tabPanel("Mapa",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput(
                   "dateRange",
                   "Rango de fechas",
                   start = "2018-01-01",
                   end = "2025-05-31",
                   format = "dd-mm-yyyy"
                 ),
                 selectInput(
                   "provinceInput",
                   "Filtro específico por provincia/s",
                   choices = c(unique(Protestas$admin1)),
                   multiple = TRUE
                 ),
                 selectInput("actorInput", "Filtro específico por convocante", choices = c("Todos", unique(
                   Actores$assoc_actor_1
                 ))),
                 radioButtons(
                   "mapType",
                   "Tipo de mapa",
                   choices = c("Mapa de marcadores" = "markers", "Mapa de calor" = "heat")
                 ),
                 
                 HTML(
                   '<br>
            <h4>Sobre este proyecto</h4>
            <p>💜 Desarrollado por <a href="http://marialasa.ar" target="_blank">Mar&iacute;a de los &Aacute;ngeles Lasa</a> con datos del <em>Armed Conflict Location and Event Data Project</em> (<a href="https://acleddata.com/data-export-tool/" target="_blank">ACLED</a>).</p>
            <p>👩‍💻 C&oacute;digo disponible en <a href="https://github.com/marialasa/mapa_protestas/blob/main/Mapa_Protesta.R" target="_blank">GitHub</a>.</p>
            <p>📅 Rango de fechas de protestas: 1 de enero de 2018 - 31 de mayo de 2025 con actualización mensual.</p>
            <p>✊ Cantidad de protestas registradas: 15.157.</p>
            <p> 💻 Aplicaci&oacute;n web <strong>no</strong> optimizada para tel&eacute;fonos celulares.</p>
            <p>⌛ La app puede demorar algunos segundos en cargarse dada la complejidad de los procesos de análisis y cálculo que se ejecutan en el servidor.</p>
            <p>⚠️ CC BY-NC-SA 4.0</p>'
                 )
               ),
               mainPanel(leafletOutput(
                 "map", width = "100%", height = "750px"
               ))
             )),
    tabPanel("Estadísticas",
             fluidRow(
               column(10,
                      plotlyOutput("dailyProtestsPlot", height = "1000px")),
               column(
                 2,
                 tags$br(),
                 tags$br(),
                 selectInput(
                   "yearSelect",
                   "Seleccione un año",
                   choices = c("2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025")
                 ),
                 uiOutput("yearSpecificBoxes")
               )
             ),
             tags$style(
               HTML(".value-box { border: 1px solid #333; }")
             )),
    tabPanel("Provincias",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   "yearInputPcias",
                   "Seleccione el año",
                   choices = c("2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"),
                   selected = "2018"
                 )
               ),
               mainPanel(fluidRow(column(
                 12, plotOutput("provinciaPlot", width = "80%", height = "600px")
               )))
             )),
    tabPanel("Actores",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   "yearInputActors",
                   "Seleccione el año",
                   choices = c("2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"),
                   selected = "2018"
                 )
               ),
               mainPanel(plotOutput(
                 "plotActores", width = "80%", height = "600px"
               ))
             )),
    tabPanel("Hotspots",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   "yearInputHotspots",
                   "Seleccione el año",
                   choices = c("2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"),
                   selected = "2018"
                 ),
                 HTML(
                   '<br>
            <h4>Nota</h4>
            <p>🗺️ Los mapas de <em>hotspots</em> fueron desarrollados utilizando el análisis de <em>Local Indicators of Spatial Association</em> (LISA), un método estadístico empleado para identificar patrones espaciales y detectar áreas con características significativamente similares o diferentes dentro de un contexto geográfico.</p>
            <p>👉 Para acceder a una explicación detallada sobre la metodología empleada, <a href="https://rpubs.com/mlasa/lacallehabla" target="_blank">clic aquí</a>.</p>'
                 )
               ),
               mainPanel(imageOutput("dynamicImage"))
             ))
  )
)

#### Server ####

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    data <- Protestas
    
    if (!is.null(input$dateRange)) {
      data <-
        data %>% filter(event_date >= input$dateRange[1] &
                          event_date <= input$dateRange[2])
    }
    
    if (!is.null(input$provinceInput)) {
      if (!"Todos" %in% input$provinceInput) {
        data <- data %>% filter(admin1 %in% input$provinceInput)
      }
    }
    
    data <- data %>%
      separate_rows(assoc_actor_1, sep = "; ") %>%
      mutate(assoc_actor_1 = trimws(assoc_actor_1))
    
    if (!is.null(input$actorInput) && input$actorInput != "Todos") {
      data <- data %>% filter(assoc_actor_1 == input$actorInput)
    }
    
    map <- leaflet(data) %>% addTiles()
    if (input$mapType == "markers") {
      map %>% addMarkers(
        lng = ~ longitude,
        lat = ~ latitude,
        popup = ~ notes,
        clusterOptions = markerClusterOptions(
          spiderfyOnMaxZoom = TRUE,
          showCoverageOnHover = TRUE,
          zoomToBoundsOnClick = TRUE
        )
      )
    } else {
      map %>% addHeatmap(
        lng = ~longitude,
        lat = ~latitude,
        blur = 15,
        radius = 20,
        max = 0.3,
        gradient = colorRampPalette(c("blue", "cyan", "yellow", "red"))(10)
      )
    }
  })
  
  total_protests_by_year <- reactive({
    Protestas %>%
      group_by(year) %>%
      summarise(total_protests = n(), .groups = 'drop')
  })
  
  renderTotalProtestsBox <- function(year) {
    total_data <- total_protests_by_year()
    total_for_year <-
      total_data %>% filter(year == year) %>% pull(total_protests)
    
    renderValueBox({
      valueBox(total_for_year,
               paste("protestas registradas en", year))
    })
  }
  
  average_protests_by_year <- reactive({
    Protestas %>%
      mutate(year = year(as.Date(event_date, format = "%Y-%m-%d"))) %>%
      group_by(year) %>%
      summarise(avg_protests_per_day = n() / 365)
  })
  
  output$avgProtests2018 <- renderValueBox({
    avg_data <- average_protests_by_year()
    valueBox(value = round(avg_data[avg_data$year == 2018,]$avg_protests_per_day, 2),
             subtitle = "promedio protestas diarias")
  })
  
  output$avgProtests2019 <- renderValueBox({
    avg_data <- average_protests_by_year()
    valueBox(value = round(avg_data[avg_data$year == 2019,]$avg_protests_per_day, 2),
             subtitle = "promedio protestas diarias")
  })
  
  
  output$avgProtests2020 <- renderValueBox({
    avg_data <- average_protests_by_year()
    valueBox(value = round(avg_data[avg_data$year == 2020,]$avg_protests_per_day, 2),
             subtitle = "promedio protestas diarias")
  })
  
  output$avgProtests2021 <- renderValueBox({
    avg_data <- average_protests_by_year()
    valueBox(value = round(avg_data[avg_data$year == 2021,]$avg_protests_per_day, 2),
             subtitle = "promedio protestas diarias")
  })
  
  output$avgProtests2022 <- renderValueBox({
    avg_data <- average_protests_by_year()
    valueBox(value = round(avg_data[avg_data$year == 2022,]$avg_protests_per_day, 2),
             subtitle = "promedio protestas diarias")
  })
  
  output$avgProtests2023 <- renderValueBox({
    avg_data <- average_protests_by_year()
    valueBox(value = round(avg_data[avg_data$year == 2023,]$avg_protests_per_day, 2),
             subtitle = "promedio protestas diarias")
  })
  
  output$avgProtests2024 <- renderValueBox({
    avg_data <- average_protests_by_year()
    valueBox(value = round(avg_data[avg_data$year == 2024,]$avg_protests_per_day, 2),
             subtitle = "promedio protestas diarias")
  })
  
  output$avgProtests2025 <- renderValueBox({
    avg_data <- average_protests_by_year()
    valueBox(value = round(avg_data[avg_data$year == 2025,]$avg_protests_per_day, 2),
             subtitle = "promedio protestas diarias")
  })
  
  output$mostCommonProtestMonthBox2018 <- renderValueBox({
    month <- Mes_Protestas$mes[Mes_Protestas$year == 2018]
    valueBox(value = month,
             subtitle = "mes con más protestas")
  })
  
  output$mostCommonProtestMonthBox2019 <- renderValueBox({
    month <- Mes_Protestas$mes[Mes_Protestas$year == 2019]
    valueBox(value = month,
             subtitle = "mes con más protestas")
  })
  
  output$mostCommonProtestMonthBox2020 <- renderValueBox({
    month <- Mes_Protestas$mes[Mes_Protestas$year == 2020]
    valueBox(value = month,
             subtitle = "mes con más protestas")
  })
  
  output$mostCommonProtestMonthBox2021 <- renderValueBox({
    month <- Mes_Protestas$mes[Mes_Protestas$year == 2021]
    valueBox(value = month,
             subtitle = "mes con más protestas")
  })
  
  output$mostCommonProtestMonthBox2022 <- renderValueBox({
    month <- Mes_Protestas$mes[Mes_Protestas$year == 2022]
    valueBox(value = month,
             subtitle = "mes con más protestas")
  })
  
  output$mostCommonProtestMonthBox2023 <- renderValueBox({
    month <- Mes_Protestas$mes[Mes_Protestas$year == 2023]
    valueBox(value = month,
             subtitle = "mes con más protestas")
  })
  
  output$mostCommonProtestMonthBox2024 <- renderValueBox({
    month <- Mes_Protestas$mes[Mes_Protestas$year == 2024]
    valueBox(value = month,
             subtitle = "mes con más protestas")
  })
  
  output$mostCommonProtestMonthBox2025 <- renderValueBox({
    month <- Mes_Protestas$mes[Mes_Protestas$year == 2025]
    valueBox(value = month,
             subtitle = "mes con más protestas")
  })
  
  output$mostCommonProtestDayBox2018 <- renderValueBox({
    day <- Dias_Protestas$dia_semana[Dias_Protestas$year == 2018]
    valueBox(value = day,
             subtitle = "día más frecuente de protestas")
  })
  
  output$mostCommonProtestDayBox2019 <- renderValueBox({
    day <- Dias_Protestas$dia_semana[Dias_Protestas$year == 2019]
    valueBox(value = day,
             subtitle = "día más frecuente de protestas")
  })
  
  output$mostCommonProtestDayBox2020 <- renderValueBox({
    day <- Dias_Protestas$dia_semana[Dias_Protestas$year == 2020]
    valueBox(value = day,
             subtitle = "día más frecuente de protestas")
  })
  
  output$mostCommonProtestDayBox2021 <- renderValueBox({
    day <- Dias_Protestas$dia_semana[Dias_Protestas$year == 2021]
    valueBox(value = day,
             subtitle = "día más frecuente de protestas")
  })
  
  output$mostCommonProtestDayBox2022 <- renderValueBox({
    day <- Dias_Protestas$dia_semana[Dias_Protestas$year == 2022]
    valueBox(value = day,
             subtitle = "día más frecuente de protestas")
  })
  
  output$mostCommonProtestDayBox2023 <- renderValueBox({
    day <- Dias_Protestas$dia_semana[Dias_Protestas$year == 2023]
    valueBox(value = day,
             subtitle = "día más frecuente de protestas")
  })
  
  output$mostCommonProtestDayBox2024 <- renderValueBox({
    day <- Dias_Protestas$dia_semana[Dias_Protestas$year == 2024]
    valueBox(value = day,
             subtitle = "día más frecuente de protestas")
  })
  
  output$mostCommonProtestDayBox2025 <- renderValueBox({
    day <- Dias_Protestas$dia_semana[Dias_Protestas$year == 2025]
    valueBox(value = day,
             subtitle = "día más frecuente de protestas")
  })
  
  output$yearSpecificBoxes <- renderUI({
    year <- input$yearSelect
    
    if (year == "2018") {
      tagList(fluidRow(column(
        12, valueBoxOutput("totalProtests2018")
      )),
      fluidRow(column(
        12, valueBoxOutput("avgProtests2018")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestMonthBox2018")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestDayBox2018")
      )))
    } else if (year == "2019") {
      tagList(fluidRow(column(
        12, valueBoxOutput("totalProtests2019")
      )),
      fluidRow(column(
        12, valueBoxOutput("avgProtests2019")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestMonthBox2019")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestDayBox2019")
      )))
    } else if (year == "2020") {
      tagList(fluidRow(column(
        12, valueBoxOutput("totalProtests2020")
      )),
      fluidRow(column(
        12, valueBoxOutput("avgProtests2020")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestMonthBox2020")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestDayBox2020")
      )))
    } else if (year == "2021") {
      tagList(fluidRow(column(
        12, valueBoxOutput("totalProtests2021")
      )),
      fluidRow(column(
        12, valueBoxOutput("avgProtests2021")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestMonthBox2021")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestDayBox2021")
      )))
    } else if (year == "2022") {
      tagList(fluidRow(column(
        12, valueBoxOutput("totalProtests2022")
      )),
      fluidRow(column(
        12, valueBoxOutput("avgProtests2022")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestMonthBox2022")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestDayBox2022")
      )))
    } else if (year == "2023") {
      tagList(fluidRow(column(
        12, valueBoxOutput("totalProtests2023")
      )),
      fluidRow(column(
        12, valueBoxOutput("avgProtests2023")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestMonthBox2023")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestDayBox2023")
      )))
    } else if (year == "2024") {
      tagList(fluidRow(column(
        12, valueBoxOutput("totalProtests2024")
      )),
      fluidRow(column(
        12, valueBoxOutput("avgProtests2024")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestMonthBox2024")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestDayBox2024")
      )))
    } else if (year == "2025") {
      tagList(fluidRow(column(
        12, valueBoxOutput("totalProtests2025")
      )),
      fluidRow(column(
        12, valueBoxOutput("avgProtests2025")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestMonthBox2025")
      )),
      fluidRow(column(
        12, valueBoxOutput("mostCommonProtestDayBox2025")
      )))
    }
  })
  
  output$dailyProtestsPlot <- renderPlotly({
    data <- Protestas %>%
      mutate(
        Fecha = as.Date(event_date, format = "%Y-%m-%d"),
        Anio = year(Fecha),
        Mes = month(Fecha),
        Dia = day(Fecha)
      ) %>%
      group_by(Fecha, Anio) %>%
      summarise(Cantidad_Protestas = n(), .groups = 'drop')
    
    p <-
      ggplot(data, aes(x = Fecha, y = Cantidad_Protestas, group = Anio)) +
      geom_line(color = "#F8766D") +
      facet_grid(rows = vars(Anio)) +
      facet_wrap(
        ~ Anio,
        scales = "free_x",
        ncol = 2,
        strip.position = "bottom"
      ) +
      theme_minimal() +
      labs(title = "Protestas diarias agrupadas por año<br>",
           x = NULL, y = NULL) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_y_continuous(breaks = seq(0, max(data$Cantidad_Protestas, na.rm = TRUE), by = 20)) +
      geom_point(
        aes(
          text = paste(
            "Fecha:",
            format(Fecha, "%d-%m-%Y"),
            "<br>Cantidad de Protestas:",
            Cantidad_Protestas
          )
        ),
        size = 0.01,
        alpha = 0,
        color = "#F8766D"
      ) +
      theme(
        strip.text = element_text(
          hjust = 0.5,
          face = "bold",
          size = 12
        ),
        strip.background = element_rect(fill = "grey", colour = "grey"),
        plot.title = element_text(
          face = "bold",
          colour = "#3C3C3C",
          size = 20
        ),
        plot.margin = margin(
          t = 30,
          r = 10,
          b = 10,
          l = 10,
          unit = "pt"
        ),
        panel.spacing = unit(1.5, "lines")
      )
    
    ggplotly(p, tooltip = "text", height = 700)
  })
  
  output$provinciaPlot <- renderPlot({
    selected_year <- as.numeric(input$yearInputPcias)
    
    data <- Resumen_Provincias %>% filter(year == selected_year)
    
    p_pcias <-
      ggplot(data, aes(
        x = reorder(provincia, porcentaje_pcia),
        y = protestas_pcia,
        fill = as.factor(year)
      )) +
      geom_bar(stat = "identity", width = 0.7) +
      scale_y_continuous(limits = c(0, 600)) +
      labs(
        title = "Concentración de protestas por provincia",
        subtitle = "Porcentajes sobre el total de observaciones registradas cada año \n",
        caption = "\n CC By-NC-SA 4.0 marialasa.ar",
        x = NULL,
        y = "\n Cantidad de protestas",
      ) +
      coord_flip() +
      geom_text(aes(label = paste0(round(
        porcentaje_pcia, 2
      ), "%")), hjust = -0.2, size = 4) +
      theme_test() +
      theme(
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        legend.position = "None",
        plot.title = element_text(
          face = "bold",
          colour = "#3C3C3C",
          size = 22
        ),
        plot.subtitle = element_text(colour = "#838383",
                                     size = 14),
        plot.caption = element_text(
          colour = "#838383",
          size = 12,
          hjust = 0,
          margin = margin(r = 20)
        ),
        plot.margin = unit(c(2, 2, 2, 2), "lines")
      )
    
    p_pcias
    
  })
  
  output$plotActores <- renderPlot({
    Actores_Plot <- Protestas %>%
      filter(year == as.numeric(input$yearInputActors)) %>%
      separate_rows(assoc_actor_1, sep = "; ") %>%
      mutate(assoc_actor_1 = trimws(assoc_actor_1)) %>%
      mutate(assoc_actor_1 = replace(
        assoc_actor_1,
        assoc_actor_1 == "",
        "Self-Organized Protesters"
      )) %>%
      mutate(
        assoc_actor_1 = replace(
          assoc_actor_1,
          assoc_actor_1 == "UNTRA: National Union of Transport and Allied Workers of the Republic of Argentina",
          "UNTRA"
        )
      ) %>%
      mutate(
        assoc_actor_1 = replace(
          assoc_actor_1,
          assoc_actor_1 == "CTEP: Confederation of Workers of the Popular Economy",
          "CTEP"
        )
      ) %>%
      mutate(
        assoc_actor_1 = replace(
          assoc_actor_1,
          assoc_actor_1 == "UOCRA: Construction Workers' Union of Argentina",
          "UOCRA"
        )
      ) %>%
      mutate(
        assoc_actor_1 = replace(
          assoc_actor_1,
          assoc_actor_1 == "CTERA: Confederation of Education Workers of the Republic of Argentina",
          "CTERA"
        )
      ) %>%
      mutate(
        assoc_actor_1 = replace(
          assoc_actor_1,
          assoc_actor_1 == "CTA-A: Argentine Workers' Central Union - Autonomous",
          "CTA Autonomous"
        )
      ) %>%
      mutate(
        assoc_actor_1 = replace(
          assoc_actor_1,
          assoc_actor_1 == "UTEP: Union of Workers of the Popular Economy",
          "UTEP"
        )
      ) %>%
      group_by(assoc_actor_1) %>%
      summarise(total_protests = n()) %>%
      mutate(percentage = (total_protests / sum(total_protests)) * 100) %>%
      arrange(desc(total_protests))
    
    Top_20_Argentina <- Actores_Plot %>% top_n(20, total_protests)
    
    p_actors <-
      ggplot(Top_20_Argentina, aes(
        x = reorder(assoc_actor_1, total_protests),
        y = total_protests
      )) +
      geom_bar(stat = "identity", fill = "#F8766D", width = 0.7) +
      geom_text(
        aes(label = paste0(round(percentage, 2), "%")),
        hjust = -0.2,
        color = "black",
        size = 4
      ) +
      coord_flip() +
      scale_y_continuous(expand = expansion(add = c(20, 200))) +
      labs(
        x = NULL,
        y = "\n Cantidad de protestas",
        title = "Actores más activos en protestas sociales",
        subtitle = "Top 20 anual \n",
        caption = "\n CC By-NC-SA 4.0 marialasa.ar"
      ) +
      theme_test() +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        plot.title = element_text(
          face = "bold",
          colour = "#3C3C3C",
          size = 22
        ),
        plot.subtitle = element_text(colour = "#838383",
                                     size = 14),
        plot.caption = element_text(
          colour = "#838383",
          size = 12,
          hjust = 0,
          margin = margin(r = 20)
        ),
        plot.margin = unit(c(2, 2, 2, 2), "lines")
      )
    
    p_actors
  })
  
  output$dynamicImage <- renderImage({
    image_path <- switch(
      input$yearInputHotspots,
      "2018" = "www/LISA_2018.png",
      "2019" = "www/LISA_2019.png",
      "2020" = "www/LISA_2020.png",
      "2021" = "www/LISA_2021.png",
      "2022" = "www/LISA_2022.png",
      "2023" = "www/LISA_2023.png",
      "2024" = "www/LISA_2024.png",
      "2025" = "www/LISA_2025.png",
      NULL
    )
    
    if (!is.null(image_path)) {
      list(
        src = image_path,
        alt = paste("Mapa LISA", input$yearInputHotspots),
        style = "margin-top: 20px;",
        width = "75%",
        height = "auto"
      )
    }
  }, deleteFile = FALSE)
}

#### Play ####

shinyApp(ui = ui, server = server)