############################################################
# Social Protest Map in Argentina (ACLED) — Shiny App
# https://marialasa.shinyapps.io/Mapa_ProtestaSocial/
############################################################

#### Libraries ####
library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shinydashboard)

#### Data loading ####
# Expected file: Argentina_ACLED.csv in the working directory
# Expected columns (at minimum): event_date, latitude, longitude, admin1, notes, assoc_actor_1
Protestas <- read.csv("Argentina_ACLED.csv", stringsAsFactors = FALSE)

#### Date parsing (robust) ####
# ACLED exports often provide event_date like "01 January 2018".
# This parsing can fail if OS locale is not English; we include a fallback.
Protestas$event_date_parsed <- as.Date(Protestas$event_date, format = "%d %B %Y")

# Fallback: if parsing failed completely (e.g., event_date already in ISO "YYYY-MM-DD")
if (all(is.na(Protestas$event_date_parsed))) {
  Protestas$event_date_parsed <- as.Date(Protestas$event_date)
}

# Canonical date + derived time fields
Protestas <- Protestas %>%
  mutate(
    event_date = event_date_parsed,
    year = lubridate::year(event_date),
    weekday_en = weekdays(event_date),
    month_en = format(event_date, "%B")
  )

#### Translate weekday/month to Spanish + ordered factors ####
days_map <- c(
  "Monday" = "Lunes",
  "Tuesday" = "Martes",
  "Wednesday" = "Miércoles",
  "Thursday" = "Jueves",
  "Friday" = "Viernes",
  "Saturday" = "Sábado",
  "Sunday" = "Domingo"
)

months_map <- c(
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

weekday_levels <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
month_levels <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

Protestas <- Protestas %>%
  mutate(
    weekday_es = dplyr::recode(weekday_en, !!!days_map),
    month_es = dplyr::recode(month_en, !!!months_map),
    dia_semana = factor(weekday_es, levels = weekday_levels),
    mes = factor(month_es, levels = month_levels)
  )

#### Precompute exploded actor dataset (performance improvement) ####
# assoc_actor_1 can contain multiple actors separated by "; "
# We explode it once and reuse everywhere (map filters, actor plots, etc.)
Protestas_actores <- Protestas %>%
  tidyr::separate_rows(assoc_actor_1, sep = ";\\s*") %>%
  mutate(
    assoc_actor_1 = trimws(assoc_actor_1),
    assoc_actor_1 = if_else(is.na(assoc_actor_1) | assoc_actor_1 == "",
                            "Self-Organized Protesters",
                            assoc_actor_1)
  )

#### Actor summary table (year, actor) ####
Actores <- Protestas_actores %>%
  group_by(year, assoc_actor_1) %>%
  summarise(total_protests = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(percentage = (total_protests / sum(total_protests)) * 100) %>%
  ungroup() %>%
  arrange(desc(total_protests))

#### Most frequent weekday/month by year ####
Dias_Protestas <- Protestas %>%
  count(year, dia_semana, name = "cantidad_protestas") %>%
  group_by(year) %>%
  slice_max(cantidad_protestas, n = 1, with_ties = FALSE) %>%
  ungroup()

Mes_Protestas <- Protestas %>%
  count(year, mes, name = "cantidad_protestas") %>%
  group_by(year) %>%
  slice_max(cantidad_protestas, n = 1, with_ties = FALSE) %>%
  ungroup()

#### Province summary (share of yearly protests) ####
Resumen_Provincias <- Protestas %>%
  filter(!is.na(year), !is.na(admin1)) %>%
  group_by(year, admin1) %>%
  summarise(protestas_pcia = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(total_protestas = sum(protestas_pcia),
         porcentaje_pcia = (protestas_pcia / total_protestas) * 100) %>%
  ungroup() %>%
  mutate(admin1 = ifelse(admin1 == "Ciudad Autonoma de Buenos Aires", "CABA", admin1)) %>%
  rename(provincia = admin1)

#### Helper: actor label shortening for plotting ####
shorten_actor_names <- function(x) {
  x <- ifelse(x == "", "Self-Organized Protesters", x)
  x <- ifelse(x == "UNTRA: National Union of Transport and Allied Workers of the Republic of Argentina", "UNTRA", x)
  x <- ifelse(x == "CTEP: Confederation of Workers of the Popular Economy", "CTEP", x)
  x <- ifelse(x == "UOCRA: Construction Workers' Union of Argentina", "UOCRA", x)
  x <- ifelse(x == "CTERA: Confederation of Education Workers of the Republic of Argentina", "CTERA", x)
  x <- ifelse(x == "CTA-A: Argentine Workers' Central Union - Autonomous", "CTA Autonomous", x)
  x <- ifelse(x == "UTEP: Union of Workers of the Popular Economy", "UTEP", x)
  x
}

#### UI ####
ui <- fluidPage(
  titlePanel("🪧 Social Protest Map in Argentina"),
  tabsetPanel(
    tabPanel(
      "Map",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput(
            "dateRange",
            "Date range",
            start = "2018-01-01",
            end = "2025-05-31",
            format = "dd-mm-yyyy"
          ),
          selectInput(
            "provinceInput",
            "Filter by province(s)",
            choices = sort(unique(Protestas$admin1)),
            multiple = TRUE
          ),
          selectInput(
            "actorInput",
            "Filter by convener (assoc_actor_1)",
            choices = c("All", sort(unique(Actores$assoc_actor_1))),
            selected = "All"
          ),
          radioButtons(
            "mapType",
            "Map type",
            choices = c("Marker map" = "markers", "Heat map" = "heat"),
            selected = "markers"
          ),
          HTML(
            '<br>
            <h4>About this project</h4>
            <p>💜 Developed by <a href="http://marialasa.ar" target="_blank">Mar&iacute;a de los &Aacute;ngeles Lasa</a> using data from the <em>Armed Conflict Location and Event Data Project</em> (<a href="https://acleddata.com/data-export-tool/" target="_blank">ACLED</a>).</p>
            <p>👩‍💻 Code on <a href="https://github.com/marialasa/mapa_protestas/blob/main/Mapa_Protesta.R" target="_blank">GitHub</a>.</p>
            <p>📅 Protest date range: 2018-01-01 to 2025-05-31 (monthly updates).</p>
            <p>💻 Web app is <strong>not</strong> optimized for mobile.</p>
            <p>⌛ The app may take a few seconds to load due to server-side processing.</p>
            <p>⚠️ License: CC BY-NC-SA 4.0</p>'
          )
        ),
        mainPanel(
          leafletOutput("map", width = "100%", height = "750px")
        )
      )
    ),

    tabPanel(
      "Statistics",
      fluidRow(
        column(10, plotlyOutput("dailyProtestsPlot", height = "1000px")),
        column(
          2,
          tags$br(), tags$br(),
          selectInput(
            "yearSelect",
            "Select a year",
            choices = as.character(2018:2025),
            selected = "2018"
          ),
          uiOutput("yearSpecificBoxes")
        )
      ),
      tags$style(HTML(".value-box { border: 1px solid #333; }"))
    ),

    tabPanel(
      "Provinces",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "yearInputPcias",
            "Select year",
            choices = as.character(2018:2025),
            selected = "2018"
          )
        ),
        mainPanel(
          fluidRow(column(12, plotOutput("provinciaPlot", width = "80%", height = "600px")))
        )
      )
    ),

    tabPanel(
      "Actors",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "yearInputActors",
            "Select year",
            choices = as.character(2018:2025),
            selected = "2018"
          )
        ),
        mainPanel(
          plotOutput("plotActores", width = "80%", height = "600px")
        )
      )
    ),

    tabPanel(
      "Hotspots",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "yearInputHotspots",
            "Select year",
            choices = as.character(2018:2025),
            selected = "2018"
          ),
          HTML(
            '<br>
            <h4>Note</h4>
            <p>🗺️ The <em>hotspot</em> maps were produced using <em>Local Indicators of Spatial Association</em> (LISA),
            a statistical method to identify spatial clusters and outliers.</p>
            <p>👉 For a detailed methodology explanation, <a href="https://rpubs.com/mlasa/lacallehabla" target="_blank">click here</a>.</p>'
          )
        ),
        mainPanel(
          imageOutput("dynamicImage")
        )
      )
    )
  )
)

#### Server ####
server <- function(input, output, session) {

  #### MAP ####
  output$map <- renderLeaflet({
    data <- Protestas_actores

    # Filter by date range (canonical event_date)
    if (!is.null(input$dateRange)) {
      data <- data %>%
        filter(!is.na(event_date)) %>%
        filter(event_date >= input$dateRange[1], event_date <= input$dateRange[2])
    }

    # Filter by province
    if (!is.null(input$provinceInput) && length(input$provinceInput) > 0) {
      data <- data %>% filter(admin1 %in% input$provinceInput)
    }

    # Filter by actor
    if (!is.null(input$actorInput) && input$actorInput != "All") {
      data <- data %>% filter(assoc_actor_1 == input$actorInput)
    }

    # Basic sanity filter for coordinates
    data <- data %>% filter(!is.na(latitude), !is.na(longitude))

    map <- leaflet(data) %>% addTiles()

    if (input$mapType == "markers") {
      map %>% addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~notes,
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

  #### Yearly totals (for totalProtestsYYYY boxes) ####
  total_protests_by_year <- reactive({
    Protestas %>%
      filter(!is.na(year)) %>%
      count(year, name = "total_protests")
  })

  renderTotalProtestsBox <- function(y) {
    renderValueBox({
      totals <- total_protests_by_year()
      v <- totals %>% filter(year == y) %>% pull(total_protests)
      if (length(v) == 0) v <- 0
      valueBox(value = v, subtitle = paste("protests recorded in", y))
    })
  }

  # Create output$totalProtests2018 ... output$totalProtests2025
  for (y in 2018:2025) {
    local({
      yy <- y
      output[[paste0("totalProtests", yy)]] <- renderTotalProtestsBox(yy)
    })
  }

  #### Average protests/day by year (uses observed days, handles partial years) ####
  average_protests_by_year <- reactive({
    Protestas %>%
      filter(!is.na(event_date), !is.na(year)) %>%
      group_by(year) %>%
      summarise(
        total_protests = n(),
        observed_days = n_distinct(event_date),
        avg_protests_per_day = total_protests / observed_days,
        .groups = "drop"
      )
  })

  renderAvgBox <- function(y) {
    renderValueBox({
      avg_data <- average_protests_by_year()
      v <- avg_data %>% filter(year == y) %>% pull(avg_protests_per_day)
      if (length(v) == 0) v <- 0
      valueBox(value = round(v, 2), subtitle = "average protests per day")
    })
  }

  for (y in 2018:2025) {
    local({
      yy <- y
      output[[paste0("avgProtests", yy)]] <- renderAvgBox(yy)
    })
  }

  #### Most common month/day boxes ####
  renderTopMonthBox <- function(y) {
    renderValueBox({
      m <- Mes_Protestas %>% filter(year == y) %>% pull(mes)
      if (length(m) == 0) m <- NA
      valueBox(value = as.character(m), subtitle = "month with most protests")
    })
  }

  renderTopDayBox <- function(y) {
    renderValueBox({
      d <- Dias_Protestas %>% filter(year == y) %>% pull(dia_semana)
      if (length(d) == 0) d <- NA
      valueBox(value = as.character(d), subtitle = "most frequent protest weekday")
    })
  }

  for (y in 2018:2025) {
    local({
      yy <- y
      output[[paste0("mostCommonProtestMonthBox", yy)]] <- renderTopMonthBox(yy)
      output[[paste0("mostCommonProtestDayBox", yy)]] <- renderTopDayBox(yy)
    })
  }

  #### Dynamic box panel for a selected year (generic) ####
  output$yearSpecificBoxes <- renderUI({
    y <- input$yearSelect
    tagList(
      fluidRow(column(12, valueBoxOutput(paste0("totalProtests", y)))),
      fluidRow(column(12, valueBoxOutput(paste0("avgProtests", y)))),
      fluidRow(column(12, valueBoxOutput(paste0("mostCommonProtestMonthBox", y)))),
      fluidRow(column(12, valueBoxOutput(paste0("mostCommonProtestDayBox", y))))
    )
  })

  #### Daily protests plot (plotly) ####
  output$dailyProtestsPlot <- renderPlotly({
    data <- Protestas %>%
      filter(!is.na(event_date), !is.na(year)) %>%
      group_by(event_date, year) %>%
      summarise(count = n(), .groups = "drop") %>%
      rename(Fecha = event_date, Anio = year, Cantidad_Protestas = count)

    p <- ggplot(data, aes(x = Fecha, y = Cantidad_Protestas, group = Anio)) +
      geom_line(color = "#F8766D") +
      facet_wrap(~Anio, scales = "free_x", ncol = 2, strip.position = "bottom") +
      theme_minimal() +
      labs(
        title = "Daily protests by year<br>",
        x = NULL, y = NULL
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_y_continuous(
        breaks = seq(0, max(data$Cantidad_Protestas, na.rm = TRUE), by = 20)
      ) +
      geom_point(
        aes(text = paste(
          "Date:", format(Fecha, "%d-%m-%Y"),
          "<br>Protests:", Cantidad_Protestas
        )),
        size = 0.01, alpha = 0, color = "#F8766D"
      ) +
      theme(
        strip.text = element_text(hjust = 0.5, face = "bold", size = 12),
        strip.background = element_rect(fill = "grey", colour = "grey"),
        plot.title = element_text(face = "bold", colour = "#3C3C3C", size = 20),
        plot.margin = margin(t = 30, r = 10, b = 10, l = 10, unit = "pt"),
        panel.spacing = unit(1.5, "lines")
      )

    ggplotly(p, tooltip = "text", height = 700)
  })

  #### Provinces plot ####
  output$provinciaPlot <- renderPlot({
    selected_year <- as.numeric(input$yearInputPcias)

    data <- Resumen_Provincias %>% filter(year == selected_year)

    ggplot(data, aes(
      x = reorder(provincia, porcentaje_pcia),
      y = protestas_pcia,
      fill = as.factor(year)
    )) +
      geom_bar(stat = "identity", width = 0.7) +
      labs(
        title = "Protest concentration by province",
        subtitle = "Shares over total annual observations\n",
        caption = "\n CC BY-NC-SA 4.0 marialasa.ar",
        x = NULL,
        y = "\n Protest count"
      ) +
      coord_flip() +
      geom_text(aes(label = paste0(round(porcentaje_pcia, 2), "%")),
                hjust = -0.2, size = 4) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", colour = "#3C3C3C", size = 22),
        plot.subtitle = element_text(colour = "#838383", size = 14),
        plot.caption = element_text(colour = "#838383", size = 12, hjust = 0,
                                    margin = margin(r = 20)),
        plot.margin = unit(c(2, 2, 2, 2), "lines")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  })

  #### Actors plot (Top 20 yearly) ####
  output$plotActores <- renderPlot({
    selected_year <- as.numeric(input$yearInputActors)

    actores_plot <- Protestas_actores %>%
      filter(year == selected_year) %>%
      mutate(assoc_actor_1 = shorten_actor_names(assoc_actor_1)) %>%
      group_by(assoc_actor_1) %>%
      summarise(total_protests = n(), .groups = "drop") %>%
      mutate(percentage = (total_protests / sum(total_protests)) * 100) %>%
      arrange(desc(total_protests))

    top_20 <- actores_plot %>% slice_max(total_protests, n = 20, with_ties = FALSE)

    ggplot(top_20, aes(
      x = reorder(assoc_actor_1, total_protests),
      y = total_protests
    )) +
      geom_bar(stat = "identity", fill = "#F8766D", width = 0.7) +
      geom_text(aes(label = paste0(round(percentage, 2), "%")),
                hjust = -0.2, color = "black", size = 4) +
      coord_flip() +
      scale_y_continuous(expand = expansion(add = c(20, 200))) +
      labs(
        x = NULL,
        y = "\n Protest count",
        title = "Most active actors in social protests",
        subtitle = "Annual Top 20\n",
        caption = "\n CC BY-NC-SA 4.0 marialasa.ar"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        plot.title = element_text(face = "bold", colour = "#3C3C3C", size = 22),
        plot.subtitle = element_text(colour = "#838383", size = 14),
        plot.caption = element_text(colour = "#838383", size = 12, hjust = 0,
                                    margin = margin(r = 20)),
        plot.margin = unit(c(2, 2, 2, 2), "lines")
      )
  })

  #### Hotspots images (LISA maps stored in www/) ####
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
        alt = paste("LISA map", input$yearInputHotspots),
        style = "margin-top: 20px;",
        width = "75%",
        height = "auto"
      )
    }
  }, deleteFile = FALSE)
}

#### Run ####
shinyApp(ui = ui, server = server)
