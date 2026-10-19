# ===================================================================
# [INICIO LIBRERÍAS Y DATOS]
# ===================================================================
library(shiny)
library(leaflet)
library(ggplot2)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(hexbin)

dt <- readRDS("../../Data/data.rds")
# [FIN LIBRERÍAS Y DATOS]

# ===================================================================
# [INICIO VARIABLES GLOBALES Y DICCIONARIOS]
# ===================================================================
coords_ofi <- unique(dt[, c("ofi", "lat", "lon")])
year_min <- 2000
year_max <- 2022

# Paletas de colores centralizadas
colores_gpo <- c("Gav" = "darkgreen", "Gui" = "darkblue", "MyR-RM" = "purple")
colores_nombres <- c("Gavilán" = "darkgreen", "Guitarra" = "darkblue", "Mantarrayas" = "purple", "Total" = "steelblue")
colores_edo <- c("B.C." = "lightskyblue3", "B.C.S." = "cornflowerblue", "Sonora" = "#445AB4", "Sinaloa" = "steelblue")
colores_temporada <- c("Cuaresma (Mar-Abr)" = "#E69F00", "Resto del Año" = "grey60")

# Diccionarios para traducir acrónimos a nombres limpios rápidamente
dict_edos <- c("SON" = "Sonora", "SIN" = "Sinaloa", "BC" = "B.C.", "BCS" = "B.C.S.")
dict_gpos <- c("Gav" = "Gavilán", "Gui" = "Guitarra", "MyR-RM" = "Mantarrayas")

# Tema estético estandarizado para todas las gráficas
tema_base_graficas <- function() {
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 16, face = "bold", color = "#2c3e50")
  )
}

# Función de agregación para el mapa
resumir_datos <- function(df) {
  sum_desem <- aggregate(desem_kg ~ ofi + gpo2, data = df, FUN = sum)
  sum_valor <- aggregate(valor_d2 ~ ofi + gpo2, data = df, FUN = sum)
  med_price <- aggregate(price_d2 ~ ofi + gpo2, data = df, FUN = median)
  q1_price  <- aggregate(price_d2 ~ ofi + gpo2, data = df, FUN = function(x) quantile(x, 0.25, na.rm = TRUE))
  q3_price  <- aggregate(price_d2 ~ ofi + gpo2, data = df, FUN = function(x) quantile(x, 0.75, na.rm = TRUE))

  names(sum_desem)[3] <- "desem_sum"
  names(sum_valor)[3] <- "valor_sum"
  names(med_price)[3] <- "price_median"
  names(q1_price)[3]  <- "price_q1"
  names(q3_price)[3]  <- "price_q3"

  tmp <- Reduce(function(a,b) merge(a,b), list(sum_desem, sum_valor, med_price, q1_price, q3_price))
  merge(tmp, coords_ofi, by = "ofi", all.x = TRUE)
}
# [FIN VARIABLES GLOBALES Y DICCIONARIOS]

# ===================================================================
# [INICIO UI] INTERFAZ DE USUARIO
# ===================================================================
ui <- page_sidebar(
  
  title = HTML("Monitoreo Pesquero de Batoideos - Noroeste <span style='font-size: 0.6em; color: gray;'>v0.9.0-beta</span>"),
  theme = bs_theme(bootswatch = "litera"),
  
  tags$head(
    tags$style(HTML("
      .shiny-input-container { margin-bottom: 0px; margin-top: 0px; padding-top: 0px;}
      .shiny-options-group { margin-top: 0px; margin-bottom: 0px; }
      .control-label { margin-bottom: 0px; padding-bottom: 0px;}
      .card-body { padding-top: 5px !important; }
      .nav-tabs { border-bottom: 1px solid #dee2e6; }
    "))
  ),

  # [INICIO BARRA LATERAL]
  sidebar = sidebar(
    title = "Filtros Principales",
    selectInput("edo", "Estado:", choices = c("Todos", unique(dt$edo)), selected = "Todos", multiple = TRUE),
    radioButtons("modo_tiempo", "Modo temporal:", choices = c("Por Año específico", "Por Época de manejo"), selected = "Por Año específico"),
    conditionalPanel(condition = "input.modo_tiempo == 'Por Año específico'",
      sliderInput("year", "Año (*)", min = year_min, max = year_max, value = year_min, step = 1, sep = "", ticks = TRUE, animate = animationOptions(interval = 1400, loop = TRUE))),
    conditionalPanel(condition = "input.modo_tiempo == 'Por Época de manejo'",
      checkboxGroupInput("epoca_veda", "Época de manejo:", choiceNames = c("Pre-veda (2000-2011)", "Veda (2012-2022)"), choiceValues = c("pre", "veda"), selected = c("pre", "veda"))),
    radioButtons("m_veda_mode", "Periodo mensual:", choices  = c("No separar", "Pre veda", "Veda", "Post Veda"), selected = "No separar"),
    checkboxGroupInput("gpo2", "Grupos:", 
                       choiceNames = list(HTML("<span style='color:darkgreen;font-weight:bold;'>Gavilán</span>"), HTML("<span style='color:darkblue;font-weight:bold;'>Guitarra</span>"), HTML("<span style='color:purple;font-weight:bold;'>Mantas y Rayas - RM</span>")),
                       choiceValues = c("Gav","Gui","MyR-RM"), selected = c("Gav","Gui","MyR-RM"))
  ),
  # [FIN BARRA LATERAL]
  
  # [INICIO CONTENEDOR DE PESTAÑAS]
  navset_card_underline(
    
    # [INICIO PESTAÑA: ACERCA DE]
    nav_panel("Acerca de",
              br(),
              card(
                card_header("Información del Proyecto y Uso del Tablero"),
                div(style = "padding: 20px;",
                  h4("Versión (Beta)"),
                  p("Esta aplicación se encuentra en fase de desarrollo."),
                  hr(),
                  h4("Instrucciones de Uso"),
                  tags$ul(
                    tags$li(strong("Filtros Globales: "), "Cualquier selección en la barra lateral afectará a todas las pestañas en tiempo real."),
                    tags$li(strong("Mapa Interactivo: "), "Haz clic en los círculos del mapa para ver el desglose exacto por oficina."),
                    tags$li(strong("Vistas Interactivas: "), "Usa los botones superiores en las gráficas históricas para alternar análisis y escalas logarítmicas.")
                  )
                )
              )
    ),
    # [FIN PESTAÑA: ACERCA DE]

    # [INICIO PESTAÑA: MAPA Y RESUMEN]
    nav_panel("Mapa y Resumen",
              div(style = "padding-top: 15px; padding-bottom: 5px;",
                  # [INICIO KPIs]
                  fluidRow(
                    column(4, div(style="background-color:#f8f9fa; border: 1px solid #ddd; border-radius:8px; height: 90px; display: flex; flex-direction: column; justify-content: center; align-items: center;", div(style="margin:0; color:#555; font-size:14px; font-weight:bold;", "Peso Desembarcado (t)"), div(style="margin:3px 0 0 0; color:#2c3e50; font-size:22px; font-weight:bold;", textOutput("kpi_peso")))),
                    column(4, div(style="background-color:#f8f9fa; border: 1px solid #ddd; border-radius:8px; height: 90px; display: flex; flex-direction: column; justify-content: center; align-items: center;", div(style="margin:0; color:#555; font-size:14px; font-weight:bold;", "Valor Total (MXN)"), div(style="margin:3px 0 0 0; color:#2c3e50; font-size:22px; font-weight:bold;", textOutput("kpi_valor")))),
                    column(4, div(style="background-color:#f8f9fa; border: 1px solid #ddd; border-radius:8px; height: 90px; display: flex; flex-direction: column; justify-content: center; align-items: center;", div(style="margin:0; color:#555; font-size:14px; font-weight:bold;", "Precio Mediano (Q1 - Q3)"), div(style="margin:3px 0 0 0; color:#2c3e50; font-size:18px; font-weight:bold;", textOutput("kpi_precio"))))
                  )
                  # [FIN KPIs]
              ),
              card(full_screen = TRUE, card_header("Distribución Espacial y Proporción Estatal"), uiOutput("layout_dinamico"))
    ),
    # [FIN PESTAÑA: MAPA Y RESUMEN]
    
    # [INICIO PESTAÑA: TENDENCIA HISTÓRICA]
    nav_panel("Tendencia Histórica",
              br(),
              card(
                full_screen = TRUE, card_header("Evolución Interanual y Estacional"),
                div(style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 30px; margin-bottom: 0px;",
                    radioButtons("var_tendencia", "Variable:", choices = c("Peso (t)", "Precio"), inline = TRUE),
                    conditionalPanel(condition = "input.var_tendencia == 'Peso (t)'", radioButtons("escala_peso", "Visualización del Peso:", choices = c("Volumen (Lineal)", "Composición 100%"), inline = TRUE)),
                    conditionalPanel(condition = "input.var_tendencia == 'Precio'", radioButtons("desglosar_precio", "Desglosar por especie:", choices = c("No", "Sí"), inline = TRUE))
                ),
                withSpinner(plotOutput("plot_tendencia", height = "55vh"), type = 8, color = "#2c3e50")
              )
    ),
    # [FIN PESTAÑA: TENDENCIA HISTÓRICA]

    # [INICIO PESTAÑA: MECÁNICA DEL MERCADO]
    nav_panel("Dinámica del Mercado",
              br(),
              card(
                full_screen = TRUE, card_header("Oferta-Demanda"),
                div(style = "text-align: center; margin-bottom: 5px; padding: 2px; background-color: #f8f9fa; border-radius: 8px;",
                    radioButtons("vista_oferta", label = NULL, choices = c("Clásica (Tendencia)", "Densidad (Hexágonos)", "Efecto Cuaresma"), inline = TRUE)),
                withSpinner(plotOutput("plot_mercado", height = "70vh"), type = 8, color = "#2c3e50")
              )
    )
    
  )
  # [FIN CONTENEDOR DE PESTAÑAS]
)
# ===================================================================
# [FIN UI]
# ===================================================================

# ===================================================================
# [INICIO SERVER] LÓGICA DEL SERVIDOR
# ===================================================================
server <- function(input, output, session) {
  
  # [INICIO OBSERVE: ESTADOS]
  observeEvent(input$edo, {
    if ("Todos" %in% input$edo && length(input$edo) > 1) { updateSelectInput(session, "edo", selected = setdiff(input$edo, "Todos")) } 
    else if (length(input$edo) == 0) { updateSelectInput(session, "edo", selected = "Todos") }
  }, ignoreInit = TRUE)
  # [FIN OBSERVE: ESTADOS]

  # [INICIO REACTIVE: DATOS FILTRADOS]
  datos_filtrados <- reactive({
    req(input$edo, input$gpo2)
    
    if (input$modo_tiempo == "Por Año específico") {
      df <- dt[dt$year == input$year, ]
    } else {
      años_sel <- c()
      if ("pre" %in% input$epoca_veda) años_sel <- c(años_sel, 2000:2011)
      if ("veda" %in% input$epoca_veda) años_sel <- c(años_sel, 2012:2022)
      if (length(años_sel) == 0) return(dt[FALSE, ])
      df <- dt[dt$year %in% años_sel, ]
    }
    
    if (!"Todos" %in% input$edo) df <- df[df$edo %in% input$edo, ]
    df <- df[df$gpo2 %in% input$gpo2, ]
    
    if (input$m_veda_mode != "No separar") {
      modo_internal <- switch(input$m_veda_mode, "Pre veda" = "pre", "Veda" = "veda", "Post Veda" = "post", "No separar")
      if (modo_internal != "No separar") df <- df[df$m_veda == modo_internal, ]
    }
    df
  })
  # [FIN REACTIVE: DATOS FILTRADOS]
  
  # [INICIO RENDER: KPIs]
  output$kpi_peso <- renderText({
    df <- datos_filtrados()
    if(nrow(df) == 0) return("0")
    format(round(sum(df$desem_kg, na.rm=TRUE) / 1000, 1), big.mark=",")
  })
  
  output$kpi_valor <- renderText({
    df <- datos_filtrados()
    if(nrow(df) == 0) return("$ 0")
    paste0("$ ", format(round(sum(df$valor_d2, na.rm=TRUE)), big.mark=","))
  })
  
  output$kpi_precio <- renderText({
    df <- datos_filtrados()
    if(nrow(df) == 0) return("$ 0.00 ($ 0.00 - $ 0.00)")
    paste0("$ ", format(round(median(df$price_d2, na.rm=TRUE), 2), nsmall=2), 
           " ($ ", format(round(quantile(df$price_d2, 0.25, na.rm=TRUE), 2), nsmall=2), 
           " - $ ", format(round(quantile(df$price_d2, 0.75, na.rm=TRUE), 2), nsmall=2), ")")
  })
  # [FIN RENDER: KPIs]

  # [INICIO RENDER: LAYOUT DINÁMICO MAPA]
  output$layout_dinamico <- renderUI({
    if (length(unique(datos_filtrados()$edo)) > 1) {
      layout_columns(col_widths = c(8, 4), withSpinner(leafletOutput("mapa", height = "70vh"), type=8, color="#2c3e50"), withSpinner(plotOutput("ranking_estados", height = "70vh"), type=8, color="#2c3e50"))
    } else {
      withSpinner(leafletOutput("mapa", height = "70vh"), type=8, color="#2c3e50")
    }
  })
  # [FIN RENDER: LAYOUT DINÁMICO MAPA]
  
  # [INICIO RENDER: RANKING ESTADOS]
  output$ranking_estados <- renderPlot({
    df <- datos_filtrados()
    if(nrow(df) == 0) return(NULL)
    
    agg_edo <- aggregate(desem_kg ~ edo, data = df, FUN = sum)
    agg_edo$edo_label <- unname(dict_edos[as.character(agg_edo$edo)])
    agg_edo$pct <- round((agg_edo$desem_kg / sum(agg_edo$desem_kg)) * 100, 1)
    agg_edo$edo_label <- factor(agg_edo$edo_label, levels = agg_edo$edo_label[order(agg_edo$pct)])
    
    limite_y <- min(max(agg_edo$pct) * 1.15, 110)
    
    ggplot(agg_edo, aes(x = edo_label, y = pct)) +
      geom_segment(aes(x = edo_label, xend = edo_label, y = 0, yend = pct), color = "grey70", linewidth = 1) +
      geom_point(aes(color = edo_label), size = 6) +
      geom_text(aes(label = paste0(pct, "%")), hjust = -0.4, size = 5, fontface = "bold", color = "#2c3e50") +
      scale_color_manual(values = colores_edo) +
      scale_y_continuous(limits = c(0, limite_y)) +
      coord_flip() + 
      tema_base_graficas() +
      labs(x = "", y = "Proporción del Volumen (%)", title = "Proporción por Estado") +
      theme(legend.position = "none", panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold", size = 12))
  })
  # [FIN RENDER: RANKING ESTADOS]
  
  # [INICIO REACTIVE: DATOS RESUMEN MAPA]
  datos_resumen <- reactive({
    df <- datos_filtrados()
    if (nrow(df) == 0) return(df)
    
    agg <- resumir_datos(df)
    n <- length(input$gpo2)
    offset <- 0.03
    gs <- sort(input$gpo2)
    agg$lon_plot <- agg$lon
    agg$lat_plot <- agg$lat
    
    for(i in seq_len(n)) {
      idx <- which(agg$gpo2 == gs[i])
      if (n == 2) agg$lon_plot[idx] <- agg$lon[idx] + c(-offset, offset)[i]
      if (n == 3) {
        if (i == 1) agg$lon_plot[idx] <- agg$lon[idx] - offset
        if (i == 2) agg$lon_plot[idx] <- agg$lon[idx] + offset
        if (i == 3) agg$lat_plot[idx] <- agg$lat[idx] + offset
      }
    }
    
    agg$radius <- pmin(pmax(sqrt(agg$desem_sum / max(1, length(unique(df$year))))/50, 3), 25)
    agg
  })
  # [FIN REACTIVE: DATOS RESUMEN MAPA]
  
  # [INICIO RENDER: MAPA LEAFLET]
  output$mapa <- renderLeaflet({
    df <- datos_resumen()
    pal <- colorFactor(palette = colores_gpo, domain = names(colores_gpo))
    
    m <- leaflet() |> addProviderTiles(providers$CartoDB.Positron) |> setView(lng = -110, lat = 27, zoom = 6.2)
    if (nrow(df) == 0) return(m)
    
    m |> addCircleMarkers(
        data = df, lng = ~lon_plot, lat = ~lat_plot, radius = ~radius,
        color = ~pal(gpo2), fillColor = ~pal(gpo2), fillOpacity = 0.85, weight = 2,
        layerId = ~paste(ofi, gpo2, sep = "||"),
        popup = ~paste0("<b>Oficina:</b> ", ofi, "<br><b>Grupo:</b> ", unname(dict_gpos[gpo2]), 
                        "<br><b>Peso (kg):</b> ", format(round(desem_sum), big.mark=","),
                        "<br><b>Valor (MXN):</b> $", format(round(valor_sum), big.mark=","),
                        "<br><br><b>Precio (MXN/kg):</b><br>Mediana = $", round(price_median,2), 
                        "<br>Q1 = $", round(price_q1,2), "<br>Q3 = $", round(price_q3,2))
      )
  })
  # [FIN RENDER: MAPA LEAFLET]

  # [INICIO RENDER: PLOT TENDENCIA]
  output$plot_tendencia <- renderPlot({
    df <- datos_filtrados()
    if(nrow(df) == 0) return(NULL)
    
    df$desem_t <- df$desem_kg / 1000
    df$gpo2_label <- unname(dict_gpos[as.character(df$gpo2)])
    
    if ("Todos" %in% input$edo) df$edo_facet <- "" 
    else df$edo_facet <- factor(unname(dict_edos[as.character(df$edo)]))
    
    escala_x <- if(max(df$year) == min(df$year)) scale_x_date(date_breaks="1 month", date_labels="%b") else scale_x_date(date_breaks="1 year", date_labels="%Y")
    tema_fondo_facet <- if("Todos" %in% input$edo) element_blank() else element_rect(fill = "#f8f9fa", color = NA)

    if (input$var_tendencia == "Peso (t)") {
      agg_esp <- aggregate(desem_t ~ year + mes2 + edo_facet + gpo2_label, data = df, FUN = sum)
      
      if (input$escala_peso == "Volumen (Lineal)") {
        agg_tot <- aggregate(desem_t ~ year + mes2 + edo_facet, data = df, FUN = sum)
        agg_tot$gpo2_label <- "Total"
        df_plot <- rbind(agg_esp, agg_tot)
        df_plot$fecha <- as.Date(paste(df_plot$year, df_plot$mes2, "01", sep = "-"))
        df_plot$gpo2_label <- factor(df_plot$gpo2_label, levels = c("Gavilán", "Guitarra", "Mantarrayas", "Total"))
        
        p <- ggplot(df_plot, aes(x = fecha, y = desem_t, color = gpo2_label, group = gpo2_label)) +
          geom_line(aes(linewidth = gpo2_label == "Total", linetype = gpo2_label == "Total"), alpha = 0.85) +
          scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.8), guide = "none") +
          scale_linetype_manual(values = c("TRUE" = "dashed", "FALSE" = "solid"), guide = "none") +
          scale_color_manual(values = colores_nombres) +
          facet_wrap(~ edo_facet, ncol = 2, scales = "free_y") + escala_x +
          labs(x = "", y = "Peso desembarcado (t)", color = "Especie / Total")
        
      } else {
        agg_esp$fecha <- as.Date(paste(agg_esp$year, agg_esp$mes2, "01", sep = "-"))
        agg_esp$gpo2_label <- factor(agg_esp$gpo2_label, levels = c("Gavilán", "Guitarra", "Mantarrayas"))
        
        p <- ggplot(agg_esp, aes(x = fecha, y = desem_t, fill = gpo2_label)) +
          geom_area(position = "fill", alpha = 0.9, color = "white", linewidth = 0.2) + 
          scale_fill_manual(values = colores_nombres) + scale_y_continuous(labels = scales::percent_format()) +
          facet_wrap(~ edo_facet, ncol = 2) + escala_x +
          labs(x = "", y = "Composición Relativa (%)", fill = "Especie")
      }
    } else {
      df_precio <- df[!is.na(df$price_d2) & df$price_d2 > 0, ]
      df_precio$gpo2_label <- factor(df_precio$gpo2_label, levels = c("Gavilán", "Guitarra", "Mantarrayas"))
      
      if (input$desglosar_precio == "Sí") {
        p <- ggplot(df_precio, aes(x = factor(year), y = price_d2, fill = gpo2_label, color = gpo2_label)) +
          geom_boxplot(outlier.alpha = 0.2, outlier.size = 1, alpha = 0.75) +
          scale_fill_manual(values = colores_nombres) + scale_color_manual(values = colores_nombres) +
          facet_wrap(~ edo_facet, ncol = 2, scales = "free_y") +
          labs(x = "Año", y = "Precio (MXN/kg) [Escala Log]", fill = "Especie", color = "Especie")
      } else {
        p <- ggplot(df_precio, aes(x = factor(year), y = price_d2)) +
          geom_boxplot(fill = "steelblue", color = "steelblue", outlier.alpha = 0.2, outlier.size = 1, alpha = 0.75) +
          stat_summary(fun = median, geom = "line", aes(group = 1), color = "darkred", linewidth = 1.2) +
          stat_summary(fun = median, geom = "point", color = "darkred", size = 2) +
          facet_wrap(~ edo_facet, ncol = 2, scales = "free_y") +
          labs(x = "Año", y = "Precio (MXN/kg) [Escala Log]")
      }
      p <- p + scale_y_log10(labels = scales::comma_format())
    }
    p + tema_base_graficas() + theme(axis.text.x = element_text(angle=45, hjust=1), strip.background = tema_fondo_facet)
  })
  # [FIN RENDER: PLOT TENDENCIA]

  # [INICIO RENDER: PLOT MERCADO]
  output$plot_mercado <- renderPlot({
    df <- datos_filtrados()
    if (nrow(df) == 0) return(NULL)
    
    df$gpo2_label <- unname(dict_gpos[as.character(df$gpo2)])
    if ("Todos" %in% input$edo) df$edo_facet <- "" 
    else df$edo_facet <- factor(unname(dict_edos[as.character(df$edo)]))

    df_mercado <- df[!is.na(df$price_d2) & df$price_d2 > 0, ]
    agg_vol <- aggregate(desem_kg ~ year + mes2 + edo + edo_facet + gpo2_label, data = df_mercado, FUN = sum)
    agg_precio <- aggregate(price_d2 ~ year + mes2 + edo + edo_facet + gpo2_label, data = df_mercado, FUN = median)
    
    df_scatter <- merge(agg_vol, agg_precio, by = c("year", "mes2", "edo", "edo_facet", "gpo2_label"))
    df_scatter$desem_t <- df_scatter$desem_kg / 1000 
    df_scatter <- df_scatter[df_scatter$desem_t > 0, ]
    
    tema_fondo_facet <- if("Todos" %in% input$edo) element_blank() else element_rect(fill = "#f8f9fa", color = NA)

    if (input$vista_oferta == "Clásica (Tendencia)") {
      df_scatter$epoca_manejo <- ifelse(df_scatter$year <= 2011, "Pre-veda (2000-2011)", "Veda (2012-2022)")
      
      p <- ggplot(df_scatter, aes(x = desem_t, y = price_d2, color = gpo2_label)) +
        geom_point(aes(shape = epoca_manejo), alpha = 0.6, size = 3) +
        geom_smooth(aes(linetype = epoca_manejo), method = "lm", se = FALSE, linewidth = 1) +
        scale_x_log10(labels = scales::comma_format()) + scale_y_log10(labels = scales::comma_format()) +
        scale_color_manual(values = colores_nombres) + facet_wrap(~ edo_facet, ncol = 2, scales = "free") +
        labs(x = "Oferta Mensual (t) [Log]", y = "Precio Mediano (MXN/kg) [Log]", color = "Especie", shape = "Época", linetype = "Época",
             title = "Relación Clásica: Precio vs Volumen", caption = "Nota: Las líneas marcan tendencias descriptivas.") +
        tema_base_graficas() + theme(legend.position = "bottom", strip.background = tema_fondo_facet)

    } else if (input$vista_oferta == "Densidad (Hexágonos)") {
      p <- ggplot(df_scatter, aes(x = desem_t, y = price_d2)) +
        geom_hex(bins = 30, alpha = 0.9, color = "white", linewidth = 0.1) +
        scale_fill_gradient(low = "#e0e0e0", high = "#0d47a1", name = "Frecuencia\n(Meses)") +
        scale_x_log10(labels = scales::comma_format()) + scale_y_log10(labels = scales::comma_format()) +
        facet_wrap(~ edo_facet, ncol = 2, scales = "free") +
        labs(x = "Oferta Mensual (t) [Log]", y = "Precio Mediano (MXN/kg) [Log]", title = "Mapa de Frecuencia del Mercado (Hexbins)") +
        tema_base_graficas() + theme(legend.key.height = unit(2, "cm"), legend.position = "right", strip.background = tema_fondo_facet)

    } else {
      df_scatter$temporada <- ifelse(as.numeric(as.character(df_scatter$mes2)) %in% c(3, 4), "Cuaresma (Mar-Abr)", "Resto del Año")
      
      p <- ggplot(df_scatter, aes(x = desem_t, y = price_d2, color = temporada)) +
        geom_point(aes(shape = gpo2_label), alpha = 0.7, size = 3) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
        scale_x_log10(labels = scales::comma_format()) + scale_y_log10(labels = scales::comma_format()) +
        scale_color_manual(values = colores_temporada) + facet_wrap(~ edo_facet, ncol = 2, scales = "free") +
        labs(x = "Oferta Mensual (t) [Log]", y = "Precio Mediano (MXN/kg) [Log]", color = "Temporada", shape = "Especie", title = "Efecto Estacional de Alta Demanda") +
        tema_base_graficas() + theme(strip.background = tema_fondo_facet)
    }
    p
  })
  # [FIN RENDER: PLOT MERCADO]

}
# ===================================================================
# [FIN SERVER]
# ===================================================================

# Ejecutar la aplicación
shinyApp(ui, server)
