
# Load Packages ----------------------------------------------------------

library(tidyverse)
library(shiny)
library(bs4Dash)
library(fresh)
library(echarts4r)
library(reactable)

# Data -------------------------------------------------------------------

fca <- read_rds("data/processed_data/clean/fca_clean.rds")
dre <- read_rds("data/processed_data/dre.rds")
bpa <- read_rds("data/processed_data/clean/bpa_clean.rds")
bpp <- read_rds("data/processed_data/clean/bpp_clean.rds")

roe <- read_rds("data/processed_data/roe.rds")
liq <- read_rds("data/processed_data/liq.rds")

bpa_tib <- read_rds("data/processed_data/tables/bpa_tib.rds")
bpp_tib <- read_rds("data/processed_data/tables/bpp_tib.rds")
dre_tib <- read_rds("data/processed_data/tables/dre_tib.rds")

list_cias <- dre |> distinct(nome_sa) |> arrange(nome_sa) |> pull()

# Misc -------------------------------------------------------------------

axis_label_js <- c("
        function(value){
          const absVal = Math.abs(value);
          let formatted = '';

          if (absVal >= 1e9) {
            formatted = 'R$ ' + (value / 1e9).toFixed(0) + 'B';
          } else if (absVal >= 1e6) {
            formatted = 'R$ ' + (value / 1e6).toFixed(0) + 'M';
          } else if (absVal >= 1e3) {
            formatted = 'R$ ' + (value / 1e3).toFixed(0) + 'K';
          } else {
            formatted = 'R$ ' + value.toFixed(0);
          }

          return formatted;
        }
      "
)

# Functions --------------------------------------------------------------

format_value <- function(value) {
  if (abs(value) >= 1e9) {
    return(paste0(format(round(value / 1e9, 1), nsmall = 2), " B"))
  } else if (abs(value) >= 1e6) {
    return(paste0(format(round(value / 1e6, 1), nsmall = 2), " M"))
  } else if (abs(value) >= 1e3) {
    return(paste0(format(round(value / 1e3, 1), nsmall = 2), " K"))
  } else {
    return(format(value, big.mark = "'"))
  }
}

calc_var_pct <- function(atual, anterior) {
  ifelse(anterior == 0, NA, ((atual - anterior) / anterior) * 100)
}

pct_color_style <- function(value) {
  if (is.na(value)) return(NULL)
  
  color <- if (value > 0) {
    "#28a745"
  } else if (value < 0) {
    "#dc3545"
  } else {
    "#6c757d"
  }
  
  list(color = color, fontWeight = "bold")
}

get_indent_level <- function(cd_conta) {
  dots <- nchar(gsub("[^.]", "", cd_conta))
  return(dots * 10)
}


# App --------------------------------------------------------------------

theme <- create_theme(
  bs4dash_color(purple = "#3c34c3"),
  bs4dash_status(primary = " #1A162D", secondary = " #FAFAFA")
)

# UI ---------------------------------------------------------------------
ui <- dashboardPage(

  # Setup -----
  freshTheme = theme,
  help = NULL,
  dark = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  title = "IndicadoresB3", 

  # Header -----
  header = dashboardHeader(
    status = "primary",
    compact = FALSE,
    title = dashboardBrand(
      title = tags$b("IndicadoresB3"),
      color = "primary",
      # image = "https://luc-dasilva.github.io/img/profile.png"
    )
  ),

  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    title = "Menu",
    collapsed = FALSE,
    minified = FALSE,
    sidebarMenu(
      menuItem("Dashboard", tabName = "DashTab", icon = icon("dashboard")),
      menuItem("Indicadores", tabName = "IndTab", icon = icon("chart-bar")),
      menuItem("Demonstrações", tabName = "ReportTab", icon = icon("chart-line")),
      menuItem("Sobre", icon = icon("info-circle"), href = "https://luc-dasilva.github.io/")
    ),

    selectInput(
      inputId = "InputEmpresa",
      label = "Escolha a companhia:",
      choices = list_cias,
      selected = "VALE"
    )
  ),

  # Body -----
  body = dashboardBody(

    tabItems(

      # Dashboard -----
      tabItem(
        tabName = "DashTab",
        # Boxes
        fluidRow(
          infoBoxOutput("receita", width = 3),
          infoBoxOutput("resultado", width = 3),
          infoBoxOutput("setor", width = 3),
          infoBoxOutput("sit", width = 3)
        ),

        # Plots
        fluidRow(
          box(
            title = "Receita",
            status = "primary",
            solidHeader = TRUE,
            echarts4rOutput("receita_tri"),
            width = 6
          ),
          box(
            title = "Margens",
            status = "primary",
            solidHeader = TRUE,
            echarts4rOutput("margem_bruta"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Receita vs Resultado",
            status = "primary",
            solidHeader = TRUE,
            echarts4rOutput("receita_resultado_tri"),
            width = 12
          )
        )
      ),
      # Indicadores Financeiros -----
      tabItem(
        tabName = "IndTab",
        fluidRow(
          box(
            title = "Returno Sobre o PL",
            status = "primary",
            solidHeader = TRUE,
            echarts4rOutput("profit"),
            width = 12
          )
        ),
        fluidRow(
          box(
            title = "Liquidez",
            status = "primary",
            solidHeader = TRUE,
            echarts4rOutput("liquidity"),
            width = 12
          )
        )
      ),
      # Relatórios Financeiros -----
      tabItem(
        tabName = "ReportTab",

        fluidRow(
          box(
            title = "Demostração do Resultado",
            status = "primary",
            solidHeader = TRUE,
            reactableOutput("dre_tib"),
            width = 12
          )
        ),
        fluidRow(
          box(
            title = "Balanço Patrimonial - Ativo",
            status = "primary",
            solidHeader = TRUE,
            reactableOutput("bpa_tib"),
            width = 12
          )
        ),
        fluidRow(
          box(
            title = "Balanço Patrimonial - Passivo",
            status = "primary",
            solidHeader = TRUE,
            reactableOutput("bpp_tib"),
            width = 12
          )
        )

      )
    )
  ),

  # Footer -----
  footer = dashboardFooter(
    left = "© Lucas da Silva, 2025",
    right = "Dados: CVM"
  )
)

# Server -----------------------------------------------------------------

server <- function(input, output, session) {

  # Data -----

  load_metadata <- reactive({
    cia <- input$InputEmpresa

    fca_data <- fca |> 
      filter(nome_sa == cia)
  })

  load_dre_data <- reactive({
    cia <- input$InputEmpresa

    dre_data <- dre |> 
      filter(nome_sa == cia)

    return(dre_data)

  })

  load_roe_data <- reactive({
    cia <- input$InputEmpresa

    roe_data <- roe |> 
      filter(nome_sa == cia)

    return(roe_data)

  })

  load_liq_data <- reactive({
    cia <- input$InputEmpresa

    liq_data <- liq |> 
      filter(nome_sa == cia)

    return(liq_data)

  })

  latest_dre_data <- reactive({
    dre_data <- load_dre_data()

    latest_dre_data <- dre_data |> 
      select(cd_conta, ds_conta, x2024)

    return(latest_dre_data)

  })

  load_dre_tib_data <- reactive({
    cia <- input$InputEmpresa

    dre_tib_data <- dre_tib |> 
      filter(nome_sa == cia)

    return(dre_tib_data)
  })

  load_bpa_tib_data <- reactive({
    cia <- input$InputEmpresa

    bpa_tib_data <- bpa_tib |> 
      filter(nome_sa == cia)

    return(bpa_tib_data)
  })

  load_bpp_tib_data <- reactive({
    cia <- input$InputEmpresa

    bpp_tib_data <- bpp_tib |> 
      filter(nome_sa == cia)

    return(bpp_tib_data)
  })

  # Boxes -----

  output$receita <- renderInfoBox({
    latest <- latest_dre_data()
    value <- latest |> filter(cd_conta == 301) |> pull(x2024)

    if (abs(value) >= 1e9) {
        value_print <- paste0("R$ ", round(value / 1e9, 1), " B")
      } else if (abs(value) >= 1e6) {
        value_print <- paste0("R$ ", round(value / 1e6, 1), " M")
      } else if (abs(value) >= 1e3) {
        value_print <- paste0("R$ ", round(value / 1e3, 1), " K")
    }

    infoBox(
      "Receita Anual", 
      value_print, 
      icon = icon("chart-simple"),
      color = "primary"
    )
  })

  output$resultado <- renderInfoBox({
    latest <- latest_dre_data()
    value <- latest |> filter(cd_conta == 309) |>pull(x2024)

    if (abs(value) >= 1e9) {
        value_print <- paste0("R$ ", round(value / 1e9, 1), " B")
      } else if (abs(value) >= 1e6) {
        value_print <- paste0("R$ ", round(value / 1e6, 1), " M")
      } else if (abs(value) >= 1e3) {
        value_print <- paste0("R$ ", round(value / 1e3, 1), " K")
    }

    infoBox(
      "Resultado Anual", 
      value_print, 
      icon = icon("sack-dollar"),
      color = "primary"
    )
  })

  output$setor <- renderInfoBox({
    metadata <- load_metadata() |> 
      select(setor) |> 
      pull()

    infoBox(
      "Setor", 
      metadata, 
      icon = icon("industry"),
      color = "primary"
    )
  })

  output$sit <- renderInfoBox({
    metadata <- load_metadata()
    infoBox(
      "Situação da Cia", 
      metadata |> 
        select(sit_emissor) |> 
        pull(), 
      icon = icon("search"),
      color = "primary"
    )
  })

  # Charts -----

  output$receita_tri <- renderEcharts4r({
    load_dre_data() |> 
    select(!c(x2020, x2021, x2022, x2023, x2024)) |> 
    pivot_longer(
      cols = starts_with("x"),
      names_to = "tri"
    ) |> 
    mutate(
      tri = str_remove(tri, "^x"),
      tri = factor(
        tri, levels = c(
          "1T23", "2T23",
          "3T23", "4T23",
          "1T24", "2T24",
          "3T24", "4T24"
        )
      )
    ) |> 
    arrange(tri) |> 
    filter(cd_conta == 301) |> 
    e_charts(tri, reorder = FALSE) |> 
    e_bar(value, name = "Receita Líquida") |> 
    e_y_axis(
      axisLabel = list(formatter = htmlwidgets::JS(axis_label_js))
    ) |> 
    e_tooltip(
      formatter = e_tooltip_item_formatter(
        style = "currency",
        digits = 0,
        locale = "pt",
        currency = "BRL"
      )
    ) |> 
    e_legend(show = FALSE)

  })

  output$margem_bruta <- renderEcharts4r({
    load_dre_data() |> 
    select(!c(x2020, x2021, x2022, x2023, x2024)) |> 
    filter(cd_conta %in% c(301, 303, 309)) |> 
    select(!cd_conta) |> 
    pivot_longer(
      cols = starts_with("x"),
      names_to = "period"
    ) |> 
    pivot_wider(
      names_from = ds_conta,
      values_from = value
    ) |> 
    rename(
      RL = `Receita de Venda de Bens e/ou Serviços`,
      LB = `Resultado Bruto`,
      LL = `Resultado Líquido das Operações Continuadas`
    ) |> 
    mutate(
      m_bruta = LB / RL,
      m_liq = LL/ RL,
      period = str_remove(period, "x"),
      .keep = "unused"
    ) |> 
    e_charts(period) |>
    e_line(m_bruta, name = "Margem Bruta") |> 
    e_area(m_liq, name = "Margem Líquida") |> 
    e_y_axis(
      formatter = e_axis_formatter(
        style = "percent",
        digits = 0,
        locale = "pt"
      )
    ) |> 
    e_tooltip(
      trigger = "axis",
      formatter = e_tooltip_pointer_formatter(
        style = "percent",
        digits = 1,
        locale = "pt"
      )
    ) |> 
    e_legend(top = "bottom", selectedMode = TRUE)
  })

  output$receita_resultado_tri <- renderEcharts4r({
    load_dre_data() |> 
    select(!c(x2020, x2021, x2022, x2023, x2024)) |> 
    pivot_longer(
      cols = starts_with("x"),
      names_to = "tri"
    ) |> 
    mutate(
      tri = str_remove(tri, "^x"),
      tri = factor(
        tri, levels = c(
          "1T23", "2T23",
          "3T23", "4T23",
          "1T24", "2T24",
          "3T24", "4T24"
        )
      )
    ) |> 
    arrange(tri) |> 
    filter(cd_conta %in% c(301, 309)) |> 
    select(!ds_conta) |> 
    pivot_wider(
      names_from = cd_conta,
      values_from = value
    ) |> 
    e_charts(tri, reorder = FALSE) |>
    e_bar(`301`, name = "Receita Bruta") |>
    e_bar(`309`, name = "Resultado Líquido") |>
    e_y_axis(
      index = 0,
      axisLabel = list(formatter = htmlwidgets::JS(axis_label_js))
    ) |> 
    e_tooltip(
      formatter = e_tooltip_item_formatter(
        style = "currency",
        digits = 0,
        locale = "pt",
        currency = "BRL"
      )
    ) |> 
    e_legend(top = "bottom", selectedMode = TRUE)
  })

  output$profit <- renderEcharts4r({
    load_roe_data() |> 
      e_chart(period) |> 
      e_bar(roe, name = "ROE") |> 
      e_y_axis(
        max = 1,
        formatter = e_axis_formatter(
          style = "percent",
          digits = 0,
          locale = "pt"
       )
     ) |> 
      e_tooltip(
        formatter = e_tooltip_item_formatter(
          style = "percent",
          digits = 0,
          locale = "pt"
        )
      ) |> 
    e_legend(show = FALSE)
  })

  output$liquidity <- renderEcharts4r({
    load_liq_data() |> 
    e_chart(period) |>
    e_line(liq_cor, name = "Corrente") |> 
    e_line(liq_seca, name = "Seca") |> 
    e_area(liq_imed, name = "Imediata") |> 
    e_y_axis(
      formatter = e_axis_formatter(
        style = "decimal",
        digits = 1,
        locale = "pt"
      )
    ) |> 
   e_tooltip(
      trigger = "axis",
      formatter = e_tooltip_pointer_formatter(
        style = "decimal",
        digits = 2,
        locale = "pt"
      )
    ) |> 
    e_legend(top = "bottom", selectedMode = TRUE)
  })

  # Tables -----

  output$dre_tib <- renderReactable({
    load_dre_tib_data() |> 
      reactable(
        pagination = FALSE,
        
        columns = list(

          nome_sa = colDef(show = FALSE),
      
          ds_conta = colDef(
            name = "Conta",
            minWidth = 250,
            align = "left",
          ),
      
          x2020 = colDef(
            name = "2020",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          x2021 = colDef(
            name = "2021",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          var_20_21 = colDef(
            name = "Var %",
            format = colFormat(suffix = "%", digits = 1),
            style = function(value) pct_color_style(value),
            align = "right",
            # width = 100
          ),
      
          x2022 = colDef(
            name = "2022",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          var_21_22 = colDef(
            name = "Var %",
            format = colFormat(suffix = "%", digits = 1),
            style = function(value) pct_color_style(value),
            align = "right",
            # width = 100
          ),
      
          x2023 = colDef(
            name = "2023",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          var_22_23 = colDef(
            name = "Var %",
            format = colFormat(suffix = "%", digits = 1),
            style = function(value) pct_color_style(value),
            align = "right",
            # width = 100
          ),
      
          x2024 = colDef(
            name = "2024",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
    
          var_23_24 = colDef(
            name = "Var %",
            format = colFormat(suffix = "%", digits = 1),
            style = function(value) pct_color_style(value),
            align = "right",
            # width = 100
          )
      
        ),
      
        striped = TRUE,
        bordered = TRUE,
        theme = reactableTheme(
          stripedColor = "#f5f5f5",
          cellPadding = "8px 12px",
          style = list(
            fontFamily = "Lato, -apple-system, sans-serif",
            fontSize = "14.5px"
          ),
          headerStyle = list(
            backgroundColor = "#f8f9fa",
            borderBottom = "2px solid #dee2e6",
            fontWeight = "bold"
          )
        )
      )
  })

  output$bpa_tib <- renderReactable({
    load_bpa_tib_data() |> 
      reactable(
        pagination = FALSE,
        
        columns = list(

          nome_sa = colDef(show = FALSE),    
          cd_conta = colDef(show = FALSE),
      
          ds_conta = colDef(
            name = "Conta",
            minWidth = 250,
            align = "left",
            cell = function(value, index) {
              div(
                style = list(
                  paddingLeft = paste0(bpa_tib$indent_level[index], "px"), 
                  fontWeight = if(nchar(bpa_tib$cd_conta[index]) <= 3) "bold" else "normal"
                ),
                value
              )
            }
          ),
      
          x2020 = colDef(
            name = "2020",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          x2021 = colDef(
            name = "2021",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          var_20_21 = colDef(
            name = "Var %",
            format = colFormat(suffix = "%", digits = 1),
            style = function(value) pct_color_style(value),
            align = "right",
            # width = 100
          ),
      
          x2022 = colDef(
            name = "2022",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          var_21_22 = colDef(
            name = "Var %",
            format = colFormat(suffix = "%", digits = 1),
            style = function(value) pct_color_style(value),
            align = "right",
            # width = 100
          ),
      
          x2023 = colDef(
            name = "2023",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          var_22_23 = colDef(
            name = "Var %",
            format = colFormat(suffix = "%", digits = 1),
            style = function(value) pct_color_style(value),
            align = "right",
            # width = 100
          ),
      
          x2024 = colDef(
            name = "2024",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          var_23_24 = colDef(
            name = "Var %",
            format = colFormat(suffix = "%", digits = 1),
            style = function(value) pct_color_style(value),
            align = "right",
            # width = 100
          ),
      
          indent_level = colDef(show = FALSE)
        ),
      
        striped = TRUE,
        bordered = TRUE,
        theme = reactableTheme(
          stripedColor = "#f5f5f5",
          cellPadding = "8px 12px",
          style = list(
            fontFamily = "Lato, -apple-system, sans-serif",
            fontSize = "14.5px"
          ),
          headerStyle = list(
            backgroundColor = "#f8f9fa",
            borderBottom = "2px solid #dee2e6",
            fontWeight = "bold"
          )
        )
      )
  })

  output$bpp_tib <- renderReactable({
    load_bpp_tib_data() |> 
      reactable(
        pagination = FALSE,
        
        columns = list(

          nome_sa = colDef(show = FALSE),    
          cd_conta = colDef(show = FALSE),
      
          ds_conta = colDef(
            name = "Conta",
            minWidth = 250,
            align = "left",
            cell = function(value, index) {
              div(
                style = list(
                  paddingLeft = paste0(bpp_tib$indent_level[index], "px"), 
                  fontWeight = if(nchar(bpp_tib$cd_conta[index]) <= 3) "bold" else "normal"
                ),
                value
              )
            }
          ),
      
          x2020 = colDef(
            name = "2020",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          x2021 = colDef(
            name = "2021",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          var_20_21 = colDef(
            name = "Var %",
            format = colFormat(suffix = "%", digits = 1),
            style = function(value) pct_color_style(value),
            align = "right",
            # width = 100
          ),
      
          x2022 = colDef(
            name = "2022",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          var_21_22 = colDef(
            name = "Var %",
            format = colFormat(suffix = "%", digits = 1),
            style = function(value) pct_color_style(value),
            align = "right",
            # width = 100
          ),
      
          x2023 = colDef(
            name = "2023",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          var_22_23 = colDef(
            name = "Var %",
            format = colFormat(suffix = "%", digits = 1),
            style = function(value) pct_color_style(value),
            align = "right",
            # width = 100
          ),
      
          x2024 = colDef(
            name = "2024",
            format = colFormat(prefix = "R$ "),
            cell = function(value) format_value(value),
            align = "right",
            # width = 120
          ),
      
          var_23_24 = colDef(
            name = "Var %",
            format = colFormat(suffix = "%", digits = 1),
            style = function(value) pct_color_style(value),
            align = "right",
            # width = 100
          ),
      
          indent_level = colDef(show = FALSE)
        ),
      
        striped = TRUE,
        bordered = TRUE,
        theme = reactableTheme(
          stripedColor = "#f5f5f5",
          cellPadding = "8px 12px",
          style = list(
            fontFamily = "Lato, -apple-system, sans-serif",
            fontSize = "14.5px"
          ),
          headerStyle = list(
            backgroundColor = "#f8f9fa",
            borderBottom = "2px solid #dee2e6",
            fontWeight = "bold"
          )
        )
      )
  })

}

# Run the app
shinyApp(ui, server)
