
# Load Packages ----------------------------------------------------------

library(tidyverse)
library(echarts4r)
library(reactable)
library(htmlwidgets)

# Importing --------------------------------------------------------------

dre <- read_rds("data/processed_data/dre.rds")
bpa <- read_rds("data/processed_data/clean/bpa_clean.rds")
bpp <- read_rds("data/processed_data/clean/bpp_clean.rds")

roe <- read_rds("data/processed_data/roe.rds")
liq <- read_rds("data/processed_data/liq.rds")

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

# Visualizing ------------------------------------------------------------

# Receita Líquida
dre |>
  filter(cd_cvm == "009512") |> 
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

# Receita Líquida vs Lucro Líquido
dre |>
  filter(cd_cvm == "009512") |> 
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

# Margem Bruta vs Margem Liquida
dre |> 
  filter(cd_cvm == "009512") |> 
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
    max = 1,
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

# Lucratividade (ROE)
roe |> 
  filter(cd_cvm == "009512") |> 
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

# Liquidez
liq |> 
  filter(cd_cvm == "009512") |> 
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


# Tabelas ----------------------------------------------------------------

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

# var
calc_var_pct <- function(atual, anterior) {
  ifelse(anterior == 0, NA, ((atual - anterior) / anterior) * 100)
}

# cor var
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

# recuo
get_indent_level <- function(cd_conta) {
  dots <- nchar(gsub("[^.]", "", cd_conta))
  return(dots * 10)
}

bpa_tib <- bpa |> 
  filter(cd_cvm == "009512") |> 
  select(!cd_cvm) |> 
  mutate(
    var_20_21 = ifelse(x2020 == 0, 0, (x2021 - x2020) / abs(x2020) * 100),
    var_21_22 = ifelse(x2021 == 0, 0, (x2022 - x2021) / abs(x2021) * 100),
    var_22_23 = ifelse(x2022 == 0, 0, (x2023 - x2022) / abs(x2022) * 100),
    var_23_24 = ifelse(x2023 == 0, 0, (x2024 - x2023) / abs(x2023) * 100),
    indent_level = str_length(as.character(cd_conta)) * 2
  ) |> 
  select(
    cd_conta, ds_conta, x2020, x2021, var_20_21, x2022, var_21_22,
    x2023, var_22_23, x2024, var_23_24, indent_level
  )

reactable(
  bpa_tib,
  pagination = FALSE,
  
  columns = list(

    cd_conta = colDef(show = FALSE),

    ds_conta = colDef(
      name = "Descrição",
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
      name = "Var % 2023",
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

bpp_tib <- bpp |> 
  filter(cd_cvm == "009512") |> 
  select(!cd_cvm) |> 
  mutate(
    var_20_21 = ifelse(x2020 == 0, 0, (x2021 - x2020) / abs(x2020) * 100),
    var_21_22 = ifelse(x2021 == 0, 0, (x2022 - x2021) / abs(x2021) * 100),
    var_22_23 = ifelse(x2022 == 0, 0, (x2023 - x2022) / abs(x2022) * 100),
    var_23_24 = ifelse(x2023 == 0, 0, (x2024 - x2023) / abs(x2023) * 100),
    indent_level = str_length(as.character(cd_conta)) * 2
  ) |> 
  select(
    cd_conta, ds_conta, x2020, x2021, var_20_21, x2022, var_21_22,
    x2023, var_22_23, x2024, var_23_24, indent_level
  )

  reactable(
    bpp_tib,
    pagination = FALSE,
    
    columns = list(
  
      cd_conta = colDef(show = FALSE),
  
      ds_conta = colDef(
        name = "Descrição",
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
  
dre_tib <- dre |> 
  filter(cd_cvm == "009512") |> 
  select(!cd_cvm) |> 
  mutate(
    var_20_21 = ifelse(x2020 == 0, 0, (x2021 - x2020) / abs(x2020) * 100),
    var_21_22 = ifelse(x2021 == 0, 0, (x2022 - x2021) / abs(x2021) * 100),
    var_22_23 = ifelse(x2022 == 0, 0, (x2023 - x2022) / abs(x2022) * 100),
    var_23_24 = ifelse(x2023 == 0, 0, (x2024 - x2023) / abs(x2023) * 100)
  ) |> 
  select(
    ds_conta, x2020, x2021, var_20_21, x2022, var_21_22,
    x2023, var_22_23, x2024, var_23_24
  )

  reactable(
    dre_tib,
    pagination = FALSE,
    
    columns = list(
  
      ds_conta = colDef(
        name = "Descrição",
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
