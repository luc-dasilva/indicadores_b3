
# Load Packages ----------------------------------------------------------

library(tidyverse)

# Importing --------------------------------------------------------------

dre <- read_rds("data/processed_data/dre.rds")
bpa <- read_rds("data/processed_data/clean/bpa_clean.rds")
bpp <- read_rds("data/processed_data/clean/bpp_clean.rds")

# Transforming -----------------------------------------------------------

# ROE
dre_ll <- dre |> 
  filter(cd_conta == 309) |>
  select(cd_cvm:ds_conta, c(x2020, x2021, x2022, x2023, x2024)) |>
  pivot_longer(
    cols = starts_with("x"),
    names_to = "period",
    values_to = "value"
  ) |> 
  select(!setor) |> 
  mutate(tipo = "dre")

bpa_pl <- bpp |> 
  filter(cd_conta == 203) |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "period",
    values_to = "value"
  ) |> 
  select(!c(cnpj, setor, desc)) |> 
  mutate(tipo = "bpp")

bpa_pl_filtered <- bpa_pl |> 
  semi_join(dre_ll, by = "cd_cvm") 

roe_tibble <- bind_rows(dre_ll, bpa_pl_filtered) |> 
  select(!c(tipo, ds_conta)) |> 
  pivot_wider(
    names_from = cd_conta,
    values_from = value
  ) |> 
  mutate(
    period = str_remove(period, "^x"),
    period = factor(
      period, levels = c(
        "2020", "2021",
        "2022", "2023",
        "2024"
      )
    ),
    roe = `309`/`203`, .keep = "unused"
  ) 

# LIQ

bpa_ac <- bpa |> 
  filter(cd_conta %in% c(101, 10101, 10104)) |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "period",
    values_to = "value"
  ) |> 
  select(!c(cnpj, setor, desc)) |> 
  mutate(tipo = "bpa")

bpp_pc <- bpp |> 
  filter(cd_conta == 201) |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "period",
    values_to = "value"
  ) |> 
  select(!c(cnpj, setor, desc)) |> 
  mutate(tipo = "bpp")

liq_tibble <- bind_rows(bpa_ac, bpp_pc) |> 
  select(!c(tipo, ds_conta)) |> 
  pivot_wider(
    names_from = cd_conta,
    values_from = value
  ) |> 
  mutate(
    period = str_remove(period, "^x"),
    period = factor(
      period, levels = c(
        "2020", "2021",
        "2022", "2023",
        "2024"
      )
    ),
    liq_cor = `101` / `201`,
    liq_seca = (`101` - `10104`) / `201`,
    liq_imed = `10101` / `201`,
    .keep = "unused"
  )

# Exporting --------------------------------------------------------------

write_rds(roe_tibble, "data/processed_data/roe.rds")
write_rds(liq_tibble, "data/processed_data/liq.rds")