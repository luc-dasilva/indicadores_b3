
# Load Packages ----------------------------------------------------------

library(tidyverse)

# Importing --------------------------------------------------------------

fca <- read_csv2(
  file = "data/raw_data/fca/fca_cia_aberta_geral_2024.csv",
  locale = locale(encoding = "latin1")
)

# Cleaning ---------------------------------------------------------------

setores_constr <- c(
  "Construção Civil, Mat. Constr. e Decoração",
  "Const. Civil, Mat. Const. e Decoração"
)

setores_saude <- c("Serviços médicos", "Farmacêutico e Higiene")
setores_ind <- c(
  "Máquinas, Equipamentos, Veículos e Peças",
  "Máqs., Equip., Veíc. e Peças",
  "Petroquímicos e Borracha"
)

setores_min <- c("Extração Mineral", "Metalurgia e Siderurgia") 
setores_fin <- c(
  "Securitização de Recebíveis", "Bolsas de Valores/Mercadorias e Futuros",
  "Crédito Imobiliário", "Intermediação Financeira"
)

fca_clean <- fca |> 
  select(
    cnpj = 1, cd_cvm = 9, nome = 5,
    setor = 17, desc = 18, sit_emissor = 19
  ) |> 
  mutate(
    setor = str_remove(setor, "Emp. Adm. Part. - "),
    setor = str_remove(setor, " \\(.*\\)$"),
    setor = case_when(
      setor %in% c("Energia Elétrica", "Petróleo e Gás") ~ "Energia",
      setor %in% c("Comércio", "Têxtil e Vestuário")     ~ "Varejo",
      setor == "Serviços Transporte e Logística"         ~ "Logística",
      setor == "Saneamento, Serv. Água e Gás"            ~ "Utilidades",
      setor %in% setores_constr                          ~ "Construção Civil",
      setor %in% c("Alimentos", "Bebidas e Fumo")        ~ "Alimentos",
      setor == "Telecomunicações"                        ~ "Telecom",
      setor %in% setores_saude                           ~ "Saúde",
      setor %in%  setores_ind                            ~ "Indústria",
      setor == "Brinquedos e Lazer"                      ~ "Lazer",
      setor == "Comunicação e Informática"               ~ "Tecnologia",
      setor %in% c("Papel e Celulose", "Embalagens")     ~ "Materiais",
      setor %in% setores_min                             ~ "Mineração",
      setor %in%  setores_fin                            ~ "Serv. Financeiros",
      setor == "Seguradoras e Corretoras"                ~ "Seguros",
      setor == "Agricultura"                             ~ "Agropecuária",
      .default = setor
    ),
  setor = if_else(cd_cvm == "027111", "Serv. Financeiros", setor),
  nome = str_to_upper(nome),
  nome_sa = str_extract(nome, "^.* (S\\.A\\.|S\\/A|SA$|S\\.A)"),
  nome_sa = str_remove_all(nome, " (S\\.A\\.|S\\/A|SA$|S\\.A)"),
  nome_sa = str_remove(nome_sa, "( \\-|\\-(?=d)).*$"),
  desc = str_to_sentence(desc),
  sit_emissor = case_when(
    sit_emissor == "Fase Operacional" ~ "Operacional",
    sit_emissor == "Fase Pré-Operacional" ~ "Pré-Operacional",
    sit_emissor == "Em Liquidação Extrajudicial" ~ "Liq. Extrajudicial",
    sit_emissor == "Em Recuperação Judicial ou Equivalente" ~ "Rec. Judicial",
    sit_emissor == "Em Recuperação Extrajudicial" ~ "Rec. Extrajudicial",
    sit_emissor == "Em Falência" ~ "Falência",
    sit_emissor == "Paralisada" ~ "Paralisada"
  )
  ) |> relocate(nome_sa, .after = cd_cvm) |> select(!nome)

# Exporting --------------------------------------------------------------

if (dir.exists("data/processed_data/clean/") ==  FALSE) {
  dir.create("data/processed_data/clean/")
}

write_rds(fca_clean, "data/processed_data/clean/fca_clean.rds")
