# sincronizar_links.R
library(readxl)
library(tidyverse)

caminhos_excel <- c(
  "E:/UNIFEI/2026.2/enade/Link simulados ENADE.xlsm",
  "../Link simulados ENADE.xlsm",
  "Link simulados ENADE.xlsm",
  "E:/UNIFEI/ENADE/Link simulados ENADE.xlsm"
)

caminho_excel <- caminhos_excel[file.exists(caminhos_excel)][1]

if (is.na(caminho_excel)) {
  stop("Arquivo 'Link simulados ENADE.xlsm' não foi localizado nas pastas esperadas.")
}

message("Importando links a partir de: ", caminho_excel)

df_raw <- read_excel(caminho_excel, sheet = "Teste", skip = 2, col_names = FALSE)

mapear_faculdade <- function(nome_ies) {
  norm <- nome_ies %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    tolower() %>%
    trimws()
  
  case_when(
    grepl("mackenzie", norm) ~ "Mackenzie",
    grepl("ufam", norm) ~ "UFAM",
    grepl("uff", norm) ~ "UFF",
    grepl("ufpb", norm) ~ "UFPB",
    grepl("guara", norm) ~ "UNESP Guara",
    grepl("itapeva", norm) ~ "UNESP Itapeva",
    grepl("camp", norm) ~ "UNICAMP",
    grepl("itabira", norm) ~ "UNIFEI Itabira",
    grepl("itajub", norm) ~ "UNIFEI Itajuba",
    TRUE ~ trimws(nome_ies)
  )
}

df_dados <- df_raw[3:11, c(3, 6:15)]
colnames(df_dados) <- c("ies_raw", sprintf("Teste %02d", 1:10))

df_final <- df_dados %>%
  mutate(faculdade = sapply(ies_raw, mapear_faculdade)) %>%
  select(faculdade, starts_with("Teste")) %>%
  pivot_longer(
    cols = starts_with("Teste"),
    names_to = "id_teste",
    values_to = "raw_val"
  ) %>%
  mutate(
    testmoz_id = stringr::str_extract(raw_val, "\\d+"),
    url = ifelse(!is.na(testmoz_id), paste0("https://testmoz.com/q/", testmoz_id), "")
  ) %>%
  select(faculdade, id_teste, url) %>%
  arrange(faculdade, id_teste)

if (!dir.exists("config")) {
  dir.create("config", recursive = TRUE)
}

readr::write_delim(
  df_final,
  file = "config/links_testes.csv",
  delim = ";",
  quote = "none"
)

message("Arquivo 'config/links_testes.csv' atualizado com sucesso!")
message("Total de testes mapeados: ", nrow(df_final))
message("Total de links preenchidos: ", sum(df_final$url != ""))