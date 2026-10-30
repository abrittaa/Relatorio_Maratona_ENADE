# ==============================================================================
# SCRIPT: links_testes.R
# OBJETIVO: Camada de serviço para o mapeamento de links dos testes por
#           instituição (Maratona ENADE 2026).
#
# FONTE DOS DADOS: config/links_testes.csv (separador ";")
#   Colunas: faculdade;id_teste;url
#   - faculdade : nome da instituição como aparece no nome dos CSVs de
#                 respostas (ex: "UNIFEI Itajuba", "UNESP Guara", "UFPB").
#   - id_teste  : identificação do teste (ex: "Teste 01", "teste 5", "01").
#   - url       : link completo (http/https). Vazio = teste sem link
#                 (será exibido desabilitado no portal).
# ==============================================================================

library(tidyverse)

CAMINHO_LINKS_TESTES <- "config/links_testes.csv"

# Padroniza o identificador do teste para o formato "Teste NN"
# (resolve divergências como "teste 05" x "Teste 05" x "Teste 5").
padronizar_id_teste <- function(id) {
  num <- suppressWarnings(as.integer(stringr::str_extract(id, "\\d+")))
  ifelse(is.na(num), trimws(id), sprintf("Teste %02d", num))
}

# Gera uma chave de comparação para a instituição: sem acentos, minúsculas,
# sem trechos entre parênteses (ex: "Mackenzie (Sem resposta)") e espaços únicos.
chave_instituicao <- function(nome) {
  nome %>%
    as.character() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    tolower() %>%
    gsub("\\([^)]*\\)", " ", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws()
}

# Aceita somente URLs http(s) — evita links quebrados ou esquemas inseguros.
url_valida <- function(url) {
  !is.na(url) & grepl("^https?://[^\\s]+$", trimws(url), perl = TRUE)
}

# Sincroniza links a partir do arquivo Excel 'Link simulados ENADE.xlsm'
sincronizar_links_excel <- function(caminho_excel = NULL, caminho_saida = CAMINHO_LINKS_TESTES) {
  if (is.null(caminho_excel)) {
    candidatos <- c(
      "E:/UNIFEI/ENADE/Link simulados ENADE.xlsm",
      "../Link simulados ENADE.xlsm",
      "Link simulados ENADE.xlsm"
    )
    caminho_excel <- candidatos[file.exists(candidatos)][1]
  }

  if (is.na(caminho_excel) || !file.exists(caminho_excel)) {
    warning("Arquivo 'Link simulados ENADE.xlsm' não encontrado para sincronização.")
    return(invisible(FALSE))
  }

  if (!requireNamespace("readxl", quietly = TRUE)) {
    warning("Pacote 'readxl' não disponível para ler o arquivo Excel.")
    return(invisible(FALSE))
  }

  df_raw <- tryCatch(
    readxl::read_excel(caminho_excel, sheet = "Teste", skip = 2, col_names = FALSE),
    error = function(e) {
      warning("Erro ao ler aba 'Teste' de ", caminho_excel, ": ", e$message)
      NULL
    }
  )

  if (is.null(df_raw) || nrow(df_raw) < 11) {
    return(invisible(FALSE))
  }

  mapear_faculdade <- function(nome_ies) {
    norm <- nome_ies %>%
      as.character() %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      tolower() %>%
      trimws()

    dplyr::case_when(
      grepl("mackenzie", norm) ~ "Mackenzie",
      grepl("ufam", norm) ~ "UFAM",
      grepl("uff", norm) ~ "UFF",
      grepl("ufpb", norm) ~ "UFPB",
      grepl("guara", norm) ~ "UNESP Guara",
      grepl("itapeva", norm) ~ "UNESP Itapeva",
      grepl("camp", norm) ~ "UNICAMP",
      grepl("itabira", norm) ~ "UNIFEI Itabira",
      grepl("itajub", norm) ~ "UNIFEI Itajuba",
      TRUE ~ trimws(as.character(nome_ies))
    )
  }

  df_dados <- df_raw[3:11, c(3, 6:15)]
  colnames(df_dados) <- c("ies_raw", sprintf("Teste %02d", 1:10))

  df_final <- df_dados %>%
    dplyr::mutate(faculdade = sapply(ies_raw, mapear_faculdade)) %>%
    dplyr::select(faculdade, dplyr::starts_with("Teste")) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("Teste"),
      names_to = "id_teste",
      values_to = "raw_val"
    ) %>%
    dplyr::mutate(
      testmoz_id = stringr::str_extract(raw_val, "\\d+"),
      url = ifelse(!is.na(testmoz_id), paste0("https://testmoz.com/q/", testmoz_id), "")
    ) %>%
    dplyr::select(faculdade, id_teste, url) %>%
    dplyr::arrange(faculdade, id_teste)

  dir_config <- dirname(caminho_saida)
  if (!dir.exists(dir_config)) {
    dir.create(dir_config, recursive = TRUE)
  }

  readr::write_delim(
    df_final,
    file = caminho_saida,
    delim = ";",
    quote = "none"
  )

  message("Links sincronizados com sucesso a partir de ", caminho_excel, " para ", caminho_saida)
  invisible(TRUE)
}

# Carrega o mapeamento. Se o arquivo não existir ou estiver vazio, tenta sincronizar
# do Excel antes de retornar vazio.
carregar_links_testes <- function(caminho = CAMINHO_LINKS_TESTES) {
  vazio <- tibble(chave_fac = character(), id_teste = character(), url = character())

  if (!file.exists(caminho)) {
    sincronizar_links_excel(caminho_saida = caminho)
  }

  if (!file.exists(caminho)) {
    warning("Arquivo de links não encontrado (", caminho, "). Os testes serão exibidos sem link.")
    return(vazio)
  }

  dados <- readr::read_delim(
    caminho,
    delim = ";",
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    trim_ws = TRUE,
    show_col_types = FALSE
  )

  # Se o CSV existir mas não tiver URLs válidas, tenta auto-sincronizar do Excel
  if (sum(url_valida(dados$url), na_rm = TRUE) == 0) {
    if (sincronizar_links_excel(caminho_saida = caminho)) {
      dados <- readr::read_delim(
        caminho,
        delim = ";",
        col_types = cols(.default = "c"),
        locale = locale(encoding = "UTF-8"),
        trim_ws = TRUE,
        show_col_types = FALSE
      )
    }
  }

  dados %>%
    filter(!is.na(faculdade), !is.na(id_teste)) %>%
    mutate(
      chave_fac = chave_instituicao(faculdade),
      id_teste  = padronizar_id_teste(id_teste),
      url       = trimws(url)
    ) %>%
    filter(url_valida(url)) %>%
    distinct(chave_fac, id_teste, .keep_all = TRUE) %>%
    select(chave_fac, id_teste, url)
}

# Retorna a URL do teste para a instituição, ou NA quando não houver cadastro.
obter_url_teste <- function(links, faculdade, id_teste) {
  achado <- links$url[links$chave_fac == chave_instituicao(faculdade) &
                        links$id_teste == padronizar_id_teste(id_teste)]
  if (length(achado) == 0) NA_character_ else achado[1]
}

# Lista os testes que possuem link cadastrado para a instituição.
testes_com_link <- function(links, faculdade) {
  links$id_teste[links$chave_fac == chave_instituicao(faculdade)]
}
