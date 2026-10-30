library(tidyverse)
library(tools)
library(magrittr)

# 1. Caminho principal onde ficam as pastas dos testes
caminho_principal <- "Dados_testes/Testes respondidos"

# 2. Localizar TODOS os ficheiros CSV em todas as subpastas
todos_arquivos <- list.files(path = caminho_principal, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

# 3. Ler todos os CSVs e garantir que a coluna faculdade seja a PRIMEIRA
dados_completos <- todos_arquivos %>% 
  map_dfr(function(file_path) {
    # Identifica o nome da pasta (ex: "Teste 01")
    nome_pasta <- basename(dirname(file_path))
    
    # Extrai o nome da faculdade removendo "Teste XX" (independente de maiúsculas/minúsculas)
    nome_sem_ext <- file_path_sans_ext(basename(file_path))
    nome_faculdade <- sub("^teste[ _]*\\d+[ _]*", "", nome_sem_ext, ignore.case = TRUE)
    
    # Lê o CSV, adiciona as colunas e coloca 'faculdade' na primeira posição
    read_csv(
      file_path, 
      col_types = cols(.default = "c"), 
      show_col_types = FALSE
    ) %>% 
      mutate(
        faculdade = nome_faculdade,
        id_teste = nome_pasta,
        Name_norm = Name %>% trimws() %>% tolower() %>% gsub("\\s+", " ", .)
      ) %>% 
      relocate(faculdade, Name, Name_norm, .before = 1)
  })

# 3.1 Padronização Canônica: Garante que cada aluno tenha exatamente a mesma grafia do Name em todos os testes
mapa_nomes <- dados_completos %>%
  group_by(faculdade, Name_norm) %>%
  summarise(
    Name_canonico = {
      nomes <- unique(Name)
      com_maiuscula <- nomes[grepl("[A-ZÀ-Ú]", nomes)]
      if (length(com_maiuscula) > 0) com_maiuscula[1] else nomes[1]
    },
    .groups = "drop"
  )

dados_completos <- dados_completos %>%
  left_join(mapa_nomes, by = c("faculdade", "Name_norm")) %>%
  mutate(Name = Name_canonico) %>%
  select(-Name_canonico)

# 4. Separar os dados e criar um objeto para cada teste no R (teste_01, teste_02, etc.)
dados_completos %>% 
  group_split(id_teste) %>% 
  walk(function(df_teste) {
    # Formata o nome do teste (ex: "Teste 01" vira "teste_01")
    nome_original <- df_teste$id_teste[1]
    nome_objeto <- nome_original %>% tolower() %>% gsub(" ", "_", .)
    
    # Remove a coluna auxiliar 'id_teste'
    df_final <- df_teste %>% select(-id_teste)
    
    # Guarda o objeto no ambiente global do R
    assign(nome_objeto, df_final, envir = .GlobalEnv)
  })