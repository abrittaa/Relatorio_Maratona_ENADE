library(tidyverse)

# 1. Função para obter o primeiro valor válido
obter_valor_valido <- function(vetor) {
  validos <- vetor[!is.na(vetor) & vetor != "NULL" & vetor != "NA" & trimws(vetor) != ""]
  if (length(validos) > 0) {
    return(validos[1])
  } else {
    return("Não Realizou o Teste")
  }
}

# 2. Função para encurtar e desambiguar nomes duplicados
formatar_cabecalho <- function(col_names) {
  # Remove espaços (ex: "GP 01" vira "GP01") e pega os 4 primeiros caracteres
  curtos <- col_names %>% 
    gsub(" ", "", .) %>% 
    substr(1, 4)
  
  # Adiciona sufixo (_1, _2) automaticamente se houver códigos duplicados
  make.unique(curtos, sep = "_")
}

# 3. Criação do 'dados_limpos'
dados_limpos <- dados_completos %>% 
  group_by(Name, faculdade) %>% 
  summarise(
    across(everything(), obter_valor_valido),
    .groups = "drop"
  ) %>% 
  relocate(faculdade, Name, .before = 1) %>% 
  rename_with(
    formatar_cabecalho,
    .cols = !any_of(c("Name", "faculdade", "Start Datetime", "End Datetime", "Time", "Percent", "Points Earned", "Points Possible", "id_teste"))
  )

# Visualizar o resultado
head(dados_limpos)