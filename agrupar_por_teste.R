library(tidyverse)
library(writexl)

# 1. Objeto 'dados_per_teste' (1 linha para cada Teste que o Aluno realizou)
dados_per_teste <- dados_completos %>% 
  # 1. Deduplicação: se houver mais de um envio do mesmo teste, mantém o de maior percentual
  arrange(faculdade, Name_norm, id_teste, desc(as.numeric(Percent))) %>% 
  distinct(faculdade, Name_norm, id_teste, .keep_all = TRUE) %>% 
  # 2. Agrupamento seguro pelo nome normalizado
  group_by(faculdade, Name_norm, id_teste) %>% 
  summarise(
    Name              = first(Name), # Preserva a melhor grafia
    `Points Earned`   = as.numeric(`Points Earned`),
    `Points Possible` = as.numeric(`Points Possible`),
    Percent           = round((`Points Earned` / `Points Possible`) * 100, 1),
    Time              = first(Time),
    `Start Datetime`  = first(`Start Datetime`),
    `End Datetime`    = first(`End Datetime`),
    .groups = "drop"
  ) %>% 
  arrange(Name, id_teste)

# 2. (Bônus) Matriz Lado a Lado: Visualização em colunas para cada teste
# Mostra uma linha por aluno e a nota (% de acerto) de cada Teste em colunas separadas
dados_per_teste_matriz <- dados_per_teste %>% 
  select(faculdade, Name, id_teste, Percent) %>% 
  pivot_wider(
    names_from = id_teste,
    values_from = Percent,
    names_prefix = "Nota_%_"
  )

# Visualizar no RStudio
head(dados_per_teste)
head(dados_per_teste_matriz)

# Exportar para CSV e Excel
write_csv(dados_per_teste, "dados_per_teste.csv")
write_xlsx(dados_per_teste, "dados_per_teste.xlsx")

write_csv(dados_per_teste_matriz, "dados_per_teste_matriz.csv")
write_xlsx(dados_per_teste_matriz, "dados_per_teste_matriz.xlsx")