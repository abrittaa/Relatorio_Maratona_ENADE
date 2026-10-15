library(tidyverse)
library(writexl)

# 1. Objeto 'dados_per_teste' (1 linha para cada Teste que o Aluno realizou)
dados_per_teste <- dados_completos %>% 
  group_by(faculdade, Name, id_teste) %>% 
  summarise(
    `Points Earned`   = sum(as.numeric(`Points Earned`), na.rm = TRUE),
    `Points Possible` = sum(as.numeric(`Points Possible`), na.rm = TRUE),
    Percent           = ifelse(`Points Possible` > 0, round((`Points Earned` / `Points Possible`) * 100, 1), 0),
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