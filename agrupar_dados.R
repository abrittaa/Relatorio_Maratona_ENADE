library(tidyverse)

# Consolida 1 linha por aluno, expandindo as respostas/notas em novas colunas
dados_agrupados <- dados_completos %>% 
  pivot_wider(
    names_from = id_teste,      # As pastas/testes viram novas colunas
    values_from = -c(Name, faculdade, id_teste) # Mantém as respostas organizadas
  )

# Substitui tanto NAs (valores faltantes do R) quanto o texto "NULL"
dados_limpos <- dados_agrupados %>% 
  mutate(
    across(
      everything(), 
      ~ ifelse(is.na(.) | . == "NULL", "Não Realizou o Teste", .)
    )
  )