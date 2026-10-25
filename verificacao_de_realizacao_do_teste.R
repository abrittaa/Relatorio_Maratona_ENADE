library(tidyverse)
library(writexl)

# 0. Verificar se 'dados_completos' existe no ambiente -------------------
# Se não existir, executa o script de criação de dados
if (!exists("dados_completos")) {
  source("criar_dados.R")
}

# 1. Mapear a lista completa de todos os testes existentes ----------------

todos_testes <- unique(dados_completos$id_teste)
total_testes <- length(todos_testes)


# 2. Resumo por Aluno: Testes Pendentes e Porcentagem de Conclusão -------

relatorio_testes_alunos <- dados_completos %>% 
  group_by(faculdade, Name) %>% 
  summarise(
    # Contagem de testes únicos que o aluno realizou
    testes_realizados_qtd = n_distinct(id_teste),
    
    # Porcentagem de testes realizados em relação ao total
    pct_testes_realizados = round((testes_realizados_qtd / total_testes) * 100, 1),
    
    # Lista os nomes dos testes que o aluno NÃO respondeu
    testes_nao_respondidos = paste(
      setdiff(todos_testes, unique(id_teste)), 
      collapse = ", "
    ),
    .groups = "drop"
  ) %>% 
  # Substitui o texto em branco por aviso claro quando o aluno fez 100% dos testes
  mutate(
    testes_nao_respondidos = if_else(
      testes_nao_respondidos == "", 
      "Nenhum (Respondeu a todos)", 
      testes_nao_respondidos
    )
  ) %>% 
  arrange(faculdade, Name)


# 3. Visão Matriz Lado a Lado: Status por Teste (Respondido / Não Respondido)

matriz_status_testes <- dados_completos %>% 
  distinct(faculdade, Name, id_teste) %>% 
  mutate(status = "Respondido") %>% 
  pivot_wider(
    names_from = id_teste, 
    values_from = status, 
    values_fill = "Não Respondido"
  ) %>% 
  arrange(faculdade, Name)


# 4. Visualizar os resultados no RStudio ------------------------------------

head(relatorio_testes_alunos)
head(matriz_status_testes)


# 5. Exportar para CSV e Excel ----------------------------------------------

write_csv(relatorio_testes_alunos, "relatorio_testes_alunos.csv")
write_xlsx(relatorio_testes_alunos, "relatorio_testes_alunos.xlsx")

write_csv(matriz_status_testes, "matriz_status_testes_matriz.csv")
write_xlsx(matriz_status_testes, "matriz_status_testes_matriz.xlsx")