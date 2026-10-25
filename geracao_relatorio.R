# ==============================================================================
# SCRIPT: gerar_portal_html.R
# OBJETIVO: Unir relatórios de desempenho e gerar Portal HTML com consulta
#           individual LGPD Compliant e Emissão de Certificado Personalizado.
# ==============================================================================

library(tidyverse)
library(jsonlite)
library(digest)

# 1. Carregar e Executar Scripts Anteriores ------------------------------------
message("Carregando e integrando dados dos testes...")

if (file.exists("criar_dados.R")) source("criar_dados.R")
if (file.exists("agrupar_dados.R")) source("agrupar_dados.R")
if (file.exists("agrupar_por_teste.R")) source("agrupar_por_teste.R")
if (file.exists("verificacao_de_realizacao_do_teste.R")) source("verificacao_de_realizacao_do_teste.R")

# 2. Funções de Normalização e Hashing (LGPD) ----------------------------------
normalizar_nome <- function(nome) {
  nome %>% 
    trimws() %>% 
    tolower() %>% 
    gsub("\\s+", " ", .)
}

gerar_hash <- function(nome) {
  nome_norm <- normalizar_nome(nome)
  digest::digest(nome_norm, algo = "sha256", serialize = FALSE)
}

# 3. Mapeamento e Consolidação dos Dados por Aluno ----------------------------
message("Consolidando base de dados por aluno...")

alunos_unicos <- dados_limpos %>% 
  select(Name, faculdade) %>% 
  distinct()

todos_testes_sistema <- sort(unique(dados_completos$id_teste))
total_testes_sistema <- length(todos_testes_sistema)

dados_alunos_dict <- list()

for (i in 1:nrow(alunos_unicos)) {
  aluno_nome <- alunos_unicos$Name[i]
  aluno_faculdade <- alunos_unicos$faculdade[i]
  hash_aluno <- gerar_hash(aluno_nome)
  
  # Desempenho Geral (dados_limpos)
  desempenho_geral <- dados_limpos %>% 
    filter(Name == aluno_nome, faculdade == aluno_faculdade)
  
  pts_earned <- ifelse(nrow(desempenho_geral) > 0, desempenho_geral$`Points Earned`[1], 0)
  pts_possible <- ifelse(nrow(desempenho_geral) > 0, desempenho_geral$`Points Possible`[1], 0)
  pct_geral <- ifelse(nrow(desempenho_geral) > 0, desempenho_geral$Percent[1], 0)
  
  # Status de Realização (relatorio_testes_alunos)
  rel_status <- relatorio_testes_alunos %>% 
    filter(Name == aluno_nome, faculdade == aluno_faculdade)
  
  qtd_realizados <- ifelse(nrow(rel_status) > 0, rel_status$testes_realizados_qtd[1], 0)
  pct_realizados <- ifelse(nrow(rel_status) > 0, rel_status$pct_testes_realizados[1], 0)
  nao_respondidos <- ifelse(nrow(rel_status) > 0, rel_status$testes_nao_respondidos[1], "")
  
  # Detalhamento individual por teste (dados_per_teste)
  testes_aluno <- dados_per_teste %>% 
    filter(Name == aluno_nome, faculdade == aluno_faculdade)
  
  detalhes_testes <- lapply(todos_testes_sistema, function(t_id) {
    t_data <- testes_aluno %>% filter(id_teste == t_id)
    if (nrow(t_data) > 0) {
      list(
        id_teste = t_id,
        status = "Concluído",
        points_earned = t_data$`Points Earned`[1],
        points_possible = t_data$`Points Possible`[1],
        percent = t_data$Percent[1],
        time = ifelse(is.na(t_data$Time[1]), "-", t_data$Time[1])
      )
    } else {
      list(
        id_teste = t_id,
        status = "Pendente",
        points_earned = 0,
        points_possible = 0,
        percent = 0,
        time = "-"
      )
    }
  })
  
  codigo_validacao <- paste0("CERT-", toupper(substr(hash_aluno, 1, 10)))
  
  dados_alunos_dict[[hash_aluno]] <- list(
    nome = aluno_nome,
    faculdade = aluno_faculdade,
    pontos_ganhos = pts_earned,
    pontos_possiveis = pts_possible,
    porcentagem_geral = pct_geral,
    testes_concluidos = qtd_realizados,
    total_testes = total_testes_sistema,
    progresso_pct = pct_realizados,
    testes_pendentes = nao_respondidos,
    detalhes_testes = detalhes_testes,
    codigo_certificado = codigo_validacao,
    data_emissao = format(Sys.Date(), "%d/%m/%Y")
  )
}

# Converter base de alunos para JSON seguro
json_alunos <- jsonlite::toJSON(dados_alunos_dict, auto_unbox = TRUE)

# 4. Construção do HTML Completo (com CSS e JS nativos) -----------------------
message("Gerando arquivo portal_alunos.html...")

conteudo_html <- paste0('<!DOCTYPE html>
<html lang="pt-BR">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Portal do Aluno - Consulta de Desempenho & Certificados</title>
  <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&family=Cinzel:wght@600;700&display=swap" rel="stylesheet">
  <style>
    :root {
      --primary: #1e3a8a;
      --primary-hover: #1d4ed8;
      --bg: #f8fafc;
      --card-bg: #ffffff;
      --text: #0f172a;
      --text-muted: #64748b;
      --border: #e2e8f0;
      --success: #16a34a;
      --warning: #d97706;
      --danger: #dc2626;
    }

    * { box-sizing: border-box; margin: 0; padding: 0; }
    body { font-family: "Inter", sans-serif; background-color: var(--bg); color: var(--text); line-height: 1.5; padding: 20px; }

    .container { max-width: 900px; margin: 30px auto; }
    
    .header { text-align: center; margin-bottom: 30px; }
    .header h1 { color: var(--primary); font-size: 2rem; margin-bottom: 8px; }
    .header p { color: var(--text-muted); font-size: 0.95rem; }

    .card { background: var(--card-bg); border-radius: 12px; border: 1px solid var(--border); padding: 24px; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05); margin-bottom: 24px; }

    .search-box { display: flex; gap: 12px; margin-top: 16px; }
    .search-box input { flex: 1; padding: 12px 16px; border: 1px solid var(--border); border-radius: 8px; font-size: 1rem; outline: none; }
    .search-box input:focus { border-color: var(--primary); box-shadow: 0 0 0 3px rgba(30, 58, 138, 0.1); }
    .btn { background: var(--primary); color: white; border: none; padding: 12px 20px; border-radius: 8px; font-weight: 600; cursor: pointer; transition: background 0.2s; font-size: 0.95rem; }
    .btn:hover { background: var(--primary-hover); }
    .btn-secondary { background: #475569; }
    .btn-secondary:hover { background: #334155; }

    .error-msg { color: var(--danger); font-size: 0.9rem; margin-top: 10px; display: none; }

    .hidden { display: none !important; }

    /* Performance Dashboard */
    .student-header { border-bottom: 1px solid var(--border); padding-bottom: 16px; margin-bottom: 20px; }
    .student-header h2 { color: var(--primary); font-size: 1.5rem; }
    .student-header p { color: var(--text-muted); font-size: 0.95rem; }

    .metrics-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 16px; margin-bottom: 24px; }
    .metric-card { background: #f1f5f9; padding: 16px; border-radius: 8px; text-align: center; }
    .metric-card .value { font-size: 1.6rem; font-weight: 700; color: var(--primary); }
    .metric-card .label { font-size: 0.85rem; color: var(--text-muted); text-transform: uppercase; font-weight: 600; margin-top: 4px; }

    .progress-bar-bg { background: #e2e8f0; height: 10px; border-radius: 5px; overflow: hidden; margin-top: 8px; }
    .progress-bar-fill { background: var(--primary); height: 100%; width: 0%; transition: width 0.4s ease; }

    table { width: 100%; border-collapse: collapse; margin-top: 16px; }
    th, td { padding: 12px; text-align: left; border-bottom: 1px solid var(--border); font-size: 0.9rem; }
    th { background: #f8fafc; color: var(--text-muted); font-weight: 600; }

    .badge { display: inline-block; padding: 4px 8px; border-radius: 4px; font-size: 0.75rem; font-weight: 700; text-transform: uppercase; }
    .badge-success { background: #dcfce7; color: #15803d; }
    .badge-pending { background: #fef3c7; color: #b45309; }

    /* Certificado */
    .certificate-container { background: #ffffff; border: 10px solid #1e3a8a; padding: 40px; text-align: center; border-radius: 8px; margin-top: 30px; position: relative; box-shadow: 0 10px 25px rgba(0,0,0,0.1); }
    .certificate-title { font-family: "Cinzel", serif; font-size: 2.2rem; color: #1e3a8a; letter-spacing: 2px; margin-bottom: 10px; }
    .certificate-subtitle { font-size: 1rem; color: #64748b; text-transform: uppercase; letter-spacing: 1.5px; margin-bottom: 30px; }
    .certificate-name { font-size: 2rem; font-weight: 700; color: #0f172a; border-bottom: 2px solid #1e3a8a; display: inline-block; padding: 0 20px 5px; margin: 20px 0; }
    .certificate-body { font-size: 1.1rem; color: #334155; line-height: 1.8; margin-bottom: 30px; }
    .certificate-footer { display: flex; justify-content: space-between; align-items: flex-end; margin-top: 40px; padding-top: 20px; border-top: 1px solid #e2e8f0; font-size: 0.85rem; color: #64748b; }

    @media print {
      body * { visibility: hidden; }
      .certificate-container, .certificate-container * { visibility: visible; }
      .certificate-container { position: absolute; left: 0; top: 0; width: 100%; border: 8px solid #1e3a8a; padding: 30px; }
      .no-print { display: none !important; }
    }
  </style>
</head>
<body>

  <div class="container">
    <div class="header no-print">
      <h1>Portal do Aluno</h1>
      <p>Consulte seu desempenho nos testes e obtenha seu certificado de conclusão</p>
    </div>

    <!-- Tela de Busca LGPD -->
    <div class="card no-print" id="searchSection">
      <h3>Consulta Individual</h3>
      <p style="font-size: 0.88rem; color: var(--text-muted); margin-top: 4px;">
        Por motivos de privacidade (LGPD), insira seu <strong>Nome Completo</strong> exatamente como cadastrado para visualizar suas notas.
      </p>
      <div class="search-box">
        <input type="text" id="studentNameInput" placeholder="Digite seu Nome Completo aqui..." onkeypress="if(event.key === \'Enter\') consultarAlunos()">
        <button class="btn" onclick="consultarAlunos()">Consultar</button>
      </div>
      <div class="error-msg" id="errorMsg">Nome não encontrado. Por favor, verifique o nome digitado e tente novamente.</div>
    </div>

    <!-- Dashboard de Desempenho -->
    <div id="resultsSection" class="hidden">
      <div class="card no-print">
        <div class="student-header">
          <h2 id="dispName">Nome do Aluno</h2>
          <p id="dispFaculdade">Faculdade / Unidade</p>
        </div>

        <div class="metrics-grid">
          <div class="metric-card">
            <div class="value" id="dispGeralPct">0%</div>
            <div class="label">Aproveitamento Geral</div>
          </div>
          <div class="metric-card">
            <div class="value" id="dispPontos">0 / 0</div>
            <div class="label">Pontos Obtidos</div>
          </div>
          <div class="metric-card">
            <div class="value" id="dispTestesQtd">0 / 0</div>
            <div class="label">Testes Realizados</div>
          </div>
        </div>

        <div style="margin-bottom: 20px;">
          <div style="display: flex; justify-content: space-between; font-size: 0.85rem; font-weight: 600; color: var(--text-muted);">
            <span>Progresso da Carga Horária / Testes</span>
            <span id="dispProgresoPct">0%</span>
          </div>
          <div class="progress-bar-bg">
            <div class="progress-bar-fill" id="dispProgressoFill"></div>
          </div>
        </div>

        <div id="pendentesAlert" class="hidden" style="background: #fffbeb; border: 1px solid #fde68a; padding: 12px; border-radius: 8px; font-size: 0.88rem; color: #92400e; margin-bottom: 20px;">
          <strong>Atenção:</strong> Você ainda possui os seguintes testes pendentes: <span id="dispPendentesLista"></span>.
        </div>

        <h3>Detalhamento por Teste</h3>
        <table>
          <thead>
            <tr>
              <th>Teste / Módulo</th>
              <th>Status</th>
              <th>Pontos</th>
              <th>Aproveitamento</th>
              <th>Tempo</th>
            </tr>
          </thead>
          <tbody id="tableBodyTestes">
          </tbody>
        </table>

        <div style="margin-top: 24px; text-align: right;">
          <button class="btn btn-secondary" onclick="novaBusca()">Nova Consulta</button>
        </div>
      </div>

      <!-- Certificado Personalizado -->
      <div id="certificateSection" class="certificate-container">
        <div class="certificate-title">CERTIFICADO DE DESEMPENHO</div>
        <div class="certificate-subtitle">Certificamos a participação e conclusão de avaliação</div>
        
        <p class="certificate-body">
          Certificamos que o(a) estudante
        </p>
        <div class="certificate-name" id="certName">Nome do Estudante</div>
        <p class="certificate-body">
          filiado(a) à <strong><span id="certFaculdade">Faculdade</span></strong>, realizou a bateria de avaliações acadêmicas, obtendo um aproveitamento geral de <strong><span id="certPct">0%</span></strong> com <strong><span id="certPontos">0</span></strong> pontos computados.
        </p>

        <div class="certificate-footer">
          <div>
            <strong>Código de Autenticidade:</strong> <span id="certCodigo">CERT-XXXXX</span><br>
            <strong>Data de Emissão:</strong> <span id="certData">00/00/0000</span>
          </div>
          <div class="no-print">
            <button class="btn" onclick="window.print()">🖨️ Imprimir / Salvar PDF</button>
          </div>
        </div>
      </div>
    </div>
  </div>

  <script>
    // Dicionário criptografado contendo os dados dos alunos
    const DB_ALUNOS = ', json_alunos, ';

    // Função Hash SHA-256 no cliente
    async function sha256(str) {
      const buf = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(str));
      return Array.prototype.map.call(new Uint8Array(buf), x => (("0" + x.toString(16)).slice(-2))).join("");
    }

    function normalizarTexto(txt) {
      return txt.trim().toLowerCase().replace(/\\s+/g, " ");
    }

    async function consultarAlunos() {
      const inputVal = document.getElementById("studentNameInput").value;
      const errorMsg = document.getElementById("errorMsg");
      
      if (!inputVal.trim()) {
        errorMsg.style.display = "block";
        errorMsg.innerText = "Por favor, digite seu nome completo.";
        return;
      }

      const nomeNorm = normalizarTexto(inputVal);
      const hashKey = await sha256(nomeNorm);

      if (DB_ALUNOS[hashKey]) {
        errorMsg.style.display = "none";
        renderizarAluno(DB_ALUNOS[hashKey]);
      } else {
        errorMsg.style.display = "block";
        errorMsg.innerText = "Nome não encontrado. Certifique-se de digitar seu nome completo exatamente como cadastrado.";
      }
    }

    function renderizarAluno(aluno) {
      document.getElementById("searchSection").classList.add("hidden");
      document.getElementById("resultsSection").classList.remove("hidden");

      // Header
      document.getElementById("dispName").innerText = aluno.nome;
      document.getElementById("dispFaculdade").innerText = "Instituição: " + aluno.faculdade;

      // Métricas
      document.getElementById("dispGeralPct").innerText = aluno.porcentagem_geral + "%";
      document.getElementById("dispPontos").innerText = aluno.pontos_ganhos + " / " + aluno.pontos_possiveis;
      document.getElementById("dispTestesQtd").innerText = aluno.testes_concluidos + " / " + aluno.total_testes;
      
      // Barra de Progresso
      document.getElementById("dispProgresoPct").innerText = aluno.progresso_pct + "%";
      document.getElementById("dispProgressoFill").style.width = aluno.progresso_pct + "%";

      // Alerta de Pendências
      const alertBox = document.getElementById("pendentesAlert");
      if (aluno.testes_pendentes && aluno.testes_pendentes !== "Nenhum (Respondeu a todos)") {
        alertBox.classList.remove("hidden");
        document.getElementById("dispPendentesLista").innerText = aluno.testes_pendentes;
      } else {
        alertBox.classList.add("hidden");
      }

      // Tabela de Testes
      const tbody = document.getElementById("tableBodyTestes");
      tbody.innerHTML = "";
      
      aluno.detalhes_testes.forEach(t => {
        const tr = document.createElement("tr");
        const statusBadge = t.status === "Concluído" 
          ? \'<span class="badge badge-success">Concluído</span>\' 
          : \'<span class="badge badge-pending">Pendente</span>\';

        tr.innerHTML = `
          <td><strong>${t.id_teste}</strong></td>
          <td>${statusBadge}</td>
          <td>${t.points_earned} / ${t.points_possible}</td>
          <td>${t.percent}%</td>
          <td>${t.time}</td>
        `;
        tbody.appendChild(tr);
      });

      // Certificado
      document.getElementById("certName").innerText = aluno.nome;
      document.getElementById("certFaculdade").innerText = aluno.faculdade;
      document.getElementById("certPct").innerText = aluno.porcentagem_geral + "%";
      document.getElementById("certPontos").innerText = aluno.pontos_ganhos + " de " + aluno.pontos_possiveis;
      document.getElementById("certCodigo").innerText = aluno.codigo_certificado;
      document.getElementById("certData").innerText = aluno.data_emissao;
    }

    function novaBusca() {
      document.getElementById("studentNameInput").value = "";
      document.getElementById("resultsSection").classList.add("hidden");
      document.getElementById("searchSection").classList.remove("hidden");
    }
  </script>
</body>
</html>')

# 5. Salvar arquivo HTML -------------------------------------------------------
writeLines(conteudo_html, con = "portal_alunos.html", useBytes = TRUE)
message("✅ Sucesso! O arquivo 'portal_alunos.html' foi gerado com sucesso.")