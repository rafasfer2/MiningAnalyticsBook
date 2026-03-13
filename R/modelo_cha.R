# ==============================================================================
# ARQUIVO: modelo_cha.R
# DESCRIÇÃO: Leitura, tratamento (LGPD) e cálculo ARM - RODADA 1 (Formulário 1)
# AUTOR: Rafael (Mining Analytics Book)
# DATA: 2026-03-13
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE PACOTES
# ------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor, stringr, knitr, kableExtra)

# ------------------------------------------------------------------------------
# 2. DIRETÓRIOS E LEITURA DOS DADOS BRUTOS (RODADA 1)
# ------------------------------------------------------------------------------
caminho_respostas_f1 <- "C:/Users/rafas/OneDrive/UNIFEI/PL 2026.1/FORMULÁRIO 1 — Autopercepção de Desempenho (Individual).xlsx"
caminho_matriz       <- "C:/Users/rafas/OneDrive/UNIFEI/PL 2026.1/Matriz Pesos e CHA.xlsx"

# Importa os dados brutos tagueando que pertencem ao Form 1
df_bruto_f1 <- read_excel(caminho_respostas_f1)
df_matriz_pesos <- read_excel(caminho_matriz, sheet = "Matriz Perfil x Dim x Int")

# ==============================================================================
# BLOCO DE PROCESSAMENTO 1: PARAMETRIZAÇÃO DAS RESPOSTAS DO ALUNO (RODADA 1)
# ==============================================================================

# O dataframe principal ganha o sufixo _f1 para não confundir com os futuros formulários
df_scores_f1 <- df_bruto_f1 %>%
  # 1. Limpa e extrai os códigos (C01, H01, A01) dos cabeçalhos longos do Forms
  rename_with(~ str_extract(., "[CHA][0-9]{2}"), matches("\\.[CHA][0-9]{2}:")) %>%
  clean_names() %>%
  rename_with(toupper, matches("^[cha][0-9]{2}$")) %>%

  # 2. Transforma os textos da escala Likert em valores numéricos (Autopercepção)
  mutate(
    across(matches("^C[0-9]{2}$"), ~ case_when(
      . == "Ruim" ~ 1, . == "Médio" ~ 2, . == "Bom" ~ 3, . == "Muito Bom" ~ 4, TRUE ~ NA_real_
    )),
    across(matches("^H[0-9]{2}$"), ~ case_when(
      str_detect(., "Nenhuma") ~ 1, str_detect(., "Pouca") ~ 2, str_detect(., "moderada") ~ 3,
      str_detect(., "plena") ~ 4, str_detect(., "Muita") ~ 5, TRUE ~ NA_real_
    )),
    across(matches("^A[0-9]{2}$"), ~ case_when(
      . == "Nunca" ~ 1, . == "Raramente" ~ 2, . == "Às vezes" ~ 3,
      . == "Frequentemente" ~ 4, . == "Sempre" ~ 5, TRUE ~ NA_real_
    ))
  ) %>%

  # 3. CAMADA DE ANONIMIZAÇÃO (LGPD)
  mutate(
    # Cria um ID cego sequencial
    ID_Respondente = paste0("Aluno_", str_pad(row_number(), 2, pad = "0"))
  ) %>%
  # Remove todas as colunas que possuam dados de identificação pessoal (PII)
  select(
    -any_of(c("email", "nome", "nome_completo", "matricula", "e_mail_institucional")),
    -starts_with("declaro_")
  ) %>%
  # Reorganiza o dataframe para colocar o novo ID no início
  select(id, ID_Respondente, hora_de_inicio, hora_de_conclusao, turma, starts_with("C"), starts_with("H"), starts_with("A"), everything())

# Visualizando o resultado limpo da Rodada 1
print(df_scores_f1, n = Inf)

# ==============================================================================
# BLOCO DE PROCESSAMENTO 2: EXTRAÇÃO E TRATAMENTO DOS PESOS BASE (W_i)
# ==============================================================================

# O objetivo aqui é isolar o 'DNA' de importância de cada competência
# A matriz de pesos é a mesma para todos os formulários, logo não precisa do sufixo _f1.

df_pesos_base_competencias <- df_matriz_pesos %>%
  # Filtramos apenas o bloco das 8 Competências Macros (C01 a C08)
  filter(str_detect(Cód, "^C[0-9]{2}$")) %>%
  mutate(
    # Conversão de string (ex: "14,62%") para valor numérico decimal (0.1462)
    Peso_Base = as.numeric(str_replace(Peso, ",", ".")) / 100
  ) %>%
  # Mantemos apenas as colunas de identificação e o peso base estrutural
  select(P, Perfil, Cód, Peso_Base)

df_wide_pesos <- df_pesos_base_competencias %>%
  select(-Perfil) %>%
  pivot_wider(names_from = P, values_from = Peso_Base)

# Adicionando a linha de totalização (100% / 1.0) para conferência metodológica
df_totais_pesos <- df_wide_pesos %>%
  summarise(
    Cód = "TOTAL",
    across(starts_with("P0"), sum)
  )

df_final_pesos <- bind_rows(df_wide_pesos, df_totais_pesos)

print(df_final_pesos)

# ==============================================================================
# BLOCO DE PROCESSAMENTO 3: O MOTOR DO ARM E CÁLCULO DOS PERFIS (RODADA 1)
# ==============================================================================

# Para dar prosseguimento ao processamento matemático dos Scores, precisamos
# das matrizes completas com as Intensidades (Forte/Moderada/Fraca) parametrizadas.

df_pesos_automatizados <- df_matriz_pesos %>%
  mutate(
    Peso_Base = as.numeric(str_replace(Peso, ",", ".")) / 100,

    # AUTOMAÇÃO DA INTENSIDADE
    Int_Num = case_when(
      Intensidade == "Forte" ~ 2.0,
      Intensidade == "Moderada" ~ 1.0,
      Intensidade == "Fraca" ~ 0.2,
      TRUE ~ 0
    ),

    W_i = Peso_Base * Int_Num
  ) %>%
  select(P, Perfil, Cód, Peso_Base, W_i)

# Cruzamento: S_i_f1 (Notas de Autopercepção) * W_i (Pesos do Mercado)
df_arm_f1 <- df_scores_f1 %>%
  select(id, ID_Respondente, matches("^[CHA][0-9]{2}$")) %>%
  pivot_longer(
    cols = matches("^[CHA][0-9]{2}$"),
    names_to = "Cód",
    values_to = "Score_Si"
  ) %>%
  # Cruza a nota do aluno com o peso automatizado de cada um dos 6 perfis
  inner_join(df_pesos_automatizados, by = "Cód", relationship = "many-to-many") %>%
  mutate(
    # A EQUAÇÃO CENTRAL DO ARM MULTIVARIADO PARA A RODADA 1:
    Valor_Variavel = Score_Si * W_i,
    # Cálculo Linear (Sem Intensidade) para o Teste de Estresse
    Valor_Linear = Score_Si * Peso_Base
  ) %>%
  # Soma todos os itens para dar a nota (Theta absoluto) final do perfil na Rodada 1
  group_by(id, ID_Respondente, P, Perfil) %>%
  summarise(
    ARM_Score = sum(Valor_Variavel, na.rm = TRUE),
    Linear_Score = sum(Valor_Linear, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# BLOCO DE PROCESSAMENTO 4: NORMALIZAÇÃO E PERFIL DOMINANTE (RODADA 1)
# ==============================================================================

# --- CÁLCULO ARM (COM INTENSIDADES) ---
df_perfis_wide_f1 <- df_arm_f1 %>%
  select(id, ID_Respondente, P, ARM_Score) %>%
  pivot_wider(names_from = P, values_from = ARM_Score)

df_perfis_relativos_f1 <- df_perfis_wide_f1 %>%
  rowwise() %>%
  mutate(
    Soma_Total = sum(c_across(P01:P06), na.rm = TRUE),
    P01_pct = (P01 / Soma_Total) * 100,
    P02_pct = (P02 / Soma_Total) * 100,
    P03_pct = (P03 / Soma_Total) * 100,
    P04_pct = (P04 / Soma_Total) * 100,
    P05_pct = (P05 / Soma_Total) * 100,
    P06_pct = (P06 / Soma_Total) * 100,
    Max_Score = max(c_across(P01_pct:P06_pct), na.rm = TRUE),
    Perfil_Dominante = case_when(
      abs(Max_Score - P01_pct) < 1e-9 ~ "P1 (Analítico-Técnico)",
      abs(Max_Score - P02_pct) < 1e-9 ~ "P2 (Processos e Operações)",
      abs(Max_Score - P03_pct) < 1e-9 ~ "P3 (Criativo-Inovador)",
      abs(Max_Score - P04_pct) < 1e-9 ~ "P4 (Comunicador-Articulador)",
      abs(Max_Score - P05_pct) < 1e-9 ~ "P5 (Organizacional-Líder)",
      abs(Max_Score - P06_pct) < 1e-9 ~ "P6 (Sustentável-Responsável)"
    )
  ) %>%
  ungroup()

# Exibe os alunos como amostra de conferência do Radar (Rodada 1)
df_perfis_relativos_f1 %>%
  select(ID_Respondente, P01_pct:P06_pct, Perfil_Dominante) %>%
  print(n = Inf)

# ==============================================================================
# BLOCO DE PROCESSAMENTO 5: INVENTÁRIO DO CAPITAL HUMANO E TESTE DE ESTRESSE
# ==============================================================================

# --- CÁLCULO LINEAR (SEM INTENSIDADES - PARA O TESTE DE ESTRESSE) ---
df_perfis_wide_linear <- df_arm_f1 %>%
  select(id, ID_Respondente, P, Linear_Score) %>%
  pivot_wider(names_from = P, values_from = Linear_Score)

df_perfis_relativos_linear <- df_perfis_wide_linear %>%
  rowwise() %>%
  mutate(
    Soma_Total_Lin = sum(c_across(P01:P06), na.rm = TRUE),
    P01_pct_lin = (P01 / Soma_Total_Lin) * 100,
    P02_pct_lin = (P02 / Soma_Total_Lin) * 100,
    P03_pct_lin = (P03 / Soma_Total_Lin) * 100,
    P04_pct_lin = (P04 / Soma_Total_Lin) * 100,
    P05_pct_lin = (P05 / Soma_Total_Lin) * 100,
    P06_pct_lin = (P06 / Soma_Total_Lin) * 100,
    Max_Score_Lin = max(c_across(P01_pct_lin:P06_pct_lin), na.rm = TRUE),
    Perfil_Dominante_Linear = case_when(
      abs(Max_Score_Lin - P01_pct_lin) < 1e-9 ~ "P1 (Analítico-Técnico)",
      abs(Max_Score_Lin - P02_pct_lin) < 1e-9 ~ "P2 (Processos e Operações)",
      abs(Max_Score_Lin - P03_pct_lin) < 1e-9 ~ "P3 (Criativo-Inovador)",
      abs(Max_Score_Lin - P04_pct_lin) < 1e-9 ~ "P4 (Comunicador-Articulador)",
      abs(Max_Score_Lin - P05_pct_lin) < 1e-9 ~ "P5 (Organizacional-Líder)",
      abs(Max_Score_Lin - P06_pct_lin) < 1e-9 ~ "P6 (Sustentável-Responsável)"
    )
  ) %>%
  ungroup()

# Consolidação da Tabela Comparativa (Teste de Estresse)
df_resumo_linear <- df_perfis_relativos_linear %>%
  filter(ID_Respondente != "Aluno_01") %>%
  group_by(Perfil_Dominante_Linear) %>%
  summarise(Qtd_Linear = n(), .groups = "drop") %>%
  rename(Perfil = Perfil_Dominante_Linear)

df_resumo_arm <- df_perfis_relativos_f1 %>%
  filter(ID_Respondente != "Aluno_01") %>%
  group_by(Perfil_Dominante) %>%
  summarise(Qtd_ARM = n(), .groups = "drop") %>%
  rename(Perfil = Perfil_Dominante)

df_perfis_base <- data.frame(
  Perfil = c(
    "P1 (Analítico-Técnico)", "P2 (Processos e Operações)",
    "P3 (Criativo-Inovador)", "P4 (Comunicador-Articulador)",
    "P5 (Organizacional-Líder)", "P6 (Sustentável-Responsável)"
  )
)

df_impacto_final <- df_perfis_base %>%
  left_join(df_resumo_linear, by = "Perfil") %>%
  left_join(df_resumo_arm, by = "Perfil") %>%
  mutate(
    Qtd_Linear = replace_na(Qtd_Linear, 0),
    Qtd_ARM = replace_na(Qtd_ARM, 0),
    Delta = Qtd_ARM - Qtd_Linear,
    Delta_Formatado = case_when(
      Delta > 0 ~ paste0("+", Delta),
      Delta < 0 ~ as.character(Delta),
      TRUE ~ "-"
    )
  )

print(df_impacto_final)
