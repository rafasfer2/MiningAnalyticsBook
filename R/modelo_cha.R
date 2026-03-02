# ==============================================================================
# ARQUIVO: modelo_cha.R
# DESCRIÇÃO: Leitura, automação de pesos e cálculo Multivariado (ARM)
# AUTOR: Rafael (Mining Analytics Book)
# DATA: 2026-03-02
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE PACOTES
# ------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor, stringr)

# ------------------------------------------------------------------------------
# 2. DIRETÓRIOS E LEITURA DOS DADOS BRUTOS
# ------------------------------------------------------------------------------
caminho_respostas <- "C:/Users/rafas/OneDrive/UNIFEI/PL 2026.1/FORMULÁRIO 1 — Autopercepção de Desempenho (Individual).xlsx"
caminho_matriz    <- "C:/Users/rafas/OneDrive/UNIFEI/PL 2026.1/Matriz Pesos e CHA.xlsx"

df_bruto_forms <- read_excel(caminho_respostas)
df_matriz_pesos <- read_excel(caminho_matriz, sheet = "Matriz Perfil x Dim x Int")

# ==============================================================================
# BLOCO DE PROCESSAMENTO 1: PARAMETRIZAÇÃO DAS RESPOSTAS DO ALUNO (S_i)
# ==============================================================================

df_scores <- df_bruto_forms %>%
  # Limpa e extrai os códigos (C01, H01, A01) dos cabeçalhos longos do Forms
  rename_with(~ str_extract(., "[CHA][0-9]{2}"), matches("\\.[CHA][0-9]{2}:")) %>%
  clean_names() %>%
  rename_with(toupper, matches("^[cha][0-9]{2}$")) %>%

  # Transforma os textos da escala Likert em valores numéricos
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
  )

# ==============================================================================
# BLOCO DE PROCESSAMENTO 2: AUTOMAÇÃO MATEMÁTICA DOS PESOS (W_i)
# ==============================================================================

df_pesos_automatizados <- df_matriz_pesos %>%
  mutate(
    # Trata a coluna de Peso Base (remove vírgulas e converte percentual)
    Peso_Base = as.numeric(str_replace(Peso, ",", ".")) / 100,

    # AUTOMAÇÃO DA INTENSIDADE: Transforma texto em multiplicador numérico
    Int_Num = case_when(
      Intensidade == "Forte" ~ 2.0,
      Intensidade == "Moderada" ~ 1.0,
      Intensidade == "Fraca" ~ 0.2,
      TRUE ~ 0 # Segurança contra erros de digitação
    ),

    # Calcula o Peso Efetivo Final (W_i) daquela variável para aquele perfil
    W_i = Peso_Base * Int_Num
  ) %>%
  select(P, Perfil, Cód, W_i)

# ==============================================================================
# BLOCO DE PROCESSAMENTO 3: O MOTOR DO ARM (CRUZAMENTO S_i * W_i)
# ==============================================================================

# Transforma as notas dos alunos em formato de lista (Long) para cruzar com pesos
df_arm_calculo <- df_scores %>%
  select(id, nome_completo, matricula, matches("^[CHA][0-9]{2}$")) %>%
  pivot_longer(
    cols = matches("^[CHA][0-9]{2}$"),
    names_to = "Cód",
    values_to = "Score_Si"
  ) %>%
  # Cruza a nota do aluno com o peso automatizado de cada um dos 6 perfis
  inner_join(df_pesos_automatizados, by = "Cód", relationship = "many-to-many") %>%
  mutate(
    # A EQUAÇÃO CENTRAL DO ARM MULTIVARIADO:
    Valor_Variavel = Score_Si * W_i
  ) %>%
  # Soma todos os itens para dar a nota final do perfil
  group_by(id, nome_completo, matricula, P, Perfil) %>%
  summarise(
    ARM_Score = sum(Valor_Variavel, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# BLOCO DE PROCESSAMENTO 4: TABELA EXECUTIVA E DIAGNÓSTICO
# ==============================================================================

# 1. Descobre qual é o Perfil Dominante (Maior Nota) de cada aluno
df_vencedor <- df_arm_calculo %>%
  group_by(id) %>%
  slice_max(order_by = ARM_Score, n = 1) %>%
  ungroup() %>%
  select(id, Perfil_Vencedor = Perfil, Score_Vencedor = ARM_Score)

# 2. Pivota os 6 perfis para colunas (P01, P02, P03...)
df_perfis_wide <- df_arm_calculo %>%
  select(id, P, ARM_Score) %>%
  pivot_wider(names_from = P, values_from = ARM_Score)

# 3. Monta a Tabela Executiva Final
df_classificacao_final <- df_scores %>%
  # Calcula a soma linear clássica (Apenas para referência/comparação C-H-A)
  mutate(
    C = rowSums(select(., matches("^C[0-9]{2}$")), na.rm = TRUE),
    H = rowSums(select(., matches("^H[0-9]{2}$")), na.rm = TRUE),
    A = rowSums(select(., matches("^A[0-9]{2}$")), na.rm = TRUE)
  ) %>%
  select(id, nome_completo, matricula, C, H, A) %>%
  # Junta com as notas dos 6 perfis processadas pelo ARM
  left_join(df_perfis_wide, by = "id") %>%
  # Junta com a conclusão de qual perfil ganhou
  left_join(df_vencedor, by = "id")

# Visualização instantânea do resultado final para a formação dos grupos
print(df_classificacao_final)
