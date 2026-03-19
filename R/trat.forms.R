# ==============================================================================
# ARQUIVO: trat.forms.R
# DATA DE CRIACAO: 16/03/2026
# ULTIMA ATUALIZACAO: 18/03/2026
#
# OBJETIVO: Pipeline de dados silencioso para o Capitulo 7.
# ==============================================================================

# 0. Preservar variaveis de controle do Quarto
quiet_source_backup <- if (exists("quiet_source")) quiet_source else NULL

# Limpeza de Ambiente (mantendo backup)
rm(list = setdiff(ls(all.names = TRUE), "quiet_source_backup"))
gc(verbose = FALSE)

if (!is.null(quiet_source_backup)) quiet_source <- quiet_source_backup
rm(quiet_source_backup)

# Carregamento Silencioso de Pacotes
suppressPackageStartupMessages({
    library(Microsoft365R)
    library(readxl)
    library(dplyr)
    library(stringr)
    library(tidyr)
    library(tibble)
})

# ==============================================================================
# PARTE 1: DOWNLOAD SEGURO (ONEDRIVE)
# ==============================================================================

meu_onedrive <- get_business_onedrive()

caminho_form1 <- "UNIFEI/PL 2026.1/FORMULÁRIO 1 — Autopercepção de Desempenho (Individual).xlsx"
caminho_form2 <- "UNIFEI/PL 2026.1/FORMULÁRIO 2 — Exigência das Competências na Prática Profissional (Em Grupo).xlsx"
caminho_form3 <- "UNIFEI/PL 2026.1/FORMULÁRIO 3 — Atribuição de Desempenho dos Colegas (Individual).xlsx"
caminho_form4 <- "UNIFEI/PL 2026.1/FORMULÁRIO 4 — Simulação de Gestão de Crises e Dilemas Operacionais (Individual).xlsx"
caminho_matriz <- "UNIFEI/PL 2026.1/Matriz Pesos e CHA.xlsx"

temp_form1 <- tempfile(fileext = ".xlsx")
temp_form2 <- tempfile(fileext = ".xlsx")
temp_form3 <- tempfile(fileext = ".xlsx")
temp_form4 <- tempfile(fileext = ".xlsx")
temp_matriz <- tempfile(fileext = ".xlsx")

meu_onedrive$download_file(src = caminho_form1, dest = temp_form1, overwrite = TRUE)
meu_onedrive$download_file(src = caminho_form2, dest = temp_form2, overwrite = TRUE)
meu_onedrive$download_file(src = caminho_form3, dest = temp_form3, overwrite = TRUE)
meu_onedrive$download_file(src = caminho_form4, dest = temp_form4, overwrite = TRUE)
meu_onedrive$download_file(src = caminho_matriz, dest = temp_matriz, overwrite = TRUE)

df_form1_bruto <- read_excel(temp_form1)
df_form2_bruto <- read_excel(temp_form2)
df_form3_bruto <- read_excel(temp_form3)
df_form4_bruto <- read_excel(temp_form4)
df_matriz_pesos <- read_excel(temp_matriz, sheet = "Matriz Perfil x Dim x Int")

df_indices_bruto <- read_excel(
  temp_matriz,
  sheet = "Indices_Academicos",
  col_types = c("text", "text", "text", "numeric", "numeric", "numeric")
)

# ==============================================================================
# PARTE 2: TRATAMENTO E DICIONARIOS
# ==============================================================================

escala_competencias <- c("Muito Ruim" = 1, "Ruim" = 2, "Médio" = 3, "Bom" = 4, "Muito Bom" = 5)
escala_habilidades <- c("Pouquíssima experiência" = 1, "Pouca experiência" = 2, "Experiência moderada" = 3, "Muita experiência" = 4, "Experiência plena" = 5)
escala_atitudes <- c("Nunca" = 1, "Raramente" = 2, "Às vezes" = 3, "Frequentemente" = 4, "Sempre" = 5)

# Form 1 - Autopercepcao
df_form1_tratado <- df_form1_bruto %>%
    filter(!is.na(Matrícula)) %>%
    select(Id, Matricula = Matrícula, Turma, matches("[CHA][0-9]{2}")) %>%
    rename_with(~ str_extract(., "[CHA][0-9]{2}"), matches("[CHA][0-9]{2}")) %>%
    mutate(
        across(starts_with("C"), ~ recode(str_trim(.), !!!escala_competencias, .default = NA_real_)),
        across(starts_with("H"), ~ recode(str_trim(.), !!!escala_habilidades, .default = NA_real_)),
        across(starts_with("A"), ~ recode(str_trim(.), !!!escala_atitudes, .default = NA_real_))
    )

# Form 2 - Mercado (Pivoteado)
df_form2_tratado <- df_form2_bruto %>%
    mutate(ID_Grupo = paste0("G", str_pad(row_number(), 2, pad = "0"))) %>%
    rename_with(~ paste0("Req_", str_pad(seq_along(.), 2, pad = "0")), starts_with("Classifique")) %>%
    pivot_longer(cols = matches("\\(Aluno \\d\\)"), names_to = c(".value", "Posicao_Grupo"), names_pattern = "(.*) \\(Aluno (\\d)\\)") %>%
    rename(Matricula = `Matrícula`) %>%
    filter(!is.na(Matricula) & str_trim(Matricula) != "") %>%
    mutate(across(starts_with("Req_"), ~ as.numeric(str_trim(as.character(.)))))

# Form 3 - Pares
df_form3_tratado <- df_form3_bruto %>%
    filter(!is.na(Matrícula)) %>%
    select(Matricula_Avaliador = Matrícula, Turma, matches("[CHA][0-9]{2}")) %>%
    rename_with(~ str_extract(., "[CHA][0-9]{2}"), matches("[CHA][0-9]{2}")) %>%
    mutate(
        across(matches("^[CH]"), ~ recode(str_trim(.), !!!escala_competencias, .default = NA_real_)),
        across(starts_with("A"), ~ recode(str_trim(.), !!!escala_atitudes, .default = NA_real_))
    )

# Form 4 - Simulação de Gestão de Crises e Dilemas Operacionais
df_form4_tratado <- df_form4_bruto %>%
    # Usa a melhor chave disponível sem quebrar quando alguma coluna não existir.
    mutate(
        Matricula_Validada = coalesce(
            if ("Matrícula" %in% names(df_form4_bruto)) as.character(.data[["Matrícula"]]) else NA_character_,
            if ("E-mail" %in% names(df_form4_bruto)) as.character(.data[["E-mail"]]) else NA_character_,
            if ("Email" %in% names(df_form4_bruto)) as.character(.data[["Email"]]) else NA_character_,
            if ("Id" %in% names(df_form4_bruto)) as.character(.data[["Id"]]) else NA_character_
        ),
        Matricula_Validada = na_if(str_trim(Matricula_Validada), "")
    ) %>%
    select(Matricula = Matricula_Validada, any_of("Turma"), contains("Conflito")) %>%
    rename_with(~ paste0("Dilema_", str_pad(seq_along(.), 2, pad = "0")), contains("Conflito")) %>%
    mutate(
        # Transforma as respostas longas de texto em categorias A, B, C (com base nos níveis únicos)
        across(starts_with("Dilema_"), ~ LETTERS[as.numeric(as.factor(str_trim(.)))])
    )

# Matriz de Pesos (Motor ARM)
df_pesos_automatizados <- df_matriz_pesos %>%
    mutate(
        Peso_Base = as.numeric(str_replace(Peso, ",", ".")) / 100,
        Int_Num = case_when(
            Intensidade == "Forte" ~ 2.0,
            Intensidade == "Moderada" ~ 1.0,
            Intensidade == "Fraca" ~ 0.2,
            TRUE ~ 0
        ),
        W_i = Peso_Base * Int_Num
    ) %>%
    select(P, Perfil, Cód, Peso_Base, W_i)

# Tratamento dos Índices Acadêmicos
df_indices_tratado <- df_indices_bruto %>%
  filter(!is.na(Matrícula)) %>%
  mutate(
    Matricula = str_trim(Matrícula),
    Nome = str_to_upper(str_trim(Nome))
  ) %>%
  select(Matricula, MC, IEA, IRA)

# ==============================================================================
# PARTE 3: ANONIMIZAÇÃO E RELACIONAMENTO (CHAVE COMPOSTA)
# ==============================================================================

todas_matriculas <- unique(c(
  as.character(df_form1_tratado$Matricula),
  as.character(df_form2_tratado$Matricula),
  as.character(df_form3_tratado$Matricula_Avaliador),
  as.character(df_form4_tratado$Matricula),
  as.character(df_indices_tratado$Matricula) # Adiciona Matrículas Acadêmicas no dicionário
))
todas_matriculas <- todas_matriculas[!is.na(todas_matriculas)]

df_de_para <- tibble(
  Matricula_Original = todas_matriculas,
  ID_Aluno = paste0("A", str_pad(seq_along(todas_matriculas), 3, pad = "0"))
)

mapa_grupos <- df_form2_tratado %>%
    mutate(Matricula = as.character(Matricula)) %>%
    left_join(df_de_para, by = c("Matricula" = "Matricula_Original")) %>%
    select(ID_Aluno, ID_Grupo, Turma) %>%
    distinct()

# MAPA ÚNICO: Remove a duplicidade de alunos que estão em mais de um grupo
# Essa chave será usada por TODOS os formulários anonimizados para evitar o Cross-Join Leakage
mapa_grupos_unico <- mapa_grupos %>%
    filter(!is.na(ID_Aluno)) %>%
    distinct(ID_Aluno, .keep_all = TRUE)

# Bases Anonimizadas cruzando pelo Mapa Único
df_form1_anon <- df_form1_tratado %>%
    mutate(Matricula = as.character(Matricula)) %>%
    left_join(df_de_para, by = c("Matricula" = "Matricula_Original")) %>%
    left_join(mapa_grupos_unico, by = "ID_Aluno") %>%
    select(ID_Aluno, ID_Grupo, any_of("Turma"), matches("[CHA][0-9]{2}"))

# (Form 2 não precisa de join composto porque o Grupo já nasce nele)
df_form2_anon <- df_form2_tratado %>%
    mutate(Matricula = as.character(Matricula)) %>%
    left_join(df_de_para, by = c("Matricula" = "Matricula_Original")) %>%
    mutate(
        C01 = rowMeans(select(., Req_02, Req_03, Req_11, Req_14, Req_30), na.rm = TRUE),
        C02 = rowMeans(select(., Req_01, Req_06, Req_10, Req_13, Req_21), na.rm = TRUE),
        C03 = rowMeans(select(., Req_05, Req_19, Req_22, Req_29, Req_30), na.rm = TRUE),
        C04 = rowMeans(select(., Req_09, Req_16, Req_18, Req_25, Req_28), na.rm = TRUE),
        C05 = rowMeans(select(., Req_04, Req_12, Req_15, Req_20, Req_28), na.rm = TRUE),
        C06 = rowMeans(select(., Req_02, Req_12, Req_15, Req_20, Req_25), na.rm = TRUE),
        C07 = rowMeans(select(., Req_08, Req_09, Req_18, Req_23, Req_24), na.rm = TRUE),
        C08 = rowMeans(select(., Req_07, Req_17, Req_23, Req_26, Req_30), na.rm = TRUE)
    ) %>%
    select(ID_Aluno, ID_Grupo, Turma, starts_with("C0"))

df_form3_anon <- df_form3_tratado %>%
    mutate(Matricula_Avaliador = as.character(Matricula_Avaliador)) %>%
    left_join(df_de_para, by = c("Matricula_Avaliador" = "Matricula_Original")) %>%
    left_join(mapa_grupos_unico, by = "ID_Aluno") %>%
    select(ID_Aluno, ID_Grupo, any_of("Turma"), matches("[CHA][0-9]{2}"))

# Base do Formulário 4 Anonimizada
df_form4_anon <- df_form4_tratado %>%
    mutate(Matricula = as.character(Matricula)) %>%
    left_join(df_de_para, by = c("Matricula" = "Matricula_Original")) %>%
    left_join(mapa_grupos_unico, by = "ID_Aluno") %>%
    select(ID_Aluno, ID_Grupo, any_of("Turma"), starts_with("Dilema_"))

# Base de Índices Acadêmicos Anonimizada
df_indices_anon <- df_indices_tratado %>%
  left_join(df_de_para, by = c("Matricula" = "Matricula_Original")) %>%
  left_join(mapa_grupos_unico, by = "ID_Aluno") %>%
  select(ID_Aluno, ID_Grupo, any_of("Turma"), MC, IEA, IRA)

# ==============================================================================
# PARTE 4: CALCULOS MULTIVARIADOS (BASE vs ARM)
# ==============================================================================

# Base longa consolidada da Rodada 1
df_form1_long <- df_form1_anon %>%
    pivot_longer(cols = matches("^[CHA][0-9]{2}$"), names_to = "Cód", values_to = "Nota") %>%
    filter(!is.na(Nota))

df_form1_pesos <- df_form1_long %>%
    inner_join(df_pesos_automatizados, by = "Cód", relationship = "many-to-many")

# Cenario Base: Nota x Peso_Base
df_scores_base_individual <- df_form1_pesos %>%
    mutate(Score_Base = Nota * Peso_Base) %>%
    group_by(ID_Aluno, P, Perfil) %>%
    summarise(Score_Total_Base = sum(Score_Base, na.rm = TRUE), .groups = "drop")

# Cenario ARM: Nota x W_i
df_scores_individual <- df_form1_pesos %>%
    mutate(Score_ARM = Nota * W_i) %>%
    group_by(ID_Aluno, P, Perfil) %>%
    summarise(Score_Total = sum(Score_ARM, na.rm = TRUE), .groups = "drop")

# Rodada 2 - Mercado
df_scores_mercado <- df_form2_anon %>%
    distinct(ID_Grupo, C01, C02, C03, C04, C05, C06, C07, C08) %>%
    pivot_longer(cols = starts_with("C0"), names_to = "Cód", values_to = "Nota_Mercado") %>%
    inner_join(df_pesos_automatizados, by = "Cód", relationship = "many-to-many") %>%
    mutate(Score_Ponderado = Nota_Mercado * W_i) %>%
    group_by(ID_Grupo, P, Perfil) %>%
    summarise(Score_Total_Mercado = sum(Score_Ponderado, na.rm = TRUE), .groups = "drop")

# Rodada 3 - Pares
df_scores_pares <- df_form3_anon %>%
    pivot_longer(cols = matches("^[CHA][0-9]{2}$"), names_to = "Cód", values_to = "Nota_Pares") %>%
    group_by(ID_Grupo, Cód) %>%
    summarise(Media_Pares = mean(Nota_Pares, na.rm = TRUE), .groups = "drop") %>%
    inner_join(df_pesos_automatizados, by = "Cód", relationship = "many-to-many") %>%
    mutate(Score_Ponderado = Media_Pares * W_i) %>%
    group_by(ID_Grupo, P, Perfil) %>%
    summarise(Score_Total_Pares = sum(Score_Ponderado, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# PARTE 5: INVENTÁRIOS E COMPARATIVOS
# ==============================================================================

perfis_base <- tibble(
    P = paste0("P0", 1:6),
    `Arquétipo Profissional (Perfil Dominante)` = c(
        "P1 (Analítico-Técnico)",
        "P2 (Processos e Operações)",
        "P3 (Criativo-Inovador)",
        "P4 (Comunicador-Articulador)",
        "P5 (Organizacional-Líder)",
        "P6 (Sustentável-Responsável)"
    )
)

df_perfil_base <- df_scores_base_individual %>%
    group_by(ID_Aluno) %>%
    slice_max(order_by = Score_Total_Base, n = 1, with_ties = FALSE) %>%
    ungroup()

df_perfil_arm <- df_scores_individual %>%
    group_by(ID_Aluno) %>%
    slice_max(order_by = Score_Total, n = 1, with_ties = FALSE) %>%
    ungroup()

# Identifica o Perfil Dominante do Grupo no Form 2 (Mercado)
df_perfil_mercado <- df_scores_mercado %>%
    group_by(ID_Grupo) %>%
    slice_max(order_by = Score_Total_Mercado, n = 1, with_ties = FALSE) %>%
    ungroup()

df_inventario_base <- perfis_base %>%
    left_join(df_perfil_base %>% count(P, name = "Nº de Alunos"), by = "P") %>%
    mutate(
        `Nº de Alunos` = replace_na(`Nº de Alunos`, 0),
        `Participação (%)` = sprintf("%.1f%%", (`Nº de Alunos` / sum(`Nº de Alunos`)) * 100)
    ) %>%
    select(`Arquétipo Profissional (Perfil Dominante)`, `Nº de Alunos`, `Participação (%)`)

df_inventario_perfis <- perfis_base %>%
    left_join(df_perfil_arm %>% count(P, name = "Nº de Alunos"), by = "P") %>%
    mutate(
        `Nº de Alunos` = replace_na(`Nº de Alunos`, 0),
        `Participação (%)` = sprintf("%.1f%%", (`Nº de Alunos` / sum(`Nº de Alunos`)) * 100)
    ) %>%
    select(`Arquétipo Profissional (Perfil Dominante)`, `Nº de Alunos`, `Participação (%)`)

df_comparativo_estresse <- perfis_base %>%
    left_join(df_perfil_base %>% count(P, name = "Qtd_Base"), by = "P") %>%
    left_join(df_perfil_arm %>% count(P, name = "Qtd_ARM"), by = "P") %>%
    mutate(
        Qtd_Base = replace_na(Qtd_Base, 0),
        Qtd_ARM = replace_na(Qtd_ARM, 0),
        Delta = Qtd_ARM - Qtd_Base
    ) %>%
    select(`Arquétipo Profissional (Perfil Dominante)`, Qtd_Base, Qtd_ARM, Delta)

# CRIAÇÃO DA MATRIZ DE TRANSIÇÃO (F1 vs F2) PARA ANÁLISE DE VIESES
df_transicao_f1_f2 <- df_perfil_arm %>%
    select(ID_Aluno, P_F1 = P) %>%
    left_join(mapa_grupos_unico, by = "ID_Aluno") %>%
    left_join(df_perfil_mercado %>% select(ID_Grupo, P_F2 = P), by = "ID_Grupo") %>%
    left_join(perfis_base %>% select(P_F1 = P, Desc_F1 = `Arquétipo Profissional (Perfil Dominante)`), by = "P_F1") %>%
    left_join(perfis_base %>% select(P_F2 = P, Desc_F2 = `Arquétipo Profissional (Perfil Dominante)`), by = "P_F2") %>%
    filter(!is.na(Desc_F2))

# ==============================================================================
# PARTE 6: OBJETOS DE COMPATIBILIDADE PARA O CAPÍTULO
# ==============================================================================

df_scores_calculados <- df_scores_base_individual %>%
    transmute(ID_Aluno, P, Perfil, Score_Total = Score_Total_Base)
