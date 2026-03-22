# ==============================================================================
# ARQUIVO: trat.forms.R
# DATA DE CRIACAO: 16/03/2026
# ULTIMA ATUALIZACAO: 20/03/2026
#
# OBJETIVO: Pipeline de dados silencioso para o Capitulo 7 (People Analytics).
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
    library(ggplot2)
    library(ggrepel)
})

# ==============================================================================
# PARTE 1: DOWNLOAD SEGURO (ONEDRIVE)
# ==============================================================================
meu_onedrive <- get_business_onedrive()

caminho_form1 <- "UNIFEI/PL 2026.1/FORMULÁRIO 1 — Autopercepção de Desempenho (Individual).xlsx"
caminho_form2 <- "UNIFEI/PL 2026.1/FORMULÁRIO 2 — Exigência das Competências na Prática Profissional (Em Grupo).xlsx"
caminho_form3 <- "UNIFEI/PL 2026.1/FORMULÁRIO 3 — Atribuição de Desempenho dos Colegas (Individual).xlsx"
caminho_form4 <- "UNIFEI/PL 2026.1/FORMULÁRIO 4 — Simulação de Gestão de Crises e Dilemas Operacionais (Individual).xlsx"
caminho_form5 <- "UNIFEI/PL 2026.1/FORMULÁRIO 5 — Mapeamento Técnico de Hard Skills (Individual).xlsx"
caminho_matriz <- "UNIFEI/PL 2026.1/Matriz Pesos e CHA.xlsx"

temp_form1 <- tempfile(fileext = ".xlsx")
temp_form2 <- tempfile(fileext = ".xlsx")
temp_form3 <- tempfile(fileext = ".xlsx")
temp_form4 <- tempfile(fileext = ".xlsx")
temp_form5 <- tempfile(fileext = ".xlsx")
temp_matriz <- tempfile(fileext = ".xlsx")

meu_onedrive$download_file(src = caminho_form1, dest = temp_form1, overwrite = TRUE)
meu_onedrive$download_file(src = caminho_form2, dest = temp_form2, overwrite = TRUE)
meu_onedrive$download_file(src = caminho_form3, dest = temp_form3, overwrite = TRUE)
meu_onedrive$download_file(src = caminho_form4, dest = temp_form4, overwrite = TRUE)
meu_onedrive$download_file(src = caminho_form5, dest = temp_form5, overwrite = TRUE)
meu_onedrive$download_file(src = caminho_matriz, dest = temp_matriz, overwrite = TRUE)

df_form1_bruto <- read_excel(temp_form1)
df_form2_bruto <- read_excel(temp_form2)
df_form3_bruto <- read_excel(temp_form3)
df_form4_bruto <- read_excel(temp_form4) %>% mutate(Matrícula = as.character(Matrícula))
df_form5_bruto <- read_excel(temp_form5)
df_matriz_pesos <- read_excel(temp_matriz, sheet = "Matriz Perfil x Dim x Int")

df_indices_bruto <- read_excel(temp_matriz, sheet = "Indices_Academicos", col_types = "text")
df_Matrículas_bruto <- read_excel(temp_matriz, sheet = "Matrículas", col_types = "text")

# ==============================================================================
# PARTE 2: DICIONÁRIOS E CHAVES UNIVERSAIS
# ==============================================================================
limpar_mat <- function(x) {
    x <- as.character(x)
    x <- str_remove(x, "\\.0$")
    str_remove_all(x, "\\D")
}

escala_competencias <- c("Muito Ruim" = 1, "Ruim" = 2, "Médio" = 3, "Bom" = 4, "Muito Bom" = 5)
escala_habilidades <- c("Pouquíssima experiência" = 1, "Pouca experiência" = 2, "Experiência moderada" = 3, "Muita experiência" = 4, "Experiência plena" = 5)
escala_atitudes <- c("Nunca" = 1, "Raramente" = 2, "Às vezes" = 3, "Frequentemente" = 4, "Sempre" = 5)
escala_notas_hardskills <- c("Ainda não cursei" = NA_real_, "60 a 69" = 65, "70 a 79" = 75, "80 a 89" = 85, "90 a 100" = 95)

col_matricula_form5 <- intersect(c("Matrícula", "Matricula"), names(df_form5_bruto))[1]
if (is.na(col_matricula_form5)) {
    stop("Coluna de matrícula do Formulário 5 não encontrada.")
}

mat_f1 <- limpar_mat(df_form1_bruto$Matrícula[!is.na(df_form1_bruto$Matrícula)])
mat_f2 <- df_form2_bruto %>%
    pivot_longer(cols = matches("\\(Aluno \\d\\)"), names_to = c(".value", "Posicao_Grupo"), names_pattern = "(.*) \\(Aluno (\\d)\\)") %>%
    pull(`Matrícula`) %>%
    .[!is.na(.)] %>%
    limpar_mat()
mat_f3 <- limpar_mat(df_form3_bruto$Matrícula[!is.na(df_form3_bruto$Matrícula)])
mat_f4 <- coalesce(
    if ("Matrícula" %in% names(df_form4_bruto)) as.character(df_form4_bruto[["Matrícula"]]) else NA_character_,
    if ("E-mail" %in% names(df_form4_bruto)) as.character(df_form4_bruto[["E-mail"]]) else NA_character_,
    if ("Id" %in% names(df_form4_bruto)) as.character(df_form4_bruto[["Id"]]) else NA_character_
) %>%
    .[!is.na(.)] %>%
    limpar_mat()
mat_f5 <- df_form5_bruto %>%
    mutate(Matrícula = as.character(.data[[col_matricula_form5]])) %>%
    filter(!is.na(Matrícula)) %>%
    pull(Matrícula) %>%
    limpar_mat()
mat_ind <- limpar_mat(df_indices_bruto[[1]][!is.na(df_indices_bruto[[1]])])
mat_of <- limpar_mat(df_Matrículas_bruto[[1]][!is.na(df_Matrículas_bruto[[1]])])

df_de_para <- tibble(Matrícula_Original = sort(unique(c(mat_f1, mat_f2, mat_f3, mat_f4, mat_f5, mat_ind, mat_of)))) %>%
    filter(Matrícula_Original != "") %>%
    mutate(ID_Aluno = paste0("A", str_pad(row_number(), 3, pad = "0")))

df_Matrículas_oficial <- df_Matrículas_bruto %>%
    select(Matrícula = 1, Nome = 2, Turma = 3) %>%
    filter(!is.na(Matrícula)) %>%
    mutate(Matrícula_Original = limpar_mat(Matrícula), Turma_Oficial = str_trim(Turma)) %>%
    select(Matrícula_Original, Turma_Oficial)

df_pesos_automatizados <- df_matriz_pesos %>%
    mutate(
        Peso_Base = as.numeric(str_replace(Peso, ",", ".")) / 100,
        Int_Num = case_when(Intensidade == "Forte" ~ 2.0, Intensidade == "Moderada" ~ 1.0, Intensidade == "Fraca" ~ 0.2, TRUE ~ 0),
        W_i = Peso_Base * Int_Num
    ) %>%
    select(P, Perfil, Cód, Peso_Base, W_i)

# ==============================================================================
# PARTE 3: PIPELINE DIRETO: LIMPEZA E ANONIMIZAÇÃO
# ==============================================================================
df_form2_base <- df_form2_bruto %>%
    mutate(ID_Grupo = paste0("G", str_pad(row_number(), 2, pad = "0"))) %>%
    rename_with(~ paste0("Req_", str_pad(seq_along(.), 2, pad = "0")), starts_with("Classifique")) %>%
    pivot_longer(cols = matches("\\(Aluno \\d\\)"), names_to = c(".value", "Posicao_Grupo"), names_pattern = "(.*) \\(Aluno (\\d)\\)") %>%
    rename(Matr_Bruta = `Matrícula`) %>%
    filter(!is.na(Matr_Bruta) & str_trim(Matr_Bruta) != "") %>%
    mutate(
        Matrícula = limpar_mat(Matr_Bruta),
        across(starts_with("Req_"), ~ as.numeric(str_trim(as.character(.))))
    ) %>%
    left_join(df_de_para, by = c("Matrícula" = "Matrícula_Original")) %>%
    left_join(df_Matrículas_oficial, by = c("Matrícula" = "Matrícula_Original")) %>%
    mutate(Turma_Final = coalesce(Turma_Oficial, Turma))

mapa_grupos_unico <- df_form2_base %>%
    filter(!is.na(ID_Aluno)) %>%
    distinct(ID_Aluno, .keep_all = TRUE) %>%
    select(ID_Aluno, ID_Grupo, Turma = Turma_Final)

df_form2_anon <- df_form2_base %>%
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
    select(ID_Aluno, ID_Grupo, any_of("Turma"), starts_with("C0"))

df_form1_anon <- df_form1_bruto %>%
    filter(!is.na(Matrícula)) %>%
    mutate(Matrícula = limpar_mat(Matrícula)) %>%
    select(Id, Matrícula, Turma, matches("[CHA][0-9]{2}")) %>%
    rename_with(~ str_extract(., "[CHA][0-9]{2}"), matches("[CHA][0-9]{2}")) %>%
    mutate(
        across(starts_with("C"), ~ recode(str_trim(.), !!!escala_competencias, .default = NA_real_)),
        across(starts_with("H"), ~ recode(str_trim(.), !!!escala_habilidades, .default = NA_real_)),
        across(starts_with("A"), ~ recode(str_trim(.), !!!escala_atitudes, .default = NA_real_))
    ) %>%
    left_join(df_de_para, by = c("Matrícula" = "Matrícula_Original")) %>%
    left_join(mapa_grupos_unico, by = "ID_Aluno", suffix = c("_f1", "")) %>%
    mutate(Turma = coalesce(Turma, Turma_f1)) %>%
    select(ID_Aluno, ID_Grupo, Turma, matches("[CHA][0-9]{2}"))

df_form3_anon <- df_form3_bruto %>%
    filter(!is.na(Matrícula)) %>%
    mutate(Matrícula = limpar_mat(Matrícula)) %>%
    select(Matrícula, Turma, matches("[CHA][0-9]{2}")) %>%
    rename_with(~ str_extract(., "[CHA][0-9]{2}"), matches("[CHA][0-9]{2}")) %>%
    mutate(
        across(matches("^[CH]"), ~ recode(str_trim(.), !!!escala_competencias, .default = NA_real_)),
        across(starts_with("A"), ~ recode(str_trim(.), !!!escala_atitudes, .default = NA_real_))
    ) %>%
    left_join(df_de_para, by = c("Matrícula" = "Matrícula_Original")) %>%
    left_join(mapa_grupos_unico, by = "ID_Aluno", suffix = c("_f3", "")) %>%
    mutate(Turma = coalesce(Turma, Turma_f3)) %>%
    select(ID_Aluno, ID_Grupo, Turma, matches("[CHA][0-9]{2}"))

df_form4_anon <- df_form4_bruto %>%
    mutate(
        Mat_Validada = coalesce(
            if ("Matrícula" %in% names(.)) as.character(.[["Matrícula"]]) else NA_character_,
            if ("E-mail" %in% names(.)) as.character(.[["E-mail"]]) else NA_character_,
            if ("Id" %in% names(.)) as.character(.[["Id"]]) else NA_character_
        ),
        Matrícula = limpar_mat(Mat_Validada)
    ) %>%
    filter(Matrícula != "") %>%
    select(Matrícula, any_of("Turma"), contains("Conflito")) %>%
    rename_with(~ paste0("Dilema_", str_pad(seq_along(.), 2, pad = "0")), contains("Conflito")) %>%
    mutate(across(starts_with("Dilema_"), ~ LETTERS[as.numeric(as.factor(str_trim(.)))])) %>%
    left_join(df_de_para, by = c("Matrícula" = "Matrícula_Original")) %>%
    left_join(mapa_grupos_unico, by = "ID_Aluno", suffix = c("_f4", "")) %>%
    mutate(Turma = if ("Turma" %in% names(.)) coalesce(Turma, Turma_f4) else Turma_f4) %>%
    select(ID_Aluno, ID_Grupo, any_of("Turma"), starts_with("Dilema_"))

df_indices_anon <- df_indices_bruto %>%
    select(Matrícula = 1, MC = 4, IEA = 5, IRA = 6) %>%
    filter(!is.na(Matrícula)) %>%
    mutate(
        Matrícula = limpar_mat(Matrícula),
        MC = as.numeric(str_replace(MC, ",", ".")),
        IEA = as.numeric(str_replace(IEA, ",", ".")),
        IRA = as.numeric(str_replace(IRA, ",", "."))
    ) %>%
    left_join(df_de_para, by = c("Matrícula" = "Matrícula_Original")) %>%
    left_join(mapa_grupos_unico, by = "ID_Aluno") %>%
    select(ID_Aluno, ID_Grupo, any_of("Turma"), MC, IEA, IRA)

df_form5_anon <- df_form5_bruto %>%
    mutate(Matrícula = limpar_mat(.data[[col_matricula_form5]])) %>%
    filter(!is.na(Matrícula) & Matrícula != "") %>%
    left_join(df_de_para, by = c("Matrícula" = "Matrícula_Original")) %>%
    left_join(mapa_grupos_unico, by = "ID_Aluno", suffix = c("_f5", "")) %>%
    select(ID_Aluno, ID_Grupo, any_of("Turma"), matches("Ciclo|Componentes")) %>%
    pivot_longer(cols = matches("Ciclo|Componentes"), names_to = "Coluna_Bruta", values_to = "Nota_Texto") %>%
    mutate(
        Categoria = str_extract(Coluna_Bruta, "^[^\\.]+"),
        Disciplina = str_remove(Coluna_Bruta, "^[^\\.]+\\."),
        Nota_Numerica = recode(str_trim(Nota_Texto), !!!escala_notas_hardskills)
    ) %>%
    filter(!is.na(Nota_Numerica)) %>%
    select(ID_Aluno, ID_Grupo, Turma, Categoria, Disciplina, Nota_Numerica)

df_form5_respondentes <- df_form5_bruto %>%
    mutate(Matrícula = limpar_mat(.data[[col_matricula_form5]])) %>%
    filter(!is.na(Matrícula) & Matrícula != "") %>%
    left_join(df_de_para, by = c("Matrícula" = "Matrícula_Original")) %>%
    distinct(ID_Aluno)

n_respostas_form5 <- nrow(df_form5_respondentes)

# ==============================================================================
# PARTE 4: CALCULOS MULTIVARIADOS (BASE vs ARM) - F1, F2, F3
# ==============================================================================
df_form1_long <- df_form1_anon %>%
    pivot_longer(cols = matches("^[CHA][0-9]{2}$"), names_to = "Cód", values_to = "Nota") %>%
    filter(!is.na(Nota))
df_form1_pesos <- df_form1_long %>% inner_join(df_pesos_automatizados, by = "Cód", relationship = "many-to-many")

df_scores_base_individual <- df_form1_pesos %>%
    mutate(Score_Base = Nota * Peso_Base) %>%
    group_by(ID_Aluno, P, Perfil) %>%
    summarise(Score_Total_Base = sum(Score_Base, na.rm = TRUE), .groups = "drop")

df_scores_individual <- df_form1_pesos %>%
    mutate(Score_ARM = Nota * W_i) %>%
    group_by(ID_Aluno, P, Perfil) %>%
    summarise(Score_Total = sum(Score_ARM, na.rm = TRUE), .groups = "drop")

df_scores_mercado <- df_form2_anon %>%
    distinct(ID_Grupo, C01, C02, C03, C04, C05, C06, C07, C08) %>%
    pivot_longer(cols = starts_with("C0"), names_to = "Cód", values_to = "Nota_Mercado") %>%
    inner_join(df_pesos_automatizados, by = "Cód", relationship = "many-to-many") %>%
    mutate(Score_Ponderado = Nota_Mercado * W_i) %>%
    group_by(ID_Grupo, P, Perfil) %>%
    summarise(Score_Total_Mercado = sum(Score_Ponderado, na.rm = TRUE), .groups = "drop")

df_scores_pares <- df_form3_anon %>%
    pivot_longer(cols = matches("^[CHA][0-9]{2}$"), names_to = "Cód", values_to = "Nota_Pares") %>%
    group_by(ID_Grupo, Cód) %>%
    summarise(Media_Pares = mean(Nota_Pares, na.rm = TRUE), .groups = "drop") %>%
    inner_join(df_pesos_automatizados, by = "Cód", relationship = "many-to-many") %>%
    mutate(Score_Ponderado = Media_Pares * W_i) %>%
    group_by(ID_Grupo, P, Perfil) %>%
    summarise(Score_Total_Pares = sum(Score_Ponderado, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# PARTE 5: FUSÃO DE DADOS (F4 Crise + F5 Hard Skills)
# ==============================================================================
# 5.1 SJT - Matriz de Crise (F4)
df_form4_long <- df_form4_anon %>%
    pivot_longer(cols = starts_with("Dilema_"), names_to = "Dilema", values_to = "Resposta") %>%
    filter(!is.na(Resposta))

pesos_raw <- rbind(
    c(2, 0, 0, 0, 0, 5), c(0, 2, 0, 5, 0, 0), c(5, 0, 0, 0, 2, 0),
    c(0, 0, 0, 0, 2, 5), c(0, 0, 2, 5, 0, 0), c(2, 0, 0, 0, 5, 0),
    c(0, 0, 0, 0, 2, 5), c(2, 5, 0, 0, 0, 0), c(0, 0, 5, 2, 0, 0),
    c(0, 0, 0, 0, 5, 2), c(0, 0, 0, 5, 2, 0), c(0, 2, 5, 0, 0, 0),
    c(0, 0, 0, 2, 0, 5), c(0, 0, 5, 2, 0, 0), c(5, 2, 0, 0, 0, 0),
    c(0, 0, 2, 5, 0, 0), c(0, 0, 0, 0, 5, 2), c(5, 2, 0, 0, 0, 0),
    c(2, 0, 0, 0, 5, 0), c(2, 0, 5, 0, 0, 0), c(0, 5, 0, 2, 0, 0),
    c(0, 0, 0, 0, 5, 2), c(0, 2, 5, 0, 0, 0), c(2, 5, 0, 0, 0, 0),
    c(5, 0, 0, 0, 0, 2), c(0, 2, 5, 0, 0, 0), c(0, 5, 0, 0, 2, 0),
    c(0, 0, 0, 0, 2, 5), c(0, 2, 0, 0, 5, 0), c(0, 5, 2, 0, 0, 0)
)
Dilemas_Nomes <- paste0("Dilema_", str_pad(rep(1:10, each = 3), 2, pad = "0"))
Respostas_Letras <- rep(c("A", "B", "C"), 10)
df_matriz_pesos_f4 <- as.data.frame(pesos_raw)
colnames(df_matriz_pesos_f4) <- c("Analítico", "Operações", "Criativo", "Comunicador", "Líder", "Sustentável")

df_matriz_pesos_f4 <- df_matriz_pesos_f4 %>%
    mutate(Dilema = Dilemas_Nomes, Resposta = Respostas_Letras) %>%
    pivot_longer(cols = `Analítico`:`Sustentável`, names_to = "Perfil_F4", values_to = "Peso_F4") %>%
    filter(Peso_F4 > 0)

df_scores_f4 <- df_form4_long %>% inner_join(df_matriz_pesos_f4, by = c("Dilema", "Resposta"), relationship = "many-to-many")

df_resumo_perfis_f4 <- df_scores_f4 %>%
    group_by(ID_Aluno, ID_Grupo, Turma, Perfil_F4) %>%
    summarise(Score_Total_F4 = sum(Peso_F4, na.rm = TRUE), .groups = "drop")

# Extração do Instinto Puro (Sem bônus, usado na transição F3->F4)
df_f4_puro <- df_resumo_perfis_f4 %>%
    group_by(ID_Aluno) %>%
    slice_max(order_by = Score_Total_F4, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(Instinto_F4 = Perfil_F4)

# 5.2 Algoritmo de Data Fusion (Bônus/Punição F5)
df_bonus_f5 <- df_form5_anon %>%
    group_by(ID_Aluno, Categoria) %>%
    summarise(Media_Ciclo = mean(Nota_Numerica, na.rm = TRUE), .groups = "drop") %>%
    mutate(
        Bonus_Analítico = if_else(str_detect(Categoria, regex("Básico|Basico", ignore_case = TRUE)), if_else(Media_Ciclo >= 80, 2, if_else(Media_Ciclo < 70, -2, 0)), 0),
        Bonus_Operações = if_else(str_detect(Categoria, regex("Profissionalizante", ignore_case = TRUE)), if_else(Media_Ciclo >= 80, 2, if_else(Media_Ciclo < 70, -2, 0)), 0),
        Bonus_Criativo = if_else(str_detect(Categoria, regex("Optativos|Optativas|TCC", ignore_case = TRUE)), if_else(Media_Ciclo >= 80, 1.5, 0), 0),
        Bonus_Comunicador = if_else(str_detect(Categoria, regex("Optativos|Optativas|TCC", ignore_case = TRUE)), if_else(Media_Ciclo >= 80, 1.5, 0), 0),
        Bonus_Líder = if_else(str_detect(Categoria, regex("Específico|Especifico", ignore_case = TRUE)), if_else(Media_Ciclo >= 80, 2, 0), 0),
        Bonus_Sustentável = if_else(str_detect(Categoria, regex("Específico|Especifico", ignore_case = TRUE)), if_else(Media_Ciclo >= 80, 2, 0), 0)
    ) %>%
    group_by(ID_Aluno) %>%
    summarise(across(starts_with("Bonus_"), sum, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(cols = starts_with("Bonus_"), names_to = "Perfil_F4", values_to = "Bonus_HS") %>%
    mutate(Perfil_F4 = str_replace(Perfil_F4, "Bonus_", ""))

# 5.3 Perfil Definitivo Recalibrado
df_resumo_perfis_final <- df_resumo_perfis_f4 %>%
    left_join(df_bonus_f5, by = c("ID_Aluno", "Perfil_F4")) %>%
    mutate(Bonus_HS = replace_na(Bonus_HS, 0), Score_Final_Recalibrado = Score_Total_F4 + Bonus_HS)

df_perfil_dominante_f4 <- df_resumo_perfis_final %>%
    group_by(ID_Aluno) %>%
    slice_max(order_by = Score_Final_Recalibrado, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(Perfil_Dominante_Dilemas = Perfil_F4)

# Função única para padronizar nomenclaturas de perfis em todas as rodadas (F1-F5)
normalizar_perfil <- function(p, rotulo_pendente = "Pendente/Sem Dados") {
    p <- as.character(p)
    p <- str_trim(p)
    p <- str_replace_all(p, "[‑–—−]", "-")

    case_when(
        is.na(p) | p == "" ~ rotulo_pendente,
        str_detect(p, regex("pendente|sem dados|sem f4", ignore_case = TRUE)) ~ rotulo_pendente,
        str_detect(p, regex("^P0?1$|^P1\\b|analit", ignore_case = TRUE)) ~ "P1 (Analítico-Técnico)",
        str_detect(p, regex("^P0?2$|^P2\\b|operac", ignore_case = TRUE)) ~ "P2 (Operações)",
        str_detect(p, regex("^P0?3$|^P3\\b|criativ", ignore_case = TRUE)) ~ "P3 (Criativo-Inovador)",
        str_detect(p, regex("^P0?4$|^P4\\b|comunic", ignore_case = TRUE)) ~ "P4 (Comunicador-Articulador)",
        str_detect(p, regex("^P0?5$|^P5\\b|lider", ignore_case = TRUE)) ~ "P5 (Líder-Organizacional)",
        str_detect(p, regex("^P0?6$|^P6\\b|sustent", ignore_case = TRUE)) ~ "P6 (Sustentável-Responsável)",
        TRUE ~ p
    )
}

# ==============================================================================
# PARTE 6: INVENTÁRIOS E MATRIZ DE TALENTOS (ESTÁTICA F1 x IEA)
# ==============================================================================
df_perfil_arm <- df_scores_individual %>%
    group_by(ID_Aluno) %>%
    slice_max(order_by = Score_Total, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Perfil = normalizar_perfil(Perfil))
df_autopercepcao_media <- df_form1_anon %>%
    mutate(Media_Autopercepcao = rowMeans(select(., matches("^[CHA][0-9]{2}$")), na.rm = TRUE)) %>%
    select(ID_Aluno, Media_Autopercepcao)

df_analise_talentos <- df_perfil_arm %>%
    select(ID_Aluno, Perfil_Dominante = Perfil, Score_Total) %>%
    left_join(df_indices_anon %>% select(ID_Aluno, ID_Grupo, MC, IEA, IRA), by = "ID_Aluno") %>%
    left_join(df_autopercepcao_media, by = "ID_Aluno") %>%
    mutate(
        Benchmark_IEA = max(IEA, na.rm = TRUE),
        Aderencia_Benchmark = if_else(!is.na(IEA) & Benchmark_IEA > 0, 100 * IEA / Benchmark_IEA, NA_real_),
        MC_Equivalente_5pt = MC / 2, Delta_Vies = Media_Autopercepcao - MC_Equivalente_5pt
    )

limiar_iea <- quantile(df_analise_talentos$IEA, 0.7, na.rm = TRUE)
limiar_arm <- quantile(df_analise_talentos$Score_Total, 0.7, na.rm = TRUE)

df_analise_talentos <- df_analise_talentos %>%
    mutate(Status_Talento = case_when(is.na(IEA) ~ "Sem Dados Técnicos", IEA >= limiar_iea & Score_Total >= limiar_arm ~ "High Potential", IEA >= limiar_iea & Score_Total < limiar_arm ~ "Técnico Subutilizado", IEA < limiar_iea & Score_Total >= limiar_arm ~ "Risco Técnico", TRUE ~ "Operacional"))

# ==============================================================================
# PARTE 7: MATRIZ DE TALENTOS DEFINITIVA (DNA RECALIBRADO vs IEA)
# ==============================================================================
df_analise_talentos_final <- df_perfil_dominante_f4 %>%
    left_join(df_indices_anon %>% select(ID_Aluno, IEA), by = "ID_Aluno") %>%
    filter(!is.na(IEA) & !is.na(Score_Final_Recalibrado))

limiar_iea_final <- quantile(df_analise_talentos_final$IEA, 0.7, na.rm = TRUE)
limiar_score_final <- quantile(df_analise_talentos_final$Score_Final_Recalibrado, 0.7, na.rm = TRUE)

df_analise_talentos_final <- df_analise_talentos_final %>%
    mutate(
        Status_Talento = case_when(
            IEA >= limiar_iea_final & Score_Final_Recalibrado >= limiar_score_final ~ "High Potential",
            IEA >= limiar_iea_final & Score_Final_Recalibrado < limiar_score_final ~ "Técnico Subutilizado",
            IEA < limiar_iea_final & Score_Final_Recalibrado >= limiar_score_final ~ "Risco Técnico",
            TRUE ~ "Operacional"
        )
    )

# ==============================================================================
# PARTE 8: RESUMOS ESTATÍSTICOS SEPARADOS POR FORMULÁRIO
# ==============================================================================
df_resumo_f1 <- df_form1_anon %>%
    rowwise() %>%
    mutate(Media_Autopercepcao_F1 = mean(c_across(matches("^[CHA][0-9]{2}$")), na.rm = TRUE)) %>%
    ungroup() %>%
    select(ID_Aluno, ID_Grupo, Turma, Media_Autopercepcao_F1) %>%
    arrange(desc(Media_Autopercepcao_F1))

df_resumo_f2 <- df_form2_anon %>%
    group_by(ID_Grupo, Turma) %>%
    summarise(across(starts_with("C0"), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(Exigencia_Geral_Mercado = rowMeans(select(., starts_with("C0")), na.rm = TRUE)) %>%
    arrange(desc(Exigencia_Geral_Mercado))

df_resumo_f3 <- df_form3_anon %>%
    pivot_longer(cols = matches("^[CHA][0-9]{2}$"), names_to = "Cod", values_to = "Nota_Recebida") %>%
    group_by(ID_Aluno, ID_Grupo, Turma) %>%
    summarise(Media_Avaliacao_Pares_F3 = mean(Nota_Recebida, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(Media_Avaliacao_Pares_F3))

df_resumo_f4 <- df_perfil_dominante_f4 %>%
    group_by(Turma, Perfil_Dominante_Dilemas) %>%
    summarise(Quantidade_Alunos = n(), Score_Medio_Perfil = mean(Score_Final_Recalibrado, na.rm = TRUE), .groups = "drop") %>%
    arrange(Turma, desc(Quantidade_Alunos))

df_resumo_f5 <- df_indices_anon %>%
    group_by(Turma) %>%
    summarise(Total_Alunos_Mapeados = n(), Media_MC = mean(MC, na.rm = TRUE), Media_IEA = mean(IEA, na.rm = TRUE), Media_IRA = mean(IRA, na.rm = TRUE), Max_IEA = max(IEA, na.rm = TRUE), .groups = "drop")

resumo_perfil_f1 <- df_perfil_arm %>%
    count(Perfil, name = "Qtd_F1") %>%
    arrange(desc(Qtd_F1))
resumo_perfil_f4 <- df_perfil_dominante_f4 %>%
    count(Perfil_Dominante_Dilemas, name = "Qtd_F4") %>%
    rename(Perfil = Perfil_Dominante_Dilemas) %>%
    arrange(desc(Qtd_F4))

# ==============================================================================
# PARTE 9: MATRIZES DE TRANSIÇÃO (F1 -> F2 -> F3 -> F4_Puro)
# ==============================================================================
perfil_desc <- function(p) {
    normalizar_perfil(p)
}

df_perfil_f2_grupo <- df_scores_mercado %>%
    group_by(ID_Grupo) %>%
    slice_max(order_by = Score_Total_Mercado, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(ID_Grupo, Desc_F2 = perfil_desc(P))
df_perfil_f3_grupo <- df_scores_pares %>%
    group_by(ID_Grupo) %>%
    slice_max(order_by = Score_Total_Pares, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(ID_Grupo, Desc_F3 = perfil_desc(P))

df_migracao_f2_f3 <- df_perfil_f2_grupo %>%
    inner_join(df_perfil_f3_grupo, by = "ID_Grupo") %>%
    count(Desc_F2, Desc_F3, name = "Qtd de Grupos") %>%
    mutate(`Fatia da Turma` = sprintf("%.1f%%", (`Qtd de Grupos` / sum(`Qtd de Grupos`)) * 100)) %>%
    arrange(desc(`Qtd de Grupos`))

df_migracao_f3_f4 <- mapa_grupos_unico %>%
    inner_join(df_perfil_f3_grupo %>% rename(Mascara_F3 = Desc_F3), by = "ID_Grupo") %>%
    inner_join(df_f4_puro %>% transmute(ID_Aluno, Instinto_F4 = perfil_desc(Instinto_F4)), by = "ID_Aluno") %>%
    count(Mascara_F3, Instinto_F4, name = "Nº de Alunos") %>%
    mutate(`Fatia da Turma` = sprintf("%.1f%%", (`Nº de Alunos` / sum(`Nº de Alunos`)) * 100)) %>%
    arrange(desc(`Nº de Alunos`))

# ==============================================================================
# PARTE 10: FORMAÇÃO DOS NOVOS SQUADS (USANDO O PERFIL RECALIBRADO)
# ==============================================================================
df_dna_alunos <- df_Matrículas_bruto %>%
    select(Matrícula = 1, Nome = 2, Turma = 3) %>%
    filter(!is.na(Matrícula) & Turma != "EPRI0001 - INTRODUÇÃO À ENGENHARIA DE PRODUÇÃO - T01") %>%
    mutate(Matrícula_Original = limpar_mat(Matrícula), Nome = str_to_upper(str_trim(Nome)), Turma_Oficial = str_trim(Turma)) %>%
    left_join(df_de_para, by = "Matrícula_Original") %>%
    left_join(df_perfil_dominante_f4 %>% select(ID_Aluno, Perfil_F4 = Perfil_Dominante_Dilemas), by = "ID_Aluno") %>%
    left_join(df_indices_anon %>% select(ID_Aluno, IEA), by = "ID_Aluno") %>%
    mutate(Perfil_F4 = normalizar_perfil(coalesce(Perfil_F4, "Pendente (Sem F4)")), IEA_Sort = coalesce(IEA, 0)) %>%
    distinct(Matrícula_Original, Turma_Oficial, .keep_all = TRUE) %>%
    arrange(Turma_Oficial, Nome)

set.seed(2026)
df_novos_grupos <- df_dna_alunos %>%
    group_by(Turma_Oficial) %>%
    arrange(Turma_Oficial, Perfil_F4, desc(IEA_Sort)) %>%
    mutate(
        Num_Grupos = floor(n() / 5),
        Novo_ID_Grupo = paste0("Squad_", str_pad((row_number() - 1) %% Num_Grupos + 1, 2, pad = "0"))
    ) %>%
    ungroup() %>%
    arrange(Turma_Oficial, Novo_ID_Grupo, desc(IEA_Sort)) %>%
    select(Turma = Turma_Oficial, Novo_ID_Grupo, Matrícula = Matrícula_Original, Nome, Perfil_F4, IEA)

# ==============================================================================
# PARTE 11: CONSOLIDADO DE ENTREGAS E ALOCAÇÃO (CHECKLIST DE GOVERNANÇA)
# ==============================================================================
df_consolidado_entregas <- df_Matrículas_bruto %>%
    select(Matrícula = 1, Nome = 2, Turma = 3) %>%
    mutate(Matrícula = limpar_mat(Matrícula), Nome = str_to_upper(str_trim(Nome))) %>%
    filter(Turma != "EPRI0001 - INTRODUÇÃO À ENGENHARIA DE PRODUÇÃO - T01") %>%
    left_join(df_form1_bruto %>% mutate(Matrícula = limpar_mat(Matrícula)) %>% select(Matrícula, Turma) %>% distinct() %>% mutate(F1 = "✔"), by = c("Matrícula", "Turma")) %>%
    left_join(df_form2_bruto %>% pivot_longer(cols = matches("\\(Aluno \\d\\)"), names_to = c(".value", "Posicao_Grupo"), names_pattern = "(.*) \\(Aluno (\\d)\\)") %>% mutate(Matrícula = limpar_mat(`Matrícula`)) %>% filter(Matrícula != "") %>% select(Matrícula, Turma) %>% distinct() %>% mutate(F2 = "✔"), by = c("Matrícula", "Turma")) %>%
    left_join(df_form3_bruto %>% mutate(Matrícula = limpar_mat(Matrícula)) %>% select(Matrícula, Turma) %>% distinct() %>% mutate(F3 = "✔"), by = c("Matrícula", "Turma")) %>%
    left_join(df_form4_bruto %>% mutate(Matrícula = limpar_mat(coalesce(if ("Matrícula" %in% names(.)) as.character(.[["Matrícula"]]) else NA_character_, if ("E-mail" %in% names(.)) as.character(.[["E-mail"]]) else NA_character_))) %>% filter(Matrícula != "") %>% select(Matrícula) %>% distinct() %>% mutate(F4 = "✔"), by = "Matrícula") %>%
    left_join(df_indices_bruto %>% select(Matrícula = 1) %>% mutate(Matrícula = limpar_mat(Matrícula)) %>% distinct() %>% mutate(F5_IEA = "✔"), by = "Matrícula") %>%
    left_join(df_form5_bruto %>% mutate(Matrícula = limpar_mat(Matrícula)) %>% select(Matrícula) %>% distinct() %>% mutate(F5_HS = "✔"), by = "Matrícula") %>%
    mutate(
        across(c(F1, F2, F3, F4, F5_HS, F5_IEA), ~ replace_na(.x, "Pendente")),
        Qtd_Pendencias = rowSums(across(c(F1, F2, F3, F4, F5_HS, F5_IEA), ~ .x == "Pendente"))
    ) %>%
    distinct(Matrícula, Turma, .keep_all = TRUE) %>%
    rename(IEA = F5_IEA) %>%
    left_join(df_novos_grupos %>% select(Turma, Matrícula, Perfil = Perfil_F4, Squad = Novo_ID_Grupo), by = c("Turma", "Matrícula")) %>%
    select(Turma, Squad, Matrícula, Nome, Perfil, F1, F2, F3, F4, F5_HS, IEA, Qtd_Pendencias) %>%
    arrange(desc(Qtd_Pendencias), Turma, Squad, Nome)

df_consolidado_entregas_completo <- df_consolidado_entregas

df_consolidado_entregas <- df_consolidado_entregas %>%
    select(Turma, Matrícula, Nome, Squad, Perfil) %>%
    arrange(Squad)

df_resumo_adesao <- df_consolidado_entregas_completo %>%
    group_by(Turma) %>%
    summarise(
        Total_Alunos = n(),
        Entregas_100_pct = sum(Qtd_Pendencias == 0),
        Com_Pendencias = sum(Qtd_Pendencias > 0),
        Falta_F1 = sum(F1 == "Pendente"),
        Falta_F4 = sum(F4 == "Pendente"),
        Sem_IEA = sum(IEA == "Pendente"),
        .groups = "drop"
    )

# ==============================================================================
# PARTE 12: EVOLUÇÃO DE PERFIS (A LINHA DO TEMPO) - 100% BLINDADO
# ==============================================================================

# 1. Blindagem absoluta no R Base (Fora do pipe para evitar erros de sintaxe)
if (!"Turma" %in% names(df_consolidado_entregas_completo)) df_consolidado_entregas_completo$Turma <- "EPRI4003 - GESTÃO DA QUALIDADE - T01"
if (!"Squad" %in% names(df_consolidado_entregas_completo)) df_consolidado_entregas_completo$Squad <- "Sem Squad"
if (!"Perfil" %in% names(df_consolidado_entregas_completo)) df_consolidado_entregas_completo$Perfil <- "Pendente/Sem Dados"

col_mapa_de_para <- if ("Matricula_Original" %in% names(df_de_para)) "Matricula_Original" else if ("Matrícula_Original" %in% names(df_de_para)) "Matrícula_Original" else NA_character_

# 2. Mapeia Squad por ID_Aluno para uso na trilha evolutiva
df_squad_por_id <- df_consolidado_entregas_completo %>%
    transmute(Matrícula = as.character(Matrícula), Squad) %>%
    distinct() %>%
    left_join(df_de_para, by = setNames(col_mapa_de_para, "Matrícula")) %>%
    transmute(ID_Aluno, Squad)

# 3. Executa a Linha do Tempo
df_evolucao_perfis <- df_consolidado_entregas_completo %>%
    select(Turma, Matrícula, Nome, Squad, Perfil_Final = Perfil) %>%
    left_join(df_de_para, by = setNames(col_mapa_de_para, "Matrícula")) %>%
    # Removemos a Turma do mapa_grupos para não criar conflito (Turma.x e Turma.y)
    left_join(mapa_grupos_unico %>% select(-any_of("Turma")), by = "ID_Aluno") %>%
    left_join(df_perfil_arm %>% select(ID_Aluno, Perfil_F1 = Perfil), by = "ID_Aluno") %>%
    left_join(df_perfil_f2_grupo %>% select(ID_Grupo, Perfil_F2 = Desc_F2), by = "ID_Grupo") %>%
    left_join(df_perfil_f3_grupo %>% select(ID_Grupo, Perfil_F3 = Desc_F3), by = "ID_Grupo") %>%
    left_join(df_f4_puro %>% select(ID_Aluno, Perfil_F4 = Instinto_F4), by = "ID_Aluno") %>%
    left_join(df_indices_anon %>% select(ID_Aluno, IEA), by = "ID_Aluno") %>%
    mutate(
        across(starts_with("Perfil_"), ~ normalizar_perfil(.x, rotulo_pendente = "Pendente/Sem Dados")),
        Perfil_Final = normalizar_perfil(Perfil_Final, rotulo_pendente = "Pendente/Sem Dados")
    ) %>%
    select(Turma, Squad, Matrícula, Nome, IEA, `F1 (Ego)` = Perfil_F1, `F2 (Consenso)` = Perfil_F2, `F3 (Pares)` = Perfil_F3, `F4 (Instinto)` = Perfil_F4, `F5 (Recalibrado)` = Perfil_Final) %>%
    arrange(Turma, Squad, Nome)

df_evolucao_perfis %>%
    filter(is.na(IEA)) %>%
    print(n = Inf)
