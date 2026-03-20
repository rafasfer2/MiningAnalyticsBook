# ==============================================================================
# ARQUIVO: trat.forms.R
# DATA DE CRIACAO: 16/03/2026
# ULTIMA ATUALIZACAO: 19/03/2026
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
df_form4_bruto <- read_excel(temp_form4) %>% mutate(Matrícula = as.character(Matrícula))
df_matriz_pesos <- read_excel(temp_matriz, sheet = "Matriz Perfil x Dim x Int")

df_indices_bruto <- read_excel(temp_matriz, sheet = "Indices_Academicos", col_types = "text")
df_Matrículas_bruto <- read_excel(temp_matriz, sheet = "Matrículas", col_types = "text")

caminho_form5 <- "UNIFEI/PL 2026.1/FORMULÁRIO 5 — Mapeamento Técnico de Hard Skills (Individual).xlsx"
temp_form5 <- tempfile(fileext = ".xlsx")
meu_onedrive$download_file(src = caminho_form5, dest = temp_form5, overwrite = TRUE)
df_form5_bruto <- read_excel(temp_form5)

# ==============================================================================
# PARTE 2: DICIONÁRIOS E CHAVES UNIVERSAIS
# ==============================================================================

# Função Global: Blinda a matrícula contra erros de digitação e formatação
limpar_mat <- function(x) {
    x <- as.character(x)
    x <- str_remove(x, "\\.0$")
    str_remove_all(x, "\\D")
}

escala_competencias <- c("Muito Ruim" = 1, "Ruim" = 2, "Médio" = 3, "Bom" = 4, "Muito Bom" = 5)
escala_habilidades <- c("Pouquíssima experiência" = 1, "Pouca experiência" = 2, "Experiência moderada" = 3, "Muita experiência" = 4, "Experiência plena" = 5)
escala_atitudes <- c("Nunca" = 1, "Raramente" = 2, "Às vezes" = 3, "Frequentemente" = 4, "Sempre" = 5)

# 2.1 Coleta Universal (Geração do Dicionário sem tabelas intermediárias)
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

mat_ind <- limpar_mat(df_indices_bruto[[1]][!is.na(df_indices_bruto[[1]])])
mat_of <- limpar_mat(df_Matrículas_bruto[[1]][!is.na(df_Matrículas_bruto[[1]])])

df_de_para <- tibble(Matrícula_Original = sort(unique(c(mat_f1, mat_f2, mat_f3, mat_f4, mat_ind, mat_of)))) %>%
    filter(Matrícula_Original != "") %>%
    mutate(ID_Aluno = paste0("A", str_pad(row_number(), 3, pad = "0")))

# Tratamento da Lista Oficial para apoiar o Mapa de Grupos
df_Matrículas_oficial <- df_Matrículas_bruto %>%
    select(Matrícula = 1, Nome = 2, Turma = 3) %>%
    filter(!is.na(Matrícula)) %>%
    mutate(Matrícula_Original = limpar_mat(Matrícula), Turma_Oficial = str_trim(Turma)) %>%
    select(Matrícula_Original, Turma_Oficial)

# Matriz de Pesos (ARM)
df_pesos_automatizados <- df_matriz_pesos %>%
    mutate(
        Peso_Base = as.numeric(str_replace(Peso, ",", ".")) / 100,
        Int_Num = case_when(Intensidade == "Forte" ~ 2.0, Intensidade == "Moderada" ~ 1.0, Intensidade == "Fraca" ~ 0.2, TRUE ~ 0),
        W_i = Peso_Base * Int_Num
    ) %>%
    select(P, Perfil, Cód, Peso_Base, W_i)

# Dicionário de conversão Likert -> Ponto Médio
escala_notas_hardskills <- c("Ainda não cursei" = NA_real_, "60 a 69" = 65, "70 a 79" = 75, "80 a 89" = 85, "90 a 100" = 95)

# Adicione a captura das matrículas do F5 no bloco 2.1
mat_f5 <- limpar_mat(df_form5_bruto$Matrícula[!is.na(df_form5_bruto$Matrícula)])

# E atualize o df_de_para para incluir o mat_f5
df_de_para <- tibble(Matrícula_Original = sort(unique(c(mat_f1, mat_f2, mat_f3, mat_f4, mat_f5, mat_ind, mat_of)))) %>%
    filter(Matrícula_Original != "") %>%
    mutate(ID_Aluno = paste0("A", str_pad(row_number(), 3, pad = "0")))

# ==============================================================================
# PARTE 3: PIPELINE DIRETO: LIMPEZA E ANONIMIZAÇÃO
# ==============================================================================

# Base Form 2 & Mapa de Grupos (Feito primeiro pois gera o ID_Grupo)
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

# Form 1 - Anonimizado
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

# Form 3 - Anonimizado
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

# Form 4 - Anonimizado
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

# Índices Acadêmicos - Anonimizado
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

# Form 5 - Anonimizado e Pivotado
df_form5_anon <- df_form5_bruto %>%
    filter(!is.na(Matrícula)) %>%
    mutate(Matrícula = limpar_mat(Matrícula)) %>%
    left_join(df_de_para, by = c("Matrícula" = "Matrícula_Original")) %>%
    left_join(mapa_grupos_unico, by = "ID_Aluno", suffix = c("_f5", "")) %>%
    select(ID_Aluno, ID_Grupo, any_of("Turma"), matches("Ciclo|Componentes")) %>%
    # Transforma de Largo para Longo
    pivot_longer(cols = matches("Ciclo|Componentes"), names_to = "Coluna_Bruta", values_to = "Nota_Texto") %>%
    # Separa o Ciclo do Nome da Disciplina e converte a nota
    mutate(
        Categoria = str_extract(Coluna_Bruta, "^[^\\.]+"),
        Disciplina = str_remove(Coluna_Bruta, "^[^\\.]+\\."),
        Nota_Numerica = recode(str_trim(Nota_Texto), !!!escala_notas_hardskills)
    ) %>%
    filter(!is.na(Nota_Numerica)) %>% # Remove o que o aluno ainda não cursou
    select(ID_Aluno, ID_Grupo, Turma, Categoria, Disciplina, Nota_Numerica)

# ==============================================================================
# PARTE 4: CALCULOS MULTIVARIADOS (BASE vs ARM)
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
# PARTE 5: INVENTÁRIOS E ESTRATÉGIAS ANALÍTICAS
# ==============================================================================

df_perfil_arm <- df_scores_individual %>%
    group_by(ID_Aluno) %>%
    slice_max(order_by = Score_Total, n = 1, with_ties = FALSE) %>%
    ungroup()
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
        MC_Equivalente_5pt = MC / 2,
        Delta_Vies = Media_Autopercepcao - MC_Equivalente_5pt
    )

limiar_iea <- quantile(df_analise_talentos$IEA, 0.7, na.rm = TRUE)
limiar_arm <- quantile(df_analise_talentos$Score_Total, 0.7, na.rm = TRUE)

df_analise_talentos <- df_analise_talentos %>%
    mutate(
        Status_Talento = case_when(
            is.na(IEA) ~ "Sem Dados Técnicos",
            IEA >= limiar_iea & Score_Total >= limiar_arm ~ "High Potential",
            IEA >= limiar_iea & Score_Total < limiar_arm ~ "Técnico Subutilizado",
            IEA < limiar_iea & Score_Total >= limiar_arm ~ "Risco Técnico",
            TRUE ~ "Operacional"
        )
    )

# ==============================================================================
# PARTE 6: GRÁFICO DE TALENTOS
# ==============================================================================

df_plot_talentos <- df_analise_talentos %>%
    filter(Status_Talento != "Sem Dados Técnicos") %>%
    distinct(ID_Aluno, .keep_all = TRUE) %>%
    mutate(Label = if_else(Status_Talento != "Operacional", ID_Aluno, ""))

cores_talentos <- c("High Potential" = "#24A379", "Técnico Subutilizado" = "#1D5185", "Risco Técnico" = "#C40E0E", "Operacional" = "#7F8C8D")

fig_talentos <- ggplot(df_plot_talentos, aes(x = IEA, y = Score_Total, color = Status_Talento)) +
    geom_vline(xintercept = limiar_iea, linetype = "dashed", color = "grey50", alpha = 0.7) +
    geom_hline(yintercept = limiar_arm, linetype = "dashed", color = "grey50", alpha = 0.7) +
    geom_point(size = 3.5, alpha = 0.8) +
    geom_text_repel(aes(label = Label), color = "black", size = 3.5, fontface = "bold", show.legend = FALSE, box.padding = 0.5, point.padding = 0.5, max.overlaps = Inf) +
    annotate("text", x = max(df_plot_talentos$IEA, na.rm = TRUE), y = max(df_plot_talentos$Score_Total, na.rm = TRUE), label = "High Potentials", hjust = 1, vjust = 1, color = "#24A379", fontface = "bold", alpha = 0.6, size = 5) +
    annotate("text", x = max(df_plot_talentos$IEA, na.rm = TRUE), y = min(df_plot_talentos$Score_Total, na.rm = TRUE), label = "Técnicos\nSubutilizados", hjust = 1, vjust = 0, color = "#1D5185", fontface = "bold", alpha = 0.6, size = 5) +
    annotate("text", x = min(df_plot_talentos$IEA, na.rm = TRUE), y = max(df_plot_talentos$Score_Total, na.rm = TRUE), label = "Risco\nTécnico", hjust = 0, vjust = 1, color = "#C40E0E", fontface = "bold", alpha = 0.6, size = 5) +
    scale_color_manual(values = cores_talentos) +
    labs(title = "Matriz de Talentos: Comportamento (ARM) vs. Eficiência (IEA)", x = "Índice de Eficiência Acadêmica (IEA)", y = "Score Total (Modelo ARM)", color = "Classificação:") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(fig_talentos)

# ==============================================================================
# AUDITORIA CIRÚRGICA DIRETA
# ==============================================================================
df_consolidado <- df_Matrículas_bruto %>%
    select(Matrícula = 1, Nome = 2, Turma = 3) %>%
    mutate(Matrícula = limpar_mat(Matrícula), Nome = str_to_upper(str_trim(Nome))) %>%
    filter(Turma != "EPRI0001 - INTRODUÇÃO À ENGENHARIA DE PRODUÇÃO - T01") %>%
    # F1
    left_join(df_form1_bruto %>% mutate(Matrícula = limpar_mat(Matrícula)) %>% select(Matrícula, Turma) %>% distinct() %>% mutate(F1 = "✔"), by = c("Matrícula", "Turma")) %>%
    # F2
    left_join(
        df_form2_bruto %>%
            pivot_longer(cols = matches("\\(Aluno \\d\\)"), names_to = c(".value", "Posicao_Grupo"), names_pattern = "(.*) \\(Aluno (\\d)\\)") %>%
            mutate(Matrícula = limpar_mat(`Matrícula`)) %>% filter(Matrícula != "") %>%
            select(Matrícula, Turma) %>% distinct() %>% mutate(F2 = "✔"),
        by = c("Matrícula", "Turma")
    ) %>%
    # F3
    left_join(df_form3_bruto %>% mutate(Matrícula = limpar_mat(Matrícula)) %>% select(Matrícula, Turma) %>% distinct() %>% mutate(F3 = "✔"), by = c("Matrícula", "Turma")) %>%
    # F4 (Match apenas por Matrícula, pois as colunas de Turma podem não vir padronizadas)
    left_join(
        df_form4_bruto %>%
            mutate(Matrícula = limpar_mat(coalesce(if ("Matrícula" %in% names(.)) as.character(.[["Matrícula"]]) else NA_character_, if ("E-mail" %in% names(.)) as.character(.[["E-mail"]]) else NA_character_))) %>%
            filter(Matrícula != "") %>% select(Matrícula) %>% distinct() %>% mutate(F4 = "✔"),
        by = "Matrícula"
    ) %>%
    # F5
    left_join(df_indices_bruto %>% select(Matrícula = 1) %>% mutate(Matrícula = limpar_mat(Matrícula)) %>% distinct() %>% mutate(F5 = "✔"), by = "Matrícula") %>%
    # Cálculo Qtd_NAs
    mutate(Qtd_NA = rowSums(is.na(across(F1:F5))), Qtd_NA = ifelse(Qtd_NA == 0, NA, Qtd_NA)) %>%
    distinct(Matrícula, Turma, .keep_all = TRUE) %>%
    arrange(Turma, Nome)

print(df_consolidado, n = Inf)

df_consolidado %>% count(F1)

df_consolidado %>%
    filter(F1 == "✔" & F5 == "✔") %>%
    distinct(Matrícula) %>%
    nrow()

# ==============================================================================
# ANÁLISE DE PERFIS - FORMULÁRIO 4 (MATRIZ FUZZY w_SJT)
# ==============================================================================

# 1. Transformar o df_form4_anon de "Largo" para "Longo"
df_form4_long <- df_form4_anon %>%
    pivot_longer(
        cols = starts_with("Dilema_"),
        names_to = "Dilema",
        values_to = "Resposta"
    ) %>%
    filter(!is.na(Resposta))

# ------------------------------------------------------------------------------
# 2. DEFINIÇÃO DA MATRIZ DE PESOS CRUZADOS (w_SJT)
# ------------------------------------------------------------------------------
# Importando os pesos brutos exatamente como estruturados no seu Quarto
pesos_raw <- rbind(
    c(2, 0, 0, 0, 0, 5), c(0, 2, 0, 5, 0, 0), c(5, 0, 0, 0, 2, 0), # D1
    c(0, 0, 0, 0, 2, 5), c(0, 0, 2, 5, 0, 0), c(2, 0, 0, 0, 5, 0), # D2
    c(0, 0, 0, 0, 2, 5), c(2, 5, 0, 0, 0, 0), c(0, 0, 5, 2, 0, 0), # D3
    c(0, 0, 0, 0, 5, 2), c(0, 0, 0, 5, 2, 0), c(0, 2, 5, 0, 0, 0), # D4
    c(0, 0, 0, 2, 0, 5), c(0, 0, 5, 2, 0, 0), c(5, 2, 0, 0, 0, 0), # D5
    c(0, 0, 2, 5, 0, 0), c(0, 0, 0, 0, 5, 2), c(5, 2, 0, 0, 0, 0), # D6
    c(2, 0, 0, 0, 5, 0), c(2, 0, 5, 0, 0, 0), c(0, 5, 0, 2, 0, 0), # D7
    c(0, 0, 0, 0, 5, 2), c(0, 2, 5, 0, 0, 0), c(2, 5, 0, 0, 0, 0), # D8
    c(5, 0, 0, 0, 0, 2), c(0, 2, 5, 0, 0, 0), c(0, 5, 0, 0, 2, 0), # D9
    c(0, 0, 0, 0, 2, 5), c(0, 2, 0, 0, 5, 0), c(0, 5, 2, 0, 0, 0) # D10
)

# Criando a estrutura amigável para o Join no R
Dilemas_Nomes <- paste0("Dilema_", str_pad(rep(1:10, each = 3), 2, pad = "0"))
Respostas_Letras <- rep(c("A", "B", "C"), 10)

df_matriz_pesos_f4 <- as.data.frame(pesos_raw)
colnames(df_matriz_pesos_f4) <- c("Analítico", "Operações", "Criativo", "Comunicador", "Líder", "Sustentável")

# Transformando no formato "Longo" para cruzar com as respostas dos alunos
df_matriz_pesos_f4 <- df_matriz_pesos_f4 %>%
    mutate(Dilema = Dilemas_Nomes, Resposta = Respostas_Letras) %>%
    pivot_longer(
        cols = `Analítico`:`Sustentável`,
        names_to = "Perfil_F4",
        values_to = "Peso_F4"
    ) %>%
    filter(Peso_F4 > 0) # Mantém apenas os vínculos +5 e +2

# ------------------------------------------------------------------------------
# 3. CRUZAMENTO: INJEÇÃO DOS PESOS (O ALGORITMO EM AÇÃO)
# ------------------------------------------------------------------------------
df_scores_f4 <- df_form4_long %>%
    # O Join agora usa DUAS chaves: Qual foi a resposta E em qual Dilema
    inner_join(df_matriz_pesos_f4, by = c("Dilema", "Resposta"), relationship = "many-to-many")

# ------------------------------------------------------------------------------
# 4. CONSOLIDAÇÃO E RECALIBRAGEM TÉCNICA (FUSÃO F4 + F5)
# ------------------------------------------------------------------------------
# Calcula a soma dos dilemas
df_resumo_perfis_f4 <- df_scores_f4 %>%
    group_by(ID_Aluno, ID_Grupo, Turma, Perfil_F4) %>%
    summarise(Score_Total_F4 = sum(Peso_F4, na.rm = TRUE), .groups = "drop")

# Perfil de instinto puro (apenas F4, sem bônus do F5)
df_instinto_f4 <- df_resumo_perfis_f4 %>%
    group_by(ID_Aluno) %>%
    slice_max(order_by = Score_Total_F4, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(ID_Aluno, Instinto_F4 = Perfil_F4)

# Algoritmo de Bônus/Punição baseado nas Hard Skills Declaradas (F5)
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

# Funde as matrizes e gera o Score Final Recalibrado
df_resumo_perfis_final <- df_resumo_perfis_f4 %>%
    left_join(df_bonus_f5, by = c("ID_Aluno", "Perfil_F4")) %>%
    mutate(
        Bonus_HS = replace_na(Bonus_HS, 0),
        Score_Final_Recalibrado = Score_Total_F4 + Bonus_HS
    )

# ------------------------------------------------------------------------------
# 5. IDENTIFICAÇÃO DO PERFIL DOMINANTE (COM LASTRO TÉCNICO)
# ------------------------------------------------------------------------------
df_perfil_dominante_f4 <- df_resumo_perfis_final %>%
    group_by(ID_Aluno) %>%
    slice_max(order_by = Score_Final_Recalibrado, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(Perfil_Dominante_Dilemas = Perfil_F4)

# ==============================================================================
# AUDITORIA DE RESULTADOS REAL
# ==============================================================================
print(df_perfil_dominante_f4 %>% arrange(Turma, ID_Aluno), n = Inf)

# ==============================================================================
# PARTE 8: RESUMOS ESTATÍSTICOS SEPARADOS POR FORMULÁRIO
# ==============================================================================

# ------------------------------------------------------------------------------
# RESUMO FORM 1: Autopercepção (Média geral de CHA declarada por aluno)
# ------------------------------------------------------------------------------
df_resumo_f1 <- df_form1_anon %>%
    rowwise() %>%
    mutate(Media_Autopercepcao_F1 = mean(c_across(matches("^[CHA][0-9]{2}$")), na.rm = TRUE)) %>%
    ungroup() %>%
    select(ID_Aluno, ID_Grupo, Turma, Media_Autopercepcao_F1) %>%
    arrange(desc(Media_Autopercepcao_F1))

print("--- Resumo Form 1 (Top 5 Autopercepção) ---")
print(head(df_resumo_f1, 5))

# ------------------------------------------------------------------------------
# RESUMO FORM 2: Visão de Mercado (Média das exigências das competências por Grupo)
# ------------------------------------------------------------------------------
df_resumo_f2 <- df_form2_anon %>%
    group_by(ID_Grupo, Turma) %>%
    summarise(across(starts_with("C0"), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    # Cria uma coluna com a média geral de exigência que o grupo enxerga no mercado
    mutate(Exigencia_Geral_Mercado = rowMeans(select(., starts_with("C0")), na.rm = TRUE)) %>%
    arrange(desc(Exigencia_Geral_Mercado))

print("--- Resumo Form 2 (Top 5 Exigência de Mercado por Grupo) ---")
print(head(df_resumo_f2, 5))

# ------------------------------------------------------------------------------
# RESUMO FORM 3: Avaliação dos Pares (Média que o aluno recebeu dos colegas)
# ------------------------------------------------------------------------------
df_resumo_f3 <- df_form3_anon %>%
    pivot_longer(cols = matches("^[CHA][0-9]{2}$"), names_to = "Cod", values_to = "Nota_Recebida") %>%
    group_by(ID_Aluno, ID_Grupo, Turma) %>%
    summarise(Media_Avaliacao_Pares_F3 = mean(Nota_Recebida, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(Media_Avaliacao_Pares_F3))

print("--- Resumo Form 3 (Top 5 Avaliação dos Pares) ---")
print(head(df_resumo_f3, 5))

# ------------------------------------------------------------------------------
# RESUMO FORM 4: Teste de Crise/SJT (Contagem de Perfis Dominantes por Turma)
# ------------------------------------------------------------------------------
# Utiliza o df_perfil_dominante_f4 gerado na injeção de pesos cruzados
df_resumo_f4 <- df_perfil_dominante_f4 %>%
    group_by(Turma, Perfil_Dominante_Dilemas) %>%
    summarise(
        Quantidade_Alunos = n(),
        Score_Medio_Perfil = mean(Score_Total_F4, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(Turma, desc(Quantidade_Alunos))

print("--- Resumo Form 4 (Distribuição de Perfis Revelados na Crise) ---")
print(df_resumo_f4)

# ------------------------------------------------------------------------------
# RESUMO FORM 5: Índices Acadêmicos (Métricas de Eficiência Técnica por Turma)
# ------------------------------------------------------------------------------
df_resumo_f5 <- df_indices_anon %>%
    group_by(Turma) %>%
    summarise(
        Total_Alunos_Mapeados = n(),
        Media_MC = mean(MC, na.rm = TRUE),
        Media_IEA = mean(IEA, na.rm = TRUE),
        Media_IRA = mean(IRA, na.rm = TRUE),
        Max_IEA = max(IEA, na.rm = TRUE),
        .groups = "drop"
    )

print("--- Resumo Form 5 (Métricas Acadêmicas Oficiais por Turma) ---")
print(df_resumo_f5)

# ==============================================================================
# RESUMO: PERFIL x QUANTIDADE (F1 e F4)
# ==============================================================================

# 1. Perfil x Quantidade (Formulário 1 - Autopercepção / Declarado)
resumo_perfil_f1 <- df_perfil_arm %>%
    count(Perfil, name = "Qtd_F1") %>%
    arrange(desc(Qtd_F1))

print("--- Perfil x Quantidade (Formulário 1 - Declarado) ---")
print(resumo_perfil_f1)


# 2. Perfil x Quantidade (Formulário 4 - SJT Crise / Revelado)
resumo_perfil_f4 <- df_perfil_dominante_f4 %>%
    count(Perfil_Dominante_Dilemas, name = "Qtd_F4") %>%
    rename(Perfil = Perfil_Dominante_Dilemas) %>%
    arrange(desc(Qtd_F4))

print("--- Perfil x Quantidade (Formulário 4 - Revelado na Crise) ---")
print(resumo_perfil_f4)


# 3. Opcional: Comparativo Lado a Lado (F1 vs F4)
resumo_comparativo_perfis <- resumo_perfil_f1 %>%
    full_join(resumo_perfil_f4, by = "Perfil") %>%
    mutate(across(starts_with("Qtd"), ~ replace_na(.x, 0))) %>%
    mutate(Variacao = Qtd_F4 - Qtd_F1) %>%
    arrange(desc(Qtd_F4))

print("--- Comparativo Geral de Perfis (F1 vs F4) ---")
print(resumo_comparativo_perfis)

# ==============================================================================
# PARTE 9: ANÁLISE DE VIESES, TESTE DE ESTRESSE E TRANSIÇÃO (F1 vs F2)
# ==============================================================================

# ------------------------------------------------------------------------------
# 9.1 Teste de Estresse: Peso Base (Cego) vs. Peso ARM (Intensidades)
# ------------------------------------------------------------------------------
# Calcula o Perfil Dominante usando apenas o Peso Base (sem a malha de intensidades)
df_perfil_base <- df_scores_base_individual %>%
    group_by(ID_Aluno) %>%
    slice_max(order_by = Score_Total_Base, n = 1, with_ties = FALSE) %>%
    ungroup()

# Agrupa a quantidade de alunos por perfil no Cenário Base
resumo_base <- df_perfil_base %>% count(Perfil, name = "Qtd_Base")

# Agrupa a quantidade de alunos por perfil no Cenário ARM (já calculado na Parte 5)
resumo_arm <- df_perfil_arm %>% count(Perfil, name = "Qtd_ARM")

# Consolida o Teste de Estresse para expor o Efeito Lupa
df_comparativo_estresse <- full_join(resumo_base, resumo_arm, by = "Perfil") %>%
    mutate(
        across(starts_with("Qtd"), ~ replace_na(.x, 0)),
        Delta = Qtd_ARM - Qtd_Base
    ) %>%
    arrange(Perfil) %>%
    rename(`Arquétipo Profissional (Perfil Dominante)` = Perfil)

print("--- Teste de Estresse: Impacto da Intensidade ARM sobre o Peso Base ---")
print(df_comparativo_estresse)

# ------------------------------------------------------------------------------
# 9.2 Matriz de Transição: Autopercepção (F1) vs Dinâmica de Grupo (F2)
# ------------------------------------------------------------------------------
# 1. Descobre qual foi o Perfil exigido pelo Mercado para cada GRUPO no F2
df_perfil_f2_grupo <- df_scores_mercado %>%
    group_by(ID_Grupo) %>%
    slice_max(order_by = Score_Total_Mercado, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(Desc_F2 = Perfil) # Usando a nomenclatura do seu Quarto

# 2. Cruza o perfil que o aluno declarou (F1) com o perfil que o grupo exigiu (F2)
df_transicao_f1_f2 <- df_perfil_arm %>%
    select(ID_Aluno, Desc_F1 = Perfil) %>%
    left_join(mapa_grupos_unico %>% select(ID_Aluno, ID_Grupo), by = "ID_Aluno") %>%
    left_join(df_perfil_f2_grupo %>% select(ID_Grupo, Desc_F2), by = "ID_Grupo") %>%
    filter(!is.na(Desc_F2)) # Filtra apenas alunos que possuem grupo e F2 calculado

# 3. Consolida o agrupamento para visualizar a migração
df_migracao_resumo <- df_transicao_f1_f2 %>%
    count(Desc_F1, Desc_F2, name = "Qtd_Alunos") %>%
    arrange(desc(Qtd_Alunos)) %>%
    mutate(Percentual = sprintf("%.1f%%", (Qtd_Alunos / sum(Qtd_Alunos)) * 100))

print("--- Matriz de Transição: Autopercepção (F1) vs Mercado (F2) ---")
print(df_migracao_resumo)

# ------------------------------------------------------------------------------
# 9.3 Matriz de Transição: Exigência do Mercado (F2) vs Avaliação dos Pares (F3)
# Nível de Análise: GRUPO
# ------------------------------------------------------------------------------

# 1. Descobre o Perfil Consolidado do Grupo na Avaliação de Pares (F3)
df_perfil_f3_grupo <- df_scores_pares %>%
    group_by(ID_Grupo) %>%
    slice_max(order_by = Score_Total_Pares, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(Desc_F3 = Perfil)

# 2. Cruza o perfil que o mercado exigiu (F2) com o perfil operado pelos pares (F3)
df_transicao_f2_f3 <- df_perfil_f2_grupo %>%
    select(ID_Grupo, Desc_F2) %>%
    inner_join(df_perfil_f3_grupo %>% select(ID_Grupo, Desc_F3), by = "ID_Grupo") %>%
    filter(!is.na(Desc_F3) & !is.na(Desc_F2))

# 3. Consolida a migração dos Grupos
df_migracao_f2_f3 <- df_transicao_f2_f3 %>%
    count(Desc_F2, Desc_F3, name = "Qtd_Grupos") %>%
    arrange(desc(Qtd_Grupos)) %>%
    mutate(Percentual = sprintf("%.1f%%", (Qtd_Grupos / sum(Qtd_Grupos)) * 100))

print("--- Matriz de Transição: Mercado (F2) vs Pares (F3) [NÍVEL GRUPO] ---")
print(df_migracao_f2_f3)


# ------------------------------------------------------------------------------
# 9.4 Matriz de Transição: Avaliação dos Pares (F3) vs Simulação de Crise (F4)
# Nível de Análise: INDIVÍDUO vs GRUPO
# ------------------------------------------------------------------------------

# 1. Cruza o Perfil do Grupo no F3 com o Perfil Individual Revelado no F4 (Dilemas)
df_transicao_f3_f4 <- df_perfil_dominante_f4 %>%
    select(ID_Aluno, ID_Grupo, Desc_F4 = Perfil_Dominante_Dilemas) %>%
    inner_join(df_perfil_f3_grupo %>% select(ID_Grupo, Desc_F3), by = "ID_Grupo") %>%
    filter(!is.na(Desc_F3) & !is.na(Desc_F4))

# 2. Consolida a migração: Do comportamento do Grupo para o Instinto Individual
df_migracao_f3_f4 <- df_transicao_f3_f4 %>%
    count(Desc_F3, Desc_F4, name = "Qtd_Alunos") %>%
    arrange(desc(Qtd_Alunos)) %>%
    mutate(Percentual = sprintf("%.1f%%", (Qtd_Alunos / sum(Qtd_Alunos)) * 100))

print("--- Matriz de Transição: Pares (F3) vs Simulação de Crise (F4) [INDIVÍDUO vs GRUPO] ---")
print(df_migracao_f3_f4)

# ==============================================================================
# PARTE 10: ENGENHARIA REVERSA E FORMAÇÃO DOS NOVOS SQUADS (ALL-INCLUSIVE)
# ==============================================================================

# 1. Quebra do Anonimato: Consolidar o DNA garantindo 100% dos matriculados
df_dna_alunos <- df_Matrículas_bruto %>%
    select(Matrícula = 1, Nome = 2, Turma = 3) %>%
    filter(!is.na(Matrícula) & Turma != "EPRI0001 - INTRODUÇÃO À ENGENHARIA DE PRODUÇÃO - T01") %>%
    mutate(
        Matrícula_Original = limpar_mat(Matrícula),
        Nome = str_to_upper(str_trim(Nome)),
        Turma_Oficial = str_trim(Turma)
    ) %>%
    left_join(df_de_para, by = "Matrícula_Original") %>%
    left_join(df_perfil_dominante_f4 %>% select(ID_Aluno, Perfil_F4 = Perfil_Dominante_Dilemas), by = "ID_Aluno") %>%
    left_join(df_indices_anon %>% select(ID_Aluno, IEA), by = "ID_Aluno") %>%
    # TRATAMENTO DE EXCEÇÃO: Garante que os atrasados entrem na roleta
    mutate(
        Perfil_F4 = coalesce(Perfil_F4, "Pendente (Sem F4)"),
        IEA_Sort = coalesce(IEA, 0) # Usado apenas para não quebrar a ordenação matemática
    ) %>%
    distinct(Matrícula_Original, Turma_Oficial, .keep_all = TRUE) %>%
    arrange(Turma_Oficial, Nome)

# 2. Algoritmo de Alocação de Equipes de Alta Performance (Round-Robin)
set.seed(2026) # Garante reprodutibilidade

df_novos_grupos <- df_dna_alunos %>%
    group_by(Turma_Oficial) %>%
    # O SEGREDO DO BALANCEAMENTO: Ordena por Perfil, depois por IEA.
    arrange(Turma_Oficial, Perfil_F4, desc(IEA_Sort)) %>%
    mutate(
        # Define o número de grupos por turma assumindo o tamanho mínimo ideal de 5 alunos
        Num_Grupos = floor(n() / 5),
        # Distribui os alunos (1, 2, 3, 4... 1, 2, 3, 4) cruzando perfis e notas
        Novo_ID_Grupo = paste0("Squad_", str_pad((row_number() - 1) %% Num_Grupos + 1, 2, pad = "0"))
    ) %>%
    ungroup() %>%
    # Reordena para visualização focada no Grupo final
    arrange(Turma_Oficial, Novo_ID_Grupo, desc(IEA_Sort)) %>%
    select(Turma = Turma_Oficial, Novo_ID_Grupo, Matrícula = Matrícula_Original, Nome, Perfil_F4, IEA)

# 3. Exibir o resultado real com os nomes e as novas equipes
print("--- Proposta Oficial de Nova Alocação de Equipes (100% Alocados) ---")
print(df_novos_grupos, n = Inf)

# ==============================================================================
# AUDITORIA DO BALANCEAMENTO DOS NOVOS GRUPOS
# ==============================================================================
df_auditoria_squads <- df_novos_grupos %>%
    group_by(Turma, Novo_ID_Grupo) %>%
    summarise(
        Qtd_Alunos = n(),
        Media_IEA = mean(IEA, na.rm = TRUE),
        Diversidade_Perfis = n_distinct(Perfil_F4[Perfil_F4 != "Pendente (Sem F4)"]),
        Pendentes_F4 = sum(Perfil_F4 == "Pendente (Sem F4)"),
        .groups = "drop"
    )

print("--- Auditoria de Equilíbrio Técnico e Comportamental dos Novos Squads ---")
print(df_auditoria_squads)

# ==============================================================================
# PARTE 11: CONSOLIDADO DE ENTREGAS E ALOCAÇÃO (CHECKLIST DE GOVERNANÇA)
# ==============================================================================

# Gera a matriz de checklist cruzando a lista oficial de matrículas com os formulários e os novos squads
df_consolidado_entregas <- df_Matrículas_bruto %>%
    select(Matrícula = 1, Nome = 2, Turma = 3) %>%
    mutate(Matrícula = limpar_mat(Matrícula), Nome = str_to_upper(str_trim(Nome))) %>%
    filter(Turma != "EPRI0001 - INTRODUÇÃO À ENGENHARIA DE PRODUÇÃO - T01") %>%
    # F1: Autopercepção
    left_join(df_form1_bruto %>% mutate(Matrícula = limpar_mat(Matrícula)) %>% select(Matrícula, Turma) %>% distinct() %>% mutate(F1 = "✔"), by = c("Matrícula", "Turma")) %>%
    # F2: Mercado
    left_join(
        df_form2_bruto %>%
            pivot_longer(cols = matches("\\(Aluno \\d\\)"), names_to = c(".value", "Posicao_Grupo"), names_pattern = "(.*) \\(Aluno (\\d)\\)") %>%
            mutate(Matrícula = limpar_mat(`Matrícula`)) %>% filter(Matrícula != "") %>%
            select(Matrícula, Turma) %>% distinct() %>% mutate(F2 = "✔"),
        by = c("Matrícula", "Turma")
    ) %>%
    # F3: Pares
    left_join(df_form3_bruto %>% mutate(Matrícula = limpar_mat(Matrícula)) %>% select(Matrícula, Turma) %>% distinct() %>% mutate(F3 = "✔"), by = c("Matrícula", "Turma")) %>%
    # F4: SJT (Crise)
    left_join(
        df_form4_bruto %>%
            mutate(Matrícula = limpar_mat(coalesce(if ("Matrícula" %in% names(.)) as.character(.[["Matrícula"]]) else NA_character_, if ("E-mail" %in% names(.)) as.character(.[["E-mail"]]) else NA_character_))) %>%
            filter(Matrícula != "") %>% select(Matrícula) %>% distinct() %>% mutate(F4 = "✔"),
        by = "Matrícula"
    ) %>%
    # F5: IEA (Dados do Sistema UF)
    left_join(df_indices_bruto %>% select(Matrícula = 1) %>% mutate(Matrícula = limpar_mat(Matrícula)) %>% distinct() %>% mutate(F5_IEA = "✔"), by = "Matrícula") %>%
    # Tratamento visual e cálculo de pendências
    mutate(
        across(c(F1, F2, F3, F4, F5_IEA), ~ replace_na(.x, "Pendente")),
        Qtd_Pendencias = rowSums(across(c(F1, F2, F3, F4, F5_IEA), ~ .x == "Pendente"))
    ) %>%
    distinct(Matrícula, Turma, .keep_all = TRUE) %>%
    # F5: Mapeamento Técnico de Hard Skills
    left_join(df_form5_bruto %>% mutate(Matrícula = limpar_mat(Matrícula)) %>% select(Matrícula) %>% distinct() %>% mutate(F5_HS = "✔"), by = "Matrícula") %>%
    # ----------------------------------------------------------------------------
    # Integração com os Novos Squads e Renomeação do IEA
    # ----------------------------------------------------------------------------
    rename(IEA = F5_IEA) %>%
    left_join(
        df_novos_grupos %>% select(Turma, Matrícula, Perfil = Perfil_F4, Squad = Novo_ID_Grupo),
        by = c("Turma", "Matrícula")
    ) %>%
    # Reorganiza as colunas para leitura otimizada e ordena os mais atrasados no topo
    select(Turma, Squad, Matrícula, Nome, Perfil, F1, F2, F3, F4, IEA, Qtd_Pendencias) %>%
    arrange(desc(Qtd_Pendencias), Turma, Squad, Nome)

# Exibe o status completo de todos os alunos
print("--- CHECKLIST MASTER: STATUS, ENTREGAS E ALOCAÇÃO POR ALUNO ---")
print(df_consolidado_entregas, n = Inf)

# ==============================================================================
# EXPORTAÇÃO FINAL: df_consolidado_entregas simplificado para o Capítulo 7
# ==============================================================================
# Salva a versão completa para uso nos resumos antes de simplificar
df_consolidado_entregas_completo <- df_consolidado_entregas

# Cria a versão simplificada para capítulo 7
df_consolidado_entregas <- df_consolidado_entregas %>%
    select(Turma, Matrícula, Nome, Squad, Perfil) %>%
    arrange(Squad)

# ==============================================================================
# RESUMO EXECUTIVO DE ADESÃO
# ==============================================================================
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

print("--- RESUMO DE ADESÃO À METODOLOGIA POR TURMA ---")
print(df_resumo_adesao)

# ==============================================================================
# PARTE 12: MATRIZES FINAIS PARA RENDERIZAÇÃO NO CAPÍTULO (F2 -> F3 e F3 -> F4)
# ==============================================================================

perfil_desc <- function(p) {
    case_when(
        p == "P01" ~ "P1 (Analítico)",
        p == "P02" ~ "P2 (Operações)",
        p == "P03" ~ "P3 (Criativo)",
        p == "P04" ~ "P4 (Comunicador)",
        p == "P05" ~ "P5 (Líder)",
        p == "P06" ~ "P6 (Sustentável)",
        TRUE ~ as.character(p)
    )
}

df_f2_dominante <- df_scores_mercado %>%
    group_by(ID_Grupo) %>%
    slice_max(order_by = Score_Total_Mercado, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(ID_Grupo, Perfil_F2 = perfil_desc(P))

df_f3_dominante <- df_scores_pares %>%
    group_by(ID_Grupo) %>%
    slice_max(order_by = Score_Total_Pares, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(ID_Grupo, Perfil_F3 = perfil_desc(P))

df_migracao_f2_f3 <- df_f2_dominante %>%
    inner_join(df_f3_dominante, by = "ID_Grupo") %>%
    count(Perfil_F2, Perfil_F3, name = "Qtd de Grupos") %>%
    mutate(`Fatia da Turma` = sprintf("%.1f%%", (`Qtd de Grupos` / sum(`Qtd de Grupos`)) * 100)) %>%
    arrange(desc(`Qtd de Grupos`))

pesos_raw_sjt <- rbind(
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

df_dicionario_sjt_transicao <- data.frame(
    Dilema = paste0("Dilema_", str_pad(rep(1:10, each = 3), 2, pad = "0")),
    Resposta = rep(c("A", "B", "C"), 10),
    P01 = pesos_raw_sjt[, 1],
    P02 = pesos_raw_sjt[, 2],
    P03 = pesos_raw_sjt[, 3],
    P04 = pesos_raw_sjt[, 4],
    P05 = pesos_raw_sjt[, 5],
    P06 = pesos_raw_sjt[, 6]
)

df_sjt_calculado <- df_form4_anon %>%
    pivot_longer(cols = starts_with("Dilema"), names_to = "Dilema", values_to = "Resposta") %>%
    filter(!is.na(Resposta)) %>%
    inner_join(df_dicionario_sjt_transicao, by = c("Dilema", "Resposta")) %>%
    group_by(ID_Aluno) %>%
    summarise(across(starts_with("P0"), sum), .groups = "drop") %>%
    pivot_longer(cols = starts_with("P0"), names_to = "P", values_to = "Score_SJT") %>%
    group_by(ID_Aluno) %>%
    slice_max(order_by = Score_SJT, n = 1, with_ties = FALSE) %>%
    ungroup()

df_f4_dominante <- df_sjt_calculado %>%
    mutate(Instinto_F4 = perfil_desc(P)) %>%
    select(ID_Aluno, Instinto_F4)

df_migracao_f3_f4 <- mapa_grupos_unico %>%
    inner_join(df_f3_dominante, by = "ID_Grupo") %>%
    rename(Mascara_F3 = Perfil_F3) %>%
    inner_join(df_f4_dominante, by = "ID_Aluno") %>%
    count(Mascara_F3, Instinto_F4, name = "Nº de Alunos") %>%
    mutate(`Fatia da Turma` = sprintf("%.1f%%", (`Nº de Alunos` / sum(`Nº de Alunos`)) * 100)) %>%
    arrange(desc(`Nº de Alunos`))

# ==============================================================================
# PARTE 13: RASTREABILIDADE INDIVIDUAL DE PERFIS (F1 -> F3 -> F4 -> F5)
# ==============================================================================

# Mapeia Squad por ID_Aluno para uso na trilha evolutiva
df_squad_por_id <- df_consolidado_entregas_completo %>%
    transmute(Matrícula = as.character(Matrícula), Squad) %>%
    distinct() %>%
    left_join(df_de_para, by = c("Matrícula" = "Matrícula_Original")) %>%
    transmute(ID_Aluno, Squad)

df_evolucao_perfis <- df_perfil_arm %>%
    select(ID_Aluno, `F1 (Ego)` = Perfil) %>%
    left_join(mapa_grupos_unico %>% select(ID_Aluno, ID_Grupo), by = "ID_Aluno") %>%
    left_join(df_f3_dominante %>% transmute(ID_Grupo, `F3 (Pares)` = Perfil_F3), by = "ID_Grupo") %>%
    left_join(df_instinto_f4 %>% transmute(ID_Aluno, `F4 (Instinto)` = Instinto_F4), by = "ID_Aluno") %>%
    left_join(df_perfil_dominante_f4 %>% transmute(ID_Aluno, `F5 (Recalibrado)` = Perfil_Dominante_Dilemas), by = "ID_Aluno") %>%
    left_join(df_indices_anon %>% select(ID_Aluno, IEA), by = "ID_Aluno") %>%
    left_join(df_squad_por_id, by = "ID_Aluno") %>%
    mutate(
        Squad = coalesce(Squad, "Sem Squad"),
        `F3 (Pares)` = coalesce(`F3 (Pares)`, "Sem Grupo F3"),
        `F4 (Instinto)` = coalesce(`F4 (Instinto)`, `F5 (Recalibrado)`)
    ) %>%
    select(Squad, ID_Aluno, IEA, `F1 (Ego)`, `F3 (Pares)`, `F4 (Instinto)`, `F5 (Recalibrado)`) %>%
    arrange(Squad, desc(IEA))

# Conclusão: df_consolidado_entregas está pronto para o Capítulo 7
print("--- df_consolidado_entregas exportado com sucesso ---")
print(paste("Linhas:", nrow(df_consolidado_entregas), "| Colunas:", paste(names(df_consolidado_entregas), collapse = ", ")))
