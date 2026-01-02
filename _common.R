# --- _common.R ---
set.seed(42)

# 1. Bibliotecas base para todos os capítulos
library(miningKPI)
library(tidyverse)
library(knitr)
library(ggplot2)

# 2. Configurações de saída do Knitr
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.align = "center",
  fig.retina = 3,
  out.width = "90%"
)

# 3. PALETA DE 10 CORES - IDENTIDADE FUNDAMENTAL
mining_colors <- c(
  "EFH"  = "#27ae60", # Verde (Effective)
  "ODH"  = "#f39c12", # Laranja (Operational Delay)
  "DWH"  = "#16a085", # Turquesa (Different Worked)
  "IWH"  = "#2980b9", # Azul (Infrastructure)
  "IIH"  = "#f1c40f", # Amarelo (Internal Idle)
  "EIH"  = "#d35400", # Abóbora (External Idle)
  "CMH"  = "#c0392b", # Vermelho (Corrective Maintenance)
  "ACH"  = "#000000", # Preto (Accident)
  "SPH"  = "#8e44ad", # Roxo (Systematic Preventive)
  "NSPH" = "#34495e"  # Dark Gray (Non-Systematic)
)

# 4. Tema Padrão do Livro
theme_set(theme_minimal(base_size = 12) +
            theme(legend.position = "bottom",
                  panel.grid.minor = element_blank()))

# Funções auxiliares para os gráficos
scale_fill_mining <- function() scale_fill_manual(values = mining_colors)
scale_color_mining <- function() scale_color_manual(values = mining_colors)
