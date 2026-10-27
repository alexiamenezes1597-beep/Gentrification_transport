# =============================================================================
# Modelagem Matemática e Espacial da Gentrificação em Washington, D.C.
# Período: ACS 5-Year 2006-2010 (Baseline) vs. 2019-2023 (Endline)
#
# Base Teórica e Metodológica Implementada:
#   1. Teoria do Rent Gap (Smith, 1979) e Gentrificação Endógena (Guerrieri et al., 2013)
#   2. Modelo Dicotómico de Elegibilidade e Ocorrência (Freeman, 2005)
#   3. Índice Contínuo de Percentis Socioeconómicos (McKinnish, Walsh & White, 2010)
#   4. Tipologia Multi-Estágio e Vulnerabilidade (Urban Displacement Project - UDP)
#   5. Validação Exógena de Substituição Racial ("Cappuccino City" - Hyra, 2017)

  # -----------------------------------------------------------------------------
# 0. Setup e Pacotes
needed <- c("dplyr", "tidyr", "readr", "stringr", "ggplot2", "tibble", "scales",
            "broom", "sf", "tidycensus", "survey", "writexl", "forcats")
to_install <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)
invisible(lapply(needed, library, character.only = TRUE))
options(tigris_use_cache = TRUE, timeout = 3600, survey.replicates.mse = TRUE)

# Caso a chave já esteja instalada no .Renviron, não é necessário sobrescrever:
if (Sys.getenv("CENSUS_API_KEY") == "") {
  census_api_key("6b6942a39043df44343d1eaa2ede63927839027a", install = TRUE, overwrite = TRUE)
  readRenviron("~/.Renviron")
}

  # -----------------------------------------------------------------------------
# 1. Parâmetros e Correção de Inflação (CPI-U)
YEAR_OLD <- 2010          # ACS 2006-2010
YEAR_NEW <- 2023          # ACS 2019-2023
P_OLD    <- paste0(YEAR_OLD - 4, "-", YEAR_OLD)
P_NEW    <- paste0(YEAR_NEW - 4, "-", YEAR_NEW)

# Médias anuais do CPI-U (BLS) para converter valores de 2010 para dólares reais de 2023
CPI <- c("2010" = 218.056, "2011" = 224.939, "2012" = 229.594, "2013" = 232.957,
         "2014" = 236.736, "2015" = 237.017, "2016" = 240.007, "2017" = 245.120,
         "2018" = 251.107, "2019" = 255.657, "2020" = 258.811, "2021" = 270.970,
         "2022" = 292.655, "2023" = 304.702, "2024" = 313.689)

# CORREÇÃO CRÍTICA 1: Ativação do deflator real (CPI_RATIO ~ 1.397357)
CPI_RATIO <- CPI[[as.character(YEAR_NEW)]] / CPI[[as.character(YEAR_OLD)]]

MIN_TRACT_HH <- 150       # Filtro para remover tratos não-residenciais (ex: National Mall, monumentos)

OUT_DIR <- "outputs_gentrification"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

save_plot <- function(p, name, w = 9, h = 6) {
  print(p)
  ggsave(file.path(OUT_DIR, name), p, width = w, height = h, dpi = 300, bg = "white")
}

# Funções auxiliares de Margem de Erro (MOE - Fórmulas do Census Bureau)
moe_prop_ <- function(num, den, moe_num, moe_den) {
  p <- num / den
  x <- moe_num^2 - p^2 * moe_den^2
  ifelse(x < 0, sqrt(moe_num^2 + p^2 * moe_den^2) / den, sqrt(pmax(x, 0)) / den)
}
sig_diff <- function(d, moe1, moe2) abs(d / sqrt((moe1 / 1.645)^2 + (moe2 / 1.645)^2)) > 1.645

  # -----------------------------------------------------------------------------
# 2. Coleta de Variáveis Censitárias (ACS 5-Year)
B_VARS_GENT <- c(
  # Renda e Custos Habitacionais (Rent Gap / Valorização)
  med_inc    = "B19013_001", # Rendimento Mediano do Agregado Familiar (MHI)
  med_rent   = "B25064_001", # Aluguer Bruto Mediano
  med_val    = "B25077_001", # Valor Mediano do Imóvel (Owner-Occupied)
  
  # Estrutura Domiciliar e Posse (Tenure)
  hh         = "B11001_001", # Total de domicílios
  tenure_tot = "B25003_001", # Total de unidades ocupadas
  owner_hh   = "B25003_002", # Ocupadas pelo proprietário
  renter_hh  = "B25003_003", # Ocupadas por inquilinos
  
  # Idade do Parque Habitacional (B25034 - Desinvestimento físico de Freeman)
  hu_tot     = "B25034_001", # Total de unidades habitacionais
  hu_new1    = "B25034_002", # Construídas nos anos mais recentes
  hu_new2    = "B25034_003", # Construídas na faixa seguinte
  hu_new3    = "B25034_004", # Construídas na 3ª faixa (~últimos 20 anos no ACS 2010)
  
  # Educação (B15002 - População 25+ com Bacharelado ou superior)
  pop25      = "B15002_001",
  ba_m       = "B15002_015", ma_m   = "B15002_016", prof_m = "B15002_017", phd_m = "B15002_018",
  ba_f       = "B15002_032", ma_f   = "B15002_033", prof_f = "B15002_034", phd_f = "B15002_035",
  
  # Raça e Etnia (B03002 - Para índice de vulnerabilidade UDP e validação exógena)
  pop_tot    = "B03002_001",
  white_nh   = "B03002_003", # Brancos Não-Hispânicos
  black_nh   = "B03002_004", # Negros / Afro-Americanos Não-Hispânicos``
  asian_nh   = "B03002_006", # Asiáticos Não-Hispânicos
  hisp       = "B03002_012"  # Hispânicos / Latinos
)

get_dc_gent <- function(year, geography = "tract", geometry = FALSE) {
  message(sprintf("Coletando ACS 5-year (%d) para DC [%s]...", year, geography))
  get_acs(
    geography    = geography,
    state        = "DC",
    year         = year,
    survey       = "acs5",
    variables    = B_VARS_GENT,
    output       = "wide",
    geometry     = geometry,
    progress_bar = FALSE
  ) %>%
    mutate(
      baplusE = ba_mE + ma_mE + prof_mE + phd_mE + ba_fE + ma_fE + prof_fE + phd_fE,
      baplusM = sqrt(ba_mM^2 + ma_mM^2 + prof_mM^2 + phd_mM^2 +
                       ba_fM^2 + ma_fM^2 + prof_fM^2 + phd_fM^2),
      hu_recent20E = hu_new1E + hu_new2E + hu_new3E
    )
}

adjust_dollars_gent <- function(df, ratio) {
  df %>%
    mutate(
      across(c(med_incE, med_incM, med_rentE, med_rentM, med_valE, med_valM), ~ .x * ratio)
    )
}

# Coleta de tratos e total da cidade
new_raw    <- get_dc_gent(YEAR_NEW, "tract", geometry = TRUE)
tracts_geo <- new_raw %>% select(GEOID, NAME)
new_tr     <- new_raw %>% sf::st_drop_geometry()
old_tr     <- get_dc_gent(YEAR_OLD, "tract", geometry = FALSE) %>% adjust_dollars_gent(CPI_RATIO)

dc_new     <- get_dc_gent(YEAR_NEW, "state")
dc_old     <- get_dc_gent(YEAR_OLD, "state") %>% adjust_dollars_gent(CPI_RATIO)

  # -----------------------------------------------------------------------------
# 3. Crosswalk Longitudinal (Tratos 2010 -> Tratos 2020)
xw_url <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/tract/tab20_tract20_tract10_natl.txt"
xw <- read_delim(xw_url, delim = "|", col_types = cols(.default = col_character())) %>%
  filter(str_starts(GEOID_TRACT_20, "11")) %>%
  transmute(
    GEOID20 = GEOID_TRACT_20,
    GEOID10 = GEOID_TRACT_10,
    w       = as.numeric(AREALAND_PART) / as.numeric(AREALAND_TRACT_10)
  ) %>%
  filter(is.finite(w), w > 0)

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) NA_real_ else sum(x[ok] * w[ok]) / sum(w[ok])
}
wmoe <- function(moe, w) {
  ok <- !is.na(moe) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  wn <- w[ok] / sum(w[ok])
  sqrt(sum((wn * moe[ok])^2))
}

old_xw <- xw %>%
  inner_join(old_tr, by = c("GEOID10" = "GEOID")) %>%
  group_by(GEOID = GEOID20) %>%
  summarise(
    # Médias ponderadas pelos respectivos universos domiciliares antes de somar contagens
    med_incE  = wmean(med_incE,  hhE * w),
    med_incM  = wmoe(med_incM,   hhE * w),
    med_rentE = wmean(med_rentE, renter_hhE * w),
    med_rentM = wmoe(med_rentM,  renter_hhE * w),
    med_valE  = wmean(med_valE,  owner_hhE * w),
    med_valM  = wmoe(med_valM,   owner_hhE * w),
    
    # MOEs das contagens interpoladas
    pop25M    = sqrt(sum((pop25M * w)^2, na.rm = TRUE)),
    baplusM   = sqrt(sum((baplusM * w)^2, na.rm = TRUE)),
    
    # Contagens interpoladas por fração de área
    across(c(hhE, tenure_totE, owner_hhE, renter_hhE, hu_totE, hu_recent20E,
             pop25E, baplusE, pop_totE, white_nhE, black_nhE, asian_nhE, hispE),
           ~ sum(.x * w, na.rm = TRUE)),
    .groups = "drop"
  )

  # -----------------------------------------------------------------------------
# 4. Derivação de Indicadores por Período e Construção do Painel
derive_gent <- function(df) {
  df %>%
    mutate(
      share_ba_plus   = baplusE / pop25E,
      share_ba_plusM  = moe_prop_(baplusE, pop25E, baplusM, pop25M),
      share_renter    = renter_hhE / tenure_totE,
      share_owner     = owner_hhE / tenure_totE,
      share_recent_hu = hu_recent20E / hu_totE,
      share_white     = white_nhE / pop_totE,
      share_black     = black_nhE / pop_totE,
      share_hisp      = hispE / pop_totE,
      share_poc       = 1 - (white_nhE / pop_totE), # Comunidades de cor (UDP)
      
      # Posições em Percentil (0 a 1) dentro de DC (Abordagem McKinnish et al., 2010)
      pct_rank_inc    = percent_rank(med_incE),
      pct_rank_rent   = percent_rank(med_rentE),
      pct_rank_val    = percent_rank(med_valE),
      pct_rank_ba     = percent_rank(share_ba_plus),
      
      # Custo habitacional composto em percentil (combina aluguer e valor de venda)
      pct_rank_hous   = coalesce(0.5 * pct_rank_rent + 0.5 * pct_rank_val,
                                 pct_rank_rent, pct_rank_val),
      
      # Índice Contínuo de Status Socioeconómico / Gentrificação (G_i,y)
      G_index         = (pct_rank_inc + pct_rank_hous + pct_rank_ba) / 3
    )
}

new_ind <- derive_gent(new_tr)
old_ind <- derive_gent(old_xw)
dc_new  <- derive_gent(dc_new)
dc_old  <- derive_gent(dc_old)

keep_cols <- c("GEOID", "hhE", "pop_totE", "med_incE", "med_incM", "med_rentE", "med_rentM",
               "med_valE", "med_valM", "share_ba_plus", "share_ba_plusM", "share_renter",
               "share_owner", "share_recent_hu", "share_white", "share_black", "share_hisp",
               "share_poc", "pct_rank_inc", "pct_rank_rent", "pct_rank_val", "pct_rank_hous",
               "pct_rank_ba", "G_index")

panel_df <- new_ind %>%
  select(all_of(keep_cols)) %>%
  rename_with(~ paste0(.x, "_23"), -GEOID) %>%
  inner_join(
    old_ind %>% select(all_of(keep_cols)) %>% rename_with(~ paste0(.x, "_10"), -GEOID),
    by = "GEOID"
  ) %>%
  filter(hhE_23 >= MIN_TRACT_HH, hhE_10 >= MIN_TRACT_HH)

  # -----------------------------------------------------------------------------
# 5. Modelagem Matemática da Gentrificação (Freeman, McKinnish, UDP & Guerrieri)
    # -----------------------------------------------------------------------------
# 5.1 Matriz de Adjacência Espacial (Guerrieri, Hartley & Hurst, 2013)
panel_sf_temp <- tracts_geo %>% inner_join(panel_df, by = "GEOID")
touches_list  <- sf::st_touches(panel_sf_temp)

# Identifica tratos inicialmente afluentes (Top 30% de renda ou valor imobiliário em 2010)
affluent_10_idx <- which(
  panel_sf_temp$pct_rank_inc_10 >= 0.70 | panel_sf_temp$pct_rank_hous_10 >= 0.70
)
borders_affluent_10 <- sapply(touches_list, function(nb) any(nb %in% affluent_10_idx))

# 5.2 Aplicação dos Modelos ao Painel
panel_df <- panel_df %>%
  mutate(
    borders_affluent_10 = borders_affluent_10,
    
    # --- Variações Absolutas e Percentuais Reais (2010 -> 2023) ---
    d_med_inc      = med_incE_23 - med_incE_10,
    pct_med_inc    = (med_incE_23 / med_incE_10) - 1,
    d_med_rent     = med_rentE_23 - med_rentE_10,
    pct_med_rent   = (med_rentE_23 / med_rentE_10) - 1,
    d_med_val      = med_valE_23 - med_valE_10,
    pct_med_val    = (med_valE_23 / med_valE_10) - 1,
    d_share_ba     = share_ba_plus_23 - share_ba_plus_10,
    d_share_renter = share_renter_23 - share_renter_10,
    
    # Mudança no perfil racial (Validação Exógena - Hyra, 2017)
    d_share_white  = share_white_23 - share_white_10,
    d_share_black  = share_black_23 - share_black_10,
    d_share_hisp   = share_hisp_23 - share_hisp_10,
    
    # Testes de Significância Estatística (90% MOE)
    sig_inc        = sig_diff(d_med_inc, med_incM_23, med_incM_10),
    sig_rent       = sig_diff(d_med_rent, med_rentM_23, med_rentM_10),
    sig_ba         = sig_diff(d_share_ba, share_ba_plusM_23, share_ba_plusM_10),
    
    # --- MODELO 1: ÍNDICE CONTÍNUO DE PERCENTIS (McKinnish et al., 2010) ---
    d_pct_inc      = pct_rank_inc_23 - pct_rank_inc_10,
    d_pct_hous     = pct_rank_hous_23 - pct_rank_hous_10,
    d_pct_ba       = pct_rank_ba_23 - pct_rank_ba_10,
    d_G_index      = G_index_23 - G_index_10,
    
    # --- MODELO 2: LANCE FREEMAN (2005) CORRIGIDO ---
    # Fase 1 (Elegibilidade): MHI abaixo do 40º percentil dos tratos + parque antigo
    freeman_elig   = med_incE_10 <= quantile(med_incE_10, 0.40, na.rm = TRUE) &
      share_recent_hu_10 <= quantile(share_recent_hu_10, 0.60, na.rm = TRUE),
    # Fase 2 (Ocorrência): Aumento de % BA+ > mediana E valorização real (aluguer ou imóvel) > 0
    freeman_gent   = freeman_elig &
      d_share_ba > median(d_share_ba, na.rm = TRUE) &
      (coalesce(d_med_rent > 0, FALSE) | coalesce(d_med_val > 0, FALSE)),
    
    # --- MODELO 3: URBAN DISPLACEMENT PROJECT (UDP / Bates) + CONTÁGIO ---
    # Índice de Vulnerabilidade Económica na Linha de Base (Quintis de 1 a 5 -> Soma de 4 a 20)
    q_renter_10    = ntile(share_renter_10, 5),
    q_poc_10       = ntile(share_poc_10, 5),
    q_lowinc_10    = ntile(-med_incE_10, 5),       # Invertido: menor renda = maior vulnerabilidade (5)
    q_noba_10      = ntile(-share_ba_plus_10, 5),  # Invertido: menor % BA+ = maior vulnerabilidade (5)
    udp_vuln_score = q_renter_10 + q_poc_10 + q_lowinc_10 + q_noba_10,
    
    # Elegibilidade/Vulnerabilidade inicial (Score >= 12 ou abaixo do 50º percentil de renda)
    udp_vulnerable = udp_vuln_score >= 12 | pct_rank_inc_10 <= 0.50,
    
    # Vetores de Aceleração (acima da mediana da cidade entre 2010 e 2023)
    demo_surge     = (d_share_ba > median(d_share_ba, na.rm = TRUE)) |
      (d_med_inc  > median(d_med_inc, na.rm = TRUE)),
    market_surge   = coalesce(d_med_rent > median(d_med_rent, na.rm = TRUE), FALSE) |
      coalesce(d_med_val  > median(d_med_val, na.rm = TRUE), FALSE) |
      (d_pct_hous > 0.05)
  )

# Identifica também adjacência aos tratos em Gentrificação Dinâmica para mapear a fronteira
dyn_idx <- which(panel_df$udp_vulnerable & panel_df$demo_surge & panel_df$market_surge)
borders_frontline <- sapply(touches_list, function(nb) any(nb %in% union(affluent_10_idx, dyn_idx)))

panel_df <- panel_df %>%
  mutate(
    borders_frontline = borders_frontline,
    typology_udp = case_when(
      !udp_vulnerable                                   ~ "5. Afluente / Exclusão Contínua",
      udp_vulnerable & demo_surge & market_surge        ~ "1. Gentrificação Dinâmica (Intensa)",
      udp_vulnerable & (demo_surge | market_surge)      ~ "2. Gentrificação Precoce ( Demografia ou Preço)",
      udp_vulnerable & borders_frontline                ~ "3. Suscetível (Fronteira de Contágio)",
      TRUE                                              ~ "4. Baixa Renda Estagnada"
    ),
    typology_udp = factor(typology_udp, levels = c(
      "1. Gentrificação Dinâmica (Intensa)",
      "2. Gentrificação Precoce ( Demografia ou Preço)",
      "3. Suscetível (Fronteira de Contágio)",
      "4. Baixa Renda Estagnada",
      "5. Afluente / Exclusão Contínua"
    ))
  )

panel_sf <- tracts_geo %>% inner_join(panel_df, by = "GEOID")

  # -----------------------------------------------------------------------------
# 6. Tabelas de Resumo e Modelos Econométricos Espaciais
typology_summary <- panel_df %>%
  group_by(typology_udp) %>%
  summarise(
    tratos               = n(),
    vuln_score_10        = median(udp_vuln_score, na.rm = TRUE),
    med_inc_10_usd23     = median(med_incE_10, na.rm = TRUE),
    med_inc_23_usd23     = median(med_incE_23, na.rm = TRUE),
    pct_chg_inc_real     = median(pct_med_inc, na.rm = TRUE) * 100,
    chg_ba_pts           = median(d_share_ba, na.rm = TRUE) * 100,
    chg_rent_real_usd    = median(d_med_rent, na.rm = TRUE),
    chg_homeval_real_usd = median(d_med_val, na.rm = TRUE),
    delta_G_index        = median(d_G_index, na.rm = TRUE),
    chg_black_pts        = median(d_share_black, na.rm = TRUE) * 100,
    chg_white_pts        = median(d_share_white, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

cat("\n=== RESUMO DAS TIPOLOGIAS DE GENTRIFICAÇÃO EM DC (DÓLARES REAIS DE 2023) ===\n")
print(as.data.frame(typology_summary))

# Regressão de Guerrieri, Hartley & Hurst (2013): Fecho do Rent Gap e Contágio
mod_guerrieri_val <- lm(
  pct_med_val ~ log(med_valE_10) + borders_affluent_10 + d_share_ba,
  data = panel_df %>% filter(!is.na(pct_med_val), med_valE_10 > 0)
)
mod_guerrieri_rent <- lm(
  pct_med_rent ~ log(med_rentE_10) + borders_affluent_10 + d_share_ba,
  data = panel_df %>% filter(!is.na(pct_med_rent), med_rentE_10 > 0)
)
mod_cappuccino <- lm(
  d_share_black ~ d_G_index + udp_vuln_score,
  data = panel_df
)

econ_models <- bind_rows(
  tidy(mod_guerrieri_val, conf.int = TRUE)  %>% mutate(modelo = "M1: Taxa Real Valorização Imóvel (Guerrieri)"),
  tidy(mod_guerrieri_rent, conf.int = TRUE) %>% mutate(modelo = "M2: Taxa Real Crescimento Aluguer (Guerrieri)"),
  tidy(mod_cappuccino, conf.int = TRUE)     %>% mutate(modelo = "M3: Deslocamento Racial / % Negros (Hyra)")
) %>% relocate(modelo)

cat("\n=== MODELOS ECONOMÉTRICOS (RENT GAP, CONTÁGIO E DESLOCAMENTO RACIAL) ===\n")
print(as.data.frame(econ_models %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))

  # -----------------------------------------------------------------------------
# 7. Paleta de Cores e Geração de 8 Novos Gráficos Explicativos (G1 a G8)
pal_udp <- c(
  "1. Gentrificação Dinâmica (Intensa)"             = "#b91c1c", # Vermelho escuro
  "2. Gentrificação Precoce ( Demografia ou Preço)" = "#f97316", # Laranja
  "3. Suscetível (Fronteira de Contágio)"           = "#fde047", # Amarelo ouro
  "4. Baixa Renda Estagnada"                        = "#94a3b8", # Cinza azulado
  "5. Afluente / Exclusão Contínua"                 = "#1e3a8a"  # Azul marinho
)

# ---- PLOT G1: Mapa da Tipologia Multi-Estágio (UDP + Contágio Espacial)
p_g1 <- ggplot(panel_sf) +
  geom_sf(aes(fill = typology_udp), colour = "white", linewidth = 0.15) +
  scale_fill_manual(values = pal_udp, name = "Estágio do Bairro") +
  labs(
    title    = "Tipologia Multi-Estágio de Gentrificação em Washington, D.C.",
    subtitle = paste0("Transição ", P_OLD, " a ", P_NEW, " (Metodologia UDP + Fronteira de Contágio de Guerrieri)"),
    caption  = "Fonte: U.S. Census Bureau, ACS 5-Year (B19013, B25064, B25077, B15002, B03002). Dólares reais de 2023."
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, colour = "grey30"),
    legend.position = "right"
  )
save_plot(p_g1, "G1_map_udp_typology.png", w = 9, h = 7.5)

# ---- PLOT G2: Mapa do Índice Contínuo de McKinnish (Delta G_i) + Contorno de Freeman
p_g2 <- ggplot(panel_sf) +
  geom_sf(aes(fill = d_G_index), colour = "white", linewidth = 0.12) +
  geom_sf(
    data = filter(panel_sf, freeman_gent %in% TRUE),
    fill = NA, colour = "black", linewidth = 0.45
  ) +
  scale_fill_distiller(
    palette   = "RdYlBu",
    direction = -1,
    limits    = c(-0.25, 0.25),
    oob       = squish,
    name      = expression(Delta * G[i] ~ "(Percentil)")
  ) +
  labs(
    title    = "Intensidade Contínua da Gentrificação (Índice de McKinnish et al., 2010)",
    subtitle = paste0("Mudança na posição relativa composta (Renda, Habitação e Educação), ", P_OLD, " a ", P_NEW,
                      "\nContorno preto = Tratos gentrificados pelo critério binário de Lance Freeman (2005)"),
    caption  = "Fonte: ACS 5-Year. Valores positivos (vermelho) indicam rápida ascensão hierárquica do trato em DC."
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9.5, colour = "grey30")
  )
save_plot(p_g2, "G2_map_continuous_index_dG.png", w = 8.5, h = 7.5)

# ---- PLOT G3: Diagrama de Quadrantes (Capital Cultural vs. Capital Imobiliário)
med_ba_chg   <- median(panel_df$d_share_ba, na.rm = TRUE) * 100
med_hous_chg <- median(panel_df$d_pct_hous, na.rm = TRUE) * 100

p_g3 <- panel_df %>%
  filter(udp_vulnerable) %>%
  ggplot(aes(x = d_share_ba * 100, y = d_pct_hous * 100, colour = typology_udp)) +
  geom_vline(xintercept = med_ba_chg, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = med_hous_chg, linetype = "dashed", colour = "grey40") +
  geom_point(aes(size = pop_totE_23), alpha = 0.8) +
  scale_colour_manual(values = pal_udp, name = "Tipologia UDP") +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  annotate("text", x = max(panel_df$d_share_ba * 100, na.rm = TRUE) * 0.75,
           y = max(panel_df$d_pct_hous * 100, na.rm = TRUE) * 0.85,
           label = "Gentrificação Dinâmica\n(Influxo Educacional + Choque Imobiliário)",
           fontface = "bold", size = 3.2, colour = "#b91c1c") +
  labs(
    title    = "Os Motores da Gentrificação nos Tratos Vulneráveis de D.C.",
    subtitle = "Cruzamento entre influxo de residentes com Ensino Superior (% BA+) e salto no percentil habitacional",
    x        = "Variação na % de Residentes com Bacharelado ou mais (pontos percentuais)",
    y        = "Mudança na Posição Relativa do Custo Habitacional (pontos de percentil)",
    caption  = "Linhas tracejadas = Medianas de D.C. Tamanho do ponto proporcional à população do trato em 2023."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
save_plot(p_g3, "G3_quadrant_drivers_ba_vs_housing.png", w = 9.5, h = 6.5)

# ---- PLOT G4: Fecho do Rent Gap e Contágio Endógeno (Guerrieri, Hartley & Hurst, 2013)
p_g4 <- panel_df %>%
  filter(!is.na(pct_med_val), med_valE_10 > 0, pct_med_val < 3) %>%
  mutate(
    adj_label = if_else(borders_affluent_10,
                        "Fronteira com Bairro Afluente em 2010",
                        "Sem Fronteira Direta com Bairro Afluente")
  ) %>%
  ggplot(aes(x = med_valE_10 / 1000, y = pct_med_val * 100, colour = adj_label)) +
  geom_point(alpha = 0.65, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  scale_x_log10(labels = dollar_format(suffix = "k")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_colour_manual(values = c("#b91c1c", "#3b82f6"), name = "Topologia Espacial (Guerrieri et al.)") +
  labs(
    title    = "Evidência Empírica do Fecho do 'Rent Gap' e Contágio Espacial em D.C.",
    subtitle = "Tratos com menor valor inicial (escala log) e adjacentes a zonas ricas sofrem valorização explosiva",
    x        = "Valor Mediano do Imóvel em 2006-2010 (Escala Log, Dólares Reais de 2023)",
    y        = "Taxa de Valorização Real do Imóvel, 2010 a 2023 (%)",
    caption  = "Equação de Guerrieri et al. (2013): dP/P = mu + omega * ln(HP_t0). Fonte: ACS 5-Year (B25077)."
  ) +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))
save_plot(p_g4, "G4_rent_gap_guerrieri_curve.png", w = 9.5, h = 6)

# ---- PLOT G5: Validação Exógena - O Efeito "Cappuccino City" (Mudança Racial por Tipologia)
p_g5 <- panel_df %>%
  select(GEOID, typology_udp, d_share_black, d_share_white, d_share_hisp) %>%
  pivot_longer(
    cols      = c(d_share_black, d_share_white, d_share_hisp),
    names_to  = "grupo_racial",
    values_to = "mudanca_pts"
  ) %>%
  mutate(
    mudanca_pts  = mudanca_pts * 100,
    grupo_racial = recode(
      grupo_racial,
      d_share_black = "População Negra (Não-Hispânica)",
      d_share_white = "População Branca (Não-Hispânica)",
      d_share_hisp  = "População Hispânica / Latina"
    )
  ) %>%
  ggplot(aes(x = typology_udp, y = mudanca_pts, fill = grupo_racial)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_boxplot(alpha = 0.85, outlier.size = 0.9, position = position_dodge(0.8)) +
  scale_fill_manual(
    values = c("População Negra (Não-Hispânica)"  = "#1d4ed8",
               "População Branca (Não-Hispânica)" = "#ea580c",
               "População Hispânica / Latina"     = "#16a34a"),
    name = NULL
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  labs(
    title    = "Validação Exógena: O Efeito 'Cappuccino City' (Derek Hyra) em Washington, D.C.",
    subtitle = "Substituição demográfica racial entre 2006-2010 e 2019-2023 segundo o estágio de gentrificação",
    x        = NULL,
    y        = "Variação na Proporção da População do Trato (pontos percentuais)",
    caption  = "Nota: A raça foi excluída da definição matemática de gentrificação para evitar tautologia (validação exógena)."
  ) +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))
save_plot(p_g5, "G5_cappuccino_city_racial_shift.png", w = 10.5, h = 6)

# ---- PLOT G6 (Corrigido sem depender do pacote forcats)
p_g6 <- panel_df %>%
  group_by(typology_udp) %>%
  summarise(
    Renda_10     = mean(pct_rank_inc_10, na.rm = TRUE) * 100,
    Renda_23     = mean(pct_rank_inc_23, na.rm = TRUE) * 100,
    Educacao_10  = mean(pct_rank_ba_10, na.rm = TRUE) * 100,
    Educacao_23  = mean(pct_rank_ba_23, na.rm = TRUE) * 100,
    Habitacao_10 = mean(pct_rank_hous_10, na.rm = TRUE) * 100,
    Habitacao_23 = mean(pct_rank_hous_23, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(-typology_udp, names_to = c("dimensao", "ano"), names_sep = "_") %>%
  pivot_wider(names_from = ano, values_from = value, names_prefix = "p_") %>%
  mutate(
    typology_rev = factor(typology_udp, levels = rev(levels(typology_udp))),
    dimensao = recode(dimensao,
                      "Educacao"  = "1. Capital Cultural (% BA+)",
                      "Habitacao" = "2. Custo Habitacional (Aluguer + Imóvel)",
                      "Renda"     = "3. Rendimento Familiar (MHI)")
  ) %>%
  ggplot(aes(y = typology_rev)) +
  geom_segment(aes(x = p_10, xend = p_23, yend = typology_rev),
               colour = "grey60", linewidth = 1.2) +
  geom_point(aes(x = p_10, colour = P_OLD), size = 3.2) +
  geom_point(aes(x = p_23, colour = P_NEW), size = 3.2) +
  facet_wrap(~ dimensao) +
  scale_colour_manual(values = setNames(c("#94a3b8", "#b91c1c"), c(P_OLD, P_NEW)), name = "Período") +
  labs(
    title    = "Decomposição Vetorial da Gentrificação por Dimensão (2010 vs. 2023)",
    subtitle = "Posição média em percentil (0 a 100) de cada grupo de bairros na hierarquia urbana de D.C.",
    x        = "Percentil Médio em Washington, D.C.",
    y        = NULL,
    caption  = "Fonte: ACS 5-Year. Demonstra como o choque de escolaridade (% BA+) puxa a reprecificação imobiliária."
  ) +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))
save_plot(p_g6, "G6_percentile_trajectories_dumbbell.png", w = 11, h = 5.5)

# ---- PLOT G7: Vulnerabilidade Inicial (UDP) vs. Mudança no Índice G (Relação Não-Linear)
p_g7 <- ggplot(panel_df, aes(x = udp_vuln_score, y = d_G_index, colour = typology_udp)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_jitter(width = 0.25, alpha = 0.75, size = 2.4) +
  geom_smooth(aes(group = 1), method = "loess", colour = "black", linetype = "solid", se = TRUE) +
  scale_colour_manual(values = pal_udp, name = "Tipologia UDP") +
  labs(
    title    = "Curva de Pressão: Vulnerabilidade Inicial (2010) vs. Ascensão Relativa (Delta G)",
    subtitle = "Bairros de vulnerabilidade moderada-alta (scores 12 a 16) adjacentes ao centro sofrem a maior reestruturação",
    x        = "Índice de Vulnerabilidade Económica do UDP em 2006-2010 (Escala de 4 a 20)",
    y        = expression("Mudança no Índice de Gentrificação " * (Delta * G[i])),
    caption  = "Curva preta = ajuste não-paramétrico LOESS com IC de 95%."
  ) +
  theme_minimal() +
  theme(legend.position = "right", plot.title = element_text(face = "bold"))
save_plot(p_g7, "G7_vulnerability_vs_dG.png", w = 9.5, h = 6)

# ---- PLOT G8: Painel Espacial 2x2 dos 4 Vetores Censitários Fundamentais
map_long <- panel_sf %>%
  transmute(
    GEOID,
    `1. Educação: +Pts % Ensino Superior (BA+)` = percent_rank(d_share_ba) * 100,
    `2. Renda: Crescimento Real do MHI`         = percent_rank(d_med_inc) * 100,
    `3. Aluguer: Crescimento Real Bruto`        = percent_rank(d_med_rent) * 100,
    `4. Propriedade: Valorização Real Imóvel`   = percent_rank(d_med_val) * 100
  ) %>%
  pivot_longer(cols = -c(GEOID, geometry), names_to = "indicador", values_to = "percentil_mudanca")

p_g8 <- ggplot(map_long) +
  geom_sf(aes(fill = percentil_mudanca), colour = "white", linewidth = 0.08) +
  facet_wrap(~ indicador, ncol = 2) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Percentil da\nMudança (0-100)") +
  labs(
    title    = "Geografia Comparada dos 4 Indicadores Censitários de Gentrificação em D.C.",
    subtitle = paste0("Posição em percentil da variação real entre ", P_OLD, " e ", P_NEW, " por trato censitário"),
    caption  = "Fonte: ACS 5-Year (B15002, B19013, B25064, B25077)."
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    strip.text    = element_text(face = "bold", size = 10, margin = margin(b = 4, t = 4)),
    legend.position = "right"
  )
save_plot(p_g8, "G8_map_4_pillars_comparison.png", w = 10, h = 9)

  # -----------------------------------------------------------------------------
# 8. Exportação das Tabelas e do Painel Georreferenciado
results_gent <- list(
  typology_summary = typology_summary,
  econ_models      = econ_models,
  tract_panel_gent = panel_df
)
write_xlsx(results_gent, file.path(OUT_DIR, "dc_gentrification_models.xlsx"))
write_csv(panel_df, file.path(OUT_DIR, "dc_tract_gentrification_panel.csv"))

cat("\nConcluído! Todos os 8 gráficos, CSV e ficheiro Excel foram salvos em: ./", OUT_DIR, "\n", sep = "")

# =============================================================================
# SCRIPT UNIFICADO - PARTE 2: Gentrificação, Heat Maps de Renda (2010 vs 2023)
# e Mobilidade nas 4 Faixas de Renda ($0-45k, $45k-70k, $70k-120k, >$120k)
#
# Estrutura Completa num Único Bloco:
#   0. Setup, Pacotes, Parâmetros e Definição das 4 Faixas de Renda
#   PARTE A & BLOCO H - Tratos de DC (ACS 5-Year 2006-2010 vs 2019-2023):
#                       Tipologia UDP/McKinnish/Freeman, Heat Maps de Renda
#                       Média/Mediana (H1-H4) e Impacto no Tempo de Viagem (A1-A6)
#   PARTE B - Região Metropolitana: Condados 5-Year + Série Anual 1-Year (2010-2023)
#   PARTE C - DC como Local de Trabalho (S0804) vs. Local de Residência (S0802)
#   PARTE D - Microdados PUMS (5-Year e 1-Year 2010 -> 2019 -> 2023):
#             Análise completa nas 4 Faixas de Renda (Gráficos D1 a D8,
#             incluindo D4 Mode Shares, D6 Home State Shares e D8 1-Yr Trend)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Setup e Pacotes
# -----------------------------------------------------------------------------
needed <- c("dplyr", "tidyr", "readr", "stringr", "ggplot2", "tibble", "scales",
            "broom", "sf", "tidycensus", "survey", "writexl", "forcats")
to_install <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)
invisible(lapply(needed, library, character.only = TRUE))
options(tigris_use_cache = TRUE, timeout = 3600, survey.replicates.mse = TRUE)

if (Sys.getenv("CENSUS_API_KEY") == "") {
  census_api_key("6b6942a39043df44343d1eaa2ede63927839027a", install = TRUE, overwrite = TRUE)
  readRenviron("~/.Renviron")
}

# -----------------------------------------------------------------------------
# 1. Parâmetros Globais, Deflator CPI-U e as 4 Faixas de Renda
# -----------------------------------------------------------------------------
YEAR_OLD <- 2010          # ACS 5-year 2006-2010
YEAR_NEW <- 2023          # ACS 5-year 2019-2023
P_OLD    <- paste0(YEAR_OLD - 4, "-", YEAR_OLD)
P_NEW    <- paste0(YEAR_NEW - 4, "-", YEAR_NEW)

# CPI-U anual (BLS) para converter valores de 2010 para dólares constantes de 2023
CPI <- c("2010" = 218.056, "2011" = 224.939, "2012" = 229.594, "2013" = 232.957,
         "2014" = 236.736, "2015" = 237.017, "2016" = 240.007, "2017" = 245.120,
         "2018" = 251.107, "2019" = 255.657, "2020" = 258.811, "2021" = 270.970,
         "2022" = 292.655, "2023" = 304.702, "2024" = 313.689)
CPI_RATIO <- CPI[[as.character(YEAR_NEW)]] / CPI[[as.character(YEAR_OLD)]]

MIN_MODE_WORKERS  <- 50
MAX_CV            <- 0.40
MIN_TRACT_WORKERS <- 100
MIN_PUMS_N        <- 50

# 4 FAIXAS DE RENDA PADRONIZADAS EM TODO O SCRIPT: $0-45k, $45k-70k, $70k-120k, >$120k
WAGE_BREAKS <- c(-Inf, 45000, 70000, 120000, Inf)
WAGE_LABELS <- c("Low-wage ($0-$45k)", "Lower-mid ($45k-$70k)",
                 "Upper-mid ($70k-$120k)", "High-income (> $120k)")

# Ordem alfabética dos modais para que "Worked from home" fique na base em amarelo (Set2) no D4
MODE_LEVELS_D4 <- c("Carpool", "Drove alone", "Other", "Transit", "Walk/bike", "Worked from home")

pal_wage4 <- setNames(
  c("#c2410c", "#d97706", "#2563eb", "#0f172a"),
  WAGE_LABELS
)

pal_udp <- c(
  "1. Gentrificação Dinâmica (Intensa)"            = "#b91c1c",
  "2. Gentrificação Precoce (Demografia ou Preço)" = "#f97316",
  "3. Suscetível (Fronteira de Contágio)"          = "#eab308",
  "4. Baixa Renda Estagnada"                       = "#94a3b8",
  "5. Afluente / Exclusão Contínua"                = "#1e3a8a"
)

pal_years      <- setNames(c("#94a3b8", "#1d4ed8"), c(P_OLD, P_NEW))
pal_home_state <- c("DC" = "#1d4ed8", "Maryland" = "#c2410c", "Virginia" = "#a16207")

RES_STATES          <- c("DC", "MD", "VA", "WV", "PA", "DE")
RUN_1YR_SENSITIVITY <- TRUE
YEARS_1YR_PUMS      <- c(2010, 2019, 2023)
YEARS_1YR_REGION    <- setdiff(2010:2023, 2020)

REGION <- tribble(
  ~GEOID,  ~county,                ~ring,
  "11001", "District of Columbia", "Core (DC)",
  "51013", "Arlington, VA",        "Inner suburbs",
  "51510", "Alexandria, VA",       "Inner suburbs",
  "24033", "Prince George's, MD",  "Inner suburbs",
  "24031", "Montgomery, MD",       "Inner suburbs",
  "51059", "Fairfax County, VA",   "Inner suburbs",
  "51600", "Fairfax city, VA",     "Inner suburbs",
  "51610", "Falls Church, VA",     "Inner suburbs",
  "51107", "Loudoun, VA",          "Outer suburbs",
  "51153", "Prince William, VA",   "Outer suburbs",
  "51683", "Manassas, VA",         "Outer suburbs",
  "51685", "Manassas Park, VA",    "Outer suburbs",
  "51179", "Stafford, VA",         "Outer suburbs",
  "24017", "Charles, MD",          "Outer suburbs",
  "24021", "Frederick, MD",        "Outer suburbs",
  "24003", "Anne Arundel, MD",     "Outer suburbs",
  "24027", "Howard, MD",           "Outer suburbs"
)

OUT_DIR   <- "outputs_part2_unified"
CACHE_DIR <- file.path("outputs", "cache") # Reutiliza cache PUMS existente
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
results <- list()

save_plot <- function(p, name, w = 9, h = 6) {
  print(p)
  ggsave(file.path(OUT_DIR, name), p, width = w, height = h, dpi = 300, bg = "white")
}

# Funções de Margem de Erro (MOE - Fórmulas do Census Bureau)
moe_ratio_ <- function(num, den, moe_num, moe_den) {
  r <- num / den
  sqrt(moe_num^2 + r^2 * moe_den^2) / den
}
moe_prop_ <- function(num, den, moe_num, moe_den) {
  p <- num / den
  x <- moe_num^2 - p^2 * moe_den^2
  ifelse(x < 0, moe_ratio_(num, den, moe_num, moe_den), sqrt(pmax(x, 0)) / den)
}
sig_diff <- function(d, moe1, moe2) abs(d / sqrt((moe1 / 1.645)^2 + (moe2 / 1.645)^2)) > 1.645


# =============================================================================
# PARTE A & BLOCO H - Tratos de DC: Gentrificação, Heat Maps de Renda e Commute
# =============================================================================
MODES <- c(total = "C01", drive = "C02", carpool = "C03", transit = "C04")

subject_vars_cache <- list()
get_subject_vars <- function(year, survey = "acs5") {
  key <- paste0(survey, "_", year)
  if (is.null(subject_vars_cache[[key]])) {
    subject_vars_cache[[key]] <<- load_variables(year, paste0(survey, "/subject")) %>%
      filter(str_detect(name, "^S080[24]_C0[1-5]_")) %>%
      mutate(name = str_remove(name, "E$"))
  }
  subject_vars_cache[[key]]
}

find_subject <- function(year, table, col, label_regex, survey = "acs5", strict = FALSE) {
  hits <- get_subject_vars(year, survey) %>%
    filter(str_starts(name, paste0(table, "_", col, "_")),
           str_detect(label, regex(label_regex, ignore_case = TRUE)))
  if (nrow(hits) != 1) {
    if (strict) stop(sprintf("Match error for %s in %s %d", label_regex, table, year))
    return(NA_character_)
  }
  hits$name
}

build_subject_vars <- function(year, table = "S0802", survey = "acs5", strict = FALSE) {
  v <- c()
  for (m in names(MODES)) {
    col <- MODES[[m]]
    v[paste0("workers_", m)] <- paste0(table, "_", col, "_001")
    v[paste0("time_", m)]    <- find_subject(year, table, col, "Mean travel time to work", survey, strict)
  }
  v["long60_total"] <- find_subject(year, table, "C01", "60 or more minutes", survey, strict)
  for (m in c("total", "drive", "transit")) {
    v[paste0("earn_", m)] <- find_subject(year, table, MODES[[m]], "Median earnings", survey, strict)
  }
  v[!is.na(v)]
}

B_VARS <- c(
  # Renda Mediana, Renda Agregada (p/ Renda Média Exata = B19025/B11001), Aluguer e Valor do Imóvel
  med_inc     = "B19013_001",
  agg_inc     = "B19025_001",
  med_rent    = "B25064_001",
  med_val     = "B25077_001",
  
  # Domicílios e Posse (Tenure)
  hh          = "B11001_001",
  tenure_tot  = "B25003_001",
  owner_hh    = "B25003_002",
  renter_hh   = "B25003_003",
  
  # Idade do parque habitacional (Critério de desinvestimento físico de Freeman)
  hu_tot      = "B25034_001", hu_new1 = "B25034_002", hu_new2 = "B25034_003", hu_new3 = "B25034_004",
  
  # Educação (B15002 - População 25+ com Bacharelado ou mais)
  pop25       = "B15002_001",
  ba_m        = "B15002_015", ma_m = "B15002_016", prof_m = "B15002_017", phd_m = "B15002_018",
  ba_f        = "B15002_032", ma_f = "B15002_033", prof_f = "B15002_034", phd_f = "B15002_035",
  
  # Raça/Etnia (B03002 - Vulnerabilidade UDP e Validação Exógena)
  pop_tot     = "B03002_001", white_nh = "B03002_003", black_nh = "B03002_004", hisp = "B03002_012",
  
  # Faixas de Renda Domiciliar no Trato (B19001)
  inc_tot     = "B19001_001",
  inc_0_10    = "B19001_002", inc_10_15  = "B19001_003", inc_15_20   = "B19001_004",
  inc_20_25   = "B19001_005", inc_25_30  = "B19001_006", inc_30_35   = "B19001_007",
  inc_35_40   = "B19001_008", inc_40_45  = "B19001_009", # <= $45k
  inc_45_50   = "B19001_010", inc_50_60  = "B19001_011", inc_60_75   = "B19001_012", # $45k-$75k
  inc_75_100  = "B19001_013", inc_100_125= "B19001_014", # $75k-$125k
  inc_125_150 = "B19001_015", inc_150_200= "B19001_016", inc_200p    = "B19001_017", # > $125k
  
  # Mobilidade, Teletrabalho e Tempo de Deslocamento
  tot_work    = "B08007_001", work_out_dc = "B08007_005",
  w_mode_tot  = "B08301_001", w_wfh       = "B08301_021",
  notwfh      = "B08303_001", t45 = "B08303_011", t60 = "B08303_012", t90 = "B08303_013",
  agg_time    = "B08013_001"
)

get_dc <- function(year, geography, geometry = FALSE) {
  message(sprintf("Coletando ACS 5-year (%d) para DC [%s]...", year, geography))
  subj <- get_acs(geography = geography, state = "DC", year = year, survey = "acs5",
                  variables = build_subject_vars(year, "S0802"), output = "wide",
                  geometry = geometry, progress_bar = FALSE)
  det  <- get_acs(geography = geography, state = "DC", year = year, survey = "acs5",
                  variables = B_VARS, output = "wide", progress_bar = FALSE) %>%
    select(-NAME)
  
  subj %>%
    left_join(det, by = "GEOID") %>%
    mutate(
      baplusE      = ba_mE + ma_mE + prof_mE + phd_mE + ba_fE + ma_fE + prof_fE + phd_fE,
      hu_recent20E = hu_new1E + hu_new2E + hu_new3E,
      hh_0_45kE    = inc_0_10E + inc_10_15E + inc_15_20E + inc_20_25E +
        inc_25_30E + inc_30_35E + inc_35_40E + inc_40_45E,
      hh_45_75kE   = inc_45_50E + inc_50_60E + inc_60_75E,
      hh_75_125kE  = inc_75_100E + inc_100_125E,
      hh_125kpE    = inc_125_150E + inc_150_200E + inc_200pE
    )
}

adjust_dollars <- function(df, ratio) {
  df %>%
    mutate(across(c(med_incE, med_incM, agg_incE, agg_incM,
                    med_rentE, med_rentM, med_valE, med_valM,
                    starts_with("earn_") & ends_with("E")), ~ .x * ratio))
}

derive <- function(df) {
  df %>%
    mutate(
      # Renda Média Domiciliar Exata (B19025 / B11001)
      avg_incE          = agg_incE / hhE,
      avg_incM          = moe_ratio_(agg_incE, hhE, agg_incM, hhM),
      
      # Indicadores de Mobilidade
      time_total_s0802E = time_totalE,
      time_totalE       = agg_timeE / notwfhE,
      time_totalM       = moe_ratio_(agg_timeE, notwfhE, agg_timeM, notwfhM),
      transit_share     = workers_transitE / workers_totalE,
      drive_share       = workers_driveE / workers_totalE,
      wfh_share         = w_wfhE / w_mode_totE,
      share_out_dc      = work_out_dcE / tot_workE,
      share_45plus      = (t45E + t60E + t90E) / notwfhE,
      share_60plus_tr   = (t60E + t90E) / notwfhE,
      share_90plus      = t90E / notwfhE,
      transit_penalty   = time_transitE - time_driveE,
      transit_cv        = (time_transitM / 1.645) / time_transitE,
      transit_reliable  = workers_transitE >= MIN_MODE_WORKERS &
        workers_driveE   >= MIN_MODE_WORKERS & transit_cv <= MAX_CV,
      earn_ratio_transit_drive = earn_transitE / earn_driveE,
      
      # Indicadores de Gentrificação e Composição de Renda
      share_ba_plus     = baplusE / pop25E,
      share_renter      = renter_hhE / tenure_totE,
      share_recent_hu   = hu_recent20E / hu_totE,
      share_white       = white_nhE / pop_totE,
      share_black       = black_nhE / pop_totE,
      share_poc         = 1 - (white_nhE / pop_totE),
      
      share_hh_0_45k    = hh_0_45kE / inc_totE,
      share_hh_45_75k   = hh_45_75kE / inc_totE,
      share_hh_75_125k  = hh_75_125kE / inc_totE,
      share_hh_125kp    = hh_125kpE / inc_totE,
      
      # Índice Contínuo de Percentis G_i,y (McKinnish et al., 2010)
      pct_rank_inc      = percent_rank(med_incE),
      pct_rank_rent     = percent_rank(med_rentE),
      pct_rank_val      = percent_rank(med_valE),
      pct_rank_ba       = percent_rank(share_ba_plus),
      pct_rank_hous     = coalesce(0.5 * pct_rank_rent + 0.5 * pct_rank_val,
                                   pct_rank_rent, pct_rank_val),
      G_index           = (pct_rank_inc + pct_rank_hous + pct_rank_ba) / 3
    )
}

new_raw    <- get_dc(YEAR_NEW, "tract", geometry = TRUE)
tracts_geo <- new_raw %>% select(GEOID, NAME)
new_tr     <- new_raw %>% sf::st_drop_geometry()
old_tr     <- get_dc(YEAR_OLD, "tract", geometry = FALSE) %>% adjust_dollars(CPI_RATIO)

# Crosswalk Longitudinal (Tratos 2010 -> Tratos 2020)
xw_url <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/tract/tab20_tract20_tract10_natl.txt"
xw <- read_delim(xw_url, delim = "|", col_types = cols(.default = col_character())) %>%
  filter(str_starts(GEOID_TRACT_20, "11")) %>%
  transmute(GEOID20 = GEOID_TRACT_20, GEOID10 = GEOID_TRACT_10,
            w = as.numeric(AREALAND_PART) / as.numeric(AREALAND_TRACT_10)) %>%
  filter(is.finite(w), w > 0)

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) NA_real_ else sum(x[ok] * w[ok]) / sum(w[ok])
}
wmoe <- function(moe, w) {
  ok <- !is.na(moe) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  wn <- w[ok] / sum(w[ok])
  sqrt(sum((wn * moe[ok])^2))
}

old_xw <- xw %>%
  inner_join(old_tr, by = c("GEOID10" = "GEOID")) %>%
  group_by(GEOID = GEOID20) %>%
  summarise(
    agg_timeM     = sqrt(sum((agg_timeM * w)^2, na.rm = TRUE)),
    notwfhM       = sqrt(sum((notwfhM * w)^2, na.rm = TRUE)),
    agg_incM      = sqrt(sum((agg_incM * w)^2, na.rm = TRUE)),
    hhM           = sqrt(sum((hhM * w)^2, na.rm = TRUE)),
    time_totalE   = wmean(time_totalE,   workers_totalE * w),
    time_driveE   = wmean(time_driveE,   workers_driveE * w),
    time_driveM   = wmoe(time_driveM,    workers_driveE * w),
    time_carpoolE = wmean(time_carpoolE, workers_carpoolE * w),
    time_carpoolM = wmoe(time_carpoolM,  workers_carpoolE * w),
    time_transitE = wmean(time_transitE, workers_transitE * w),
    time_transitM = wmoe(time_transitM,  workers_transitE * w),
    long60_totalE = wmean(long60_totalE, workers_totalE * w),
    earn_totalE   = wmean(earn_totalE,   workers_totalE * w),
    earn_driveE   = wmean(earn_driveE,   workers_driveE * w),
    earn_transitE = wmean(earn_transitE, workers_transitE * w),
    med_incE      = wmean(med_incE,      hhE * w),
    med_incM      = wmoe(med_incM,       hhE * w),
    med_rentE     = wmean(med_rentE,     renter_hhE * w),
    med_rentM     = wmoe(med_rentM,      renter_hhE * w),
    med_valE      = wmean(med_valE,      owner_hhE * w),
    med_valM      = wmoe(med_valM,       owner_hhE * w),
    across(c(workers_totalE, workers_driveE, workers_carpoolE, workers_transitE,
             hhE, agg_incE, tenure_totE, owner_hhE, renter_hhE, hu_totE, hu_recent20E,
             pop25E, baplusE, pop_totE, white_nhE, black_nhE, hispE,
             inc_totE, hh_0_45kE, hh_45_75kE, hh_75_125kE, hh_125kpE,
             tot_workE, work_out_dcE, w_mode_totE, w_wfhE,
             notwfhE, t45E, t60E, t90E, agg_timeE),
           ~ sum(.x * w, na.rm = TRUE)),
    .groups = "drop"
  )

new_ind <- derive(new_tr)
old_ind <- derive(old_xw)
dc_new  <- get_dc(YEAR_NEW, "state") %>% derive()
dc_old  <- get_dc(YEAR_OLD, "state") %>% adjust_dollars(CPI_RATIO) %>% derive()

keep_cols <- c("GEOID", "hhE", "pop_totE", "workers_totalE", "workers_transitE", "workers_driveE",
               "time_totalE", "time_totalM", "time_driveE", "time_driveM",
               "time_transitE", "time_transitM", "long60_totalE",
               "earn_totalE", "earn_driveE", "earn_transitE",
               "avg_incE", "avg_incM", "med_incE", "med_incM", "med_rentE", "med_valE",
               "share_ba_plus", "share_renter", "share_recent_hu", "share_white", "share_black", "share_poc",
               "share_hh_0_45k", "share_hh_45_75k", "share_hh_75_125k", "share_hh_125kp",
               "share_out_dc", "share_45plus", "share_60plus_tr", "share_90plus",
               "transit_share", "drive_share", "wfh_share", "transit_penalty", "transit_reliable",
               "earn_ratio_transit_drive", "pct_rank_inc", "pct_rank_hous", "pct_rank_ba", "G_index")

panel_df <- new_ind %>%
  select(all_of(keep_cols)) %>%
  rename_with(~ paste0(.x, "_23"), -GEOID) %>%
  inner_join(
    old_ind %>% select(all_of(keep_cols)) %>% rename_with(~ paste0(.x, "_10"), -GEOID),
    by = "GEOID"
  ) %>%
  filter(workers_totalE_23 >= MIN_TRACT_WORKERS)

# Adjacência Espacial para Fronteira de Contágio (Guerrieri et al., 2013)
panel_sf_temp   <- tracts_geo %>% inner_join(panel_df, by = "GEOID")
touches_list    <- sf::st_touches(panel_sf_temp)
affluent_10_idx <- which(panel_sf_temp$pct_rank_inc_10 >= 0.70 | panel_sf_temp$pct_rank_hous_10 >= 0.70)

panel_df <- panel_df %>%
  mutate(
    # Variações de Mobilidade
    d_time_total     = time_totalE_23 - time_totalE_10,
    d_time_transit   = if_else(transit_reliable_23 & transit_reliable_10,
                               time_transitE_23 - time_transitE_10, NA_real_),
    d_transit_share  = transit_share_23 - transit_share_10,
    d_drive_share    = drive_share_23 - drive_share_10,
    d_wfh_share      = wfh_share_23 - wfh_share_10,
    d_long60         = (share_60plus_tr_23 - share_60plus_tr_10) * 100,
    d_45plus         = share_45plus_23 - share_45plus_10,
    d_share_out_dc   = share_out_dc_23 - share_out_dc_10,
    sig_total        = sig_diff(d_time_total, time_totalM_23, time_totalM_10),
    change_class     = case_when(
      is.na(sig_total)             ~ "No data",
      sig_total & d_time_total > 0 ~ "Significantly longer",
      sig_total & d_time_total < 0 ~ "Significantly shorter",
      TRUE                         ~ "No significant change"
    ),
    
    # Variações de Renda e Gentrificação (Dólares Reais de 2023)
    d_avg_inc        = avg_incE_23 - avg_incE_10,
    pct_avg_inc      = (avg_incE_23 / avg_incE_10) - 1,
    d_med_inc        = med_incE_23 - med_incE_10,
    pct_med_inc      = (med_incE_23 / med_incE_10) - 1,
    d_med_rent       = med_rentE_23 - med_rentE_10,
    d_med_val        = med_valE_23 - med_valE_10,
    d_share_ba       = share_ba_plus_23 - share_ba_plus_10,
    d_pct_hous       = pct_rank_hous_23 - pct_rank_hous_10,
    d_G_index        = G_index_23 - G_index_10,
    
    # Variação nas 4 Faixas de Renda no Trato (em pontos percentuais)
    d_hh_0_45k_pts   = (share_hh_0_45k_23 - share_hh_0_45k_10) * 100,
    d_hh_45_75k_pts  = (share_hh_45_75k_23 - share_hh_45_75k_10) * 100,
    d_hh_75_125k_pts = (share_hh_75_125k_23 - share_hh_75_125k_10) * 100,
    d_hh_125kp_pts   = (share_hh_125kp_23 - share_hh_125kp_10) * 100,
    
    # Classificação de Gentrificação (UDP + Freeman)
    q_renter_10      = ntile(share_renter_10, 5),
    q_poc_10         = ntile(share_poc_10, 5),
    q_lowinc_10      = ntile(-med_incE_10, 5),
    q_noba_10        = ntile(-share_ba_plus_10, 5),
    udp_vuln_score   = q_renter_10 + q_poc_10 + q_lowinc_10 + q_noba_10,
    udp_vulnerable   = udp_vuln_score >= 12 | pct_rank_inc_10 <= 0.50,
    demo_surge       = (d_share_ba > median(d_share_ba, na.rm = TRUE)) |
      (d_med_inc  > median(d_med_inc, na.rm = TRUE)),
    market_surge     = coalesce(d_med_rent > median(d_med_rent, na.rm = TRUE), FALSE) |
      coalesce(d_med_val  > median(d_med_val, na.rm = TRUE), FALSE) |
      (d_pct_hous > 0.05),
    freeman_elig     = med_incE_10 <= quantile(med_incE_10, 0.40, na.rm = TRUE) &
      share_recent_hu_10 <= quantile(share_recent_hu_10, 0.60, na.rm = TRUE),
    freeman_gent     = freeman_elig & d_share_ba > median(d_share_ba, na.rm = TRUE) &
      (coalesce(d_med_rent > 0, FALSE) | coalesce(d_med_val > 0, FALSE))
  )

dyn_idx <- which(panel_df$udp_vulnerable & panel_df$demo_surge & panel_df$market_surge)
borders_frontline <- sapply(touches_list, function(nb) any(nb %in% union(affluent_10_idx, dyn_idx)))

panel_df <- panel_df %>%
  mutate(
    borders_frontline = borders_frontline,
    typology_udp = case_when(
      !udp_vulnerable                              ~ "5. Afluente / Exclusão Contínua",
      udp_vulnerable & demo_surge & market_surge   ~ "1. Gentrificação Dinâmica (Intensa)",
      udp_vulnerable & (demo_surge | market_surge) ~ "2. Gentrificação Precoce (Demografia ou Preço)",
      udp_vulnerable & borders_frontline           ~ "3. Suscetível (Fronteira de Contágio)",
      TRUE                                         ~ "4. Baixa Renda Estagnada"
    ),
    typology_udp = factor(typology_udp, levels = names(pal_udp)),
    gent_dynamic = typology_udp == "1. Gentrificação Dinâmica (Intensa)"
  )

panel <- tracts_geo %>% inner_join(panel_df, by = "GEOID")

# -----------------------------------------------------------------------------
# BLOCO H: Heat Maps de Renda Média, Mediana, 4 Faixas e Modais em DC
# -----------------------------------------------------------------------------
# H1: Heat Map da Renda MÉDIA (Mean Income: B19025 / B11001) 2010 vs 2023
income_mean_long <- panel %>%
  select(GEOID, avg_incE_10, avg_incE_23) %>%
  pivot_longer(c(avg_incE_10, avg_incE_23), names_to = "periodo", values_to = "renda_media") %>%
  mutate(
    periodo = if_else(periodo == "avg_incE_10",
                      paste0("Baseline: ", P_OLD, " (US$ Reais de 2023)"),
                      paste0("Endline: ", P_NEW, " (US$ Reais de 2023)"))
  )

p_h1 <- ggplot(income_mean_long) +
  geom_sf(aes(fill = renda_media / 1000), colour = "white", linewidth = 0.08) +
  facet_wrap(~ periodo, ncol = 2) +
  scale_fill_viridis_c(
    option = "inferno", direction = -1,
    labels = dollar_format(suffix = "k"),
    limits = c(25, 300), oob = squish,
    name   = "Renda Média\nDomiciliar"
  ) +
  labs(
    title    = "Heat Map da Renda Média (Mean Income) por Região em Washington, D.C.",
    subtitle = "Comparação 2006-2010 vs. 2019-2023 em dólares constantes de 2023 (B19025 / B11001)",
    caption  = "Fonte: U.S. Census Bureau, ACS 5-Year. Valores de 2010 ajustados pelo CPI-U e convertidos para malha de 2020."
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, colour = "grey30", margin = margin(b = 8)),
    strip.text    = element_text(face = "bold", size = 11, margin = margin(b = 5, t = 5)),
    legend.position = "right"
  )
save_plot(p_h1, "H1_heatmap_avg_income_2010_vs_2023.png", w = 11, h = 6)

# H2: Heat Map da Renda MEDIANA (Median Income: B19013) 2010 vs 2023
income_med_long <- panel %>%
  select(GEOID, med_incE_10, med_incE_23) %>%
  pivot_longer(c(med_incE_10, med_incE_23), names_to = "periodo", values_to = "renda_mediana") %>%
  mutate(
    periodo = if_else(periodo == "med_incE_10",
                      paste0("Baseline: ", P_OLD, " (US$ Reais de 2023)"),
                      paste0("Endline: ", P_NEW, " (US$ Reais de 2023)"))
  )

p_h2 <- ggplot(income_med_long) +
  geom_sf(aes(fill = renda_mediana / 1000), colour = "white", linewidth = 0.08) +
  facet_wrap(~ periodo, ncol = 2) +
  scale_fill_viridis_c(
    option = "magma", direction = -1,
    labels = dollar_format(suffix = "k"),
    limits = c(20, 220), oob = squish,
    name   = "Renda Mediana\nDomiciliar"
  ) +
  labs(
    title    = "Heat Map da Renda Mediana (Median Income) por Região em Washington, D.C.",
    subtitle = "Expansão dos estratos de alta renda do Noroeste para o Centro, Shaw, H St NE e Navy Yard (2010 vs. 2023)",
    caption  = "Fonte: U.S. Census Bureau, ACS 5-Year (Tabela B19013). Dólares constantes de 2023."
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, colour = "grey30", margin = margin(b = 8)),
    strip.text    = element_text(face = "bold", size = 11, margin = margin(b = 5, t = 5)),
    legend.position = "right"
  )
save_plot(p_h2, "H2_heatmap_median_income_2010_vs_2023.png", w = 11, h = 6)

# H3: Heat Map da Mudança nas 4 Faixas de Renda nos Tratos de DC (2010 -> 2023)
brackets_map_long <- panel %>%
  select(GEOID, d_hh_0_45k_pts, d_hh_45_75k_pts, d_hh_75_125k_pts, d_hh_125kp_pts) %>%
  pivot_longer(
    cols      = c(d_hh_0_45k_pts, d_hh_45_75k_pts, d_hh_75_125k_pts, d_hh_125kp_pts),
    names_to  = "faixa",
    values_to = "delta_pts"
  ) %>%
  mutate(
    faixa = recode(
      faixa,
      d_hh_0_45k_pts   = "1. Faixa Baixa ($0 - $45k)",
      d_hh_45_75k_pts  = "2. Faixa Média-Baixa ($45k - $70k/75k)",
      d_hh_75_125k_pts = "3. Faixa Média-Alta ($70k/75k - $120k/125k)",
      d_hh_125kp_pts   = "4. Faixa Alta (> $120k/125k)"
    )
  )

p_h3 <- ggplot(brackets_map_long) +
  geom_sf(aes(fill = delta_pts), colour = "white", linewidth = 0.08) +
  facet_wrap(~ faixa, ncol = 2) +
  scale_fill_distiller(
    palette = "RdBu", direction = -1,
    limits  = c(-25, 25), oob = squish,
    name    = "Variação\n(pts. perc.)"
  ) +
  labs(
    title    = "Substituição das 4 Faixas de Renda por Trato Censitário em D.C. (2010 a 2023)",
    subtitle = "Azul = retração daquela faixa de renda no bairro | Vermelho = expansão daquela faixa no bairro",
    caption  = "Fonte: ACS 5-Year (Tabela B19001)."
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, colour = "grey30", margin = margin(b = 6)),
    strip.text    = element_text(face = "bold", size = 10, margin = margin(b = 4, t = 4))
  )
save_plot(p_h3, "H3_heatmap_4_income_brackets_shift.png", w = 10, h = 9)

# H4: Mapa Espacial 2x2 da Mudança nos Modais (Espelho Espacial de D4 e D6 nos Tratos de DC)
modes_map_long <- panel %>%
  mutate(
    d_wfh_pts     = d_wfh_share * 100,
    d_transit_pts = d_transit_share * 100,
    d_drive_pts   = d_drive_share * 100,
    d_out_dc_pts  = d_share_out_dc * 100
  ) %>%
  select(GEOID, d_wfh_pts, d_transit_pts, d_drive_pts, d_out_dc_pts) %>%
  pivot_longer(
    cols      = c(d_wfh_pts, d_transit_pts, d_drive_pts, d_out_dc_pts),
    names_to  = "modal_metric",
    values_to = "delta_pts"
  ) %>%
  mutate(
    modal_metric = recode(
      modal_metric,
      d_wfh_pts     = "1. Worked from Home (Variação em pts. perc.)",
      d_transit_pts = "2. Public Transit (Variação em pts. perc.)",
      d_drive_pts   = "3. Drove Alone (Variação em pts. perc.)",
      d_out_dc_pts  = "4. Trabalha Fora de DC / Subúrbios (pts. perc.)"
    )
  )

p_h4 <- ggplot(modes_map_long) +
  geom_sf(aes(fill = delta_pts), colour = "white", linewidth = 0.08) +
  geom_sf(
    data = filter(panel, gent_dynamic %in% TRUE) %>% st_union(),
    fill = NA, colour = "black", linewidth = 0.4
  ) +
  facet_wrap(~ modal_metric, ncol = 2) +
  scale_fill_distiller(
    palette = "RdBu", direction = -1,
    limits  = c(-25, 25), oob = squish,
    name    = "Mudança\n(pts. perc.)"
  ) +
  labs(
    title    = "Geografia da Mudança Modal nos Tratos de DC (Espelho Espacial de D4 e D6)",
    subtitle = paste0("Variação em pontos percentuais (", P_OLD, " a ", P_NEW, "). Contorno preto = Gentrificação Dinâmica"),
    caption  = "Fonte: ACS 5-Year (S0802, B08301, B08007)."
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, colour = "grey30", margin = margin(b = 6)),
    strip.text    = element_text(face = "bold", size = 10, margin = margin(b = 4, t = 4))
  )
save_plot(p_h4, "H4_heatmap_mode_shares_dc_tracts.png", w = 10.5, h = 9)

# -----------------------------------------------------------------------------
# Tabelas, Regressões e Gráficos da PARTE A (Gentrificação x Tempo de Viagem)
# -----------------------------------------------------------------------------
citywide <- tibble(
  mode   = c("All modes", "Drove alone", "Carpooled", "Public transit"),
  min_10 = c(dc_old$time_totalE, dc_old$time_driveE, dc_old$time_carpoolE, dc_old$time_transitE),
  moe_10 = c(dc_old$time_totalM, dc_old$time_driveM, dc_old$time_carpoolM, dc_old$time_transitM),
  min_23 = c(dc_new$time_totalE, dc_new$time_driveE, dc_new$time_carpoolE, dc_new$time_transitE),
  moe_23 = c(dc_new$time_totalM, dc_new$time_driveM, dc_new$time_carpoolM, dc_new$time_transitM)
) %>% mutate(change = min_23 - min_10, significant = sig_diff(change, moe_10, moe_23))

earnings <- tibble(
  mode    = c("All workers", "Drove alone", "Public transit"),
  earn_10 = c(dc_old$earn_totalE, dc_old$earn_driveE, dc_old$earn_transitE),
  earn_23 = c(dc_new$earn_totalE, dc_new$earn_driveE, dc_new$earn_transitE)
)

typology_commute_summary <- panel_df %>%
  group_by(typology_udp) %>%
  summarise(
    tracts                = n(),
    avg_inc_10            = median(avg_incE_10, na.rm = TRUE),
    avg_inc_23            = median(avg_incE_23, na.rm = TRUE),
    med_inc_10            = median(med_incE_10, na.rm = TRUE),
    med_inc_23            = median(med_incE_23, na.rm = TRUE),
    chg_hh_0_45k_pts      = median(d_hh_0_45k_pts, na.rm = TRUE),
    chg_hh_125kp_pts      = median(d_hh_125kp_pts, na.rm = TRUE),
    chg_commute_min       = median(d_time_total, na.rm = TRUE),
    pct_sig_longer        = mean(change_class == "Significantly longer", na.rm = TRUE),
    chg_transit_min       = median(d_time_transit, na.rm = TRUE),
    transit_share_23      = median(transit_share_23, na.rm = TRUE),
    chg_transit_share     = median(d_transit_share, na.rm = TRUE),
    chg_wfh_pts           = median(d_wfh_share, na.rm = TRUE) * 100,
    transit_penalty_23    = median(transit_penalty_23[transit_reliable_23 %in% TRUE], na.rm = TRUE),
    chg_45plus_pts        = median(d_45plus, na.rm = TRUE) * 100,
    chg_60plus_pts        = median(d_long60, na.rm = TRUE),
    chg_share_work_out_dc = median(d_share_out_dc, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

cat("\n=== A. DC-WIDE MEAN COMMUTE BY MODE (minutes, residents) ===\n"); print(citywide)
cat("\n=== A. COMMUTE CHANGE BY UDP GENTRIFICATION TYPOLOGY ===\n"); print(as.data.frame(typology_commute_summary))

eligible_df <- panel_df %>% filter(udp_vulnerable)
mod_total   <- lm(d_time_total ~ gent_dynamic + d_G_index + time_totalE_10 + transit_share_10 + d_transit_share,
                  data = eligible_df, weights = workers_totalE_23)
mod_long    <- lm(d_45plus ~ gent_dynamic + d_G_index + share_45plus_10 + transit_share_10 + d_share_out_dc,
                  data = eligible_df, weights = workers_totalE_23)
mod_penalty <- lm(transit_penalty_23 ~ gent_dynamic + d_G_index + transit_share_23 + share_out_dc_23,
                  data = eligible_df %>% filter(transit_reliable_23 %in% TRUE),
                  weights = workers_transitE_23)

tract_models <- bind_rows(
  tidy(mod_total, conf.int = TRUE)   %>% mutate(model = "A1: change in mean commute (min)"),
  tidy(mod_long, conf.int = TRUE)    %>% mutate(model = "A2: change in share commuting 45+ min"),
  tidy(mod_penalty, conf.int = TRUE) %>% mutate(model = "A3: transit time penalty 2023")
) %>% relocate(model)

results[c("A_citywide_by_mode", "A_earnings_by_mode", "A_typology_commute_summary", "A_tract_models", "A_tract_panel")] <-
  list(citywide, earnings, typology_commute_summary, tract_models, panel_df)

p_a1 <- citywide %>%
  pivot_longer(c(min_10, min_23), names_to = "period", values_to = "minutes") %>%
  mutate(moe = if_else(period == "min_10", moe_10, moe_23),
         period = if_else(period == "min_10", P_OLD, P_NEW),
         mode = factor(mode, levels = citywide$mode)) %>%
  ggplot(aes(mode, minutes, fill = period)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  geom_errorbar(aes(ymin = minutes - moe, ymax = minutes + moe), position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = pal_years, name = NULL) +
  labs(title = "DC residents: mean one-way commute by mode", x = NULL, y = "Minutes",
       caption = "Source: ACS 5-year (S0802 & B08013/B08303). Error bars = 90% MOE.") +
  theme_minimal() + theme(legend.position = "top")
save_plot(p_a1, "A1_commute_by_mode.png")

p_a2 <- earnings %>%
  pivot_longer(-mode, names_to = "period", values_to = "earn") %>%
  mutate(period = if_else(period == "earn_10", P_OLD, P_NEW), mode = factor(mode, levels = earnings$mode)) %>%
  ggplot(aes(mode, earn, fill = period)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = pal_years, name = NULL) +
  labs(title = "Transit commuters earn less than solo drivers",
       subtitle = "Median earnings by commute mode, DC residents (2023 real dollars)", x = NULL, y = NULL,
       caption = paste0("Source: ACS 5-year (S0802). ", P_OLD, " values CPI-adjusted.")) +
  theme_minimal() + theme(legend.position = "top")
save_plot(p_a2, "A2_earnings_by_mode.png")

p_a3 <- panel_df %>%
  pivot_longer(c(d_time_total, d_45plus, d_share_out_dc), names_to = "measure", values_to = "value") %>%
  mutate(
    value   = if_else(measure == "d_time_total", value, value * 100),
    measure = recode(measure,
                     d_time_total   = "1. Mean commute (minutes)",
                     d_45plus       = "2. Share commuting 45+ min (pct. pts)",
                     d_share_out_dc = "3. Share working outside DC (pct. pts)")
  ) %>%
  ggplot(aes(typology_udp, value, fill = typology_udp)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.35, size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  facet_wrap(~ measure, scales = "free_y") +
  scale_fill_manual(values = pal_udp) +
  scale_x_discrete(labels = function(x) str_wrap(x, 14)) +
  labs(
    title    = paste0("Change in Mobility by Gentrification Stage (UDP), ", P_OLD, " to ", P_NEW),
    x = NULL, y = "Change", caption = "Source: ACS 5-year (B08013, B08303, B08007)."
  ) +
  theme_minimal() + theme(legend.position = "none", axis.text.x = element_text(size = 8))
save_plot(p_a3, "A3_commute_change_by_udp_typology.png", w = 12, h = 5.5)

p_a4 <- ggplot(panel) +
  geom_sf(aes(fill = d_time_total), colour = "white", linewidth = 0.1) +
  geom_sf(data = filter(panel, gent_dynamic %in% TRUE), fill = NA, colour = "#b91c1c", linewidth = 0.45) +
  scale_fill_distiller(palette = "RdBu", direction = -1, na.value = "#e5e7eb",
                       name = "Change\n(minutes)", limits = c(-12, 12), oob = squish) +
  labs(
    title    = "Where DC residents' commutes got longer vs. Dynamic Gentrification",
    subtitle = paste0("Change in mean travel time (", P_OLD, " to ", P_NEW, "). Red outline = Dynamic Gentrification tracts"),
    caption  = "Source: ACS 5-year (B08013/B08303)."
  ) +
  theme_void()
save_plot(p_a4, "A4_map_commute_change_with_gent.png", w = 8, h = 7.5)

p_a5 <- panel_df %>%
  filter(udp_vulnerable) %>%
  ggplot(aes(x = d_G_index, y = d_time_total, colour = typology_udp)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = abs(d_hh_0_45k_pts)), alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "lm", colour = "black", linetype = "solid", se = TRUE) +
  scale_colour_manual(values = pal_udp, name = "Estágio UDP") +
  scale_size_continuous(name = "Variação absoluta\nna faixa $0-$45k (pts)") +
  labs(
    title    = "Índice de Gentrificação (Delta G) vs. Tempo de Deslocamento nos Tratos Vulneráveis",
    subtitle = "A substituição demográfica por famílias >$120k encurta o tempo médio nos tratos centrais gentrificados",
    x        = expression("Intensidade da Gentrificação no Trato " * (Delta * G[i])),
    y        = "Mudança no Tempo Médio de Deslocamento (minutos)",
    caption  = "Fonte: ACS 5-Year."
  ) +
  theme_minimal() + theme(legend.position = "right")
save_plot(p_a5, "A5_scatter_dG_vs_commute_composition.png", w = 10, h = 6)

p_a6 <- ggplot(panel) +
  geom_sf(aes(fill = if_else(transit_reliable_23 %in% TRUE, transit_penalty_23, NA_real_)),
          colour = "white", linewidth = 0.1) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "#e5e7eb", name = "Extra minutes\nby transit") +
  labs(title = "The transit time penalty, 2019-2023",
       subtitle = "Mean transit commute minus mean drive-alone commute\nGrey = too few commuters or unreliable",
       caption = "Source: ACS 5-year, Table S0802.") +
  theme_void()
save_plot(p_a6, "A6_map_transit_penalty.png", w = 7.5, h = 7.5)


# =============================================================================
# PARTE B - Comparação Regional de Condados (5-Year + Série Anual 1-Year)
# =============================================================================
REG_VARS <- c(agg_time = "B08013_001", notwfh = "B08303_001", t45 = "B08303_011",
              t60 = "B08303_012", t90 = "B08303_013", tot_work = "B08007_001",
              out_state = "B08007_005", workers_mode = "B08301_001", transit = "B08301_010",
              wfh = "B08301_021", med_inc = "B19013_001")

get_region <- function(year, label, survey = "acs5") {
  cpi_mult <- if (as.character(year) %in% names(CPI)) CPI[["2023"]] / CPI[[as.character(year)]] else 1
  get_acs(geography = "county", state = c("DC", "MD", "VA"), year = year, survey = survey,
          variables = REG_VARS, output = "wide", progress_bar = FALSE) %>%
    filter(GEOID %in% REGION$GEOID) %>%
    mutate(
      period           = label,
      year_num         = year,
      mean_time        = agg_timeE / notwfhE,
      mean_time_moe    = moe_ratio_(agg_timeE, notwfhE, agg_timeM, notwfhM),
      share_60plus     = (t60E + t90E) / notwfhE,
      share_60plus_moe = moe_prop_(t60E + t90E, notwfhE, sqrt(t60M^2 + t90M^2), notwfhM),
      share_out_state  = out_stateE / tot_workE,
      share_transit    = transitE / workers_modeE,
      share_wfh        = wfhE / workers_modeE,
      med_inc_2023usd  = med_incE * cpi_mult
    ) %>%
    select(GEOID, period, year_num, mean_time, mean_time_moe, share_60plus, share_60plus_moe,
           share_out_state, share_transit, share_wfh, med_inc_2023usd,
           workers = tot_workE, notwfh = notwfhE, agg_time = agg_timeE)
}

region <- bind_rows(get_region(YEAR_OLD, P_OLD, "acs5"), get_region(YEAR_NEW, P_NEW, "acs5")) %>%
  left_join(REGION, by = "GEOID")

region_change <- region %>%
  pivot_wider(id_cols = c(GEOID, county, ring), names_from = period,
              values_from = c(mean_time, mean_time_moe, share_60plus, share_60plus_moe,
                              share_out_state, share_transit, share_wfh, med_inc_2023usd)) %>%
  rename_with(~ str_replace(.x, fixed(P_OLD), "10") %>% str_replace(fixed(P_NEW), "23")) %>%
  mutate(
    d_mean_time     = mean_time_23 - mean_time_10,
    sig_time        = sig_diff(d_mean_time, mean_time_moe_10, mean_time_moe_23),
    d_60plus_pts    = (share_60plus_23 - share_60plus_10) * 100,
    sig_60plus      = sig_diff(share_60plus_23 - share_60plus_10, share_60plus_moe_10, share_60plus_moe_23),
    d_out_state_pts = (share_out_state_23 - share_out_state_10) * 100,
    d_wfh_pts       = (share_wfh_23 - share_wfh_10) * 100,
    pct_chg_med_inc = med_inc_2023usd_23 / med_inc_2023usd_10 - 1
  ) %>%
  arrange(ring, desc(mean_time_23))

cat("\n=== B. REGIONAL COUNTIES: COMMUTE TIME CHANGE (residents) ===\n")
print(as.data.frame(region_change %>% select(county, ring, mean_time_10, mean_time_23, d_mean_time,
                                             sig_time, d_60plus_pts, d_out_state_pts, d_wfh_pts)))

p_b1 <- region_change %>%
  mutate(county = reorder(county, mean_time_23)) %>%
  ggplot(aes(y = county)) +
  geom_segment(aes(x = mean_time_10, xend = mean_time_23, yend = county), colour = "grey70", linewidth = 1) +
  geom_point(aes(x = mean_time_10, colour = P_OLD), size = 2.8) +
  geom_point(aes(x = mean_time_23, colour = P_NEW, shape = sig_time), size = 2.8) +
  scale_colour_manual(values = pal_years, name = NULL) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), name = "Change significant (90%)") +
  facet_grid(ring ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Mean commute time of residents, DC region",
       x = "Minutes (workers who did not work from home)", y = NULL,
       caption = "Source: ACS 5-year, B08013 / B08303.") +
  theme_minimal() + theme(legend.position = "top")
save_plot(p_b1, "B1_region_commute_dumbbell.png", w = 9, h = 7)

p_b2 <- ggplot(region_change, aes(share_out_state_23 * 100, d_mean_time, colour = ring)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = share_60plus_23 * 100)) +
  geom_text(aes(label = str_remove(county, ",.*")), size = 2.8, vjust = -1, show.legend = FALSE) +
  scale_size_continuous(name = "% commuting 60+ min\n(2019-2023)") +
  labs(title = "Out-of-state commuting and change in commute time",
       x = "% of resident workers working outside their state, 2019-2023 (mostly into DC)",
       y = "Change in mean commute (minutes)", colour = NULL,
       caption = "Source: ACS 5-year, B08007, B08013, B08303.") +
  theme_minimal()
save_plot(p_b2, "B2_region_outstate_vs_change.png", w = 9, h = 6)

region_1yr <- bind_rows(lapply(YEARS_1YR_REGION, function(y) {
  tryCatch(get_region(y, as.character(y), "acs1"), error = function(e) NULL)
})) %>%
  left_join(REGION, by = "GEOID")

if (nrow(region_1yr) > 0) {
  ring_annual <- region_1yr %>%
    group_by(year_num, ring) %>%
    summarise(
      mean_time    = sum(agg_time, na.rm = TRUE) / sum(notwfh, na.rm = TRUE),
      share_60plus = sum(share_60plus * notwfh, na.rm = TRUE) / sum(notwfh, na.rm = TRUE) * 100,
      share_wfh    = sum(share_wfh * workers, na.rm = TRUE) / sum(workers, na.rm = TRUE) * 100,
      .groups = "drop"
    )
  results[["B_ring_annual_1yr"]] <- ring_annual
  
  p_b3 <- ring_annual %>%
    pivot_longer(c(mean_time, share_60plus, share_wfh), names_to = "metric", values_to = "val") %>%
    mutate(metric = recode(metric,
                           mean_time    = "1. Mean Commute Time (min)",
                           share_60plus = "2. Long Commutes >= 60 min (%)",
                           share_wfh    = "3. Worked from Home (%)")) %>%
    ggplot(aes(year_num, val, colour = ring)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ metric, scales = "free_y") +
    scale_colour_manual(values = c("Core (DC)" = "#1d4ed8", "Inner suburbs" = "#ea580c", "Outer suburbs" = "#b91c1c"),
                        name = "Ring") +
    labs(
      title    = "Annual Commute Trajectory: DC Core vs. Inner & Outer Suburbs (2010-2023)",
      subtitle = "ACS 1-Year series (counties 65,000+)",
      x = "Year (ACS 1-Year)", y = NULL,
      caption = "Source: U.S. Census Bureau, ACS 1-Year (2010-2023, except 2020)."
    ) +
    theme_minimal() + theme(legend.position = "top")
  save_plot(p_b3, "B3_region_annual_1yr_trajectory.png", w = 12, h = 5)
}

results[c("B_region_long", "B_region_change")] <- list(region, region_change)


# =============================================================================
# PARTE C - DC como Local de Trabalho (S0804) vs. Local de Residência (S0802)
# =============================================================================
get_dc_workplace <- function(year) {
  v <- build_subject_vars(year, "S0804", "acs5", strict = FALSE)
  tryCatch(get_acs(geography = "county", state = "DC", year = year, survey = "acs5",
                   variables = v, output = "wide", progress_bar = FALSE) %>% filter(GEOID == "11001"),
           error = function(e) { message("S0804 indisponível para ", year); NULL })
}
wp_old <- get_dc_workplace(YEAR_OLD)
wp_new <- get_dc_workplace(YEAR_NEW)

if (!is.null(wp_old) && !is.null(wp_new)) {
  pick <- function(df, who, period) {
    tibble(who = who, period = period,
           mode    = c("All modes", "Drove alone", "Carpooled", "Public transit"),
           minutes = c(df$time_totalE, df$time_driveE, df$time_carpoolE, df$time_transitE),
           moe     = c(df$time_totalM, df$time_driveM, df$time_carpoolM, df$time_transitM),
           workers = c(df$workers_totalE, df$workers_driveE, df$workers_carpoolE, df$workers_transitE))
  }
  live_vs_work <- bind_rows(pick(dc_old, "Live in DC", P_OLD), pick(dc_new, "Live in DC", P_NEW),
                            pick(wp_old, "Work in DC", P_OLD), pick(wp_new, "Work in DC", P_NEW))
  results[["C_live_vs_work_in_dc"]] <- live_vs_work
  
  p_c1 <- live_vs_work %>%
    mutate(mode = factor(mode, levels = unique(mode))) %>%
    ggplot(aes(mode, minutes, fill = period)) +
    geom_col(position = position_dodge(0.8), width = 0.75) +
    geom_errorbar(aes(ymin = minutes - moe, ymax = minutes + moe), position = position_dodge(0.8), width = 0.2) +
    facet_wrap(~ who) +
    scale_fill_manual(values = pal_years, name = NULL) +
    labs(title = "People who WORK in DC commute longer than people who LIVE in DC",
         subtitle = "Mean one-way commute by mode; 'Work in DC' includes MD/VA residents",
         x = NULL, y = "Minutes", caption = "Source: ACS 5-year, S0802 (residence) and S0804 (workplace).") +
    theme_minimal() + theme(legend.position = "top", axis.text.x = element_text(angle = 20, hjust = 1))
  save_plot(p_c1, "C1_live_vs_work_in_dc.png", w = 10, h = 5)
}


# =============================================================================
# PARTE D - Microdados PUMS Padronizados nas 4 Faixas de Renda:
#           ($0-$45k, $45k-$70k, $70k-$120k, > $120k)
#           Inclui D1 a D8 (com os layouts exatos de D4, D6 e D8 nas 4 faixas)
# =============================================================================
FIPS <- c(DC = 11L, MD = 24L, VA = 51L, WV = 54L, PA = 42L, DE = 10L)

pow_vars <- function(year, survey) {
  if (survey == "acs5" && year %in% 2012:2015) c("POWSP05", "POWSP12") else "POWSP"
}
pums_vars <- function(year, survey) {
  c(pow_vars(year, survey), "JWMNP", if (year >= 2019) "JWTRNS" else "JWTR", "JWRIP",
    "WAGP", "ADJINC", "AGEP", "SEX", "RAC1P", "HISP")
}

pow_code <- function(df) {
  v <- intersect(c("POWSP", "POWSP12", "POWSP05"), names(df))
  if (!length(v)) stop("No place-of-work variable (POWSP) in the PUMS data")
  out <- rep(NA_integer_, nrow(df))
  for (nm in v) {
    x <- suppressWarnings(as.integer(df[[nm]]))
    x[!is.na(x) & x == 0L] <- NA_integer_
    out <- dplyr::coalesce(out, x)
  }
  out
}

read_pums_csv <- function(year, survey, st, vars) {
  folder <- if (survey == "acs5") "5-Year" else "1-Year"
  zip <- file.path(CACHE_DIR, sprintf("csv_p%s_%s_%d.zip", tolower(st), survey, year))
  if (!file.exists(zip)) {
    url <- sprintf("https://www2.census.gov/programs-surveys/acs/data/pums/%d/%s/csv_p%s.zip",
                   year, folder, tolower(st))
    message("  downloading ", url)
    download.file(url, zip, mode = "wb", quiet = TRUE)
  }
  csv <- grep("\\.csv$", unzip(zip, list = TRUE)$Name, value = TRUE)
  exdir <- file.path(CACHE_DIR, "unzipped")
  unzip(zip, files = csv, exdir = exdir, overwrite = TRUE)
  keep <- c("SERIALNO", "SPORDER", "ST", "PWGTP", vars)
  bind_rows(lapply(file.path(exdir, csv), function(f)
    read_csv(f, col_types = cols(.default = col_character()), progress = FALSE,
             col_select = c(any_of(c(keep, tolower(keep))),
                            matches("^pwgtp[0-9]+$", ignore.case = TRUE)))))
}

api_get <- function(st, year, survey, vars, vf) {
  tryCatch(get_pums(variables = vars, state = st, year = year, survey = survey,
                    variables_filter = vf, rep_weights = "person", show_call = FALSE),
           error = function(e) { message("  get_pums (", st, ") failed: ", conditionMessage(e)); NULL })
}
as_chr_df <- function(x) as.data.frame(x) %>% mutate(across(everything(), as.character))

fetch_state <- function(st, year, survey, pow_dc_only) {
  vars <- pums_vars(year, survey)
  df <- NULL
  if (!pow_dc_only) {
    df <- api_get(st, year, survey, vars, NULL)
  } else {
    parts <- list()
    for (pv in pow_vars(year, survey)) {
      got <- NULL
      for (code in c("011", "11")) {
        got <- api_get(st, year, survey, vars, setNames(list(code), pv))
        if (!is.null(got) && nrow(got) > 0) break
      }
      if (is.null(got)) { parts <- NULL; break }
      parts[[pv]] <- as_chr_df(got)
    }
    if (!is.null(parts)) df <- bind_rows(parts)
  }
  if (is.null(df)) {
    message("  falling back to Census FTP CSV file for ", st, " ", survey, " ", year)
    df <- read_pums_csv(year, survey, st, vars)
  }
  df <- as_chr_df(df)
  names(df) <- toupper(names(df))
  if (pow_dc_only) df <- df[which(pow_code(df) == 11L), , drop = FALSE]
  df$ST_REQ <- as.character(FIPS[[st]])
  df
}

fetch_pums <- function(year, survey, states, pow_dc_only) {
  cache <- file.path(CACHE_DIR, sprintf("pumsv4_%s_%d_%s_%s.rds", survey, year,
                                        paste(states, collapse = ""), if (pow_dc_only) "powdc" else "all"))
  if (file.exists(cache)) return(readRDS(cache))
  df <- bind_rows(lapply(states, fetch_state, year = year, survey = survey, pow_dc_only = pow_dc_only))
  saveRDS(df, cache)
  df
}

harmonize <- function(df, year, label) {
  mode_var <- if ("JWTRNS" %in% names(df)) "JWTRNS" else "JWTR"
  num <- function(x) suppressWarnings(as.numeric(x))
  adj <- num(df$ADJINC)
  if (isTRUE(median(adj, na.rm = TRUE) > 100)) adj <- adj / 1e6
  rep_cols <- grep("^PWGTP[0-9]*$", names(df), value = TRUE)
  df$st_ <- dplyr::coalesce(as.integer(num(df$ST_REQ)),
                            if ("ST" %in% names(df)) as.integer(num(df$ST)) else NA_integer_)
  df$pow_ <- pow_code(df)
  df$adj_ <- adj
  df %>%
    mutate(across(all_of(rep_cols), num),
           period    = label, end_year = year,
           st        = st_, pow = pow_,
           mode_code = as.integer(num(.data[[mode_var]])),
           rip       = as.integer(num(JWRIP)),
           travel    = num(JWMNP), travel = if_else(travel > 0, travel, NA_real_),
           wage_end  = num(WAGP) * adj_,
           wage_2023 = wage_end * CPI[["2023"]] / CPI[[as.character(year)]]) %>%
    filter(!is.na(pow), pow > 0, mode_code %in% 1:12) %>%
    mutate(
      res_state = case_when(st == 11 ~ "DC", st == 24 ~ "Maryland", st == 51 ~ "Virginia"),
      flow      = case_when(
        pow == 11 & st == 11 ~ "Live DC, work DC",
        pow == 11 & st == 24 ~ "Live MD, work DC",
        pow == 11 & st == 51 ~ "Live VA, work DC",
        st == 11             ~ "Live DC, work outside DC"
      ),
      # AS 4 FAIXAS DE RENDA ($0-$45k, $45k-$70k, $70k-$120k, > $120k)
      wage_band_real = cut(if_else(wage_2023 > 0, wage_2023, NA_real_), WAGE_BREAKS, WAGE_LABELS),
      wage_band_nom  = cut(if_else(wage_end  > 0, wage_end,  NA_real_), WAGE_BREAKS, WAGE_LABELS),
      wage_band      = factor(wage_band_real, levels = WAGE_LABELS),
      wage_band_nom  = factor(wage_band_nom,  levels = WAGE_LABELS),
      mode_grp       = case_when(
        mode_code == 1 & rip == 1 ~ "Drove alone",
        mode_code == 1            ~ "Carpool",
        mode_code %in% 2:6        ~ "Transit",
        mode_code %in% 9:10       ~ "Walk/bike",
        mode_code == 11           ~ "Worked from home",
        TRUE                      ~ "Other"
      ),
      mode_grp = factor(mode_grp, levels = MODE_LEVELS_D4),
      wfh      = as.numeric(mode_code == 11),
      transit  = if_else(mode_code == 11, NA_real_, as.numeric(mode_code %in% 2:6)),
      long45   = as.numeric(travel >= 45),
      long60   = as.numeric(travel >= 60),
      age_grp  = cut(num(AGEP), c(0, 29, 54, Inf), c("29 or younger", "30-54", "55 or older")),
      sex      = if_else(num(SEX) == 1, "Male", "Female"),
      race_eth = case_when(
        num(HISP) > 1   ~ "Hispanic",
        num(RAC1P) == 1 ~ "White, non-Hispanic",
        num(RAC1P) == 2 ~ "Black, non-Hispanic",
        num(RAC1P) == 6 ~ "Asian, non-Hispanic",
        TRUE            ~ "Other, non-Hispanic"
      ),
      one = 1
    ) %>%
    select(-st_, -pow_, -adj_) %>%
    filter(!is.na(flow)) %>%
    mutate(flow = factor(flow, levels = c("Live DC, work DC", "Live MD, work DC",
                                          "Live VA, work DC", "Live DC, work outside DC")))
}

load_period <- function(year, survey, label) {
  message(sprintf("PUMS %s %d ...", survey, year))
  incommuters  <- fetch_pums(year, survey, RES_STATES, pow_dc_only = TRUE)
  dc_residents <- fetch_pums(year, survey, "DC", pow_dc_only = FALSE)
  bind_rows(incommuters, dc_residents) %>%
    distinct(SERIALNO, SPORDER, .keep_all = TRUE) %>%
    harmonize(year, label)
}

make_design <- function(df) {
  svrepdesign(data = df, weights = ~PWGTP, repweights = "PWGTP[0-9]+",
              type = "successive-difference", mse = TRUE)
}

est_group <- function(des, by, label = NULL) {
  keep <- stats::complete.cases(des$variables[, by, drop = FALSE])
  d <- des[keep, ]
  f_by <- reformulate(by)
  one_measure <- function(var, FUN = svymean) {
    r <- svyby(as.formula(paste0("~", var)), f_by, d, FUN, na.rm = TRUE, na.rm.all = TRUE, keep.var = TRUE)
    r <- as.data.frame(r)
    names(r)[names(r) == "se"] <- paste0(var, "_se")
    r
  }
  n <- d$variables %>% count(across(all_of(by)), name = "n_sample")
  out <- n %>%
    left_join(one_measure("one", svytotal) %>% rename(workers = one, workers_se = one_se), by = by) %>%
    left_join(one_measure("travel")  %>% rename(mean_min = travel, mean_min_se = travel_se), by = by) %>%
    left_join(one_measure("long45")  %>% rename(share_45plus = long45, share_45plus_se = long45_se), by = by) %>%
    left_join(one_measure("long60")  %>% rename(share_60plus = long60, share_60plus_se = long60_se), by = by) %>%
    left_join(one_measure("transit") %>% rename(share_transit = transit, share_transit_se = transit_se), by = by) %>%
    left_join(one_measure("wfh")     %>% rename(share_wfh = wfh, share_wfh_se = wfh_se), by = by) %>%
    mutate(
      mean_min_lo90 = mean_min - 1.645 * mean_min_se,
      mean_min_hi90 = mean_min + 1.645 * mean_min_se,
      small_sample  = n_sample < MIN_PUMS_N
    )
  if (!is.null(label)) out <- mutate(out, period = label, .before = 1)
  out
}

change_table <- function(est_old, est_new, by) {
  cols <- c(by, "n_sample", "workers", "mean_min", "mean_min_se", "share_60plus", "share_60plus_se",
            "share_transit", "share_wfh")
  inner_join(est_old[cols], est_new[cols], by = by, suffix = c("_old", "_new")) %>%
    mutate(
      d_min           = mean_min_new - mean_min_old,
      se_d            = sqrt(mean_min_se_old^2 + mean_min_se_new^2),
      d_lo90          = d_min - 1.645 * se_d,
      d_hi90          = d_min + 1.645 * se_d,
      significant_90  = abs(d_min / se_d) > 1.645,
      pct_chg_workers = workers_new / workers_old - 1,
      d_60plus_pts    = (share_60plus_new - share_60plus_old) * 100,
      sig_60plus      = abs((share_60plus_new - share_60plus_old) /
                              sqrt(share_60plus_se_old^2 + share_60plus_se_new^2)) > 1.645,
      d_transit_pts   = (share_transit_new - share_transit_old) * 100,
      d_wfh_pts       = (share_wfh_new - share_wfh_old) * 100,
      small_sample    = pmin(n_sample_old, n_sample_new) < MIN_PUMS_N
    )
}

pums_old <- load_period(YEAR_OLD, "acs5", P_OLD)
pums_new <- load_period(YEAR_NEW, "acs5", P_NEW)
des_old  <- make_design(pums_old)
des_new  <- make_design(pums_new)

BREAKDOWNS <- list(
  wage_only      = "wage_band",
  flow           = "flow",
  flow_wage      = c("flow", "wage_band"),
  flow_wage_nom  = c("flow", "wage_band_nom"),
  flow_mode      = c("flow", "mode_grp"),
  flow_wage_mode = c("flow", "wage_band", "mode_grp"),
  flow_age       = c("flow", "age_grp"),
  flow_sex       = c("flow", "sex"),
  flow_race      = c("flow", "race_eth"),
  flow_wage_race = c("flow", "wage_band", "race_eth")
)

pums_est <- list(); pums_chg <- list()
for (nm in names(BREAKDOWNS)) {
  message("  estimating by ", nm)
  by <- BREAKDOWNS[[nm]]
  e_old <- est_group(des_old, by, P_OLD)
  e_new <- est_group(des_new, by, P_NEW)
  pums_est[[nm]] <- bind_rows(e_old, e_new)
  pums_chg[[nm]] <- change_table(e_old, e_new, by)
}

# ---- Cálculo Matemático de D4 (Quotas Modais e Mudança com SE nas 4 Faixas de Renda)
d4_mode_shares_est <- pums_est$flow_wage_mode %>%
  group_by(period, flow, wage_band) %>%
  mutate(
    total_workers_cell = sum(workers, na.rm = TRUE),
    share              = workers / total_workers_cell,
    share_se           = workers_se / total_workers_cell
  ) %>%
  ungroup()

d4_mode_shares_chg <- d4_mode_shares_est %>%
  select(period, flow, wage_band, mode_grp, workers, share, share_se) %>%
  pivot_wider(names_from = period, values_from = c(workers, share, share_se)) %>%
  rename_with(~ str_replace(.x, fixed(P_OLD), "old") %>% str_replace(fixed(P_NEW), "new")) %>%
  mutate(
    d_share_pts    = (share_new - share_old) * 100,
    se_d_pts       = sqrt(share_se_old^2 + share_se_new^2) * 100,
    significant_90 = abs(d_share_pts / se_d_pts) > 1.645
  )

# ---- Cálculo Matemático de D6 (Estado de Residência de Quem Trabalha em DC nas 4 Faixas)
d_old_pow11 <- des_old[des_old$variables$pow == 11 & !is.na(des_old$variables$res_state), ]
d_new_pow11 <- des_new[des_new$variables$pow == 11 & !is.na(des_new$variables$res_state), ]

residence_shares <- bind_rows(
  est_group(d_old_pow11, c("wage_band", "res_state"), P_OLD),
  est_group(d_new_pow11, c("wage_band", "res_state"), P_NEW)
) %>%
  group_by(period, wage_band) %>%
  mutate(
    total_dc_jobs    = sum(workers, na.rm = TRUE),
    share_of_dc_jobs = workers / total_dc_jobs,
    share_se         = workers_se / total_dc_jobs
  ) %>%
  ungroup()

residence_shares_chg <- residence_shares %>%
  select(period, wage_band, res_state, workers, share_of_dc_jobs, share_se) %>%
  pivot_wider(names_from = period, values_from = c(workers, share_of_dc_jobs, share_se)) %>%
  rename_with(~ str_replace(.x, fixed(P_OLD), "old") %>% str_replace(fixed(P_NEW), "new")) %>%
  mutate(
    d_share_pts    = (share_of_dc_jobs_new - share_of_dc_jobs_old) * 100,
    se_d_pts       = sqrt(share_se_old^2 + share_se_new^2) * 100,
    significant_90 = abs(d_share_pts / se_d_pts) > 1.645
  )

cat("\n=== D. PUMS: MEAN COMMUTE BY FLOW AND THE 4 WAGE BANDS ===\n")
print(as.data.frame(pums_chg$flow_wage %>%
                      select(flow, wage_band, n_sample_old, n_sample_new, mean_min_old, mean_min_new,
                             d_min, significant_90, d_60plus_pts, d_wfh_pts) %>%
                      mutate(across(where(is.numeric), ~ round(.x, 2)))))

cat("\n=== D4. MODE SHARE CHANGES ACROSS THE 4 WAGE BANDS (PCT. POINTS) ===\n")
print(as.data.frame(d4_mode_shares_chg %>%
                      filter(mode_grp %in% c("Worked from home", "Transit", "Drove alone")) %>%
                      select(flow, wage_band, mode_grp, share_old, share_new, d_share_pts, significant_90) %>%
                      mutate(across(where(is.numeric), ~ round(.x, 3)))))

cat("\n=== D6. WHERE DC WORKERS LIVE (HOME STATE SHARE CHANGES ACROSS 4 WAGE BANDS) ===\n")
print(as.data.frame(residence_shares_chg %>%
                      select(wage_band, res_state, share_of_dc_jobs_old, share_of_dc_jobs_new, d_share_pts, significant_90) %>%
                      mutate(across(where(is.numeric), ~ round(.x, 3)))))

# ---- Difference-in-Differences (DiD) nas 4 Faixas de Renda (Referência: High-income > $120k)
pooled <- bind_rows(pums_old, pums_new) %>%
  filter(pow == 11, !is.na(travel), !is.na(wage_band)) %>%
  mutate(
    post            = as.numeric(period == P_NEW),
    live_outside_dc = as.numeric(st != 11),
    wage_4ref       = relevel(factor(wage_band), ref = "High-income (> $120k)")
  )
des_pooled <- make_design(pooled)

did1 <- svyglm(travel ~ post * live_outside_dc * wage_4ref + age_grp + sex + race_eth, design = des_pooled)
did2 <- update(did1, . ~ . + mode_grp)
did3 <- svyglm(long60 ~ post * live_outside_dc * wage_4ref + age_grp + sex + race_eth, design = des_pooled)

did_models <- bind_rows(
  tidy(did1, conf.int = TRUE, conf.level = 0.90) %>% mutate(model = "D1: minutes"),
  tidy(did2, conf.int = TRUE, conf.level = 0.90) %>% mutate(model = "D2: minutes, mode held constant"),
  tidy(did3, conf.int = TRUE, conf.level = 0.90) %>% mutate(model = "D3: P(commute >= 60 min)")
) %>% relocate(model)

results <- c(
  results,
  setNames(pums_est, paste0("D_est_", names(pums_est))),
  setNames(pums_chg, paste0("D_change_", names(pums_chg))),
  list(
    D4_mode_shares_est   = d4_mode_shares_est,
    D4_mode_shares_chg   = d4_mode_shares_chg,
    D6_residence_est     = residence_shares,
    D6_residence_chg     = residence_shares_chg,
    D7_did_models_4bands = did_models
  )
)

# -----------------------------------------------------------------------------
# Gráficos da PARTE D (Todos utilizando as 4 Faixas de Renda)
# -----------------------------------------------------------------------------
flows_dc <- c("Live DC, work DC", "Live MD, work DC", "Live VA, work DC", "Live DC, work outside DC")
cap_pums <- "Source: ACS 5-year PUMS (2023 constant dollars). Bars/whiskers = 90% CI from replicate weights. Hollow = n < 50."

# D0: Comparação exclusiva entre as 4 Faixas de Renda
p_d0 <- pums_est$wage_only %>%
  transmute(
    period, wage_band,
    `1. Mean Commute Time (min)`      = mean_min,
    `2. Share Commuting >= 60 min (%)`= share_60plus * 100,
    `3. Worked from Home (%)`         = share_wfh * 100
  ) %>%
  pivot_longer(-c(period, wage_band), names_to = "metric", values_to = "val") %>%
  ggplot(aes(wage_band, val, fill = period)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_fill_manual(values = pal_years, name = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, 12)) +
  labs(
    title    = "Direct Comparison Across the 4 Income Bands ($0-45k, $45k-70k, $70k-120k, >$120k)",
    subtitle = "All DC-linked workers combined",
    x = NULL, y = NULL, caption = cap_pums
  ) +
  theme_minimal() + theme(legend.position = "top", plot.title = element_text(face = "bold"))
save_plot(p_d0, "D0_only_4_income_bands_comparison.png", w = 12, h = 5.5)

# D1: Mean commute by flow and the 4 wage bands
p_d1 <- pums_est$flow_wage %>%
  ggplot(aes(flow, mean_min, colour = period)) +
  geom_pointrange(aes(ymin = mean_min_lo90, ymax = mean_min_hi90, shape = small_sample),
                  position = position_dodge(0.5)) +
  facet_wrap(~ wage_band, nrow = 1) +
  scale_colour_manual(values = pal_years, name = NULL) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  labs(title = "Mean commute by where workers live and work, across the 4 wage bands",
       subtitle = "Income bands: $0-$45k, $45k-$70k, $70k-$120k, > $120k (annual earnings)",
       x = NULL, y = "Minutes (one way)", caption = cap_pums) +
  theme_minimal() + theme(legend.position = "top")
save_plot(p_d1, "D1_pums_time_by_flow_4wagebands.png", w = 13, h = 5.5)

# D2: Change in mean commute time across the 4 wage bands
p_d2 <- pums_chg$flow_wage %>%
  mutate(flow = factor(flow, levels = rev(flows_dc))) %>%
  ggplot(aes(d_min, flow, colour = significant_90)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = d_lo90, xmax = d_hi90)) +
  facet_wrap(~ wage_band, nrow = 1) +
  scale_colour_manual(values = c(`TRUE` = "#c2410c", `FALSE` = "grey55"),
                      labels = c(`TRUE` = "Yes", `FALSE` = "No"), name = "Significant at 90%") +
  labs(title = paste("Change in mean commute time,", P_OLD, "to", P_NEW, "across the 4 wage bands"),
       subtitle = "Positive = longer commutes; workers who commuted (excludes work from home)",
       x = "Change (minutes)", y = NULL, caption = cap_pums) +
  theme_minimal() + theme(legend.position = "top")
save_plot(p_d2, "D2_pums_change_by_flow_4wagebands.png", w = 13, h = 5)

# D3: Share of commuters travelling 60+ minutes across the 4 wage bands
p_d3 <- pums_est$flow_wage %>%
  ggplot(aes(flow, share_60plus * 100, fill = period)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  geom_errorbar(aes(ymin = (share_60plus - 1.645 * share_60plus_se) * 100,
                    ymax = (share_60plus + 1.645 * share_60plus_se) * 100),
                position = position_dodge(0.8), width = 0.2) +
  facet_wrap(~ wage_band, nrow = 1) +
  scale_fill_manual(values = pal_years, name = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  labs(title = "Share of commuters travelling 60+ minutes each way (4 wage bands)",
       x = NULL, y = "% of commuters", caption = cap_pums) +
  theme_minimal() + theme(legend.position = "top")
save_plot(p_d3, "D3_pums_long_commutes_4wagebands.png", w = 13, h = 5.5)

# D4 (IMAGEM 1 ANEXADA ATUALIZADA PARA AS 4 FAIXAS): How DC-linked workers get to work
p_d4 <- d4_mode_shares_est %>%
  ggplot(aes(period, share, fill = mode_grp)) +
  geom_col(width = 0.7) +
  facet_grid(wage_band ~ flow,
             labeller = labeller(flow = label_wrap_gen(14), wage_band = label_wrap_gen(14))) +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  labs(title = "How DC-linked workers get to work",
       subtitle = "Across the 4 wage bands ($0-$45k, $45k-$70k, $70k-$120k, > $120k)",
       x = NULL, y = "Share of workers", caption = "Source: ACS 5-year PUMS.") +
  theme_minimal() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))
save_plot(p_d4, "D4_pums_mode_shares.png", w = 13, h = 9.5)

# D5: Who could work from home across the 4 wage bands
p_d5 <- pums_est$flow_wage %>%
  ggplot(aes(flow, share_wfh * 100, colour = period)) +
  geom_pointrange(aes(ymin = (share_wfh - 1.645 * share_wfh_se) * 100,
                      ymax = (share_wfh + 1.645 * share_wfh_se) * 100), position = position_dodge(0.5)) +
  facet_wrap(~ wage_band, nrow = 1) +
  scale_colour_manual(values = pal_years, name = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  labs(title = "Who could work from home? (4 wage bands)",
       subtitle = "Telework surged in $70k-$120k and > $120k bands, changing who is left commuting",
       x = NULL, y = "% working from home", caption = cap_pums) +
  theme_minimal() + theme(legend.position = "top")
save_plot(p_d5, "D5_pums_work_from_home_4wagebands.png", w = 13, h = 5.5)

# D6 (IMAGEM 2 ANEXADA ATUALIZADA PARA AS 4 FAIXAS): Where people who work in DC live, by wage band
p_d6 <- residence_shares %>%
  mutate(res_state = factor(res_state, levels = c("DC", "Maryland", "Virginia"))) %>%
  ggplot(aes(period, share_of_dc_jobs, fill = res_state)) +
  geom_col(width = 0.7) +
  facet_wrap(~ wage_band, nrow = 1) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = pal_home_state, name = "Home state") +
  labs(title = "Where people who work in DC live, by wage band",
       subtitle = "Across the 4 wage bands ($0-$45k, $45k-$70k, $70k-$120k, > $120k)",
       x = NULL, y = "Share of workers employed in DC", caption = "Source: ACS 5-year PUMS.") +
  theme_minimal() + theme(legend.position = "top")
save_plot(p_d6, "D6_pums_where_dc_workers_live.png", w = 12.5, h = 5.5)

# D7: Difference-in-differences across the 4 wage bands
p_d7 <- did_models %>%
  filter(str_detect(term, "^post")) %>%
  mutate(
    term = str_replace_all(term, "wage_4ref", ""),
    term = recode(
      term,
      "post"                 = "Change for DC residents (> $120k ref.)",
      "post:live_outside_dc" = "Extra change if living outside DC (> $120k)"
    )
  ) %>%
  ggplot(aes(estimate, fct_rev(term))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), colour = "#1d4ed8") +
  facet_wrap(~ model, scales = "free_x") +
  labs(title = "Difference-in-differences: workers employed in DC (4 wage bands, ref: > $120k)",
       x = "Coefficient (90% CI)", y = NULL, caption = "Source: ACS 5-year PUMS; replicate-weight SEs.") +
  theme_minimal()
save_plot(p_d7, "D7_pums_did_coefficients_4wagebands.png", w = 13.5, h = 5.5)

# D8 (IMAGEM 3 ANEXADA ATUALIZADA PARA AS 4 FAIXAS): 1-year ACS check (2010 -> 2019 -> 2023)
if (RUN_1YR_SENSITIVITY) {
  est_1yr_list <- lapply(YEARS_1YR_PUMS, function(y) {
    dat <- tryCatch(load_period(y, "acs1", as.character(y)),
                    error = function(e) { message("1-year ", y, " failed: ", conditionMessage(e)); NULL })
    if (is.null(dat)) return(NULL)
    est_group(make_design(dat), c("flow", "wage_band"), as.character(y))
  })
  names(est_1yr_list) <- as.character(YEARS_1YR_PUMS)
  est_1yr_list <- Filter(Negate(is.null), est_1yr_list)
  one_year_4b  <- bind_rows(est_1yr_list)
  
  if (nrow(one_year_4b) > 0) {
    results[["D8_1yr_est_4bands"]] <- one_year_4b
    
    if (all(c("2010", "2019", "2023") %in% names(est_1yr_list))) {
      chg_pre  <- change_table(est_1yr_list[["2010"]], est_1yr_list[["2019"]], c("flow", "wage_band")) %>%
        mutate(subperiod = "Pre-pandemic (2010 -> 2019)", .before = 1)
      chg_post <- change_table(est_1yr_list[["2019"]], est_1yr_list[["2023"]], c("flow", "wage_band")) %>%
        mutate(subperiod = "Post-pandemic (2019 -> 2023)", .before = 1)
      chg_dec  <- change_table(est_1yr_list[["2010"]], est_1yr_list[["2023"]], c("flow", "wage_band")) %>%
        mutate(subperiod = "Full decade (2010 -> 2023)", .before = 1)
      
      results[["D8_1yr_chg_4bands"]] <- bind_rows(chg_pre, chg_post, chg_dec)
      
      cat("\n=== D8. 1-YEAR DECOMPOSITION (2010->2019 vs 2019->2023) ACROSS THE 4 WAGE BANDS ===\n")
      print(as.data.frame(
        results[["D8_1yr_chg_4bands"]] %>%
          select(subperiod, flow, wage_band, mean_min_old, mean_min_new, d_min, significant_90, d_wfh_pts) %>%
          mutate(across(where(is.numeric), ~ round(.x, 2)))
      ))
    }
    
    p_d8 <- one_year_4b %>%
      mutate(year = as.integer(period)) %>%
      ggplot(aes(year, mean_min, colour = wage_band)) +
      geom_line(linewidth = 0.85) +
      geom_pointrange(aes(ymin = mean_min_lo90, ymax = mean_min_hi90, shape = small_sample),
                      position = position_dodge(0.35), size = 0.45) +
      facet_wrap(~ flow, nrow = 1, labeller = label_wrap_gen(14)) +
      scale_x_continuous(breaks = YEARS_1YR_PUMS) +
      scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
      scale_colour_manual(values = pal_wage4, name = NULL) +
      labs(
        title    = paste0("1-year ACS check: pre-pandemic (", YEAR_OLD, "-2019) vs post-pandemic (2019-2023)"),
        subtitle = "By the 4 wage bands ($0-$45k, $45k-$70k, $70k-$120k, > $120k)",
        x        = NULL, y = "Mean commute (minutes)",
        caption  = "Source: ACS 1-year PUMS; 90% CI."
      ) +
      theme_minimal() + theme(legend.position = "top")
    save_plot(p_d8, "D8_pums_1yr_trend.png", w = 13, h = 4.8)
  }
}

# -----------------------------------------------------------------------------
# Exportação Final Consolidada (Excel Workbook + CSVs Individuais)
# -----------------------------------------------------------------------------
results_xlsx <- lapply(results, function(x) {
  x <- as.data.frame(x)
  if ("geometry" %in% names(x)) x$geometry <- NULL
  x
})
names(results_xlsx) <- substr(names(results_xlsx), 1, 31)
write_xlsx(results_xlsx, file.path(OUT_DIR, "acs_commute_gentrification_part2_unified.xlsx"))

for (nm in names(results)) {
  x <- as.data.frame(results[[nm]])
  if ("geometry" %in% names(x)) x$geometry <- NULL
  write_csv(x, file.path(OUT_DIR, paste0(nm, ".csv")))
}
cat("\nConcluído! Todas as figuras (H1-H4, A1-A6, B1-B3, C1 e D0-D8), CSVs e o Excel consolidado estão em: ./", OUT_DIR, "\n", sep = "")

# =============================================================================
# poster_extra_plots.R
# Gráficos adicionais para o pôster "Da Gentrificação ao Deslocamento Pendular"
#
# COMO USAR: rodar DEPOIS dos dois scripts originais (Parte 1 e Parte 2), na
# MESMA sessão de R. Este script usa os objetos já criados por eles:
#   panel_df, panel (sf), tracts_geo, pal_udp, pal_years, pums_old, pums_new,
#   P_OLD, P_NEW, WAGE_LABELS. (Pacote opcional: patchwork, para a legenda do P3.)
# Nota: a Parte 2 sobrescreve o panel_df da Parte 1 — este script usa a versão
# da Parte 2 (que contém tipologia + variáveis de deslocamento).
# Cada bloco (P1 … P8) é independente; se um falhar, os outros continuam.
# Saída: ./outputs_poster/
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(sf)
  library(scales); library(stringr); library(forcats); library(readr)
})

OUT_POSTER <- "outputs_poster"
dir.create(OUT_POSTER, showWarnings = FALSE, recursive = TRUE)

save_poster <- function(p, name, w = 9, h = 6) {
  print(p)
  ggsave(file.path(OUT_POSTER, name), p, width = w, height = h, dpi = 300, bg = "white")
}
run_block <- function(name, expr) {
  message("\n==== ", name, " ====")
  # 'expr' é avaliado preguiçosamente no ambiente de quem chamou (global):
  # os objetos criados no bloco ficam disponíveis na sessão.
  tryCatch(expr, error = function(e) message("  [", name, "] falhou: ", conditionMessage(e)))
  invisible(NULL)
}

# Rótulos da tipologia (os nomes exatos vêm de pal_udp da Parte 2)
LVL      <- names(pal_udp)
LVL_DYN  <- LVL[1]   # "1. Gentrificação Dinâmica (Intensa)"
LVL_AFF  <- LVL[5]   # "5. Afluente / Exclusão Contínua"

# Checagem da "nota de código" (Seção 10 do guia): a adjacência espacial foi
# calculada na ordem de panel_sf_temp e atribuída a panel_df por posição.
if (exists("panel_sf_temp")) {
  ok_order <- identical(panel_sf_temp$GEOID, panel_df$GEOID)
  message("Ordem GEOID panel_sf_temp == panel_df? ", ok_order)
  if (!ok_order) warning("As ordens diferem: borders_* podem estar desalinhados. Junte por GEOID.")
}

theme_poster <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey30"),
          legend.position = "top",
          panel.grid.minor = element_blank())
}

# -----------------------------------------------------------------------------
# P1. Diagrama do pipeline metodológico (sem pacotes extras: ggplot puro)
# -----------------------------------------------------------------------------
run_block("P1 pipeline", {
  nodes <- tribble(
    ~id, ~x, ~y, ~w, ~grp, ~label,
    "acs",   1, 3, 1.9, "Dados",         "ACS 5-Year\n2006–2010 e 2019–2023\n(tabelas B/S + PUMS)",
    "cpi",   3, 3, 1.6, "Dados",         "Deflação CPI-U\n→ US$ de 2023",
    "xw",    5, 3, 1.8, "Dados",         "Crosswalk tratos\n2010 → 2020\n(peso por área)",
    "filt",  7, 3, 1.8, "Dados",         "Filtros de confiabilidade\n(MOE, n mínimo, CV)",
    "ind",   1, 2, 1.9, "Gentrificação", "Indicadores por trato:\nMHI, %BA+, aluguel,\nvalor, posse, raça",
    "g",     3, 2, 1.6, "Gentrificação", "M1  Índice G\n(percentis)",
    "fr",    5, 2, 1.6, "Gentrificação", "M2  Freeman\n(binário)",
    "udp",   7, 2, 1.8, "Gentrificação", "M3  UDP + contágio\n→ tipologia (5 estágios)",
    "val",   9, 2, 1.8, "Gentrificação", "Validação exógena:\nΔ% negros ~ ΔG",
    "tr",    1, 1, 1.9, "Deslocamento",  "Tratos: tempo por modo,\n% 45+/60+ min, WFH,\ntrabalha fora de DC",
    "reg",   3, 1, 1.6, "Deslocamento",  "Região:\n17 condados,\nsérie 1-year",
    "pums",  5, 1, 1.8, "Deslocamento",  "PUMS: fluxos × 4 faixas,\npesos replicados, DiD",
    "x",     7, 1, 1.8, "Deslocamento",  "Cruzamento\ntipologia × Δtempo\n(IC 90%)",
    "res",   9, 1, 1.8, "Deslocamento",  "Resultado D.C.:\nΔG ≫ 0  |  Δtempo ≈ 0"
  )
  edges <- tribble(
    ~from, ~to,
    "acs","cpi", "cpi","xw", "xw","filt",
    "filt","ind", "ind","g", "ind","fr", "ind","udp", "udp","val",
    "ind","tr", "tr","reg", "reg","pums", "pums","x", "udp","x", "x","res"
  ) %>%
    left_join(nodes %>% select(from = id, x1 = x, y1 = y, w1 = w), by = "from") %>%
    left_join(nodes %>% select(to = id, x2 = x, y2 = y, w2 = w), by = "to") %>%
    mutate(
      # encurta as setas para começar/terminar na borda das caixas
      dx = x2 - x1, dy = y2 - y1,
      xs = if_else(dy == 0, x1 + w1 / 2, x1), ys = if_else(dy == 0, y1, y1 + sign(dy) * 0.33),
      xe = if_else(dy == 0, x2 - w2 / 2, x2), ye = if_else(dy == 0, y2, y2 - sign(dy) * 0.33)
    )
  pal_grp <- c("Dados" = "#dbeafe", "Gentrificação" = "#fee2e2", "Deslocamento" = "#dcfce7")
  p_p1 <- ggplot() +
    geom_segment(data = edges, aes(x = xs, y = ys, xend = xe, yend = ye),
                 arrow = arrow(length = unit(0.18, "cm"), type = "closed"), colour = "grey40", linewidth = 0.5) +
    geom_rect(data = nodes, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - 0.33, ymax = y + 0.33, fill = grp),
              colour = "grey30", linewidth = 0.4) +
    geom_text(data = nodes, aes(x = x, y = y, label = label), size = 2.9, lineheight = 0.95) +
    annotate("text", x = -0.3, y = 3, label = "DADOS", angle = 90, fontface = "bold", size = 3.5, colour = "#1e40af") +
    annotate("text", x = -0.3, y = 2, label = "GENTRIFICAÇÃO", angle = 90, fontface = "bold", size = 3.5, colour = "#991b1b") +
    annotate("text", x = -0.3, y = 1, label = "DESLOCAMENTO", angle = 90, fontface = "bold", size = 3.5, colour = "#166534") +
    scale_fill_manual(values = pal_grp, guide = "none") +
    coord_equal(xlim = c(-0.6, 10.1), ylim = c(0.5, 3.5), expand = FALSE) +
    labs(title = "Pipeline metodológico: da definição de gentrificação ao perfil de deslocamento",
         subtitle = "Implementado em R (tidycensus, sf, survey). Parâmetros no topo de cada script permitem aplicar a outras cidades.") +
    theme_void() + theme(plot.title = element_text(face = "bold", size = 13), plot.subtitle = element_text(colour = "grey30", size = 9.5))
  save_poster(p_p1, "P1_pipeline_metodologico.png", w = 12, h = 4.6)
})

# -----------------------------------------------------------------------------
# P2. Por que o LODES falha em D.C.: as 3 faixas nominais do LODES vs. os salários
#     reais dos trabalhadores ligados a D.C. (PUMS)
#     LODES: SE01 ≤ $1.250/mês (≤ $15.000/ano); SE02 $1.251–3.333 ($15.001–40.000);
#            SE03 > $3.333/mês (> $40.000/ano). Limiares fixos em dólares nominais.
# -----------------------------------------------------------------------------
run_block("P2 LODES vs PUMS", {
  LODES_BREAKS <- c(-Inf, 15000, 40000, Inf)
  LODES_LABELS <- c("SE01: ≤ $15k/ano", "SE02: $15k–40k/ano", "SE03: > $40k/ano")
  pums_both <- bind_rows(pums_old, pums_new) %>%
    filter(!is.na(flow), !is.na(wage_end), wage_end > 0) %>%
    mutate(lodes_band = cut(wage_end, LODES_BREAKS, LODES_LABELS),   # nominal do ano final da janela (como o LODES)
           PWGTP = as.numeric(PWGTP))
  
  lodes_shares <- pums_both %>%
    group_by(period, flow, lodes_band) %>% summarise(w = sum(PWGTP, na.rm = TRUE), n = n(), .groups = "drop") %>%
    group_by(period, flow) %>% mutate(share = w / sum(w)) %>% ungroup()
  write_csv(lodes_shares, file.path(OUT_POSTER, "P2_lodes_band_shares.csv"))
  cat("\nParticipação na faixa superior do LODES (SE03) por fluxo e período:\n")
  print(lodes_shares %>% filter(lodes_band == LODES_LABELS[3]) %>%
          select(period, flow, share_SE03 = share) %>% mutate(share_SE03 = round(share_SE03, 3)) %>% as.data.frame())
  
  p_p2a <- ggplot(lodes_shares, aes(period, share, fill = lodes_band)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = if_else(lodes_band == LODES_LABELS[3], percent(share, 1), "")),
              position = position_stack(vjust = 0.5), size = 3.2, colour = "white", fontface = "bold") +
    facet_wrap(~ flow, nrow = 1, labeller = label_wrap_gen(16)) +
    scale_y_continuous(labels = percent) +
    scale_fill_manual(values = c("#fde68a", "#f59e0b", "#7c2d12"), name = "Faixa de rendimento do LODES (nominal)") +
    labs(title = "Por que o LODES não discrimina renda em D.C.",
         subtitle = "Quase todos os trabalhadores ligados a D.C. caem na faixa superior (SE03 > $40 mil/ano), fixa em dólares nominais desde 2002",
         x = NULL, y = "Trabalhadores (ponderado)", caption = "Fonte: ACS 5-year PUMS (salários nominais do ano final da janela, WAGP×ADJINC); faixas do LEHD/LODES Technical Documentation v8.") +
    theme_poster()
  save_poster(p_p2a, "P2a_lodes_bands_vs_pums.png", w = 12, h = 5.2)
  
  # Painel (b): distribuição dos salários de 2019–2023 com os cortes do LODES e os do estudo
  dist23 <- pums_new %>% filter(!is.na(flow), wage_2023 > 0, wage_2023 < 400000) %>% mutate(PWGTP = as.numeric(PWGTP))
  cuts <- tibble(x = c(15000, 40000, 45000, 70000, 120000),
                 tipo = c("LODES", "LODES", "Estudo (4 faixas)", "Estudo (4 faixas)", "Estudo (4 faixas)"))
  p_p2b <- ggplot(dist23, aes(x = wage_2023, weight = PWGTP)) +
    geom_histogram(binwidth = 10000, boundary = 0, fill = "#93c5fd", colour = "white") +
    geom_vline(data = cuts, aes(xintercept = x, colour = tipo, linetype = tipo), linewidth = 0.9) +
    scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "k"), breaks = seq(0, 400000, 50000)) +
    scale_y_continuous(labels = comma) +
    scale_colour_manual(values = c("LODES" = "#b91c1c", "Estudo (4 faixas)" = "#1d4ed8"), name = NULL) +
    scale_linetype_manual(values = c("LODES" = "dashed", "Estudo (4 faixas)" = "dotted"), name = NULL) +
    labs(title = "Onde os cortes caem: LODES (US$ 15k, 40k) vs. as 4 faixas do estudo (US$ 45k, 70k, 120k)",
         subtitle = "Distribuição ponderada dos salários anuais dos trabalhadores ligados a D.C., ACS PUMS 2019–2023 (US$ de 2023)",
         x = "Salário anual", y = "Trabalhadores", caption = "Fonte: ACS 5-year PUMS 2019–2023.") +
    theme_poster()
  save_poster(p_p2b, "P2b_wage_distribution_cuts.png", w = 11, h = 5)
})

# -----------------------------------------------------------------------------
# P3. Mapa bivariado: estágio de gentrificação × mudança no tempo de deslocamento
# -----------------------------------------------------------------------------
run_block("P3 mapa bivariado", {
  biv <- panel %>%
    mutate(
      gent3 = case_when(typology_udp == LVL_DYN ~ "Gentrificação dinâmica",
                        typology_udp == LVL_AFF ~ "Afluente / exclusão",
                        TRUE                    ~ "Outros vulneráveis"),
      gent3 = factor(gent3, levels = c("Afluente / exclusão", "Outros vulneráveis", "Gentrificação dinâmica")),
      time3 = case_when(change_class == "Significantly shorter" ~ "Mais curto",
                        change_class == "Significantly longer"  ~ "Mais longo",
                        change_class == "No significant change" ~ "Sem mudança signif.",
                        TRUE ~ NA_character_),
      time3 = factor(time3, levels = c("Mais curto", "Sem mudança signif.", "Mais longo")),
      biv_class = if_else(is.na(time3), NA_character_, paste(gent3, time3, sep = " | ")),
      biv_class = factor(biv_class, levels = as.vector(outer(levels(gent3), levels(time3), paste, sep = " | ")))
    )
  # paleta 3x3 (linhas = gentrificação, colunas = tempo)
  pal_biv <- c(
    "Afluente / exclusão | Mais curto"            = "#e8e8e8",
    "Afluente / exclusão | Sem mudança signif."   = "#e4acac",
    "Afluente / exclusão | Mais longo"            = "#c85a5a",
    "Outros vulneráveis | Mais curto"             = "#b0d5df",
    "Outros vulneráveis | Sem mudança signif."    = "#ad9ea5",
    "Outros vulneráveis | Mais longo"             = "#985356",
    "Gentrificação dinâmica | Mais curto"         = "#64acbe",
    "Gentrificação dinâmica | Sem mudança signif."= "#627f8c",
    "Gentrificação dinâmica | Mais longo"         = "#574249"
  )
  tab_biv <- biv %>% st_drop_geometry() %>% count(gent3, time3) %>% group_by(gent3) %>% mutate(share = n / sum(n)) %>% ungroup()
  write_csv(tab_biv, file.path(OUT_POSTER, "P3_bivariate_counts.csv"))
  cat("\nTratos por estágio × mudança no tempo:\n"); print(as.data.frame(tab_biv))
  
  p_map <- ggplot(biv) +
    geom_sf(aes(fill = biv_class), colour = "white", linewidth = 0.1) +
    scale_fill_manual(values = pal_biv, na.value = "#f3f4f6", drop = FALSE, guide = "none") +
    labs(title = "Gentrificação intensa, tempo de deslocamento estável",
         subtitle = paste0("Estágio de gentrificação × mudança significativa (90%) no tempo médio de deslocamento, ", P_OLD, " a ", P_NEW),
         caption = "Fonte: ACS 5-Year (B19013, B15002, B25064, B25077, B08013, B08303). Cinza claro = sem dado confiável.") +
    theme_void() + theme(plot.title = element_text(face = "bold", size = 13), plot.subtitle = element_text(size = 9.5, colour = "grey30"))
  
  legend_df <- expand.grid(gent3 = levels(biv$gent3), time3 = levels(biv$time3)) %>%
    mutate(biv_class = paste(gent3, time3, sep = " | "))
  p_leg <- ggplot(legend_df, aes(time3, gent3, fill = biv_class)) +
    geom_tile(colour = "white") +
    scale_fill_manual(values = pal_biv, guide = "none") +
    labs(x = "Δ tempo de deslocamento →", y = "Gentrificação →") +
    coord_equal() + theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid = element_blank())
  
  if (requireNamespace("patchwork", quietly = TRUE)) {
    library(patchwork)
    p_p3 <- p_map + inset_element(p_leg, left = 0.62, bottom = 0.62, right = 1.0, top = 0.98)
    save_poster(p_p3, "P3_bivariate_map_gent_x_commute.png", w = 9, h = 8)
  } else {
    save_poster(p_map, "P3_bivariate_map_gent_x_commute.png", w = 9, h = 8)
    save_poster(p_leg, "P3_bivariate_legend.png", w = 3, h = 3)
    message("  (instale 'patchwork' para embutir a legenda no mapa)")
  }
})

# -----------------------------------------------------------------------------
# P4. O "resultado nulo" com inferência: Δ tempo médio por tipologia, média
#     ponderada por trabalhadores, IC 90% propagado das MOEs dos tratos
# -----------------------------------------------------------------------------
run_block("P4 resultado nulo", {
  p4_df <- panel_df %>%
    filter(!is.na(d_time_total), !is.na(time_totalM_10), !is.na(time_totalM_23), workers_totalE_23 > 0) %>%
    mutate(se_d = sqrt(time_totalM_10^2 + time_totalM_23^2) / 1.645, w = workers_totalE_23)
  agg <- function(d, lab) d %>% summarise(
    grupo = lab, n = n(),
    d_min = sum(w * d_time_total) / sum(w),
    se    = sqrt(sum((w * se_d)^2)) / sum(w),
    pct_sig_longer  = mean(change_class == "Significantly longer"),
    pct_sig_shorter = mean(change_class == "Significantly shorter")
  )
  p4_tab <- bind_rows(
    p4_df %>% group_by(typology_udp) %>% group_map(~ agg(.x, as.character(.y$typology_udp))) %>% bind_rows(),
    agg(p4_df, "D.C. (todos os tratos)")
  ) %>% mutate(lo90 = d_min - 1.645 * se, hi90 = d_min + 1.645 * se,
               grupo = factor(grupo, levels = rev(c(LVL, "D.C. (todos os tratos)"))))
  write_csv(p4_tab, file.path(OUT_POSTER, "P4_null_result_table.csv"))
  cat("\nΔ tempo médio ponderado por tipologia (IC 90%):\n"); print(as.data.frame(p4_tab %>% mutate(across(where(is.numeric), ~ round(.x, 2)))))
  
  p_p4 <- ggplot(p4_tab, aes(x = d_min, y = grupo)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_errorbar(aes(xmin = lo90, xmax = hi90), width = 0.25, colour = "grey30", orientation = "y") +
    geom_point(aes(colour = grupo, size = n)) +
    geom_text(aes(label = paste0(percent(pct_sig_longer, 1), " dos tratos\ncom alta signif."), x = hi90), hjust = -0.1, size = 2.8, colour = "grey30", lineheight = 0.9) +
    scale_colour_manual(values = c(pal_udp, "D.C. (todos os tratos)" = "black"), guide = "none") +
    scale_size_continuous(range = c(2.5, 6), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.35))) +
    scale_y_discrete(labels = function(x) str_wrap(x, 24)) +
    labs(title = paste0("Mudança no tempo médio de deslocamento dos residentes, ", P_OLD, " → ", P_NEW),
         subtitle = "Média ponderada por trabalhadores de 2023; barras = IC 90% propagado das MOEs dos tratos. Se a barra cruza zero, a mudança não é distinguível de zero.",
         x = "Δ minutos (ida)", y = NULL,
         caption = "Fonte: ACS 5-Year B08013/B08303 (tempo agregado ÷ trabalhadores que não trabalham em casa).") +
    theme_poster()
  save_poster(p_p4, "P4_null_result_commute_by_typology.png", w = 10, h = 5.5)
})

# -----------------------------------------------------------------------------
# P5. Concordância entre os três métodos (Freeman × quartil superior de ΔG × UDP dinâmica)
# -----------------------------------------------------------------------------
run_block("P5 concordância", {
  q75 <- quantile(panel_df$d_G_index, 0.75, na.rm = TRUE)
  sets <- panel_df %>% transmute(
    GEOID,
    Freeman = freeman_gent %in% TRUE,
    McKinnish = d_G_index >= q75 & !is.na(d_G_index),
    UDP = typology_udp == LVL_DYN
  )
  combos <- sets %>%
    mutate(combo = case_when(
      Freeman & McKinnish & UDP ~ "Freeman + McKinnish + UDP",
      Freeman & UDP             ~ "Freeman + UDP",
      McKinnish & UDP           ~ "McKinnish + UDP",
      Freeman & McKinnish       ~ "Freeman + McKinnish",
      UDP                       ~ "Só UDP (dinâmica)",
      McKinnish                 ~ "Só McKinnish (ΔG ≥ Q3)",
      Freeman                   ~ "Só Freeman",
      TRUE                      ~ "Nenhum")) %>%
    count(combo) %>% arrange(desc(n))
  write_csv(combos, file.path(OUT_POSTER, "P5_method_agreement.csv"))
  jacc <- function(a, b) sum(a & b) / sum(a | b)
  cat(sprintf("\nJaccard  Freeman~UDP = %.2f | McKinnish~UDP = %.2f | Freeman~McKinnish = %.2f\n",
              jacc(sets$Freeman, sets$UDP), jacc(sets$McKinnish, sets$UDP), jacc(sets$Freeman, sets$McKinnish)))
  cat(sprintf("Totais: Freeman = %d | McKinnish = %d | UDP dinâmica = %d\n", sum(sets$Freeman), sum(sets$McKinnish), sum(sets$UDP)))
  print(as.data.frame(combos))
  
  p_p5 <- combos %>% filter(combo != "Nenhum") %>%
    mutate(combo = fct_reorder(combo, n), concord = str_count(combo, "\\+") + 1) %>%
    ggplot(aes(n, combo, fill = factor(concord))) +
    geom_col(width = 0.7) +
    geom_text(aes(label = n), hjust = -0.2, size = 3.3) +
    scale_fill_manual(values = c("1" = "#cbd5e1", "2" = "#f97316", "3" = "#b91c1c"), name = "Métodos concordantes") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Três operacionalizações, o mesmo conjunto de bairros",
         subtitle = sprintf("Tratos identificados por Freeman (binário), McKinnish (ΔG no quartil superior, ≥ %.3f) e UDP (gentrificação dinâmica)", q75),
         x = "Número de tratos", y = NULL, caption = "Fonte: painel de tratos (Parte 1/2).") +
    theme_poster() + theme(legend.position = "bottom")
  save_poster(p_p5, "P5_method_agreement_upset.png", w = 9, h = 5)
})

# -----------------------------------------------------------------------------
# P6. Sensibilidade dos limiares de vulnerabilidade (score ≥ k | percentil de renda ≤ p)
# -----------------------------------------------------------------------------
run_block("P6 sensibilidade", {
  grid <- expand.grid(k = c(10, 12, 14, 16), p = c(0.40, 0.50, 0.60))
  sens <- bind_rows(Map(function(k, p) {
    vul <- (panel_df$udp_vuln_score >= k) | (panel_df$pct_rank_inc_10 <= p)
    tibble(k = k, p = p,
           vulneraveis = sum(vul, na.rm = TRUE),
           dinamicos   = sum(vul & (panel_df$demo_surge %in% TRUE) & (panel_df$market_surge %in% TRUE), na.rm = TRUE))
  }, grid$k, grid$p)) %>% mutate(escolhido = k == 12 & p == 0.50)
  write_csv(sens, file.path(OUT_POSTER, "P6_threshold_sensitivity.csv"))
  print(as.data.frame(sens))
  
  p_p6 <- ggplot(sens, aes(factor(p), factor(k), fill = dinamicos)) +
    geom_tile(colour = "white", linewidth = 1) +
    geom_tile(data = filter(sens, escolhido), fill = NA, colour = "black", linewidth = 1.4) +
    geom_text(aes(label = paste0(dinamicos, "\n(", vulneraveis, " vuln.)")), size = 3.3, lineheight = 0.9,
              colour = ifelse(sens$dinamicos > median(sens$dinamicos), "white", "black")) +
    scale_fill_gradient(low = "#fee2e2", high = "#b91c1c", name = "Tratos em\ngentrificação\ndinâmica") +
    labs(title = "A classificação não depende de um corte arbitrário",
         subtitle = "Número de tratos classificados como 'gentrificação dinâmica' sob limiares alternativos de vulnerabilidade (contorno preto = escolha do estudo)",
         x = "Vulnerável se percentil de renda 2010 ≤ p", y = "… ou se score UDP ≥ k",
         caption = "Os vetores demo_surge e market_surge são mantidos fixos; só a elegibilidade muda.") +
    theme_poster() + theme(legend.position = "right", panel.grid = element_blank())
  save_poster(p_p6, "P6_threshold_sensitivity.png", w = 8, h = 5.5)
})

# -----------------------------------------------------------------------------
# P7. Distribuições completas do tempo de deslocamento (PUMS), 2010 vs 2023,
#     por fluxo e faixa de renda — "a distribuição inteira quase não se moveu"
# -----------------------------------------------------------------------------
run_block("P7 densidades PUMS", {
  dens <- bind_rows(pums_old, pums_new) %>%
    filter(!is.na(flow), !is.na(wage_band), !is.na(travel), travel > 0) %>%
    mutate(travel_c = pmin(travel, 120), PWGTP = as.numeric(PWGTP), period = factor(period, levels = c(P_OLD, P_NEW)))
  meds <- dens %>% group_by(period, flow, wage_band) %>%
    summarise(med = { o <- order(travel_c); cw <- cumsum(PWGTP[o]) / sum(PWGTP); travel_c[o][which(cw >= 0.5)[1]] }, .groups = "drop")
  write_csv(meds, file.path(OUT_POSTER, "P7_weighted_median_minutes.csv"))
  
  p_p7 <- ggplot(dens, aes(travel_c, weight = PWGTP, fill = period, colour = period)) +
    geom_density(alpha = 0.35, adjust = 1.3, linewidth = 0.6) +
    geom_vline(data = meds, aes(xintercept = med, colour = period), linetype = "dashed", linewidth = 0.5) +
    facet_grid(wage_band ~ flow, labeller = labeller(flow = label_wrap_gen(14), wage_band = label_wrap_gen(14))) +
    scale_fill_manual(values = pal_years, name = NULL) + scale_colour_manual(values = pal_years, name = NULL) +
    scale_x_continuous(breaks = seq(0, 120, 30)) +
    labs(title = "A distribuição inteira do tempo de deslocamento quase não se moveu",
         subtitle = "Densidade ponderada dos minutos de ida (truncado em 120) por fluxo e faixa de renda; linhas tracejadas = medianas ponderadas",
         x = "Minutos (ida)", y = "Densidade", caption = "Fonte: ACS 5-year PUMS (JWMNP), exclui teletrabalho.") +
    theme_poster() + theme(axis.text.y = element_blank())
  save_poster(p_p7, "P7_pums_commute_densities.png", w = 13, h = 9)
})

# -----------------------------------------------------------------------------
# P8. Mecanismo do resultado nulo: a composição muda, o tempo não
# -----------------------------------------------------------------------------
run_block("P8 composição x tempo", {
  p8_df <- panel_df %>% filter(!is.na(d_time_total), !is.na(d_hh_125kp_pts))
  fit <- lm(d_time_total ~ d_hh_125kp_pts, data = p8_df, weights = workers_totalE_23)
  cat("\nΔtempo ~ Δ% domicílios > $125k (ponderado):\n"); print(summary(fit)$coefficients)
  p_p8 <- ggplot(p8_df, aes(d_hh_125kp_pts, d_time_total)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(aes(colour = typology_udp, size = workers_totalE_23), alpha = 0.75) +
    geom_smooth(aes(weight = workers_totalE_23), method = "lm", colour = "black", se = TRUE) +
    scale_colour_manual(values = pal_udp, name = NULL) +
    scale_size_continuous(range = c(1.5, 6), guide = "none") +
    labs(title = "A composição de renda do bairro muda; o tempo de deslocamento não",
         subtitle = sprintf("Inclinação ponderada = %.3f min por ponto percentual (p = %.3f)", coef(fit)[2], summary(fit)$coefficients[2, 4]),
         x = "Δ pontos percentuais de domicílios com renda > US$ 125k (B19001), 2010 → 2023",
         y = "Δ tempo médio de deslocamento (minutos)",
         caption = "Fonte: ACS 5-Year. Tamanho do ponto = trabalhadores em 2023.") +
    theme_poster() + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 2))
  save_poster(p_p8, "P8_composition_vs_commute.png", w = 10, h = 6.5)
})

cat("\nConcluído. Figuras e tabelas adicionais em ./", OUT_POSTER, "\n", sep = "")


