# =============================================================================
# GENTRIFICATION AND THE CHANGING COMMUTE PROFILE OF A CITY
# A transferable ACS-based pipeline — case study: Washington, D.C.
# ACS 5-Year 2006–2010 (baseline) vs. 2019–2023 (endline)
#
# Structure of this single script
#   0. Setup, parameters, CPI deflator and the UNIFIED POSTER COLOUR IDENTITY
#   PART 1 — Mathematical and spatial definition of gentrification
#            (Freeman 2005; McKinnish, Walsh & White 2010; Urban Displacement
#            Project / Bates; Guerrieri, Hartley & Hurst 2013; exogenous
#            validation with race after Hyra 2017)                 → figures G1–G8
#   PART 2 — From the gentrification profile to the commuting profile
#            Block H (income geography), Part A (tract commute), Part B (region),
#            Part C (live vs. work in DC), Part D (PUMS, 4 wage bands, DiD)
#                                                                  → H1–H4, A1–A6, B1–B3, C1, D0–D8
#   POSTER EXTRAS — P1–P8 (pipeline diagram, LODES vs PUMS, bivariate map,
#            null-result inference, method agreement, threshold sensitivity,
#            PUMS densities, composition vs. time)
#
# Everything the poster needs to look consistent is defined ONCE in section 0:
#   POSTER$...        named colour vectors (typology, period, wage band, mode, …)
#   scale_fill_div()  diverging scale for change maps (blue = down, red = up)
#   scale_fill_seq()  sequential scale for level maps (teal ramp)
#   theme_poster()    chart theme; theme_map_poster() map theme
# Change a colour here and every figure follows.
# =============================================================================

# -----------------------------------------------------------------------------
# 0.1 Packages
# -----------------------------------------------------------------------------
needed <- c("dplyr", "tidyr", "readr", "stringr", "ggplot2", "tibble", "scales",
            "broom", "sf", "tidycensus", "survey", "writexl", "forcats")
to_install <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)
invisible(lapply(needed, library, character.only = TRUE))
options(tigris_use_cache = TRUE, timeout = 3600, survey.replicates.mse = TRUE)

# Census API key: install it once with census_api_key("<key>", install = TRUE)
# and restart R; the line below only runs if no key is found in .Renviron.
if (Sys.getenv("CENSUS_API_KEY") == "") {
  census_api_key("YOUR_CENSUS_API_KEY", install = TRUE, overwrite = TRUE)
  readRenviron("~/.Renviron")
}

# -----------------------------------------------------------------------------
# 0.2 Global parameters and CPI-U deflator
# -----------------------------------------------------------------------------
YEAR_OLD <- 2010          # ACS 5-year 2006–2010
YEAR_NEW <- 2023          # ACS 5-year 2019–2023
P_OLD    <- paste0(YEAR_OLD - 4, "-", YEAR_OLD)
P_NEW    <- paste0(YEAR_NEW - 4, "-", YEAR_NEW)

# CPI-U annual averages (BLS) to express 2010 dollars in constant 2023 dollars
CPI <- c("2010" = 218.056, "2011" = 224.939, "2012" = 229.594, "2013" = 232.957,
         "2014" = 236.736, "2015" = 237.017, "2016" = 240.007, "2017" = 245.120,
         "2018" = 251.107, "2019" = 255.657, "2020" = 258.811, "2021" = 270.970,
         "2022" = 292.655, "2023" = 304.702, "2024" = 313.689)
CPI_RATIO <- CPI[[as.character(YEAR_NEW)]] / CPI[[as.character(YEAR_OLD)]]   # ≈ 1.3974

# Reliability filters (recalibrate for another city)
MIN_TRACT_HH      <- 150   # Part 1: drop non-residential tracts (National Mall, campuses, bases)
MIN_TRACT_WORKERS <- 100   # Part 2: minimum workers (2023) for a tract to enter the commute panel
MIN_MODE_WORKERS  <- 50    # minimum workers per mode for the transit penalty
MAX_CV            <- 0.40  # maximum coefficient of variation of transit travel time
MIN_PUMS_N        <- 50    # minimum PUMS sample size per cell (smaller cells drawn hollow)

# The 4 wage bands used throughout Part 2 and the poster
WAGE_BREAKS <- c(-Inf, 45000, 70000, 120000, Inf)
WAGE_LABELS <- c("Low-wage ($0-$45k)", "Lower-mid ($45k-$70k)",
                 "Upper-mid ($70k-$120k)", "High-income (> $120k)")
MODE_LEVELS_D4 <- c("Drove alone", "Carpool", "Transit", "Walk/bike", "Worked from home", "Other")

# -----------------------------------------------------------------------------
# 0.3 UNIFIED POSTER COLOUR IDENTITY
#     One hue family per concept, never reused for another concept:
#       red → orange → gold → grey → navy : gentrification stage (hot = pressure)
#       grey vs. teal                     : baseline vs. endline period
#       lilac → deep purple               : the 4 wage bands (ordinal)
#       teal / amber / olive              : home state of DC workers
#       blue → white → red                : change maps (down / none / up)
#       teal ramp (viridis "mako")        : level maps (income, percentiles)
# -----------------------------------------------------------------------------
TYPOLOGY_LEVELS <- c("1. Dynamic gentrification",
                     "2. Early gentrification",
                     "3. Susceptible (contagion edge)",
                     "4. Low-income, stagnant",
                     "5. Affluent / ongoing exclusion")

POSTER <- list(
  typology = setNames(c("#B3261E", "#E8873A", "#F2C94C", "#9AA5B1", "#1E3A5F"), TYPOLOGY_LEVELS),
  period   = setNames(c("#B0B7C3", "#0E7C86"), c(P_OLD, P_NEW)),
  wage     = setNames(c("#CDB8E6", "#9B72CF", "#6A3DB3", "#3B1E6E"), WAGE_LABELS),
  home     = c("DC" = "#0E7C86", "Maryland" = "#D98E04", "Virginia" = "#8C6D31"),
  mode     = c("Drove alone" = "#4B5563", "Carpool" = "#9CA3AF", "Transit" = "#0E7C86",
               "Walk/bike" = "#5FAF8E", "Worked from home" = "#F2C94C", "Other" = "#D1D5DB"),
  race     = c("Black (non-Hispanic)" = "#1E3A5F", "White (non-Hispanic)" = "#D98E04",
               "Hispanic / Latino" = "#5FAF8E"),
  ring     = c("Core (DC)" = "#0E7C86", "Inner suburbs" = "#D98E04", "Outer suburbs" = "#B3261E"),
  sig      = c(`TRUE` = "#B3261E", `FALSE` = "grey65"),
  adj      = c("Borders an affluent tract (2010)" = "#B3261E", "No direct affluent border" = "#0E7C86"),
  lodes    = c("SE01: ≤ $15k/yr" = "#FDE68A", "SE02: $15k–40k/yr" = "#F59E0B", "SE03: > $40k/yr" = "#7C2D12"),
  div_low  = "#2166AC", div_mid = "#F7F7F7", div_high = "#B2182B",
  na_grey  = "#E5E7EB", ink = "#1F2937", muted = "#6B7280"
)
# Short aliases kept for backward compatibility with the original code
pal_udp        <- POSTER$typology
pal_years      <- POSTER$period
pal_wage4      <- POSTER$wage
pal_home_state <- POSTER$home

# Diverging scale for change maps (symmetric limits, outliers squished)
scale_fill_div <- function(name, limits, ...) {
  scale_fill_gradient2(low = POSTER$div_low, mid = POSTER$div_mid, high = POSTER$div_high,
                       midpoint = 0, limits = limits, oob = squish, na.value = POSTER$na_grey,
                       name = name, ...)
}
# Sequential scale for level maps (dark = high)
scale_fill_seq <- function(name, ...) {
  scale_fill_viridis_c(option = "mako", direction = -1, na.value = POSTER$na_grey, name = name, ...)
}
# Poster-ready chart theme (large type, light grid, legend on top)
theme_poster <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(plot.title    = element_text(face = "bold", colour = POSTER$ink, size = rel(1.15)),
          plot.subtitle = element_text(colour = POSTER$muted, size = rel(0.9)),
          plot.caption  = element_text(colour = POSTER$muted, size = rel(0.7), hjust = 0),
          plot.title.position = "plot", plot.caption.position = "plot",
          legend.position = "top", legend.title = element_text(size = rel(0.85)),
          panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
}
# Poster-ready map theme
theme_map_poster <- function(base_size = 13) {
  theme_void(base_size = base_size) +
    theme(plot.title    = element_text(face = "bold", colour = POSTER$ink, size = rel(1.15)),
          plot.subtitle = element_text(colour = POSTER$muted, size = rel(0.9)),
          plot.caption  = element_text(colour = POSTER$muted, size = rel(0.7), hjust = 0),
          plot.title.position = "plot", plot.caption.position = "plot",
          legend.position = "right", strip.text = element_text(face = "bold", margin = margin(4, 0, 4, 0)))
}

# Output helper (OUT_DIR is looked up at call time, so each part can change it)
save_plot <- function(p, name, w = 9, h = 6) {
  print(p)
  ggsave(file.path(OUT_DIR, name), p, width = w, height = h, dpi = 300, bg = "white")
}

# -----------------------------------------------------------------------------
# 0.4 Margin-of-error helpers (Census Bureau formulas; ACS MOEs are 90%)
# -----------------------------------------------------------------------------
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

# Area-weighted helpers for the 2010 → 2020 tract crosswalk
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

# 2020-tract ↔ 2010-tract relationship file (filtered to DC: state FIPS 11)
XW_URL <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/tract/tab20_tract20_tract10_natl.txt"
load_crosswalk <- function(state_fips = "11") {
  read_delim(XW_URL, delim = "|", col_types = cols(.default = col_character())) %>%
    filter(str_starts(GEOID_TRACT_20, state_fips)) %>%
    transmute(GEOID20 = GEOID_TRACT_20, GEOID10 = GEOID_TRACT_10,
              w = as.numeric(AREALAND_PART) / as.numeric(AREALAND_TRACT_10)) %>%
    filter(is.finite(w), w > 0)
}

# =============================================================================
# PART 1 — MATHEMATICAL AND SPATIAL DEFINITION OF GENTRIFICATION
# =============================================================================
OUT_DIR <- "outputs_gentrification"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1.1 ACS 5-Year variables
# -----------------------------------------------------------------------------
B_VARS_GENT <- c(
  # Income and housing costs (rent gap / appreciation)
  med_inc    = "B19013_001", # median household income (MHI)
  med_rent   = "B25064_001", # median gross rent
  med_val    = "B25077_001", # median home value (owner-occupied)
  # Households and tenure
  hh         = "B11001_001",
  tenure_tot = "B25003_001",
  owner_hh   = "B25003_002",
  renter_hh  = "B25003_003",
  # Age of the housing stock (B25034 — physical disinvestment, Freeman)
  hu_tot     = "B25034_001",
  hu_new1    = "B25034_002",
  hu_new2    = "B25034_003",
  hu_new3    = "B25034_004",   # 3 most recent bands ≈ last 20 years in the 2010 ACS
  # Education (B15002 — population 25+ with a bachelor's degree or higher)
  pop25      = "B15002_001",
  ba_m       = "B15002_015", ma_m   = "B15002_016", prof_m = "B15002_017", phd_m = "B15002_018",
  ba_f       = "B15002_032", ma_f   = "B15002_033", prof_f = "B15002_034", phd_f = "B15002_035",
  # Race / Hispanic origin (B03002 — UDP vulnerability and exogenous validation)
  pop_tot    = "B03002_001",
  white_nh   = "B03002_003",
  black_nh   = "B03002_004",
  asian_nh   = "B03002_006",
  hisp       = "B03002_012"
)

get_dc_gent <- function(year, geography = "tract", geometry = FALSE) {
  message(sprintf("Downloading ACS 5-year (%d) for DC [%s]...", year, geography))
  get_acs(geography = geography, state = "DC", year = year, survey = "acs5",
          variables = B_VARS_GENT, output = "wide", geometry = geometry, progress_bar = FALSE) %>%
    mutate(
      baplusE = ba_mE + ma_mE + prof_mE + phd_mE + ba_fE + ma_fE + prof_fE + phd_fE,
      baplusM = sqrt(ba_mM^2 + ma_mM^2 + prof_mM^2 + phd_mM^2 +
                       ba_fM^2 + ma_fM^2 + prof_fM^2 + phd_fM^2),
      hu_recent20E = hu_new1E + hu_new2E + hu_new3E
    )
}

adjust_dollars_gent <- function(df, ratio) {
  df %>% mutate(across(c(med_incE, med_incM, med_rentE, med_rentM, med_valE, med_valM), ~ .x * ratio))
}

new_raw    <- get_dc_gent(YEAR_NEW, "tract", geometry = TRUE)
tracts_geo <- new_raw %>% select(GEOID, NAME)
new_tr     <- new_raw %>% sf::st_drop_geometry()
old_tr     <- get_dc_gent(YEAR_OLD, "tract", geometry = FALSE) %>% adjust_dollars_gent(CPI_RATIO)

dc_new     <- get_dc_gent(YEAR_NEW, "state")
dc_old     <- get_dc_gent(YEAR_OLD, "state") %>% adjust_dollars_gent(CPI_RATIO)

# -----------------------------------------------------------------------------
# 1.2 Longitudinal crosswalk (2010 tracts → 2020 tracts, area-weighted)
# -----------------------------------------------------------------------------
xw <- load_crosswalk("11")

old_xw <- xw %>%
  inner_join(old_tr, by = c("GEOID10" = "GEOID")) %>%
  group_by(GEOID = GEOID20) %>%
  summarise(
    # medians: weighted means over the relevant universe before summing counts
    med_incE  = wmean(med_incE,  hhE * w),
    med_incM  = wmoe(med_incM,   hhE * w),
    med_rentE = wmean(med_rentE, renter_hhE * w),
    med_rentM = wmoe(med_rentM,  renter_hhE * w),
    med_valE  = wmean(med_valE,  owner_hhE * w),
    med_valM  = wmoe(med_valM,   owner_hhE * w),
    # MOEs of interpolated counts
    pop25M    = sqrt(sum((pop25M * w)^2, na.rm = TRUE)),
    baplusM   = sqrt(sum((baplusM * w)^2, na.rm = TRUE)),
    # counts interpolated by land-area share
    across(c(hhE, tenure_totE, owner_hhE, renter_hhE, hu_totE, hu_recent20E,
             pop25E, baplusE, pop_totE, white_nhE, black_nhE, asian_nhE, hispE),
           ~ sum(.x * w, na.rm = TRUE)),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 1.3 Derived indicators per period and the tract panel
# -----------------------------------------------------------------------------
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
      share_poc       = 1 - (white_nhE / pop_totE),        # communities of colour (UDP)
      # Percentile positions (0–1) within DC (McKinnish et al., 2010)
      pct_rank_inc    = percent_rank(med_incE),
      pct_rank_rent   = percent_rank(med_rentE),
      pct_rank_val    = percent_rank(med_valE),
      pct_rank_ba     = percent_rank(share_ba_plus),
      # Composite housing-cost percentile (rent + sale value)
      pct_rank_hous   = coalesce(0.5 * pct_rank_rent + 0.5 * pct_rank_val, pct_rank_rent, pct_rank_val),
      # Continuous socio-economic status / gentrification index G_i,y
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
  inner_join(old_ind %>% select(all_of(keep_cols)) %>% rename_with(~ paste0(.x, "_10"), -GEOID), by = "GEOID") %>%
  filter(hhE_23 >= MIN_TRACT_HH, hhE_10 >= MIN_TRACT_HH)

# -----------------------------------------------------------------------------
# 1.4 Gentrification models (Freeman, McKinnish, UDP & Guerrieri)
# -----------------------------------------------------------------------------
# 1.4.1 Spatial adjacency (Guerrieri, Hartley & Hurst, 2013)
panel_sf_temp <- tracts_geo %>% inner_join(panel_df, by = "GEOID")
touches_list  <- sf::st_touches(panel_sf_temp)
stopifnot(identical(panel_sf_temp$GEOID, panel_df$GEOID))   # adjacency vectors are assigned by row order

# Initially affluent tracts: top 30% of income OR housing cost in 2010
affluent_10_idx <- which(panel_sf_temp$pct_rank_inc_10 >= 0.70 | panel_sf_temp$pct_rank_hous_10 >= 0.70)
borders_affluent_10 <- sapply(touches_list, function(nb) any(nb %in% affluent_10_idx))

# 1.4.2 Apply the models to the panel
panel_df <- panel_df %>%
  mutate(
    borders_affluent_10 = borders_affluent_10,
    # --- Real absolute and relative changes (2010 → 2023) ---
    d_med_inc      = med_incE_23 - med_incE_10,
    pct_med_inc    = (med_incE_23 / med_incE_10) - 1,
    d_med_rent     = med_rentE_23 - med_rentE_10,
    pct_med_rent   = (med_rentE_23 / med_rentE_10) - 1,
    d_med_val      = med_valE_23 - med_valE_10,
    pct_med_val    = (med_valE_23 / med_valE_10) - 1,
    d_share_ba     = share_ba_plus_23 - share_ba_plus_10,
    d_share_renter = share_renter_23 - share_renter_10,
    # Racial composition change (exogenous validation — Hyra, 2017)
    d_share_white  = share_white_23 - share_white_10,
    d_share_black  = share_black_23 - share_black_10,
    d_share_hisp   = share_hisp_23 - share_hisp_10,
    # Statistical significance at 90%
    sig_inc        = sig_diff(d_med_inc, med_incM_23, med_incM_10),
    sig_rent       = sig_diff(d_med_rent, med_rentM_23, med_rentM_10),
    sig_ba         = sig_diff(d_share_ba, share_ba_plusM_23, share_ba_plusM_10),
    # --- MODEL 1: continuous percentile index (McKinnish et al., 2010) ---
    d_pct_inc      = pct_rank_inc_23 - pct_rank_inc_10,
    d_pct_hous     = pct_rank_hous_23 - pct_rank_hous_10,
    d_pct_ba       = pct_rank_ba_23 - pct_rank_ba_10,
    d_G_index      = G_index_23 - G_index_10,
    # --- MODEL 2: Lance Freeman (2005), as implemented ---
    # Stage 1 (eligibility): MHI below the 40th tract percentile AND older housing stock
    freeman_elig   = med_incE_10 <= quantile(med_incE_10, 0.40, na.rm = TRUE) &
                     share_recent_hu_10 <= quantile(share_recent_hu_10, 0.60, na.rm = TRUE),
    # Stage 2 (occurrence): Δ%BA+ above the median AND real appreciation (rent or value) > 0
    freeman_gent   = freeman_elig &
                     d_share_ba > median(d_share_ba, na.rm = TRUE) &
                     (coalesce(d_med_rent > 0, FALSE) | coalesce(d_med_val > 0, FALSE)),
    # --- MODEL 3: Urban Displacement Project (UDP / Bates) + contagion ---
    # Baseline economic-vulnerability score (quintiles 1–5 → sum 4–20)
    q_renter_10    = ntile(share_renter_10, 5),
    q_poc_10       = ntile(share_poc_10, 5),
    q_lowinc_10    = ntile(-med_incE_10, 5),       # inverted: lower income = more vulnerable (5)
    q_noba_10      = ntile(-share_ba_plus_10, 5),  # inverted: lower %BA+ = more vulnerable (5)
    udp_vuln_score = q_renter_10 + q_poc_10 + q_lowinc_10 + q_noba_10,
    udp_vulnerable = udp_vuln_score >= 12 | pct_rank_inc_10 <= 0.50,
    # Acceleration vectors (above the city-wide median, 2010 → 2023)
    demo_surge     = (d_share_ba > median(d_share_ba, na.rm = TRUE)) |
                     (d_med_inc  > median(d_med_inc, na.rm = TRUE)),
    market_surge   = coalesce(d_med_rent > median(d_med_rent, na.rm = TRUE), FALSE) |
                     coalesce(d_med_val  > median(d_med_val, na.rm = TRUE), FALSE) |
                     (d_pct_hous > 0.05)
  )

# Contagion frontier: adjacency to affluent OR dynamically gentrifying tracts
dyn_idx <- which(panel_df$udp_vulnerable & panel_df$demo_surge & panel_df$market_surge)
borders_frontline <- sapply(touches_list, function(nb) any(nb %in% union(affluent_10_idx, dyn_idx)))

panel_df <- panel_df %>%
  mutate(
    borders_frontline = borders_frontline,
    typology_udp = case_when(
      !udp_vulnerable                              ~ TYPOLOGY_LEVELS[5],
      udp_vulnerable & demo_surge & market_surge   ~ TYPOLOGY_LEVELS[1],
      udp_vulnerable & (demo_surge | market_surge) ~ TYPOLOGY_LEVELS[2],
      udp_vulnerable & borders_frontline           ~ TYPOLOGY_LEVELS[3],
      TRUE                                         ~ TYPOLOGY_LEVELS[4]
    ),
    typology_udp = factor(typology_udp, levels = TYPOLOGY_LEVELS)
  )

panel_sf <- tracts_geo %>% inner_join(panel_df, by = "GEOID")

# -----------------------------------------------------------------------------
# 1.5 Summary tables and spatial econometric models
# -----------------------------------------------------------------------------
typology_summary <- panel_df %>%
  group_by(typology_udp) %>%
  summarise(
    tracts               = n(),
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

cat("\n=== GENTRIFICATION TYPOLOGY SUMMARY, DC (CONSTANT 2023 DOLLARS) ===\n")
print(as.data.frame(typology_summary))

# Guerrieri, Hartley & Hurst (2013): rent-gap closure and contagion
mod_guerrieri_val <- lm(pct_med_val ~ log(med_valE_10) + borders_affluent_10 + d_share_ba,
                        data = panel_df %>% filter(!is.na(pct_med_val), med_valE_10 > 0))
mod_guerrieri_rent <- lm(pct_med_rent ~ log(med_rentE_10) + borders_affluent_10 + d_share_ba,
                         data = panel_df %>% filter(!is.na(pct_med_rent), med_rentE_10 > 0))
mod_cappuccino <- lm(d_share_black ~ d_G_index + udp_vuln_score, data = panel_df)

econ_models <- bind_rows(
  tidy(mod_guerrieri_val, conf.int = TRUE)  %>% mutate(model = "M1: Real home-value appreciation (Guerrieri)"),
  tidy(mod_guerrieri_rent, conf.int = TRUE) %>% mutate(model = "M2: Real rent growth (Guerrieri)"),
  tidy(mod_cappuccino, conf.int = TRUE)     %>% mutate(model = "M3: Change in Black share (Hyra)")
) %>% relocate(model)

cat("\n=== ECONOMETRIC MODELS (RENT GAP, CONTAGION, RACIAL CHANGE) ===\n")
print(as.data.frame(econ_models %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))

# -----------------------------------------------------------------------------
# 1.6 Figures G1–G8
# -----------------------------------------------------------------------------
# ---- G1: Multi-stage typology map (UDP + spatial contagion)
p_g1 <- ggplot(panel_sf) +
  geom_sf(aes(fill = typology_udp), colour = "white", linewidth = 0.15) +
  scale_fill_manual(values = POSTER$typology, name = "Neighborhood stage") +
  labs(title    = "Multi-stage gentrification typology, Washington, D.C.",
       subtitle = paste0(P_OLD, " → ", P_NEW, " (UDP typology + Guerrieri contagion frontier)"),
       caption  = "Source: U.S. Census Bureau, ACS 5-Year (B19013, B25064, B25077, B15002, B03002). Constant 2023 dollars.") +
  theme_map_poster()
save_plot(p_g1, "G1_map_udp_typology.png", w = 9, h = 7.5)

# ---- G2: Continuous McKinnish index (ΔG) + Freeman outline
p_g2 <- ggplot(panel_sf) +
  geom_sf(aes(fill = d_G_index), colour = "white", linewidth = 0.12) +
  geom_sf(data = filter(panel_sf, freeman_gent %in% TRUE), fill = NA, colour = "black", linewidth = 0.45) +
  scale_fill_div(name = expression(Delta * G[i]), limits = c(-0.25, 0.25)) +
  labs(title    = "Continuous gentrification intensity (McKinnish et al., 2010 index)",
       subtitle = paste0("Change in composite relative position (income, housing, education), ", P_OLD, " → ", P_NEW,
                         "\nBlack outline = tracts gentrified under Freeman's (2005) binary criterion"),
       caption  = "Source: ACS 5-Year. Red = tract rose in DC's hierarchy; blue = fell.") +
  theme_map_poster()
save_plot(p_g2, "G2_map_continuous_index_dG.png", w = 8.5, h = 7.5)

# ---- G3: Quadrant diagram (cultural capital vs. housing capital)
med_ba_chg   <- median(panel_df$d_share_ba, na.rm = TRUE) * 100
med_hous_chg <- median(panel_df$d_pct_hous, na.rm = TRUE) * 100

p_g3 <- panel_df %>%
  filter(udp_vulnerable) %>%
  ggplot(aes(x = d_share_ba * 100, y = d_pct_hous * 100, colour = typology_udp)) +
  geom_vline(xintercept = med_ba_chg, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = med_hous_chg, linetype = "dashed", colour = "grey40") +
  geom_point(aes(size = pop_totE_23), alpha = 0.8) +
  scale_colour_manual(values = POSTER$typology, name = NULL) +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  annotate("text", x = max(panel_df$d_share_ba * 100, na.rm = TRUE) * 0.75,
           y = max(panel_df$d_pct_hous * 100, na.rm = TRUE) * 0.85,
           label = "Dynamic gentrification\n(education inflow + housing shock)",
           fontface = "bold", size = 3.2, colour = POSTER$typology[1]) +
  labs(title    = "The drivers of gentrification in DC's vulnerable tracts",
       subtitle = "Inflow of college-educated residents (% BA+) vs. jump in the housing-cost percentile",
       x        = "Change in % of residents with a bachelor's degree or higher (pct. points)",
       y        = "Change in relative housing-cost position (percentile points)",
       caption  = "Dashed lines = DC medians. Point size proportional to 2023 tract population.") +
  theme_poster() + guides(colour = guide_legend(nrow = 2))
save_plot(p_g3, "G3_quadrant_drivers_ba_vs_housing.png", w = 9.5, h = 6.5)

# ---- G4: Rent-gap closure and endogenous contagion (Guerrieri, Hartley & Hurst, 2013)
p_g4 <- panel_df %>%
  filter(!is.na(pct_med_val), med_valE_10 > 0, pct_med_val < 3) %>%
  mutate(adj_label = if_else(borders_affluent_10, names(POSTER$adj)[1], names(POSTER$adj)[2])) %>%
  ggplot(aes(x = med_valE_10 / 1000, y = pct_med_val * 100, colour = adj_label)) +
  geom_point(alpha = 0.65, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  scale_x_log10(labels = dollar_format(suffix = "k")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_colour_manual(values = POSTER$adj, name = "Spatial topology (Guerrieri et al.)") +
  labs(title    = "Empirical evidence of rent-gap closure and spatial contagion in DC",
       subtitle = "Tracts with lower initial values (log scale) and bordering affluent areas appreciate the most",
       x        = "Median home value, 2006–2010 (log scale, constant 2023 dollars)",
       y        = "Real home-value appreciation, 2010 → 2023 (%)",
       caption  = "Guerrieri et al. (2013): dP/P = mu + omega · ln(HP_t0). Source: ACS 5-Year (B25077).") +
  theme_poster()
save_plot(p_g4, "G4_rent_gap_guerrieri_curve.png", w = 9.5, h = 6)

# ---- G5: Exogenous validation — the "Cappuccino City" effect (racial change by stage)
p_g5 <- panel_df %>%
  select(GEOID, typology_udp, d_share_black, d_share_white, d_share_hisp) %>%
  pivot_longer(c(d_share_black, d_share_white, d_share_hisp), names_to = "race_group", values_to = "change_pts") %>%
  mutate(change_pts = change_pts * 100,
         race_group = recode(race_group,
                             d_share_black = "Black (non-Hispanic)",
                             d_share_white = "White (non-Hispanic)",
                             d_share_hisp  = "Hispanic / Latino"),
         race_group = factor(race_group, levels = names(POSTER$race))) %>%
  ggplot(aes(x = typology_udp, y = change_pts, fill = race_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_boxplot(alpha = 0.9, outlier.size = 0.9, position = position_dodge(0.8)) +
  scale_fill_manual(values = POSTER$race, name = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  labs(title    = "Exogenous validation: Hyra's \"Cappuccino City\" effect in Washington, D.C.",
       subtitle = paste0("Racial composition change, ", P_OLD, " → ", P_NEW, ", by gentrification stage"),
       x = NULL, y = "Change in share of tract population (pct. points)",
       caption  = "Note: race is excluded from the mathematical definition of gentrification to avoid tautology (exogenous validation).") +
  theme_poster()
save_plot(p_g5, "G5_cappuccino_city_racial_shift.png", w = 10.5, h = 6)

# ---- G6: Dumbbell decomposition by dimension (2010 vs. 2023)
p_g6 <- panel_df %>%
  group_by(typology_udp) %>%
  summarise(
    Income_10    = mean(pct_rank_inc_10, na.rm = TRUE) * 100,
    Income_23    = mean(pct_rank_inc_23, na.rm = TRUE) * 100,
    Education_10 = mean(pct_rank_ba_10, na.rm = TRUE) * 100,
    Education_23 = mean(pct_rank_ba_23, na.rm = TRUE) * 100,
    Housing_10   = mean(pct_rank_hous_10, na.rm = TRUE) * 100,
    Housing_23   = mean(pct_rank_hous_23, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(-typology_udp, names_to = c("dimension", "year"), names_sep = "_") %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "p_") %>%
  mutate(
    typology_rev = factor(typology_udp, levels = rev(levels(typology_udp))),
    dimension = recode(dimension,
                       "Education" = "1. Cultural capital (% BA+)",
                       "Housing"   = "2. Housing cost (rent + value)",
                       "Income"    = "3. Household income (MHI)")
  ) %>%
  ggplot(aes(y = typology_rev)) +
  geom_segment(aes(x = p_10, xend = p_23, yend = typology_rev), colour = "grey70", linewidth = 1.2) +
  geom_point(aes(x = p_10, colour = P_OLD), size = 3.2) +
  geom_point(aes(x = p_23, colour = P_NEW), size = 3.2) +
  facet_wrap(~ dimension) +
  scale_colour_manual(values = POSTER$period, name = "Period") +
  labs(title    = "Vector decomposition of gentrification by dimension (2010 vs. 2023)",
       subtitle = "Mean percentile position (0–100) of each neighborhood group in DC's urban hierarchy",
       x = "Mean percentile in Washington, D.C.", y = NULL,
       caption  = "Source: ACS 5-Year. The education shock (% BA+) pulls housing repricing.") +
  theme_poster()
save_plot(p_g6, "G6_percentile_trajectories_dumbbell.png", w = 11, h = 5.5)

# ---- G7: Baseline vulnerability (UDP) vs. change in the G index (non-linear)
p_g7 <- ggplot(panel_df, aes(x = udp_vuln_score, y = d_G_index, colour = typology_udp)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_jitter(width = 0.25, alpha = 0.75, size = 2.4) +
  geom_smooth(aes(group = 1), method = "loess", colour = "black", se = TRUE) +
  scale_colour_manual(values = POSTER$typology, name = NULL) +
  labs(title    = "Pressure curve: baseline vulnerability (2010) vs. relative ascent (ΔG)",
       subtitle = "Moderately-to-highly vulnerable neighborhoods (scores 12–16) near the core restructure the most",
       x = "UDP economic-vulnerability score, 2006–2010 (4–20)",
       y = expression("Change in gentrification index " * (Delta * G[i])),
       caption  = "Black curve = non-parametric LOESS fit with 95% CI.") +
  theme_poster() + guides(colour = guide_legend(nrow = 2))
save_plot(p_g7, "G7_vulnerability_vs_dG.png", w = 9.5, h = 6)

# ---- G8: 2×2 spatial panel of the four census indicators
map_long <- panel_sf %>%
  transmute(
    GEOID,
    `1. Education: +pts % BA+`        = percent_rank(d_share_ba) * 100,
    `2. Income: real MHI growth`      = percent_rank(d_med_inc) * 100,
    `3. Rent: real gross-rent growth` = percent_rank(d_med_rent) * 100,
    `4. Home value: real appreciation`= percent_rank(d_med_val) * 100
  ) %>%
  pivot_longer(cols = -c(GEOID, geometry), names_to = "indicator", values_to = "change_percentile")

p_g8 <- ggplot(map_long) +
  geom_sf(aes(fill = change_percentile), colour = "white", linewidth = 0.08) +
  facet_wrap(~ indicator, ncol = 2) +
  scale_fill_seq(name = "Percentile of\nchange (0–100)") +
  labs(title    = "Comparative geography of the four census indicators of gentrification in DC",
       subtitle = paste0("Percentile of the real change between ", P_OLD, " and ", P_NEW, ", by census tract"),
       caption  = "Source: ACS 5-Year (B15002, B19013, B25064, B25077).") +
  theme_map_poster()
save_plot(p_g8, "G8_map_4_pillars_comparison.png", w = 10, h = 9)

# -----------------------------------------------------------------------------
# 1.7 Export
# -----------------------------------------------------------------------------
results_gent <- list(typology_summary = typology_summary, econ_models = econ_models, tract_panel_gent = panel_df)
write_xlsx(results_gent, file.path(OUT_DIR, "dc_gentrification_models.xlsx"))
write_csv(panel_df, file.path(OUT_DIR, "dc_tract_gentrification_panel.csv"))
cat("\nPart 1 done. Figures G1–G8, CSV and Excel saved in ./", OUT_DIR, "\n", sep = "")

# =============================================================================
# PART 2 — FROM THE GENTRIFICATION PROFILE TO THE COMMUTING PROFILE
#   Block H  : income geography (mean/median income, 4 brackets, mode shares)
#   Part A   : DC tracts — travel time, mode, transit penalty vs. typology
#   Part B   : metropolitan counties (5-year + annual 1-year series)
#   Part C   : DC as workplace (S0804) vs. place of residence (S0802)
#   Part D   : PUMS microdata — flows × 4 wage bands, mode, home state, DiD
# NOTE: Part 2 overwrites `panel_df` from Part 1 with a panel that also carries
#       the commuting variables (same typology rules).
# =============================================================================
OUT_DIR   <- "outputs_part2_unified"
CACHE_DIR <- file.path("outputs", "cache")   # reuses an existing PUMS cache
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
results <- list()

RES_STATES          <- c("DC", "MD", "VA", "WV", "PA", "DE")   # states sending commuters into DC
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

# -----------------------------------------------------------------------------
# 2.1 ACS subject tables (S0802 residence / S0804 workplace) and detail tables
# -----------------------------------------------------------------------------
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
  # Median income, aggregate income (exact mean = B19025/B11001), rent and home value
  med_inc     = "B19013_001",
  agg_inc     = "B19025_001",
  med_rent    = "B25064_001",
  med_val     = "B25077_001",
  # Households and tenure
  hh          = "B11001_001",
  tenure_tot  = "B25003_001",
  owner_hh    = "B25003_002",
  renter_hh   = "B25003_003",
  # Age of housing stock (Freeman's physical-disinvestment criterion)
  hu_tot      = "B25034_001", hu_new1 = "B25034_002", hu_new2 = "B25034_003", hu_new3 = "B25034_004",
  # Education (B15002 — population 25+ with BA or higher)
  pop25       = "B15002_001",
  ba_m        = "B15002_015", ma_m = "B15002_016", prof_m = "B15002_017", phd_m = "B15002_018",
  ba_f        = "B15002_032", ma_f = "B15002_033", prof_f = "B15002_034", phd_f = "B15002_035",
  # Race / ethnicity (B03002 — UDP vulnerability and exogenous validation)
  pop_tot     = "B03002_001", white_nh = "B03002_003", black_nh = "B03002_004", hisp = "B03002_012",
  # Household income brackets (B19001)
  inc_tot     = "B19001_001",
  inc_0_10    = "B19001_002", inc_10_15  = "B19001_003", inc_15_20   = "B19001_004",
  inc_20_25   = "B19001_005", inc_25_30  = "B19001_006", inc_30_35   = "B19001_007",
  inc_35_40   = "B19001_008", inc_40_45  = "B19001_009",                       # ≤ $45k
  inc_45_50   = "B19001_010", inc_50_60  = "B19001_011", inc_60_75   = "B19001_012",  # $45k–$75k
  inc_75_100  = "B19001_013", inc_100_125= "B19001_014",                       # $75k–$125k
  inc_125_150 = "B19001_015", inc_150_200= "B19001_016", inc_200p    = "B19001_017",  # > $125k
  # Mobility, telework and travel time
  tot_work    = "B08007_001", work_out_dc = "B08007_005",
  w_mode_tot  = "B08301_001", w_wfh       = "B08301_021",
  notwfh      = "B08303_001", t45 = "B08303_011", t60 = "B08303_012", t90 = "B08303_013",
  agg_time    = "B08013_001"
)

get_dc <- function(year, geography, geometry = FALSE) {
  message(sprintf("Downloading ACS 5-year (%d) for DC [%s]...", year, geography))
  subj <- get_acs(geography = geography, state = "DC", year = year, survey = "acs5",
                  variables = build_subject_vars(year, "S0802"), output = "wide",
                  geometry = geometry, progress_bar = FALSE)
  det  <- get_acs(geography = geography, state = "DC", year = year, survey = "acs5",
                  variables = B_VARS, output = "wide", progress_bar = FALSE) %>% select(-NAME)
  subj %>%
    left_join(det, by = "GEOID") %>%
    mutate(
      baplusE      = ba_mE + ma_mE + prof_mE + phd_mE + ba_fE + ma_fE + prof_fE + phd_fE,
      hu_recent20E = hu_new1E + hu_new2E + hu_new3E,
      hh_0_45kE    = inc_0_10E + inc_10_15E + inc_15_20E + inc_20_25E + inc_25_30E + inc_30_35E + inc_35_40E + inc_40_45E,
      hh_45_75kE   = inc_45_50E + inc_50_60E + inc_60_75E,
      hh_75_125kE  = inc_75_100E + inc_100_125E,
      hh_125kpE    = inc_125_150E + inc_150_200E + inc_200pE
    )
}

adjust_dollars <- function(df, ratio) {
  df %>% mutate(across(c(med_incE, med_incM, agg_incE, agg_incM, med_rentE, med_rentM, med_valE, med_valM,
                         starts_with("earn_") & ends_with("E")), ~ .x * ratio))
}

derive <- function(df) {
  df %>%
    mutate(
      # Exact mean household income (B19025 / B11001)
      avg_incE          = agg_incE / hhE,
      avg_incM          = moe_ratio_(agg_incE, hhE, agg_incM, hhM),
      # Mobility indicators
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
      transit_reliable  = workers_transitE >= MIN_MODE_WORKERS & workers_driveE >= MIN_MODE_WORKERS & transit_cv <= MAX_CV,
      earn_ratio_transit_drive = earn_transitE / earn_driveE,
      # Gentrification indicators and income composition
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
      # Continuous percentile index G_i,y (McKinnish et al., 2010)
      pct_rank_inc      = percent_rank(med_incE),
      pct_rank_rent     = percent_rank(med_rentE),
      pct_rank_val      = percent_rank(med_valE),
      pct_rank_ba       = percent_rank(share_ba_plus),
      pct_rank_hous     = coalesce(0.5 * pct_rank_rent + 0.5 * pct_rank_val, pct_rank_rent, pct_rank_val),
      G_index           = (pct_rank_inc + pct_rank_hous + pct_rank_ba) / 3
    )
}

new_raw    <- get_dc(YEAR_NEW, "tract", geometry = TRUE)
tracts_geo <- new_raw %>% select(GEOID, NAME)
new_tr     <- new_raw %>% sf::st_drop_geometry()
old_tr     <- get_dc(YEAR_OLD, "tract", geometry = FALSE) %>% adjust_dollars(CPI_RATIO)

# Longitudinal crosswalk (2010 tracts → 2020 tracts)
xw <- load_crosswalk("11")

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
  inner_join(old_ind %>% select(all_of(keep_cols)) %>% rename_with(~ paste0(.x, "_10"), -GEOID), by = "GEOID") %>%
  filter(workers_totalE_23 >= MIN_TRACT_WORKERS)

# Spatial adjacency for the contagion frontier (Guerrieri et al., 2013)
panel_sf_temp   <- tracts_geo %>% inner_join(panel_df, by = "GEOID")
touches_list    <- sf::st_touches(panel_sf_temp)
stopifnot(identical(panel_sf_temp$GEOID, panel_df$GEOID))
affluent_10_idx <- which(panel_sf_temp$pct_rank_inc_10 >= 0.70 | panel_sf_temp$pct_rank_hous_10 >= 0.70)

panel_df <- panel_df %>%
  mutate(
    # Mobility changes
    d_time_total     = time_totalE_23 - time_totalE_10,
    d_time_transit   = if_else(transit_reliable_23 & transit_reliable_10, time_transitE_23 - time_transitE_10, NA_real_),
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
    # Income and gentrification changes (constant 2023 dollars)
    d_avg_inc        = avg_incE_23 - avg_incE_10,
    pct_avg_inc      = (avg_incE_23 / avg_incE_10) - 1,
    d_med_inc        = med_incE_23 - med_incE_10,
    pct_med_inc      = (med_incE_23 / med_incE_10) - 1,
    d_med_rent       = med_rentE_23 - med_rentE_10,
    d_med_val        = med_valE_23 - med_valE_10,
    d_share_ba       = share_ba_plus_23 - share_ba_plus_10,
    d_pct_hous       = pct_rank_hous_23 - pct_rank_hous_10,
    d_G_index        = G_index_23 - G_index_10,
    # Change in the 4 tract income brackets (pct. points)
    d_hh_0_45k_pts   = (share_hh_0_45k_23 - share_hh_0_45k_10) * 100,
    d_hh_45_75k_pts  = (share_hh_45_75k_23 - share_hh_45_75k_10) * 100,
    d_hh_75_125k_pts = (share_hh_75_125k_23 - share_hh_75_125k_10) * 100,
    d_hh_125kp_pts   = (share_hh_125kp_23 - share_hh_125kp_10) * 100,
    # Gentrification classification (UDP + Freeman) — same rules as Part 1
    q_renter_10      = ntile(share_renter_10, 5),
    q_poc_10         = ntile(share_poc_10, 5),
    q_lowinc_10      = ntile(-med_incE_10, 5),
    q_noba_10        = ntile(-share_ba_plus_10, 5),
    udp_vuln_score   = q_renter_10 + q_poc_10 + q_lowinc_10 + q_noba_10,
    udp_vulnerable   = udp_vuln_score >= 12 | pct_rank_inc_10 <= 0.50,
    demo_surge       = (d_share_ba > median(d_share_ba, na.rm = TRUE)) | (d_med_inc > median(d_med_inc, na.rm = TRUE)),
    market_surge     = coalesce(d_med_rent > median(d_med_rent, na.rm = TRUE), FALSE) |
                       coalesce(d_med_val  > median(d_med_val, na.rm = TRUE), FALSE) | (d_pct_hous > 0.05),
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
      !udp_vulnerable                              ~ TYPOLOGY_LEVELS[5],
      udp_vulnerable & demo_surge & market_surge   ~ TYPOLOGY_LEVELS[1],
      udp_vulnerable & (demo_surge | market_surge) ~ TYPOLOGY_LEVELS[2],
      udp_vulnerable & borders_frontline           ~ TYPOLOGY_LEVELS[3],
      TRUE                                         ~ TYPOLOGY_LEVELS[4]
    ),
    typology_udp = factor(typology_udp, levels = TYPOLOGY_LEVELS),
    gent_dynamic = typology_udp == TYPOLOGY_LEVELS[1]
  )

panel <- tracts_geo %>% inner_join(panel_df, by = "GEOID")

# -----------------------------------------------------------------------------
# BLOCK H — income geography: mean, median, 4 brackets and mode shares
# -----------------------------------------------------------------------------
period_label <- function(x, old_name, new_name) {
  if_else(x == old_name, paste0("Baseline: ", P_OLD, " (constant 2023 US$)"), paste0("Endline: ", P_NEW, " (constant 2023 US$)"))
}

# H1: mean household income (B19025 / B11001)
income_mean_long <- panel %>%
  select(GEOID, avg_incE_10, avg_incE_23) %>%
  pivot_longer(c(avg_incE_10, avg_incE_23), names_to = "period", values_to = "mean_income") %>%
  mutate(period = period_label(period, "avg_incE_10", "avg_incE_23"))

p_h1 <- ggplot(income_mean_long) +
  geom_sf(aes(fill = mean_income / 1000), colour = "white", linewidth = 0.08) +
  facet_wrap(~ period, ncol = 2) +
  scale_fill_seq(name = "Mean household\nincome", labels = dollar_format(suffix = "k"), limits = c(25, 300), oob = squish) +
  labs(title    = "Mean household income by census tract, Washington, D.C.",
       subtitle = "2006–2010 vs. 2019–2023 in constant 2023 dollars (B19025 / B11001)",
       caption  = "Source: U.S. Census Bureau, ACS 5-Year. 2010 values CPI-adjusted and converted to 2020 tract boundaries.") +
  theme_map_poster()
save_plot(p_h1, "H1_heatmap_avg_income_2010_vs_2023.png", w = 11, h = 6)

# H2: median household income (B19013)
income_med_long <- panel %>%
  select(GEOID, med_incE_10, med_incE_23) %>%
  pivot_longer(c(med_incE_10, med_incE_23), names_to = "period", values_to = "median_income") %>%
  mutate(period = period_label(period, "med_incE_10", "med_incE_23"))

p_h2 <- ggplot(income_med_long) +
  geom_sf(aes(fill = median_income / 1000), colour = "white", linewidth = 0.08) +
  facet_wrap(~ period, ncol = 2) +
  scale_fill_seq(name = "Median household\nincome", labels = dollar_format(suffix = "k"), limits = c(20, 220), oob = squish) +
  labs(title    = "Median household income by census tract, Washington, D.C.",
       subtitle = "High-income strata expand from the Northwest into the core, Shaw, H St NE and Navy Yard (2010 vs. 2023)",
       caption  = "Source: U.S. Census Bureau, ACS 5-Year (B19013). Constant 2023 dollars.") +
  theme_map_poster()
save_plot(p_h2, "H2_heatmap_median_income_2010_vs_2023.png", w = 11, h = 6)

# H3: change in the 4 household income brackets (B19001)
brackets_map_long <- panel %>%
  select(GEOID, d_hh_0_45k_pts, d_hh_45_75k_pts, d_hh_75_125k_pts, d_hh_125kp_pts) %>%
  pivot_longer(c(d_hh_0_45k_pts, d_hh_45_75k_pts, d_hh_75_125k_pts, d_hh_125kp_pts), names_to = "bracket", values_to = "delta_pts") %>%
  mutate(bracket = recode(bracket,
                          d_hh_0_45k_pts   = "1. Low ($0–$45k)",
                          d_hh_45_75k_pts  = "2. Lower-mid ($45k–$70/75k)",
                          d_hh_75_125k_pts = "3. Upper-mid ($70/75k–$120/125k)",
                          d_hh_125kp_pts   = "4. High (> $120/125k)"))

p_h3 <- ggplot(brackets_map_long) +
  geom_sf(aes(fill = delta_pts), colour = "white", linewidth = 0.08) +
  facet_wrap(~ bracket, ncol = 2) +
  scale_fill_div(name = "Change\n(pct. points)", limits = c(-25, 25)) +
  labs(title    = "Replacement of the 4 income brackets by census tract, DC (2010 → 2023)",
       subtitle = "Blue = bracket shrank in the neighborhood | Red = bracket grew",
       caption  = "Source: ACS 5-Year (B19001). B19001 cut-points (45/75/125k) approximate the PUMS bands (45/70/120k).") +
  theme_map_poster()
save_plot(p_h3, "H3_heatmap_4_income_brackets_shift.png", w = 10, h = 9)

# H4: 2×2 map of mode-share change (spatial mirror of D4 and D6)
modes_map_long <- panel %>%
  mutate(d_wfh_pts = d_wfh_share * 100, d_transit_pts = d_transit_share * 100,
         d_drive_pts = d_drive_share * 100, d_out_dc_pts = d_share_out_dc * 100) %>%
  select(GEOID, d_wfh_pts, d_transit_pts, d_drive_pts, d_out_dc_pts) %>%
  pivot_longer(c(d_wfh_pts, d_transit_pts, d_drive_pts, d_out_dc_pts), names_to = "mode_metric", values_to = "delta_pts") %>%
  mutate(mode_metric = recode(mode_metric,
                              d_wfh_pts     = "1. Worked from home (pct. points)",
                              d_transit_pts = "2. Public transit (pct. points)",
                              d_drive_pts   = "3. Drove alone (pct. points)",
                              d_out_dc_pts  = "4. Works outside DC (pct. points)"))

p_h4 <- ggplot(modes_map_long) +
  geom_sf(aes(fill = delta_pts), colour = "white", linewidth = 0.08) +
  geom_sf(data = filter(panel, gent_dynamic %in% TRUE) %>% st_union(), fill = NA, colour = "black", linewidth = 0.4) +
  facet_wrap(~ mode_metric, ncol = 2) +
  scale_fill_div(name = "Change\n(pct. points)", limits = c(-25, 25)) +
  labs(title    = "Geography of modal change in DC tracts (spatial mirror of D4 and D6)",
       subtitle = paste0("Change in percentage points (", P_OLD, " → ", P_NEW, "). Black outline = dynamic gentrification"),
       caption  = "Source: ACS 5-Year (S0802, B08301, B08007).") +
  theme_map_poster()
save_plot(p_h4, "H4_heatmap_mode_shares_dc_tracts.png", w = 10.5, h = 9)

# -----------------------------------------------------------------------------
# PART A — tables, regressions and figures (gentrification × travel time)
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
cat("\n=== A. COMMUTE CHANGE BY GENTRIFICATION TYPOLOGY ===\n"); print(as.data.frame(typology_commute_summary))

eligible_df <- panel_df %>% filter(udp_vulnerable)
mod_total   <- lm(d_time_total ~ gent_dynamic + d_G_index + time_totalE_10 + transit_share_10 + d_transit_share,
                  data = eligible_df, weights = workers_totalE_23)
mod_long    <- lm(d_45plus ~ gent_dynamic + d_G_index + share_45plus_10 + transit_share_10 + d_share_out_dc,
                  data = eligible_df, weights = workers_totalE_23)
mod_penalty <- lm(transit_penalty_23 ~ gent_dynamic + d_G_index + transit_share_23 + share_out_dc_23,
                  data = eligible_df %>% filter(transit_reliable_23 %in% TRUE), weights = workers_transitE_23)

tract_models <- bind_rows(
  tidy(mod_total, conf.int = TRUE)   %>% mutate(model = "A1: change in mean commute (min)"),
  tidy(mod_long, conf.int = TRUE)    %>% mutate(model = "A2: change in share commuting 45+ min"),
  tidy(mod_penalty, conf.int = TRUE) %>% mutate(model = "A3: transit time penalty 2023")
) %>% relocate(model)

results[c("A_citywide_by_mode", "A_earnings_by_mode", "A_typology_commute_summary", "A_tract_models", "A_tract_panel")] <-
  list(citywide, earnings, typology_commute_summary, tract_models, panel_df)

# ---- A1: DC residents' mean one-way commute by mode
p_a1 <- citywide %>%
  pivot_longer(c(min_10, min_23), names_to = "period", values_to = "minutes") %>%
  mutate(moe = if_else(period == "min_10", moe_10, moe_23),
         period = if_else(period == "min_10", P_OLD, P_NEW),
         mode = factor(mode, levels = citywide$mode)) %>%
  ggplot(aes(mode, minutes, fill = period)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  geom_errorbar(aes(ymin = minutes - moe, ymax = minutes + moe), position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = POSTER$period, name = NULL) +
  labs(title = "DC residents: mean one-way commute by mode", x = NULL, y = "Minutes",
       caption = "Source: ACS 5-year (S0802 & B08013/B08303). Error bars = 90% MOE.") +
  theme_poster()
save_plot(p_a1, "A1_commute_by_mode.png")

# ---- A2: median earnings by commute mode
p_a2 <- earnings %>%
  pivot_longer(-mode, names_to = "period", values_to = "earn") %>%
  mutate(period = if_else(period == "earn_10", P_OLD, P_NEW), mode = factor(mode, levels = earnings$mode)) %>%
  ggplot(aes(mode, earn, fill = period)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = POSTER$period, name = NULL) +
  labs(title = "Transit commuters earn less than solo drivers",
       subtitle = "Median earnings by commute mode, DC residents (constant 2023 dollars)", x = NULL, y = NULL,
       caption = paste0("Source: ACS 5-year (S0802). ", P_OLD, " values CPI-adjusted.")) +
  theme_poster()
save_plot(p_a2, "A2_earnings_by_mode.png")

# ---- A3: change in mobility by gentrification stage
p_a3 <- panel_df %>%
  pivot_longer(c(d_time_total, d_45plus, d_share_out_dc), names_to = "measure", values_to = "value") %>%
  mutate(value   = if_else(measure == "d_time_total", value, value * 100),
         measure = recode(measure,
                          d_time_total   = "1. Mean commute (minutes)",
                          d_45plus       = "2. Share commuting 45+ min (pct. pts)",
                          d_share_out_dc = "3. Share working outside DC (pct. pts)")) %>%
  ggplot(aes(typology_udp, value, fill = typology_udp)) +
  geom_boxplot(alpha = 0.85, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.35, size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  facet_wrap(~ measure, scales = "free_y") +
  scale_fill_manual(values = POSTER$typology) +
  scale_x_discrete(labels = function(x) str_wrap(x, 14)) +
  labs(title = paste0("Change in mobility by gentrification stage, ", P_OLD, " → ", P_NEW),
       x = NULL, y = "Change", caption = "Source: ACS 5-year (B08013, B08303, B08007).") +
  theme_poster() + theme(legend.position = "none", axis.text.x = element_text(size = 8))
save_plot(p_a3, "A3_commute_change_by_udp_typology.png", w = 12, h = 5.5)

# ---- A4: map of commute change with dynamic-gentrification outline
p_a4 <- ggplot(panel) +
  geom_sf(aes(fill = d_time_total), colour = "white", linewidth = 0.1) +
  geom_sf(data = filter(panel, gent_dynamic %in% TRUE), fill = NA, colour = POSTER$typology[1], linewidth = 0.45) +
  scale_fill_div(name = "Change\n(minutes)", limits = c(-12, 12)) +
  labs(title    = "Where DC residents' commutes got longer vs. dynamic gentrification",
       subtitle = paste0("Change in mean travel time (", P_OLD, " → ", P_NEW, "). Red outline = dynamic-gentrification tracts"),
       caption  = "Source: ACS 5-year (B08013/B08303).") +
  theme_map_poster()
save_plot(p_a4, "A4_map_commute_change_with_gent.png", w = 8, h = 7.5)

# ---- A5: ΔG vs. change in commute time (vulnerable tracts)
p_a5 <- panel_df %>%
  filter(udp_vulnerable) %>%
  ggplot(aes(x = d_G_index, y = d_time_total, colour = typology_udp)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = abs(d_hh_0_45k_pts)), alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "lm", colour = "black", se = TRUE) +
  scale_colour_manual(values = POSTER$typology, name = NULL) +
  scale_size_continuous(name = "Absolute change in\n$0–$45k bracket (pts)") +
  labs(title    = "Gentrification index (ΔG) vs. commute-time change in vulnerable tracts",
       subtitle = "Replacement by > $120k households does not lengthen the mean commute of central gentrified tracts",
       x = expression("Gentrification intensity " * (Delta * G[i])), y = "Change in mean commute time (minutes)",
       caption  = "Source: ACS 5-Year.") +
  theme_poster() + theme(legend.position = "right")
save_plot(p_a5, "A5_scatter_dG_vs_commute_composition.png", w = 10, h = 6)

# ---- A6: transit time penalty map
p_a6 <- ggplot(panel) +
  geom_sf(aes(fill = if_else(transit_reliable_23 %in% TRUE, transit_penalty_23, NA_real_)), colour = "white", linewidth = 0.1) +
  scale_fill_seq(name = "Extra minutes\nby transit") +
  labs(title = "The transit time penalty, 2019–2023",
       subtitle = "Mean transit commute minus mean drive-alone commute\nGrey = too few commuters or unreliable estimate",
       caption = "Source: ACS 5-year, Table S0802.") +
  theme_map_poster()
save_plot(p_a6, "A6_map_transit_penalty.png", w = 7.5, h = 7.5)

# =============================================================================
# PART B — Regional county comparison (5-Year + annual 1-Year series)
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

# ---- B1: county dumbbell
p_b1 <- region_change %>%
  mutate(county = reorder(county, mean_time_23)) %>%
  ggplot(aes(y = county)) +
  geom_segment(aes(x = mean_time_10, xend = mean_time_23, yend = county), colour = "grey70", linewidth = 1) +
  geom_point(aes(x = mean_time_10, colour = P_OLD), size = 2.8) +
  geom_point(aes(x = mean_time_23, colour = P_NEW, shape = sig_time), size = 2.8) +
  scale_colour_manual(values = POSTER$period, name = NULL) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), name = "Change significant (90%)") +
  facet_grid(ring ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Mean commute time of residents, DC region",
       x = "Minutes (workers who did not work from home)", y = NULL,
       caption = "Source: ACS 5-year, B08013 / B08303.") +
  theme_poster()
save_plot(p_b1, "B1_region_commute_dumbbell.png", w = 9, h = 7)

# ---- B2: out-of-state commuting vs. change in commute time
p_b2 <- ggplot(region_change, aes(share_out_state_23 * 100, d_mean_time, colour = ring)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = share_60plus_23 * 100)) +
  geom_text(aes(label = str_remove(county, ",.*")), size = 2.8, vjust = -1, show.legend = FALSE) +
  scale_colour_manual(values = POSTER$ring, name = NULL) +
  scale_size_continuous(name = "% commuting 60+ min\n(2019–2023)") +
  labs(title = "Out-of-state commuting and change in commute time",
       x = "% of resident workers working outside their state, 2019–2023 (mostly into DC)",
       y = "Change in mean commute (minutes)",
       caption = "Source: ACS 5-year, B08007, B08013, B08303.") +
  theme_poster()
save_plot(p_b2, "B2_region_outstate_vs_change.png", w = 9, h = 6)

region_1yr <- bind_rows(lapply(YEARS_1YR_REGION, function(y) {
  tryCatch(get_region(y, as.character(y), "acs1"), error = function(e) NULL)
})) %>% left_join(REGION, by = "GEOID")

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

  # ---- B3: annual trajectory by ring
  p_b3 <- ring_annual %>%
    pivot_longer(c(mean_time, share_60plus, share_wfh), names_to = "metric", values_to = "val") %>%
    mutate(metric = recode(metric,
                           mean_time    = "1. Mean commute time (min)",
                           share_60plus = "2. Long commutes ≥ 60 min (%)",
                           share_wfh    = "3. Worked from home (%)")) %>%
    ggplot(aes(year_num, val, colour = ring)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ metric, scales = "free_y") +
    scale_colour_manual(values = POSTER$ring, name = NULL) +
    labs(title    = "Annual commute trajectory: DC core vs. inner & outer suburbs (2010–2023)",
         subtitle = "ACS 1-Year series (counties with 65,000+ residents)",
         x = "Year (ACS 1-Year)", y = NULL,
         caption = "Source: U.S. Census Bureau, ACS 1-Year (2010–2023, except 2020).") +
    theme_poster()
  save_plot(p_b3, "B3_region_annual_1yr_trajectory.png", w = 12, h = 5)
}

results[c("B_region_long", "B_region_change")] <- list(region, region_change)

# =============================================================================
# PART C — DC as workplace (S0804) vs. place of residence (S0802)
# =============================================================================
get_dc_workplace <- function(year) {
  v <- build_subject_vars(year, "S0804", "acs5", strict = FALSE)
  tryCatch(get_acs(geography = "county", state = "DC", year = year, survey = "acs5",
                   variables = v, output = "wide", progress_bar = FALSE) %>% filter(GEOID == "11001"),
           error = function(e) { message("S0804 unavailable for ", year); NULL })
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

  # ---- C1
  p_c1 <- live_vs_work %>%
    mutate(mode = factor(mode, levels = unique(mode))) %>%
    ggplot(aes(mode, minutes, fill = period)) +
    geom_col(position = position_dodge(0.8), width = 0.75) +
    geom_errorbar(aes(ymin = minutes - moe, ymax = minutes + moe), position = position_dodge(0.8), width = 0.2) +
    facet_wrap(~ who) +
    scale_fill_manual(values = POSTER$period, name = NULL) +
    labs(title = "People who WORK in DC commute longer than people who LIVE in DC",
         subtitle = "Mean one-way commute by mode; 'Work in DC' includes MD/VA residents",
         x = NULL, y = "Minutes", caption = "Source: ACS 5-year, S0802 (residence) and S0804 (workplace).") +
    theme_poster() + theme(axis.text.x = element_text(angle = 20, hjust = 1))
  save_plot(p_c1, "C1_live_vs_work_in_dc.png", w = 10, h = 5)
}

# =============================================================================
# PART D — PUMS microdata standardized to the 4 wage bands
#          ($0–$45k, $45k–$70k, $70k–$120k, > $120k); figures D0–D8
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
    url <- sprintf("https://www2.census.gov/programs-surveys/acs/data/pums/%d/%s/csv_p%s.zip", year, folder, tolower(st))
    message("  downloading ", url)
    download.file(url, zip, mode = "wb", quiet = TRUE)
  }
  csv <- grep("\\.csv$", unzip(zip, list = TRUE)$Name, value = TRUE)
  exdir <- file.path(CACHE_DIR, "unzipped")
  unzip(zip, files = csv, exdir = exdir, overwrite = TRUE)
  keep <- c("SERIALNO", "SPORDER", "ST", "PWGTP", vars)
  bind_rows(lapply(file.path(exdir, csv), function(f)
    read_csv(f, col_types = cols(.default = col_character()), progress = FALSE,
             col_select = c(any_of(c(keep, tolower(keep))), matches("^pwgtp[0-9]+$", ignore.case = TRUE)))))
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
  df$st_ <- dplyr::coalesce(as.integer(num(df$ST_REQ)), if ("ST" %in% names(df)) as.integer(num(df$ST)) else NA_integer_)
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
      # The 4 wage bands ($0–$45k, $45k–$70k, $70k–$120k, > $120k)
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
    mutate(flow = factor(flow, levels = c("Live DC, work DC", "Live MD, work DC", "Live VA, work DC", "Live DC, work outside DC")))
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
  svrepdesign(data = df, weights = ~PWGTP, repweights = "PWGTP[0-9]+", type = "successive-difference", mse = TRUE)
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
    mutate(mean_min_lo90 = mean_min - 1.645 * mean_min_se,
           mean_min_hi90 = mean_min + 1.645 * mean_min_se,
           small_sample  = n_sample < MIN_PUMS_N)
  if (!is.null(label)) out <- mutate(out, period = label, .before = 1)
  out
}

change_table <- function(est_old, est_new, by) {
  cols <- c(by, "n_sample", "workers", "mean_min", "mean_min_se", "share_60plus", "share_60plus_se", "share_transit", "share_wfh")
  inner_join(est_old[cols], est_new[cols], by = by, suffix = c("_old", "_new")) %>%
    mutate(
      d_min           = mean_min_new - mean_min_old,
      se_d            = sqrt(mean_min_se_old^2 + mean_min_se_new^2),
      d_lo90          = d_min - 1.645 * se_d,
      d_hi90          = d_min + 1.645 * se_d,
      significant_90  = abs(d_min / se_d) > 1.645,
      pct_chg_workers = workers_new / workers_old - 1,
      d_60plus_pts    = (share_60plus_new - share_60plus_old) * 100,
      sig_60plus      = abs((share_60plus_new - share_60plus_old) / sqrt(share_60plus_se_old^2 + share_60plus_se_new^2)) > 1.645,
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

# ---- D4 arithmetic: mode shares and their change (with SE) in the 4 wage bands
d4_mode_shares_est <- pums_est$flow_wage_mode %>%
  group_by(period, flow, wage_band) %>%
  mutate(total_workers_cell = sum(workers, na.rm = TRUE),
         share = workers / total_workers_cell,
         share_se = workers_se / total_workers_cell) %>%
  ungroup()

d4_mode_shares_chg <- d4_mode_shares_est %>%
  select(period, flow, wage_band, mode_grp, workers, share, share_se) %>%
  pivot_wider(names_from = period, values_from = c(workers, share, share_se)) %>%
  rename_with(~ str_replace(.x, fixed(P_OLD), "old") %>% str_replace(fixed(P_NEW), "new")) %>%
  mutate(d_share_pts = (share_new - share_old) * 100,
         se_d_pts = sqrt(share_se_old^2 + share_se_new^2) * 100,
         significant_90 = abs(d_share_pts / se_d_pts) > 1.645)

# ---- D6 arithmetic: home state of DC workers by wage band
d_old_pow11 <- des_old[des_old$variables$pow == 11 & !is.na(des_old$variables$res_state), ]
d_new_pow11 <- des_new[des_new$variables$pow == 11 & !is.na(des_new$variables$res_state), ]

residence_shares <- bind_rows(
  est_group(d_old_pow11, c("wage_band", "res_state"), P_OLD),
  est_group(d_new_pow11, c("wage_band", "res_state"), P_NEW)
) %>%
  group_by(period, wage_band) %>%
  mutate(total_dc_jobs = sum(workers, na.rm = TRUE),
         share_of_dc_jobs = workers / total_dc_jobs,
         share_se = workers_se / total_dc_jobs) %>%
  ungroup()

residence_shares_chg <- residence_shares %>%
  select(period, wage_band, res_state, workers, share_of_dc_jobs, share_se) %>%
  pivot_wider(names_from = period, values_from = c(workers, share_of_dc_jobs, share_se)) %>%
  rename_with(~ str_replace(.x, fixed(P_OLD), "old") %>% str_replace(fixed(P_NEW), "new")) %>%
  mutate(d_share_pts = (share_of_dc_jobs_new - share_of_dc_jobs_old) * 100,
         se_d_pts = sqrt(share_se_old^2 + share_se_new^2) * 100,
         significant_90 = abs(d_share_pts / se_d_pts) > 1.645)

cat("\n=== D. PUMS: MEAN COMMUTE BY FLOW AND THE 4 WAGE BANDS ===\n")
print(as.data.frame(pums_chg$flow_wage %>%
                      select(flow, wage_band, n_sample_old, n_sample_new, mean_min_old, mean_min_new, d_min, significant_90, d_60plus_pts, d_wfh_pts) %>%
                      mutate(across(where(is.numeric), ~ round(.x, 2)))))
cat("\n=== D4. MODE SHARE CHANGES ACROSS THE 4 WAGE BANDS (PCT. POINTS) ===\n")
print(as.data.frame(d4_mode_shares_chg %>%
                      filter(mode_grp %in% c("Worked from home", "Transit", "Drove alone")) %>%
                      select(flow, wage_band, mode_grp, share_old, share_new, d_share_pts, significant_90) %>%
                      mutate(across(where(is.numeric), ~ round(.x, 3)))))
cat("\n=== D6. WHERE DC WORKERS LIVE (HOME-STATE SHARE CHANGES ACROSS 4 WAGE BANDS) ===\n")
print(as.data.frame(residence_shares_chg %>%
                      select(wage_band, res_state, share_of_dc_jobs_old, share_of_dc_jobs_new, d_share_pts, significant_90) %>%
                      mutate(across(where(is.numeric), ~ round(.x, 3)))))

# ---- Difference-in-differences across the 4 wage bands (reference: > $120k)
pooled <- bind_rows(pums_old, pums_new) %>%
  filter(pow == 11, !is.na(travel), !is.na(wage_band)) %>%
  mutate(post = as.numeric(period == P_NEW),
         live_outside_dc = as.numeric(st != 11),
         wage_4ref = relevel(factor(wage_band), ref = "High-income (> $120k)"))
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
  list(D4_mode_shares_est = d4_mode_shares_est, D4_mode_shares_chg = d4_mode_shares_chg,
       D6_residence_est = residence_shares, D6_residence_chg = residence_shares_chg,
       D7_did_models_4bands = did_models)
)

# -----------------------------------------------------------------------------
# PART D figures (all on the 4 wage bands)
# -----------------------------------------------------------------------------
flows_dc <- c("Live DC, work DC", "Live MD, work DC", "Live VA, work DC", "Live DC, work outside DC")
cap_pums <- "Source: ACS 5-year PUMS (constant 2023 dollars). Bars/whiskers = 90% CI from replicate weights. Hollow = n < 50."

# ---- D0: the 4 wage bands only
p_d0 <- pums_est$wage_only %>%
  transmute(period, wage_band,
            `1. Mean commute time (min)`       = mean_min,
            `2. Share commuting ≥ 60 min (%)`  = share_60plus * 100,
            `3. Worked from home (%)`          = share_wfh * 100) %>%
  pivot_longer(-c(period, wage_band), names_to = "metric", values_to = "val") %>%
  ggplot(aes(wage_band, val, fill = period)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_fill_manual(values = POSTER$period, name = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, 12)) +
  labs(title = "Direct comparison across the 4 income bands ($0–45k, $45–70k, $70–120k, > $120k)",
       subtitle = "All DC-linked workers combined", x = NULL, y = NULL, caption = cap_pums) +
  theme_poster()
save_plot(p_d0, "D0_only_4_income_bands_comparison.png", w = 12, h = 5.5)

# ---- D1: mean commute by flow and wage band
p_d1 <- pums_est$flow_wage %>%
  ggplot(aes(flow, mean_min, colour = period)) +
  geom_pointrange(aes(ymin = mean_min_lo90, ymax = mean_min_hi90, shape = small_sample), position = position_dodge(0.5)) +
  facet_wrap(~ wage_band, nrow = 1) +
  scale_colour_manual(values = POSTER$period, name = NULL) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  labs(title = "Mean commute by where workers live and work, across the 4 wage bands",
       subtitle = "Income bands: $0–$45k, $45k–$70k, $70k–$120k, > $120k (annual earnings)",
       x = NULL, y = "Minutes (one way)", caption = cap_pums) +
  theme_poster()
save_plot(p_d1, "D1_pums_time_by_flow_4wagebands.png", w = 13, h = 5.5)

# ---- D2: change in mean commute time
p_d2 <- pums_chg$flow_wage %>%
  mutate(flow = factor(flow, levels = rev(flows_dc))) %>%
  ggplot(aes(d_min, flow, colour = significant_90)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = d_lo90, xmax = d_hi90)) +
  facet_wrap(~ wage_band, nrow = 1) +
  scale_colour_manual(values = POSTER$sig, labels = c(`TRUE` = "Yes", `FALSE` = "No"), name = "Significant at 90%") +
  labs(title = paste("Change in mean commute time,", P_OLD, "→", P_NEW, "across the 4 wage bands"),
       subtitle = "Positive = longer commutes; workers who commuted (excludes work from home)",
       x = "Change (minutes)", y = NULL, caption = cap_pums) +
  theme_poster()
save_plot(p_d2, "D2_pums_change_by_flow_4wagebands.png", w = 13, h = 5)

# ---- D3: share commuting 60+ minutes
p_d3 <- pums_est$flow_wage %>%
  ggplot(aes(flow, share_60plus * 100, fill = period)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  geom_errorbar(aes(ymin = (share_60plus - 1.645 * share_60plus_se) * 100, ymax = (share_60plus + 1.645 * share_60plus_se) * 100),
                position = position_dodge(0.8), width = 0.2) +
  facet_wrap(~ wage_band, nrow = 1) +
  scale_fill_manual(values = POSTER$period, name = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  labs(title = "Share of commuters travelling 60+ minutes each way (4 wage bands)",
       x = NULL, y = "% of commuters", caption = cap_pums) +
  theme_poster()
save_plot(p_d3, "D3_pums_long_commutes_4wagebands.png", w = 13, h = 5.5)

# ---- D4: how DC-linked workers get to work
p_d4 <- d4_mode_shares_est %>%
  ggplot(aes(period, share, fill = mode_grp)) +
  geom_col(width = 0.7) +
  facet_grid(wage_band ~ flow, labeller = labeller(flow = label_wrap_gen(14), wage_band = label_wrap_gen(14))) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = POSTER$mode, name = NULL) +
  labs(title = "How DC-linked workers get to work",
       subtitle = "Across the 4 wage bands ($0–$45k, $45k–$70k, $70k–$120k, > $120k)",
       x = NULL, y = "Share of workers", caption = "Source: ACS 5-year PUMS.") +
  theme_poster() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_plot(p_d4, "D4_pums_mode_shares.png", w = 13, h = 9.5)

# ---- D5: who could work from home
p_d5 <- pums_est$flow_wage %>%
  ggplot(aes(flow, share_wfh * 100, colour = period)) +
  geom_pointrange(aes(ymin = (share_wfh - 1.645 * share_wfh_se) * 100, ymax = (share_wfh + 1.645 * share_wfh_se) * 100),
                  position = position_dodge(0.5)) +
  facet_wrap(~ wage_band, nrow = 1) +
  scale_colour_manual(values = POSTER$period, name = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  labs(title = "Who could work from home? (4 wage bands)",
       subtitle = "Telework surged in the $70k–$120k and > $120k bands, changing who is left commuting",
       x = NULL, y = "% working from home", caption = cap_pums) +
  theme_poster()
save_plot(p_d5, "D5_pums_work_from_home_4wagebands.png", w = 13, h = 5.5)

# ---- D6: where people who work in DC live, by wage band
p_d6 <- residence_shares %>%
  mutate(res_state = factor(res_state, levels = c("DC", "Maryland", "Virginia"))) %>%
  ggplot(aes(period, share_of_dc_jobs, fill = res_state)) +
  geom_col(width = 0.7) +
  facet_wrap(~ wage_band, nrow = 1) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = POSTER$home, name = "Home state") +
  labs(title = "Where people who work in DC live, by wage band",
       subtitle = "Across the 4 wage bands ($0–$45k, $45k–$70k, $70k–$120k, > $120k)",
       x = NULL, y = "Share of workers employed in DC", caption = "Source: ACS 5-year PUMS.") +
  theme_poster()
save_plot(p_d6, "D6_pums_where_dc_workers_live.png", w = 12.5, h = 5.5)

# ---- D7: difference-in-differences coefficients
p_d7 <- did_models %>%
  filter(str_detect(term, "^post")) %>%
  mutate(term = str_replace_all(term, "wage_4ref", ""),
         term = recode(term,
                       "post"                 = "Change for DC residents (> $120k ref.)",
                       "post:live_outside_dc" = "Extra change if living outside DC (> $120k)")) %>%
  ggplot(aes(estimate, fct_rev(term))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), colour = POSTER$period[[2]]) +
  facet_wrap(~ model, scales = "free_x") +
  labs(title = "Difference-in-differences: workers employed in DC (4 wage bands, ref: > $120k)",
       x = "Coefficient (90% CI)", y = NULL, caption = "Source: ACS 5-year PUMS; replicate-weight SEs.") +
  theme_poster()
save_plot(p_d7, "D7_pums_did_coefficients_4wagebands.png", w = 13.5, h = 5.5)

# ---- D8: 1-year ACS check (2010 → 2019 → 2023)
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
      chg_pre  <- change_table(est_1yr_list[["2010"]], est_1yr_list[["2019"]], c("flow", "wage_band")) %>% mutate(subperiod = "Pre-pandemic (2010 → 2019)", .before = 1)
      chg_post <- change_table(est_1yr_list[["2019"]], est_1yr_list[["2023"]], c("flow", "wage_band")) %>% mutate(subperiod = "Post-pandemic (2019 → 2023)", .before = 1)
      chg_dec  <- change_table(est_1yr_list[["2010"]], est_1yr_list[["2023"]], c("flow", "wage_band")) %>% mutate(subperiod = "Full decade (2010 → 2023)", .before = 1)
      results[["D8_1yr_chg_4bands"]] <- bind_rows(chg_pre, chg_post, chg_dec)
      cat("\n=== D8. 1-YEAR DECOMPOSITION (2010→2019 vs 2019→2023) ACROSS THE 4 WAGE BANDS ===\n")
      print(as.data.frame(results[["D8_1yr_chg_4bands"]] %>%
                            select(subperiod, flow, wage_band, mean_min_old, mean_min_new, d_min, significant_90, d_wfh_pts) %>%
                            mutate(across(where(is.numeric), ~ round(.x, 2)))))
    }
    p_d8 <- one_year_4b %>%
      mutate(year = as.integer(period)) %>%
      ggplot(aes(year, mean_min, colour = wage_band)) +
      geom_line(linewidth = 0.85) +
      geom_pointrange(aes(ymin = mean_min_lo90, ymax = mean_min_hi90, shape = small_sample), position = position_dodge(0.35), size = 0.45) +
      facet_wrap(~ flow, nrow = 1, labeller = label_wrap_gen(14)) +
      scale_x_continuous(breaks = YEARS_1YR_PUMS) +
      scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
      scale_colour_manual(values = POSTER$wage, name = NULL) +
      labs(title    = paste0("1-year ACS check: pre-pandemic (", YEAR_OLD, "–2019) vs. post-pandemic (2019–2023)"),
           subtitle = "By the 4 wage bands ($0–$45k, $45k–$70k, $70k–$120k, > $120k)",
           x = NULL, y = "Mean commute (minutes)", caption = "Source: ACS 1-year PUMS; 90% CI.") +
      theme_poster() + guides(colour = guide_legend(nrow = 1))
    save_plot(p_d8, "D8_pums_1yr_trend.png", w = 13, h = 4.8)
  }
}

# -----------------------------------------------------------------------------
# Consolidated export (Excel workbook + individual CSVs)
# -----------------------------------------------------------------------------
results_xlsx <- lapply(results, function(x) { x <- as.data.frame(x); if ("geometry" %in% names(x)) x$geometry <- NULL; x })
names(results_xlsx) <- substr(names(results_xlsx), 1, 31)
write_xlsx(results_xlsx, file.path(OUT_DIR, "acs_commute_gentrification_part2_unified.xlsx"))
for (nm in names(results)) {
  x <- as.data.frame(results[[nm]]); if ("geometry" %in% names(x)) x$geometry <- NULL
  write_csv(x, file.path(OUT_DIR, paste0(nm, ".csv")))
}
cat("\nPart 2 done. Figures (H1–H4, A1–A6, B1–B3, C1, D0–D8), CSVs and the Excel workbook are in ./", OUT_DIR, "\n", sep = "")

# =============================================================================
# POSTER EXTRAS — P1 … P8
# Uses objects from Parts 1–2: panel_df, panel (sf), pums_old, pums_new,
# POSTER, TYPOLOGY_LEVELS, P_OLD, P_NEW, WAGE_LABELS. Optional: patchwork (P3).
# Each block is wrapped in tryCatch so one failure does not stop the others.
# =============================================================================
OUT_POSTER <- "outputs_poster"
dir.create(OUT_POSTER, showWarnings = FALSE, recursive = TRUE)

save_poster <- function(p, name, w = 9, h = 6) {
  print(p)
  ggsave(file.path(OUT_POSTER, name), p, width = w, height = h, dpi = 300, bg = "white")
}
run_block <- function(name, expr) {
  message("\n==== ", name, " ====")
  tryCatch(expr, error = function(e) message("  [", name, "] failed: ", conditionMessage(e)))
  invisible(NULL)
}
LVL_DYN <- TYPOLOGY_LEVELS[1]
LVL_AFF <- TYPOLOGY_LEVELS[5]

# -----------------------------------------------------------------------------
# P1. Methodological pipeline diagram (pure ggplot, no extra packages)
# -----------------------------------------------------------------------------
run_block("P1 pipeline", {
  P1_TEXT <- 3.4   # box text size (ggplot units); 3.4 ≈ 10 pt — keeps the 11-inch poster placement readable
  nodes <- tribble(
    ~id, ~x, ~y, ~w, ~grp, ~label,
    "acs",   1, 3, 1.9, "Data",           "ACS 5-Year\n2006–2010 & 2019–2023\n(B/S tables + PUMS)",
    "cpi",   3, 3, 1.6, "Data",           "CPI-U deflation\n→ constant 2023 US$",
    "xw",    5, 3, 1.8, "Data",           "Tract crosswalk\n2010 → 2020\n(area weights)",
    "filt",  7, 3, 1.8, "Data",           "Reliability filters\n(MOE, minimum n, CV)",
    "ind",   1, 2, 1.9, "Gentrification", "Tract indicators:\nMHI, %BA+, rent,\nvalue, tenure, race",
    "g",     3, 2, 1.6, "Gentrification", "M1  G index\n(percentiles)",
    "fr",    5, 2, 1.6, "Gentrification", "M2  Freeman\n(binary)",
    "udp",   7, 2, 1.8, "Gentrification", "M3  UDP + contagion\n→ 5-stage typology",
    "val",   9, 2, 1.8, "Gentrification", "Exogenous validation:\nΔ% Black ~ ΔG",
    "tr",    1, 1, 1.9, "Commuting",      "Tracts: time by mode,\n% 45+/60+ min, WFH,\nworks outside DC",
    "reg",   3, 1, 1.6, "Commuting",      "Region:\n17 counties,\n1-year series",
    "pums",  5, 1, 1.8, "Commuting",      "PUMS: flows × 4 wage\nbands, replicate\nweights, DiD",
    "x",     7, 1, 1.8, "Commuting",      "Cross-tabulation\ntypology × Δtime\n(90% CI)",
    "res",   9, 1, 1.8, "Commuting",      "DC result:\nΔG ≫ 0  |  Δtime ≈ 0"
  )
  edges <- tribble(
    ~from, ~to,
    "acs","cpi", "cpi","xw", "xw","filt",
    "filt","ind", "ind","g", "ind","fr", "ind","udp", "udp","val",
    "ind","tr", "tr","reg", "reg","pums", "pums","x", "udp","x", "x","res"
  ) %>%
    left_join(nodes %>% select(from = id, x1 = x, y1 = y, w1 = w), by = "from") %>%
    left_join(nodes %>% select(to = id, x2 = x, y2 = y, w2 = w), by = "to") %>%
    mutate(dy = y2 - y1,
           xs = if_else(dy == 0, x1 + w1 / 2, x1), ys = if_else(dy == 0, y1, y1 + sign(dy) * 0.33),
           xe = if_else(dy == 0, x2 - w2 / 2, x2), ye = if_else(dy == 0, y2, y2 - sign(dy) * 0.33))
  pal_grp <- c("Data" = "#DCEBEE", "Gentrification" = "#F6DCD9", "Commuting" = "#E7E1F2")   # teal / red / purple tints
  lab_col <- c("Data" = "#0E7C86", "Gentrification" = "#B3261E", "Commuting" = "#3B1E6E")
  p_p1 <- ggplot() +
    geom_segment(data = edges, aes(x = xs, y = ys, xend = xe, yend = ye),
                 arrow = arrow(length = unit(0.18, "cm"), type = "closed"), colour = "grey40", linewidth = 0.5) +
    geom_rect(data = nodes, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - 0.33, ymax = y + 0.33, fill = grp),
              colour = "grey30", linewidth = 0.4) +
    geom_text(data = nodes, aes(x = x, y = y, label = label), size = P1_TEXT, lineheight = 0.95, colour = POSTER$ink) +
    annotate("text", x = -0.3, y = 3, label = "DATA", angle = 90, fontface = "bold", size = 3.5, colour = lab_col[["Data"]]) +
    annotate("text", x = -0.3, y = 2, label = "GENTRIFICATION", angle = 90, fontface = "bold", size = 3.5, colour = lab_col[["Gentrification"]]) +
    annotate("text", x = -0.3, y = 1, label = "COMMUTING", angle = 90, fontface = "bold", size = 3.5, colour = lab_col[["Commuting"]]) +
    scale_fill_manual(values = pal_grp, guide = "none") +
    coord_equal(xlim = c(-0.6, 10.1), ylim = c(0.5, 3.5), expand = FALSE) +
    labs(title = "Methodological pipeline: from defining gentrification to the commuting profile",
         subtitle = "Implemented in R (tidycensus, sf, survey). Parameters at the top of the script make it transferable to other cities.") +
    theme_void() + theme(plot.title = element_text(face = "bold", size = 13, colour = POSTER$ink),
                         plot.subtitle = element_text(colour = POSTER$muted, size = 9.5))
  save_poster(p_p1, "P1_pipeline_methodology.png", w = 12, h = 4.6)
})

# -----------------------------------------------------------------------------
# P2. Why LODES fails in DC: the 3 nominal LODES earnings bands vs. PUMS wages
#     LODES: SE01 ≤ $1,250/month (≤ $15,000/yr); SE02 $1,251–3,333 ($15,001–40,000);
#            SE03 > $3,333/month (> $40,000/yr). Fixed nominal thresholds since 2002.
# -----------------------------------------------------------------------------
run_block("P2 LODES vs PUMS", {
  LODES_BREAKS <- c(-Inf, 15000, 40000, Inf)
  LODES_LABELS <- names(POSTER$lodes)
  pums_both <- bind_rows(pums_old, pums_new) %>%
    filter(!is.na(flow), !is.na(wage_end), wage_end > 0) %>%
    mutate(lodes_band = cut(wage_end, LODES_BREAKS, LODES_LABELS),   # nominal dollars of the window's end year, as in LODES
           PWGTP = as.numeric(PWGTP))

  lodes_shares <- pums_both %>%
    group_by(period, flow, lodes_band) %>% summarise(w = sum(PWGTP, na.rm = TRUE), n = n(), .groups = "drop") %>%
    group_by(period, flow) %>% mutate(share = w / sum(w)) %>% ungroup()
  write_csv(lodes_shares, file.path(OUT_POSTER, "P2_lodes_band_shares.csv"))
  cat("\nShare in the top LODES band (SE03) by flow and period:\n")
  print(lodes_shares %>% filter(lodes_band == LODES_LABELS[3]) %>%
          select(period, flow, share_SE03 = share) %>% mutate(share_SE03 = round(share_SE03, 3)) %>% as.data.frame())

  p_p2a <- ggplot(lodes_shares, aes(period, share, fill = lodes_band)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = if_else(lodes_band == LODES_LABELS[3], percent(share, 1), "")),
              position = position_stack(vjust = 0.5), size = 3.4, colour = "white", fontface = "bold") +
    facet_wrap(~ flow, nrow = 1, labeller = label_wrap_gen(16)) +
    scale_y_continuous(labels = percent) +
    scale_fill_manual(values = POSTER$lodes, name = "LODES earnings band (nominal)") +
    labs(title = "Why LODES cannot discriminate income in DC",
         subtitle = "Nearly all DC-linked workers fall in the top band (SE03 > $40k/yr), fixed in nominal dollars since 2002",
         x = NULL, y = "Workers (weighted)",
         caption = "Source: ACS 5-year PUMS (nominal wages of the window's end year, WAGP×ADJINC); bands from the LEHD/LODES Technical Documentation v8.") +
    theme_poster()
  save_poster(p_p2a, "P2a_lodes_bands_vs_pums.png", w = 12, h = 5.2)

  dist23 <- pums_new %>% filter(!is.na(flow), wage_2023 > 0, wage_2023 < 400000) %>% mutate(PWGTP = as.numeric(PWGTP))
  cuts <- tibble(x = c(15000, 40000, 45000, 70000, 120000),
                 type = c("LODES", "LODES", "This study (4 bands)", "This study (4 bands)", "This study (4 bands)"))
  p_p2b <- ggplot(dist23, aes(x = wage_2023, weight = PWGTP)) +
    geom_histogram(binwidth = 10000, boundary = 0, fill = "#9CC9D0", colour = "white") +
    geom_vline(data = cuts, aes(xintercept = x, colour = type, linetype = type), linewidth = 0.9) +
    scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "k"), breaks = seq(0, 400000, 50000)) +
    scale_y_continuous(labels = comma) +
    scale_colour_manual(values = c("LODES" = "#7C2D12", "This study (4 bands)" = "#3B1E6E"), name = NULL) +
    scale_linetype_manual(values = c("LODES" = "dashed", "This study (4 bands)" = "dotted"), name = NULL) +
    labs(title = "Where the cut-points fall: LODES ($15k, $40k) vs. this study's 4 bands ($45k, $70k, $120k)",
         subtitle = "Weighted distribution of annual wages of DC-linked workers, ACS PUMS 2019–2023 (constant 2023 US$)",
         x = "Annual wage", y = "Workers", caption = "Source: ACS 5-year PUMS 2019–2023.") +
    theme_poster()
  save_poster(p_p2b, "P2b_wage_distribution_cuts.png", w = 11, h = 5)
})

# -----------------------------------------------------------------------------
# P3. Bivariate map: gentrification stage × change in mean commute time
#     Time classes use the MAGNITUDE of the change (± TIME_CUT minutes) because,
#     in DC, no tract shows a statistically significant increase at 90% —
#     a significance-based classification would be a single colour.
# -----------------------------------------------------------------------------
run_block("P3 bivariate map", {
  TIME_CUT <- 2   # minutes; change to 3 or 5 for a coarser view
  n_sig_longer <- sum(panel_df$change_class == "Significantly longer", na.rm = TRUE)
  biv <- panel %>%
    mutate(
      gent3 = case_when(typology_udp == LVL_DYN ~ "Dynamic gentrification",
                        typology_udp == LVL_AFF ~ "Affluent / exclusion",
                        TRUE                    ~ "Other vulnerable"),
      gent3 = factor(gent3, levels = c("Affluent / exclusion", "Other vulnerable", "Dynamic gentrification")),
      time3 = case_when(is.na(d_time_total)          ~ NA_character_,
                        d_time_total <= -TIME_CUT     ~ "Shorter",
                        d_time_total >=  TIME_CUT     ~ "Longer",
                        TRUE                          ~ "Stable"),
      time3 = factor(time3, levels = c("Shorter", "Stable", "Longer")),
      biv_class = if_else(is.na(time3), NA_character_, paste(gent3, time3, sep = " | ")),
      biv_class = factor(biv_class, levels = as.vector(outer(levels(gent3), levels(time3), paste, sep = " | ")))
    )
  # 3 × 3 palette: rows = gentrification (grey → teal), columns = time (light → red-purple)
  pal_biv <- c(
    "Affluent / exclusion | Shorter"     = "#E8E8E8",
    "Affluent / exclusion | Stable"      = "#E4ACAC",
    "Affluent / exclusion | Longer"      = "#C85A5A",
    "Other vulnerable | Shorter"         = "#B0D5DF",
    "Other vulnerable | Stable"          = "#AD9EA5",
    "Other vulnerable | Longer"          = "#985356",
    "Dynamic gentrification | Shorter"   = "#64ACBE",
    "Dynamic gentrification | Stable"    = "#627F8C",
    "Dynamic gentrification | Longer"    = "#574249"
  )
  tab_biv <- biv %>% st_drop_geometry() %>% count(gent3, time3) %>% group_by(gent3) %>% mutate(share = n / sum(n)) %>% ungroup()
  write_csv(tab_biv, file.path(OUT_POSTER, "P3_bivariate_counts.csv"))
  cat("\nTracts by stage × commute-time change:\n"); print(as.data.frame(tab_biv))

  p_map <- ggplot(biv) +
    geom_sf(aes(fill = biv_class), colour = "white", linewidth = 0.1) +
    scale_fill_manual(values = pal_biv, na.value = POSTER$na_grey, drop = FALSE, guide = "none") +
    labs(title = "Intense gentrification, stable commute times",
         subtitle = paste0("Gentrification stage × change in mean commute (±", TIME_CUT, " min), ", P_OLD, " → ", P_NEW,
                           "\n", n_sig_longer, " of ", nrow(panel_df), " tracts show a statistically significant increase (90%)"),
         caption = "Source: ACS 5-Year (B19013, B15002, B25064, B25077, B08013, B08303). Light grey = no reliable estimate.") +
    theme_map_poster()

  legend_df <- expand.grid(gent3 = levels(biv$gent3), time3 = levels(biv$time3)) %>%
    mutate(biv_class = paste(gent3, time3, sep = " | "))
  p_leg <- ggplot(legend_df, aes(time3, gent3, fill = biv_class)) +
    geom_tile(colour = "white") +
    scale_fill_manual(values = pal_biv, guide = "none") +
    labs(x = "Δ commute time →", y = "Gentrification →") +
    coord_equal() + theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid = element_blank())

  if (requireNamespace("patchwork", quietly = TRUE)) {
    library(patchwork)
    p_p3 <- p_map + inset_element(p_leg, left = 0.62, bottom = 0.62, right = 1.0, top = 0.98)
    save_poster(p_p3, "P3_bivariate_map_gent_x_commute.png", w = 9, h = 8)
  } else {
    save_poster(p_map, "P3_bivariate_map_gent_x_commute.png", w = 9, h = 8)
    save_poster(p_leg, "P3_bivariate_legend.png", w = 3, h = 3)
    message("  (install 'patchwork' to embed the legend in the map)")
  }
})

# -----------------------------------------------------------------------------
# P4. The null result with inference: Δ mean commute by typology, worker-weighted,
#     90% CI propagated from tract MOEs
# -----------------------------------------------------------------------------
run_block("P4 null result", {
  p4_df <- panel_df %>%
    filter(!is.na(d_time_total), !is.na(time_totalM_10), !is.na(time_totalM_23), workers_totalE_23 > 0) %>%
    mutate(se_d = sqrt(time_totalM_10^2 + time_totalM_23^2) / 1.645, w = workers_totalE_23)
  agg <- function(d, lab) d %>% summarise(
    group = lab, n = n(),
    d_min = sum(w * d_time_total) / sum(w),
    se    = sqrt(sum((w * se_d)^2)) / sum(w),
    pct_sig_longer  = mean(change_class == "Significantly longer"),
    pct_sig_shorter = mean(change_class == "Significantly shorter"))
  p4_tab <- bind_rows(
    p4_df %>% group_by(typology_udp) %>% group_map(~ agg(.x, as.character(.y$typology_udp))) %>% bind_rows(),
    agg(p4_df, "D.C. (all tracts)")
  ) %>% mutate(lo90 = d_min - 1.645 * se, hi90 = d_min + 1.645 * se,
               group = factor(group, levels = rev(c(TYPOLOGY_LEVELS, "D.C. (all tracts)"))))
  write_csv(p4_tab, file.path(OUT_POSTER, "P4_null_result_table.csv"))
  cat("\nWorker-weighted Δ mean commute by typology (90% CI):\n")
  print(as.data.frame(p4_tab %>% mutate(across(where(is.numeric), ~ round(.x, 2)))))

  p_p4 <- ggplot(p4_tab, aes(x = d_min, y = group)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_errorbar(aes(xmin = lo90, xmax = hi90), width = 0.25, colour = "grey30", orientation = "y") +
    geom_point(aes(colour = group, size = n)) +
    geom_text(aes(label = sprintf("%+.1f min  [%+.1f, %+.1f]", d_min, lo90, hi90), x = hi90),
              hjust = -0.1, size = 2.9, colour = POSTER$muted) +
    scale_colour_manual(values = c(POSTER$typology, "D.C. (all tracts)" = POSTER$ink), guide = "none") +
    scale_size_continuous(range = c(2.5, 6), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.45))) +
    scale_y_discrete(labels = function(x) str_wrap(x, 24)) +
    labs(title = paste0("Change in residents' mean commute time, ", P_OLD, " → ", P_NEW),
         subtitle = "Worker-weighted mean; bars = 90% CI propagated from tract MOEs. A bar crossing zero means no detectable change.",
         x = "Δ minutes (one way)", y = NULL,
         caption = "Source: ACS 5-Year B08013/B08303 (aggregate travel time ÷ workers who did not work from home).") +
    theme_poster()
  save_poster(p_p4, "P4_null_result_commute_by_typology.png", w = 10, h = 5.5)
})

# -----------------------------------------------------------------------------
# P5. Agreement between the three definitions (Freeman × top-quartile ΔG × UDP dynamic)
# -----------------------------------------------------------------------------
run_block("P5 method agreement", {
  q75 <- quantile(panel_df$d_G_index, 0.75, na.rm = TRUE)
  sets <- panel_df %>% transmute(GEOID,
                                 Freeman   = freeman_gent %in% TRUE,
                                 McKinnish = d_G_index >= q75 & !is.na(d_G_index),
                                 UDP       = typology_udp == LVL_DYN)
  combos <- sets %>%
    mutate(combo = case_when(
      Freeman & McKinnish & UDP ~ "Freeman + McKinnish + UDP",
      Freeman & UDP             ~ "Freeman + UDP",
      McKinnish & UDP           ~ "McKinnish + UDP",
      Freeman & McKinnish       ~ "Freeman + McKinnish",
      UDP                       ~ "UDP only (dynamic)",
      McKinnish                 ~ "McKinnish only (ΔG ≥ Q3)",
      Freeman                   ~ "Freeman only",
      TRUE                      ~ "None")) %>%
    count(combo) %>% arrange(desc(n))
  write_csv(combos, file.path(OUT_POSTER, "P5_method_agreement.csv"))
  jacc <- function(a, b) sum(a & b) / sum(a | b)
  cat(sprintf("\nJaccard  Freeman~UDP = %.2f | McKinnish~UDP = %.2f | Freeman~McKinnish = %.2f\n",
              jacc(sets$Freeman, sets$UDP), jacc(sets$McKinnish, sets$UDP), jacc(sets$Freeman, sets$McKinnish)))
  cat(sprintf("Totals: Freeman = %d | McKinnish = %d | UDP dynamic = %d | flagged by ≥ 2 methods = %d\n",
              sum(sets$Freeman), sum(sets$McKinnish), sum(sets$UDP), sum(rowSums(sets[, -1]) >= 2)))
  print(as.data.frame(combos))

  p_p5 <- combos %>% filter(combo != "None") %>%
    mutate(combo = fct_reorder(combo, n), agree = str_count(combo, "\\+") + 1) %>%
    ggplot(aes(n, combo, fill = factor(agree))) +
    geom_col(width = 0.7) +
    geom_text(aes(label = n), hjust = -0.2, size = 3.3) +
    scale_fill_manual(values = c("1" = "#C5CBD3", "2" = "#E8873A", "3" = "#B3261E"), name = "Definitions in agreement") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Three definitions, one overlapping core of neighborhoods",
         subtitle = sprintf("Tracts flagged by Freeman (binary), McKinnish (ΔG in the top quartile, ≥ %.3f) and UDP (dynamic gentrification)", q75),
         x = "Number of tracts", y = NULL, caption = "Source: tract panel (Parts 1–2).") +
    theme_poster() + theme(legend.position = "bottom")
  save_poster(p_p5, "P5_method_agreement_upset.png", w = 9, h = 5)
})

# -----------------------------------------------------------------------------
# P6. Sensitivity of the vulnerability thresholds (score ≥ k | income percentile ≤ p)
# -----------------------------------------------------------------------------
run_block("P6 threshold sensitivity", {
  grid <- expand.grid(k = c(10, 12, 14, 16), p = c(0.40, 0.50, 0.60))
  sens <- bind_rows(Map(function(k, p) {
    vul <- (panel_df$udp_vuln_score >= k) | (panel_df$pct_rank_inc_10 <= p)
    tibble(k = k, p = p,
           vulnerable = sum(vul, na.rm = TRUE),
           dynamic    = sum(vul & (panel_df$demo_surge %in% TRUE) & (panel_df$market_surge %in% TRUE), na.rm = TRUE))
  }, grid$k, grid$p)) %>% mutate(chosen = k == 12 & p == 0.50)
  write_csv(sens, file.path(OUT_POSTER, "P6_threshold_sensitivity.csv"))
  print(as.data.frame(sens))

  p_p6 <- ggplot(sens, aes(factor(p), factor(k), fill = dynamic)) +
    geom_tile(colour = "white", linewidth = 1) +
    geom_tile(data = filter(sens, chosen), fill = NA, colour = "black", linewidth = 1.4) +
    geom_text(aes(label = paste0(dynamic, "\n(", vulnerable, " vuln.)")), size = 3.3, lineheight = 0.9,
              colour = ifelse(sens$dynamic > median(sens$dynamic), "white", "black")) +
    scale_fill_gradient(low = "#F6DCD9", high = "#B3261E", name = "Tracts in\ndynamic\ngentrification") +
    labs(title = "The classification does not hinge on an arbitrary cut-off",
         subtitle = "Tracts classified as 'dynamic gentrification' under alternative vulnerability thresholds (black outline = this study)",
         x = "Vulnerable if 2010 income percentile ≤ p", y = "… or if UDP score ≥ k",
         caption = "demo_surge and market_surge are held fixed; only eligibility changes.") +
    theme_poster() + theme(legend.position = "right", panel.grid = element_blank())
  save_poster(p_p6, "P6_threshold_sensitivity.png", w = 8, h = 5.5)
})

# -----------------------------------------------------------------------------
# P7. Full commute-time distributions (PUMS), 2010 vs 2023, by flow × wage band
#     + a compact 1-row version for the poster (bands pooled)
# -----------------------------------------------------------------------------
run_block("P7 PUMS densities", {
  dens <- bind_rows(pums_old, pums_new) %>%
    filter(!is.na(flow), !is.na(wage_band), !is.na(travel), travel > 0) %>%
    mutate(travel_c = pmin(travel, 120), PWGTP = as.numeric(PWGTP), period = factor(period, levels = c(P_OLD, P_NEW)))
  wmed <- function(x, w) { o <- order(x); cw <- cumsum(w[o]) / sum(w); x[o][which(cw >= 0.5)[1]] }
  meds <- dens %>% group_by(period, flow, wage_band) %>% summarise(med = wmed(travel_c, PWGTP), .groups = "drop")
  write_csv(meds, file.path(OUT_POSTER, "P7_weighted_median_minutes.csv"))

  p_p7 <- ggplot(dens, aes(travel_c, weight = PWGTP, fill = period, colour = period)) +
    geom_density(alpha = 0.35, adjust = 1.3, linewidth = 0.6) +
    geom_vline(data = meds, aes(xintercept = med, colour = period), linetype = "dashed", linewidth = 0.5) +
    facet_grid(wage_band ~ flow, labeller = labeller(flow = label_wrap_gen(14), wage_band = label_wrap_gen(14))) +
    scale_fill_manual(values = POSTER$period, name = NULL) + scale_colour_manual(values = POSTER$period, name = NULL) +
    scale_x_continuous(breaks = seq(0, 120, 30)) +
    labs(title = "The entire commute-time distribution barely moved",
         subtitle = "Weighted density of one-way minutes (capped at 120) by flow and wage band; dashed lines = weighted medians",
         x = "Minutes (one way)", y = "Density", caption = "Source: ACS 5-year PUMS (JWMNP), excludes work from home.") +
    theme_poster() + theme(axis.text.y = element_blank())
  save_poster(p_p7, "P7_pums_commute_densities.png", w = 13, h = 9)

  # Compact poster version: one row, wage bands pooled
  meds_flow <- dens %>% group_by(period, flow) %>% summarise(med = wmed(travel_c, PWGTP), .groups = "drop")
  p_p7c <- ggplot(dens, aes(travel_c, weight = PWGTP, fill = period, colour = period)) +
    geom_density(alpha = 0.35, adjust = 1.3, linewidth = 0.7) +
    geom_vline(data = meds_flow, aes(xintercept = med, colour = period), linetype = "dashed", linewidth = 0.6) +
    facet_wrap(~ flow, nrow = 1, labeller = label_wrap_gen(16)) +
    scale_fill_manual(values = POSTER$period, name = NULL) + scale_colour_manual(values = POSTER$period, name = NULL) +
    scale_x_continuous(breaks = seq(0, 120, 30)) +
    labs(title = "Commute-time distributions, 2006–2010 vs. 2019–2023, by flow (all wage bands)",
         subtitle = "Dashed lines = weighted medians (JWMNP is reported in 5-minute steps, so shifts under 5 min are within reporting granularity)",
         x = "Minutes (one way)", y = NULL, caption = "Source: ACS 5-year PUMS (JWMNP), excludes work from home.") +
    theme_poster(base_size = 14) + theme(axis.text.y = element_blank())
  save_poster(p_p7c, "P7c_pums_commute_densities_compact.png", w = 13, h = 4.2)
})

# -----------------------------------------------------------------------------
# P8. Mechanism of the null result: composition changes, time does not
# -----------------------------------------------------------------------------
run_block("P8 composition vs time", {
  p8_df <- panel_df %>% filter(!is.na(d_time_total), !is.na(d_hh_125kp_pts))
  fit <- lm(d_time_total ~ d_hh_125kp_pts, data = p8_df, weights = workers_totalE_23)
  cat("\nΔtime ~ Δ% households > $125k (weighted):\n"); print(summary(fit)$coefficients)
  p_p8 <- ggplot(p8_df, aes(d_hh_125kp_pts, d_time_total)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(aes(colour = typology_udp, size = workers_totalE_23), alpha = 0.75) +
    geom_smooth(aes(weight = workers_totalE_23), method = "lm", colour = POSTER$ink, se = TRUE) +
    scale_colour_manual(values = POSTER$typology, name = NULL) +
    scale_size_continuous(range = c(1.5, 6), guide = "none") +
    labs(title = "The neighborhood's income composition changes; its commute time does not",
         subtitle = sprintf("Weighted slope = %.3f min per percentage point (p = %.3f)", coef(fit)[2], summary(fit)$coefficients[2, 4]),
         x = "Δ percentage points of households earning > $125k (B19001), 2010 → 2023",
         y = "Δ mean commute time (minutes)",
         caption = "Source: ACS 5-Year. Point size = workers in 2023.") +
    theme_poster() + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 2))
  save_poster(p_p8, "P8_composition_vs_commute.png", w = 10, h = 6.5)
})

# -----------------------------------------------------------------------------
# POSTER EXPORTS — re-save the figures used on the 36" × 48" poster at their
# PLACEMENT size (inches) with larger type, so nothing is scaled down at print.
# Width/height below match the blueprint (Poster_Blueprint_EN.docx). Figures
# exported this way keep axis text ≥ ~16 pt on the printed poster.
# -----------------------------------------------------------------------------
run_block("Poster exports at placement size", {
  POSTER_BASE <- 22
  big <- function(p, map = FALSE, base = POSTER_BASE) p + (if (map) theme_map_poster(base) else theme_poster(base))
  exports <- list(
    list(obj = "p_p2a", name = "POSTER_P2a_lodes_bands.png",          w = 11,  h = 4.8, map = FALSE),
    list(obj = "p_p1",  name = "POSTER_P1_pipeline.png",              w = 11,  h = 4.2, map = NA),   # theme_void: left as is
    list(obj = "p_g1",  name = "POSTER_G1_typology_map.png",          w = 11,  h = 9.0, map = TRUE),
    list(obj = "p_g6",  name = "POSTER_G6_dumbbell.png",              w = 11,  h = 5.5, map = FALSE),
    list(obj = "p_g5",  name = "POSTER_G5_racial_change.png",         w = 11,  h = 6.3, map = FALSE),
    list(obj = "p_p4",  name = "POSTER_P4_null_result.png",           w = 11,  h = 6.0, map = FALSE),
    list(obj = "p_p3",  name = "POSTER_P3_bivariate_map.png",         w = 9,   h = 8.0, map = NA),   # patchwork object: left as is
    list(obj = "p_d2",  name = "POSTER_D2_pums_change.png",           w = 11,  h = 4.5, map = FALSE),
    list(obj = "p_p7c", name = "POSTER_P7c_densities_compact.png",    w = 11,  h = 3.8, map = FALSE),
    list(obj = "p_p5",  name = "POSTER_P5_method_agreement.png",      w = 5.3, h = 3.6, map = FALSE),
    list(obj = "p_p6",  name = "POSTER_P6_threshold_sensitivity.png", w = 5.3, h = 3.8, map = FALSE)
  )
  for (e in exports) {
    p <- get0(e$obj, envir = globalenv())
    if (is.null(p)) { message("  skipped ", e$obj, " (not found)"); next }
    pp <- if (is.na(e$map)) p else big(p, map = e$map, base = if (e$w < 6) 16 else POSTER_BASE)
    ggsave(file.path(OUT_POSTER, e$name), pp, width = e$w, height = e$h, dpi = 300, bg = "white")
    message("  saved ", e$name, " (", e$w, " x ", e$h, " in)")
  }
})

cat("\nAll done. Poster figures and supporting tables are in ./", OUT_POSTER, "\n", sep = "")
