# Project Information ----
# SSECR: Diversity-Stability Relationships
# Project Objective: Compositional Stability Methods 
# Co-Leads: James Sturges & Dr. Junna Wang
# Last Updated: September 29th, 2025
# 0. Libraries / Saving Outputs / Diagnostic Files ----
library(tidyverse)
library(vegan)
library(ecotraj)
library(RColorBrewer)
library(here)

# helper: save diagnostic CSV (kept separate from CTA metrics)
save_diag <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_csv(df, path)
}

# helper: standardized path builder for CTA outputs
# Example usage:
#   make_save_path("knz", "producer", "bray", "lengths_full.csv")
#   -> "outputs/cta/knz/producer/bray/lengths_full.csv"
make_save_path <- function(site_id, group, method, filename) {
  base_dir <- here("outputs", "cta", site_id, group, method)
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(base_dir, filename)
}
# 1. Function: match_site_years cleans producer/consumer data sets by matching site-years ----
# Returns filtered versions and a diagnostics tibble of removed combos
match_site_years <- function(prod_df, con_df, by_plot = TRUE) {
  # expects both tables have columns: plot, year (can be numeric or character)
  prod_df <- prod_df %>% mutate(year = as.character(year))
  con_df  <- con_df  %>% mutate(year = as.character(year))
  
  # plots that appear in both datasets
  common_plots <- intersect(unique(prod_df$plot), unique(con_df$plot))
  
  # For each common plot, keep only years present in both
  keep_pairs <- map_df(common_plots, function(p) {
    years_prod <- prod_df %>% filter(plot == p) %>% pull(year) %>% unique()
    years_con  <- con_df  %>% filter(plot == p) %>% pull(year) %>% unique()
    tibble(plot = p, keep_year = intersect(years_prod, years_con),
           prod_years = paste(sort(years_prod), collapse = ";"),
           con_years  = paste(sort(years_con), collapse = ";"))
  })
  
  # build diagnostics: removed years per plot
  diag_removed <- keep_pairs %>%
    mutate(removed_prod = map2(prod_years, keep_year, ~ setdiff(str_split(.x, ";")[[1]], .y) %>% paste(collapse = ";")),
           removed_con  = map2(con_years,  keep_year, ~ setdiff(str_split(.x, ";")[[1]], .y) %>% paste(collapse = ";"))) %>%
    select(plot, keep_year, removed_prod, removed_con)
  
  # filter the original dfs
  prod_filt <- prod_df %>% filter(plot %in% common_plots) %>% semi_join(keep_pairs %>% select(plot, keep_year) %>% unnest(cols = c(keep_year)) %>% rename(year = keep_year), by = c("plot", "year"))
  con_filt  <- con_df  %>% filter(plot %in% common_plots) %>% semi_join(keep_pairs %>% select(plot, keep_year) %>% unnest(cols = c(keep_year)) %>% rename(year = keep_year), by = c("plot", "year"))
  
  list(prod = prod_filt, con = con_filt, diagnostics = diag_removed)
}

# 2. Function: Prepare_cta_matrix creates community assemblage matrix ----
# - method: "bray", "jaccard", "hellinger", "chord"
# - returns list(community_matrix, metadata, removed_zero_rows_tbl)
prepare_cta_matrix <- function(df, method = "bray", nmds_trymax = 100) {
  # Ensure year is numeric where possible for ordering
  df <- df %>% mutate(year = if_else(is.na(as.numeric(year)), year, as.character(as.numeric(year))))
  df <- df %>% mutate(site.year = paste(plot, year, sep = "-"))
  
  # Metadata columns we want to drop from the community matrix if present
  metadata_cols <- c("site", "ecosystem", "habitat_broad", "habitat_fine",
                     "guild", "scale_abundance", "biome", "plot", "year", "unit_abundance")
  
  # Build community matrix (rows = site.year)
  comm_matrix <- df %>%
    column_to_rownames(var = "site.year") %>%
    select(where(is.numeric)) %>%
    select(-any_of(metadata_cols))  # safe drop
  
  # Remove taxa that are all zeros across all rows (no information)
  nonzero_taxa <- which(colSums(comm_matrix, na.rm = TRUE) > 0)
  comm_matrix <- comm_matrix[, nonzero_taxa, drop = FALSE]
  
  # Identify rows (site.year) that are all zero → can't compute distances/ordination
  zero_row_idx <- which(rowSums(comm_matrix, na.rm = TRUE) == 0)
  removed_zero_rows_tbl <- tibble(
    site.year = rownames(comm_matrix)[zero_row_idx],
    reason = "all-zero row (removed for distance/ordination)"
  )
  
  if (length(zero_row_idx) > 0) {
    comm_matrix <- comm_matrix[-zero_row_idx, , drop = FALSE]
  }
  
  # Method-specific transforms
  if (tolower(method) %in% c("jaccard", "jaccardian", "jaccardian_distance")) {
    # Convert to presence / absence (1/0)
    comm_matrix_pa <- (comm_matrix > 0) * 1
    comm_matrix <- comm_matrix_pa
  } else if (tolower(method) == "hellinger") {
    # Hellinger: decostand(..., method="hellinger") in vegan
    comm_matrix <- vegan::decostand(comm_matrix, method = "hellinger")
  } else if (tolower(method) == "chord") {
    # Chord: normalized to unit length per row (decostand "normalize")
    comm_matrix <- vegan::decostand(comm_matrix, method = "normalize")
  } else {
    # For "bray" and others, leave raw numeric (counts or biomass)
    # Consider optionally standardizing or log-transforming upstream if desired
    comm_matrix <- comm_matrix
  }
  
  # NMDS: ensure using a distance that makes sense — for jaccard use distance="jaccard"; for hellinger/chord we can use "euclidean"
  nmds_distance <- case_when(
    tolower(method) == "jaccard" ~ "jaccard",
    tolower(method) %in% c("hellinger", "chord") ~ "euclidean",
    TRUE ~ "bray"
  )
  
  # Run NMDS; make vector_nums numeric and ordered.
  # Use tryCatch to avoid breaking whole pipeline for failures - return NA coords if fails.
  nmds <- tryCatch(
    vegan::metaMDS(comm_matrix, distance = nmds_distance, trymax = nmds_trymax, autotransform = FALSE),
    error = function(e) {
      warning("metaMDS failed: ", e$message)
      return(NULL)
    }
  )
  
  if (!is.null(nmds)) {
    nmds_coords <- data.frame(nmds$points, site.year = rownames(comm_matrix))
  } else {
    # create an empty coords frame so later join won't fail; PCoA plotting may fail if no coords
    nmds_coords <- tibble(site.year = rownames(comm_matrix))
  }
  
  # Attach coords back to original metadata (but only for rows that remain in comm_matrix)
  df_joined <- df %>%
    mutate(site.year = paste(plot, year, sep = "-")) %>%
    semi_join(tibble(site.year = rownames(comm_matrix)), by = "site.year") %>%
    left_join(nmds_coords, by = "site.year") %>%
    mutate(
      year = as.numeric(year),
      vector_nums = as.integer(year - min(year, na.rm = TRUE) + 1)
    ) %>%
    arrange(plot, year) # ensure ordering by numeric year within plot
  
  list(
    community_matrix = comm_matrix,
    metadata = df_joined,
    removed_zero_rows = removed_zero_rows_tbl
  )
}

# 3. Function: generate_custom_palette creates unique trajectory colors per plot ----
generate_custom_palette <- function(sites) {
  uniq <- unique(sites)
  num_sites <- length(uniq)
  n_palette <- min(max(3, num_sites), 12) # get palette baseline size
  base_colors <- brewer.pal(n_palette, "Set3")
  colors <- colorRampPalette(base_colors)(num_sites)
  names(colors) <- uniq
  # return named vector mapping plot -> hex color
  colors
}
# 4. Function: run_cta_plot creates plot structure ----
run_cta_plot <- function(D, metadata, palette, output_path) {
  # D: distance object or matrix
  # metadata must contain plot and vector_nums (numeric) and be ordered sensibly
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  png(filename = output_path, width = 12, height = 8, res = 300, units = "in")
  # Ensure unique plot ordering is consistent
  unique_plots <- unique(metadata$plot)
  traj_colors <- unname(palette[unique_plots]) # one color per unique plot, in plot order
  
  trajectoryPCoA(
    D,
    sites   = metadata$plot,
    surveys = metadata$vector_nums,
    traj.colors = traj_colors,
    lwd = 1,
    survey.labels = TRUE
  )
  legend("topright", legend = unique_plots, col = traj_colors, lwd = 2, bty = "n", cex = 0.8)
  dev.off()
}

# 5. Function: extract_all_cta_metrics + run full pipeline ----
extract_all_cta_metrics <- function(D, metadata, site_id, output_dir, method = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- Lengths ---
  lengths <- trajectoryLengths(D, sites = metadata$plot, surveys = metadata$vector_nums)
  lengths_df <- as.data.frame(lengths) %>% tibble::rownames_to_column("plot")
  lengths_df$total_trajectory <- if ("Trajectory" %in% names(lengths_df)) lengths_df$Trajectory else lengths_df[[ncol(lengths_df)]]
  segment_cols <- grep("^S\\d+$", names(lengths_df), value = TRUE)
  lengths_df$mean_segment_length <- if (length(segment_cols) > 0) rowMeans(lengths_df[, segment_cols, drop = FALSE], na.rm = TRUE) else NA_real_
  lengths_df$site <- site_id
  write_csv(lengths_df, file.path(output_dir, paste0(site_id, "_lengths", if(!is.null(method)) paste0("_", method), ".csv")))
  write_csv(lengths_df %>% select(site, plot, mean_segment_length, total_trajectory),
            file.path(output_dir, paste0(site_id, "_lengths_summary", if(!is.null(method)) paste0("_", method), ".csv")))
  
  # --- Angles ---
  angles <- trajectoryAngles(D, sites = metadata$plot, surveys = metadata$vector_nums)
  angles_df <- as.data.frame(angles) %>% tibble::rownames_to_column("plot")
  numeric_angle_cols <- names(angles_df)[sapply(angles_df, is.numeric)]
  angles_df$mean_angle <- if (length(numeric_angle_cols) > 0) rowMeans(angles_df[, numeric_angle_cols, drop = FALSE], na.rm = TRUE) else NA_real_
  angles_df$site <- site_id
  write_csv(angles_df, file.path(output_dir, paste0(site_id, "_angles", if(!is.null(method)) paste0("_", method), ".csv")))
  write_csv(angles_df %>% select(site, plot, mean_angle),
            file.path(output_dir, paste0(site_id, "_angles_summary", if(!is.null(method)) paste0("_", method), ".csv")))
  
  # --- Directionality ---
  dirct <- trajectoryDirectionality(D, sites = metadata$plot, surveys = metadata$vector_nums)
  dirct_df <- tibble::tibble(plot = names(dirct),
                             directionality = as.numeric(dirct),
                             site = site_id)
  write_csv(dirct_df, file.path(output_dir, paste0(site_id, "_directionality", if(!is.null(method)) paste0("_", method), ".csv")))
  
  # --- Convergence ---
  conv <- trajectoryConvergence(D, sites = metadata$plot, surveys = metadata$vector_nums)
  conv_df <- as.data.frame(conv) %>%
    tibble::rownames_to_column("from_plot") %>%
    tidyr::pivot_longer(-from_plot, names_to = "to_plot", values_to = "convergence") %>%
    dplyr::mutate(site = site_id, method = ifelse(is.null(method), NA_character_, as.character(method)))
  write_csv(conv_df, file.path(output_dir, paste0(site_id, "_convergence", if(!is.null(method)) paste0("_", method), ".csv")))
  
  invisible(list(lengths_full = lengths_df,
                 lengths_summary = lengths_df %>% select(site, plot, mean_segment_length, total_trajectory),
                 angles_full = angles_df,
                 angles_summary = angles_df %>% select(site, plot, mean_angle),
                 directionality = dirct_df,
                 convergence = conv_df))
}

run_cta_pipeline <- function(df, site_id, fig_path, output_dir,
                             method = "bray", nmds_trymax = 100) {
  prep <- prepare_cta_matrix(df, method = method, nmds_trymax = nmds_trymax)
  if (is.null(prep$community_matrix) || nrow(prep$community_matrix) == 0) {
    warning("No rows in community matrix after filtering. Skipping site: ", site_id, " (method: ", method, ").")
    return(invisible(NULL))
  }
  
  if (!is.null(prep$removed_zero_rows) && nrow(prep$removed_zero_rows) > 0) {
    diag_path <- file.path(output_dir, paste0("removed_zero_rows_", site_id, "_", method, ".csv"))
    save_diag(prep$removed_zero_rows, diag_path)
  }
  
  prep$metadata <- prep$metadata %>%
    dplyr::group_by(plot) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(vector_nums = as.integer(dplyr::dense_rank(year))) %>%
    dplyr::ungroup()
  
  veg_method <- switch(tolower(method),
                       "jaccard" = "jaccard",
                       "hellinger" = "euclidean",
                       "chord" = "euclidean",
                       "bray" = "bray",
                       tolower(method))
  
  D <- vegan::vegdist(prep$community_matrix, method = veg_method)
  
  pal <- generate_custom_palette(prep$metadata$plot)
  run_cta_plot(D, prep$metadata, pal, fig_path)
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  metrics <- extract_all_cta_metrics(D, prep$metadata, site_id, output_dir, method = method)
  
  invisible(metrics)
}

# 6. Function: Helper function to load model output length files ----
load_lengths <- function(path, trophic, method) {
  read_csv(path) %>%
    mutate(
      trophic = trophic,
      method = method
    )
}
# 7. Load harmonized data for producer and consumer assemblages ----

# ADK
adk_prod_wide <- readr::read_csv(here::here("data/wide_output_minimize", "adk_producers_wide_sub.csv"))
adk_con_wide  <- readr::read_csv(here::here("data/wide_output_minimize", "adk_consumers_wide_sub.csv"))

# AIMS #
aims_con_wide <- read.csv(here::here("data/wide_output_minimize", "aims_consumers_wide_sub.csv"))
aims_prod_wide <- read.csv(here::here("data/wide_output_minimize", "aims_producers_wide_sub.csv"))

#BEX
bex_ae_prod_wide <- read.csv(here::here("data/wide_output_minimize", "bex_ae_producers_wide_sub.csv"))
bex_ae_con_wide <- read.csv(here::here("data/wide_output_minimize", "bex_ae_consumers_wide_sub.csv"))
bex_he_prod_wide <- read.csv(here::here("data/wide_output_minimize", "bex_he_producers_wide_sub.csv"))
bex_he_con_wide <- read.csv(here::here("data/wide_output_minimize", "bex_he_consumers_wide_sub.csv"))
bex_se_prod_wide <- read.csv(here::here("data/wide_output_minimize", "bex_se_producers_wide_sub.csv"))
bex_se_con_wide <- read.csv(here::here("data/wide_output_minimize", "bex_se_consumers_wide_sub.csv"))

# CDR OLD FIELD #
cdr_of_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_of_producers_wide_sub.csv"))
cdr_of_con_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_of_consumers_wide_sub.csv"))

# GCE #
gce_prod_wide <- read.csv(here::here("data/wide_output_minimize", "gce_producers_wide_sub.csv"))
gce_con_wide <- read.csv(here::here("data/wide_output_minimize", "gce_consumers_wide_sub.csv"))

# JRN #
jrn_prod_wide <- read.csv(here::here("data/wide_output_minimize", "jrn_producers_wide_sub.csv"))
jrn_con_wide <- read.csv(here::here("data/wide_output_minimize", "jrn_consumers_wide_sub.csv"))

#KBS
kbs_prod_wide <- read.csv(here::here("data/wide_output_minimize", "kbs_producers_wide_sub.csv"))
kbs_con_wide <- read.csv(here::here("data/wide_output_minimize", "kbs_consumers_wide_sub.csv"))

# KNZ
knz_prod_wide <- readr::read_csv(here::here("data/wide_output_minimize", "knz_producers_wide_sub.csv"))
knz_con_wide  <- readr::read_csv(here::here("data/wide_output_minimize", "knz_consumers_wide_sub.csv"))

# MCR 
mcr_invert_prod_wide <- read.csv(here::here("data/wide_output_minimize", "mcr_invert_producers_wide_sub.csv"))
mcr_invert_wide <- read.csv(here::here("data/wide_output_minimize", "mcr_invert_consumers_wide_sub.csv"))
mcr_fish_prod_wide <- read.csv(here::here("data/wide_output_minimize", "mcr_fish_producers_wide_sub.csv"))
mcr_fish_wide <- read.csv(here::here("data/wide_output_minimize", "mcr_fish_consumers_wide_sub.csv"))

#NTL
ntl_madison_prod_wide <- read.csv(here::here("data/wide_output_minimize", "ntl_madison_producers_wide_sub.csv"))
ntl_madison_con_wide <- read.csv(here::here("data/wide_output_minimize", "ntl_madison_consumers_wide_sub.csv"))
ntl_trout_prod_wide <- read.csv(here::here("data/wide_output_minimize", "ntl_trout_producers_wide_sub.csv"))
ntl_trout_con_wide <- read.csv(here::here("data/wide_output_minimize", "ntl_trout_consumers_wide_sub.csv"))

#SBC 
sbc_invert_prod_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_invert_producers_wide_sub.csv"))
sbc_invert_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_invert_consumers_wide_sub.csv"))
sbc_fish_prod_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_fish_producers_wide_sub.csv"))
sbc_fish_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_fish_consumers_wide_sub.csv"))

#USVI #
usvi_fish_prod_wide <- read.csv(here::here("data/wide_output_minimize", "usvi_fish_producers_wide_sub.csv"))
usvi_fish_con_wide <- read.csv(here::here("data/wide_output_minimize", "usvi_fish_consumers_wide_sub.csv"))
usvi_invert_prod_wide <- read.csv(here::here("data/wide_output_minimize", "usvi_invert_producers_wide_sub.csv"))
usvi_invert_con_wide <- read.csv(here::here("data/wide_output_minimize", "usvi_invert_consumers_wide_sub.csv"))

# 8. Diagnostic check that all plots surveyed have harmonized site-year combinations ----

# AIMS Match
matched <- match_site_years(aims_prod_wide, aims_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "aims_producer_consumer_year_matching.csv"))
aims_prod_matched <- matched$prod
aims_con_matched  <- matched$con

# ADK Match
matched <- match_site_years(adk_prod_wide, adk_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "adk_producer_consumer_year_matching.csv"))
adk_prod_matched <- matched$prod
adk_con_matched  <- matched$con

# BEX AE Match
matched <- match_site_years(bex_ae_prod_wide, bex_ae_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "bex_ae_producer_consumer_year_matching.csv"))
bex_ae_prod_matched <- matched$prod
bex_ae_con_matched  <- matched$con


# BEX HE Match
matched <- match_site_years(bex_he_prod_wide, bex_he_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "bex_he_producer_consumer_year_matching.csv"))
bex_he_prod_matched <- matched$prod
bex_he_con_matched  <- matched$con

# BEX SE Match
matched <- match_site_years(bex_se_prod_wide, bex_se_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "bex_se_producer_consumer_year_matching.csv"))
bex_se_prod_matched <- matched$prod
bex_se_con_matched  <- matched$con

# CDR Match
matched <- match_site_years(cdr_of_prod_wide, cdr_of_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "cdr_of_producer_consumer_year_matching.csv"))
cdr_of_prod_matched <- matched$prod
cdr_of_con_matched  <- matched$con

# GCE Match
matched <- match_site_years(gce_prod_wide, gce_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "gce_producer_consumer_year_matching.csv"))
gce_prod_matched <- matched$prod
gce_con_matched  <- matched$con

# JRN Match
matched <- match_site_years(jrn_prod_wide, jrn_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "jrn_producer_consumer_year_matching.csv"))
jrn_prod_matched <- matched$prod
jrn_con_matched  <- matched$con

# KBS Match
matched <- match_site_years(kbs_prod_wide, kbs_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "kbs_producer_consumer_year_matching.csv"))
kbs_prod_matched <- matched$prod
kbs_con_matched  <- matched$con

# KNZ Match
matched <- match_site_years(knz_prod_wide, knz_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "knz_producer_consumer_year_matching.csv"))
knz_prod_matched <- matched$prod
knz_con_matched  <- matched$con

# mcr Fish Match
matched <- match_site_years(mcr_fish_prod_wide, mcr_fish_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "mcr_producer_fish_consumer_year_matching.csv"))
mcr_fish_prod_matched <- matched$prod
mcr_fish_con_matched  <- matched$con

# mcr Invert Match
matched <- match_site_years(mcr_invert_prod_wide, mcr_invert_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "mcr_producer_invert_consumer_year_matching.csv"))
mcr_invert_prod_matched <- matched$prod
mcr_invert_con_matched  <- matched$con

# NTL Madison Match
matched <- match_site_years(ntl_madison_prod_wide, ntl_madison_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "ntl_madison_producer_consumer_year_matching.csv"))
ntl_madison_prod_matched <- matched$prod
ntl_madison_con_matched  <- matched$con

# NTL Trout Match
matched <- match_site_years(ntl_trout_prod_wide, ntl_trout_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "ntl_trout_producer_consumer_year_matching.csv"))
ntl_trout_prod_matched <- matched$prod
ntl_trout_con_matched  <- matched$con

# SBC Invert Match
matched <- match_site_years(sbc_invert_prod_wide, sbc_invert_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "sbc_producer_invert_year_matching.csv"))
sbc_prod_inv_matched <- matched$prod
sbc_con_inv_matched  <- matched$con

# SBC Fish Match
matched <- match_site_years(sbc_fish_prod_wide, sbc_fish_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "sbc_producer_fish_year_matching.csv"))
sbc_prod_fish_matched <- matched$prod
sbc_con_fish_matched  <- matched$con

# USVI Fish Match
matched <- match_site_years(usvi_fish_prod_wide, usvi_fish_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "usvi_producer_fish_consumer_year_matching.csv"))
usvi_fish_prod_matched <- matched$prod
usvi_fish_con_matched  <- matched$con

# USVI Invert Match
matched <- match_site_years(usvi_invert_prod_wide, usvi_invert_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "usvi_producer_invert_consumer_year_matching.csv"))
usvi_invert_prod_matched <- matched$prod
usvi_invert_con_matched  <- matched$con

# 9. Run analysis pipeline ----
# Methods to run
methods <- c("bray", "jaccard", "hellinger", "chord")

# ADK 
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("adk_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "adk", "producer", m)
  message("Running ADK producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = adk_prod_matched,
    site_id = "adk",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("adk_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "adk", "consumer", m)
  message("Running ADK consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = adk_con_matched,
    site_id = "adk",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# AIMS
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("aims_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "aims", "producer", m)
  message("Running AIMS producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = aims_prod_matched,
    site_id = "aims",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("aims_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "aims", "consumer", m)
  message("Running AIMS consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = aims_con_matched,
    site_id = "aims",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# BEX AE 
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("bex_ae_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "bex_ae", "producer", m)
  message("Running BEX AE producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = bex_ae_prod_matched,
    site_id = "bex_ae",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("bex_ae_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "bex_ae", "consumer", m)
  message("Running BEX AE consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = bex_ae_con_matched,
    site_id = "bex_ae",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# BEX HE 
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("bex_he_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "bex_he", "producer", m)
  message("Running BEX HE producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = bex_he_prod_matched,
    site_id = "bex_he",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("bex_he_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "bex_he", "consumer", m)
  message("Running BEX HE consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = bex_he_con_matched,
    site_id = "bex_he",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# BEX SE 
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("bex_se_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "bex_se", "producer", m)
  message("Running BEX SE producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = bex_se_prod_matched,
    site_id = "bex_se",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("bex_se_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "bex_se", "consumer", m)
  message("Running BEX SE consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = bex_se_con_matched,
    site_id = "bex_se",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# CDR Old Forest
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("cdr_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "cdr", "producer", m)
  message("Running CDR producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = cdr_of_prod_matched,
    site_id = "cdr",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("cdr_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "cdr", "consumer", m)
  message("Running CDR consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = cdr_of_con_matched,
    site_id = "cdr",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# GCE 
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("gce_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "gce", "producer", m)
  message("Running GCE producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = gce_prod_matched,
    site_id = "gce",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("gce_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "gce", "consumer", m)
  message("Running GCE consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = gce_con_matched,
    site_id = "gce",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# JRN 
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("jrn_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "jrn", "producer", m)
  message("Running JRN producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = jrn_prod_matched,
    site_id = "jrn",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("jrn_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "jrn", "consumer", m)
  message("Running JRN consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = jrn_con_matched,
    site_id = "jrn",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# KBS 
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("kbs_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "kbs", "producer", m)
  message("Running KBS producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = kbs_prod_matched,
    site_id = "kbs",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("kbs_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "kbs", "consumer", m)
  message("Running KBS consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = kbs_con_matched,
    site_id = "kbs",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# KNZ 
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("knz_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "knz", "producer", m)
  message("Running KNZ producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = knz_prod_matched,
    site_id = "knz",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("knz_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "knz", "consumer", m)
  message("Running KNZ consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = knz_con_matched,
    site_id = "knz",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# MCR Fish
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("mcr_fish_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "mcr_fish", "producer", m)
  message("Running MCR producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = mcr_fish_prod_matched,
    site_id = "mcr",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("mcr_fish_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "mcr_fish", "consumer", m)
  message("Running MCR consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = mcr_fish_con_matched,
    site_id = "mcr",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


# MCR Invert
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("mcr_invert_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "mcr_invert", "producer", m)
  message("Running MCR producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = mcr_fish_prod_matched,
    site_id = "mcr",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("mcr_invert_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "mcr_invert", "consumer", m)
  message("Running MCR Invert consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = mcr_invert_con_matched,
    site_id = "mcr",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# NTL Madison 
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("ntl_madison_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "ntl_madison", "producer", m)
  message("Running NTL Madison producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = ntl_madison_prod_matched,
    site_id = "ntl",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("ntl_madison_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "ntl_madison", "consumer", m)
  message("Running NTL Madison consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = ntl_madison_con_matched,
    site_id = "ntl",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# NTL Trout 
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("ntl_trout_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "ntl_trout", "producer", m)
  message("Running NTL Trout producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = ntl_trout_prod_matched,
    site_id = "ntl",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("ntl_trout_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "ntl_trout", "consumer", m)
  message("Running NTL Trout consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = ntl_trout_con_matched,
    site_id = "ntl",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# SBC Fish
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("sbc_fish_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "sbc_fish", "producer", m)
  message("Running SBC producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = sbc_prod_fish_matched,
    site_id = "sbc",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("sbc_fish_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "sbc_fish", "consumer", m)
  message("Running SBC consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = sbc_con_fish_matched,
    site_id = "sbc",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# SBC Invert
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("sbc_invert_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "sbc_invert", "producer", m)
  message("Running SBC producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = sbc_prod_inv_matched,
    site_id = "sbc",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("sbc_invert_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "sbc_invert", "consumer", m)
  message("Running SBC consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = sbc_con_inv_matched,
    site_id = "sbc",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# USVI Fish
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("usvi_fish_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "usvi_fish", "producer", m)
  message("Running USVI producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = usvi_fish_prod_matched,
    site_id = "usvi",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("usvi_fish_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "usvi_fish", "consumer", m)
  message("Running USVI consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = usvi_fish_con_matched,
    site_id = "usvi",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}

# USVI Invert
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("usvi_invert_producer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "usvi_invert", "producer", m)
  message("Running USVI producer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = usvi_invert_prod_matched,
    site_id = "usvi",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}


for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("usvi_invert_consumer_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "usvi_invert", "consumer", m)
  message("Running USVI consumer CTA (method = ", m, ") ...")
  run_cta_pipeline(
    df = usvi_invert_con_matched,
    site_id = "usvi",
    fig_path = fig_path,
    output_dir = output_dir,
    method = m,
    nmds_trymax = 100
  )
}













# 10. Load model output files for compositional stability metrics ----
lengths_adk <- bind_rows(
  load_lengths(here("outputs/cta/adk/consumer/bray/adk_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/adk/consumer/jaccard/adk_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/adk/consumer/chord/adk_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/adk/consumer/hellinger/adk_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/adk/producer/bray/adk_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/adk/producer/jaccard/adk_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/adk/producer/chord/adk_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/adk/producer/hellinger/adk_lengths_hellinger.csv"), "producer", "hellinger")
)

aims_lengths <- bind_rows(
  load_lengths(here("outputs/cta/aims/consumer/bray/aims_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/aims/consumer/jaccard/aims_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/aims/consumer/chord/aims_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/aims/consumer/hellinger/aims_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/aims/producer/bray/aims_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/aims/producer/jaccard/aims_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/aims/producer/chord/aims_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/aims/producer/hellinger/aims_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_bex_ae <- bind_rows(
  load_lengths(here("outputs/cta/bex_ae/consumer/bray/bex_ae_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/bex_ae/consumer/jaccard/bex_ae_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/bex_ae/consumer/chord/bex_ae_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/bex_ae/consumer/hellinger/bex_ae_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/bex_ae/producer/bray/bex_ae_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/bex_ae/producer/jaccard/bex_ae_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/bex_ae/producer/chord/bex_ae_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/bex_ae/producer/hellinger/bex_ae_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_bex_he <- bind_rows(
  load_lengths(here("outputs/cta/bex_he/consumer/bray/bex_he_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/bex_he/consumer/jaccard/bex_he_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/bex_he/consumer/chord/bex_he_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/bex_he/consumer/hellinger/bex_he_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/bex_he/producer/bray/bex_he_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/bex_he/producer/jaccard/bex_he_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/bex_he/producer/chord/bex_he_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/bex_he/producer/hellinger/bex_he_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_bex_se <- bind_rows(
  load_lengths(here("outputs/cta/bex_se/consumer/bray/bex_se_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/bex_se/consumer/jaccard/bex_se_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/bex_se/consumer/chord/bex_se_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/bex_se/consumer/hellinger/bex_se_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/bex_se/producer/bray/bex_se_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/bex_se/producer/jaccard/bex_se_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/bex_se/producer/chord/bex_se_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/bex_se/producer/hellinger/bex_se_lengths_hellinger.csv"), "producer", "hellinger")
)



lengths_cdr <- bind_rows(
  load_lengths(here("outputs/cta/cdr/consumer/bray/cdr_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/cdr/consumer/jaccard/cdr_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/cdr/consumer/chord/cdr_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/cdr/consumer/hellinger/cdr_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/cdr/producer/bray/cdr_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/cdr/producer/jaccard/cdr_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/cdr/producer/chord/cdr_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/cdr/producer/hellinger/cdr_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_gce <- bind_rows(
  load_lengths(here("outputs/cta/gce/consumer/bray/gce_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/gce/consumer/jaccard/gce_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/gce/consumer/chord/gce_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/gce/consumer/hellinger/gce_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/gce/producer/bray/gce_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/gce/producer/jaccard/gce_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/gce/producer/chord/gce_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/gce/producer/hellinger/gce_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_jrn <- bind_rows(
  load_lengths(here("outputs/cta/jrn/consumer/bray/jrn_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/jrn/consumer/jaccard/jrn_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/jrn/consumer/chord/jrn_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/jrn/consumer/hellinger/jrn_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/jrn/producer/bray/jrn_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/jrn/producer/jaccard/jrn_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/jrn/producer/chord/jrn_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/jrn/producer/hellinger/jrn_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_kbs <- bind_rows(
  load_lengths(here("outputs/cta/kbs/consumer/bray/kbs_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/kbs/consumer/jaccard/kbs_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/kbs/consumer/chord/kbs_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/kbs/consumer/hellinger/kbs_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/kbs/producer/bray/kbs_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/kbs/producer/jaccard/kbs_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/kbs/producer/chord/kbs_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/kbs/producer/hellinger/kbs_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_knz <- bind_rows(
  load_lengths(here("outputs/cta/knz/consumer/bray/knz_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/knz/consumer/jaccard/knz_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/knz/consumer/chord/knz_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/knz/consumer/hellinger/knz_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/knz/producer/bray/knz_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/knz/producer/jaccard/knz_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/knz/producer/chord/knz_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/knz/producer/hellinger/knz_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_mcr_fish <- bind_rows(
  load_lengths(here("outputs/cta/mcr_fish/consumer/bray/mcr_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/mcr_fish/consumer/jaccard/mcr_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/mcr_fish/consumer/chord/mcr_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/mcr_fish/consumer/hellinger/mcr_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/mcr_fish/producer/bray/mcr_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/mcr_fish/producer/jaccard/mcr_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/mcr_fish/producer/chord/mcr_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/mcr_fish/producer/hellinger/mcr_lengths_hellinger.csv"), "producer", "hellinger")
)
lengths_mcr_invert <- bind_rows(
  load_lengths(here("outputs/cta/mcr_invert/consumer/bray/mcr_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/mcr_invert/consumer/jaccard/mcr_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/mcr_invert/consumer/chord/mcr_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/mcr_invert/consumer/hellinger/mcr_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/mcr_invert/producer/bray/mcr_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/mcr_invert/producer/jaccard/mcr_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/mcr_invert/producer/chord/mcr_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/mcr_invert/producer/hellinger/mcr_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_ntl_madison <- bind_rows(
  load_lengths(here("outputs/cta/ntl_madison/consumer/bray/ntl_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/ntl_madison/consumer/jaccard/ntl_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/ntl_madison/consumer/chord/ntl_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/ntl_madison/consumer/hellinger/ntl_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/ntl_madison/producer/bray/ntl_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/ntl_madison/producer/jaccard/ntl_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/ntl_madison/producer/chord/ntl_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/ntl_madison/producer/hellinger/ntl_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_ntl_trout <- bind_rows(
  load_lengths(here("outputs/cta/ntl_trout/consumer/bray/ntl_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/ntl_trout/consumer/jaccard/ntl_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/ntl_trout/consumer/chord/ntl_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/ntl_trout/consumer/hellinger/ntl_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/ntl_trout/producer/bray/ntl_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/ntl_trout/producer/jaccard/ntl_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/ntl_trout/producer/chord/ntl_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/ntl_trout/producer/hellinger/ntl_lengths_hellinger.csv"), "producer", "hellinger")
)


lengths_sbc_fish <- bind_rows(
  load_lengths(here("outputs/cta/sbc_fish/consumer/bray/sbc_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/sbc_fish/consumer/jaccard/sbc_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/sbc_fish/consumer/chord/sbc_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/sbc_fish/consumer/hellinger/sbc_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/sbc_fish/producer/bray/sbc_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/sbc_fish/producer/jaccard/sbc_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/sbc_fish/producer/chord/sbc_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/sbc_fish/producer/hellinger/sbc_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_sbc_invert <- bind_rows(
  load_lengths(here("outputs/cta/sbc_invert/consumer/bray/sbc_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/sbc_invert/consumer/jaccard/sbc_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/sbc_invert/consumer/chord/sbc_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/sbc_invert/consumer/hellinger/sbc_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/sbc_invert/producer/bray/sbc_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/sbc_invert/producer/jaccard/sbc_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/sbc_invert/producer/chord/sbc_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/sbc_invert/producer/hellinger/sbc_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_usvi_fish <- bind_rows(
  load_lengths(here("outputs/cta/usvi_fish/consumer/bray/usvi_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/usvi_fish/consumer/jaccard/usvi_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/usvi_fish/consumer/chord/usvi_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/usvi_fish/consumer/hellinger/usvi_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/usvi_fish/producer/bray/usvi_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/usvi_fish/producer/jaccard/usvi_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/usvi_fish/producer/chord/usvi_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/usvi_fish/producer/hellinger/usvi_lengths_hellinger.csv"), "producer", "hellinger")
)

lengths_usvi_invert <- bind_rows(
  load_lengths(here("outputs/cta/usvi_invert/consumer/bray/usvi_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/usvi_invert/consumer/jaccard/usvi_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/usvi_invert/consumer/chord/usvi_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/usvi_invert/consumer/hellinger/usvi_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/usvi_invert/producer/bray/usvi_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/usvi_invert/producer/jaccard/usvi_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/usvi_invert/producer/chord/usvi_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/usvi_invert/producer/hellinger/usvi_lengths_hellinger.csv"), "producer", "hellinger")
)



fix_plot <- function(df) df %>% dplyr::mutate(plot = as.character(plot))
comp_stability_results <- bind_rows(
  fix_plot(load_lengths(here("outputs/cta/adk/consumer/bray/adk_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/adk/producer/bray/adk_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/bex_ae/consumer/bray/bex_ae_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/bex_ae/producer/bray/bex_ae_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/bex_he/consumer/bray/BEX_HE_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/bex_he/producer/bray/BEX_HE_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/bex_se/consumer/bray/BEX_SE_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/bex_se/producer/bray/BEX_SE_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/cdr/consumer/bray/cdr_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/cdr/producer/bray/cdr_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/gce/consumer/bray/gce_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/gce/producer/bray/gce_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/jrn/consumer/bray/jrn_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/jrn/producer/bray/jrn_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/kbs/consumer/bray/kbs_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/kbs/producer/bray/kbs_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/knz/consumer/bray/knz_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/knz/producer/bray/knz_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/mcr_fish/consumer/bray/mcr_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/mcr_fish/producer/bray/mcr_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/mcr_invert/consumer/bray/mcr_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/mcr_invert/producer/bray/mcr_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/ntl_madison/consumer/bray/NTL_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/ntl_madison/producer/bray/NTL_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/ntl_trout/consumer/bray/NTL_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/ntl_trout/producer/bray/NTL_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/sbc_fish/consumer/bray/sbc_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/sbc_fish/producer/bray/sbc_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/sbc_invert/consumer/bray/sbc_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/sbc_invert/producer/bray/sbc_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/usvi_fish/consumer/bray/usvi_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/usvi_fish/producer/bray/usvi_lengths_bray.csv"), "producer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/usvi_invert/consumer/bray/usvi_lengths_bray.csv"), "consumer", "bray")),
  fix_plot(load_lengths(here("outputs/cta/usvi_invert/producer/bray/usvi_lengths_bray.csv"), "producer", "bray")))
  
  
  
# 11. Summary tables ----
aims_length_summary <- aims_lengths %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(aims_length_summary)

adk_length_summary <- lengths_adk %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(adk_length_summary)

bex_se_length_summary <- lengths_bex_se %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(bex_se_length_summary)


bex_ae_length_summary <- lengths_bex_ae %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(bex_ae_length_summary)

bex_he_length_summary <- lengths_bex_he %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(bex_he_length_summary)

cdr_length_summary <- lengths_cdr %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(cdr_length_summary)

gce_length_summary <- lengths_gce %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(gce_length_summary)

jrn_length_summary <- lengths_jrn %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(jrn_length_summary)

knz_length_summary <- lengths_knz %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(knz_length_summary)

kbs_length_summary <- lengths_kbs %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(kbs_length_summary)


mcr_fish_length_summary <- lengths_mcr_fish %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(mcr_fish_length_summary)

mcr_invert_length_summary <- lengths_mcr_invert %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(mcr_invert_length_summary)



sbc_fish_length_summary <- lengths_sbc_fish %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(sbc_fish_length_summary)

sbc_invert_length_summary <- lengths_sbc_invert %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(sbc_invert_length_summary)


usvi_fish_length_summary <- lengths_usvi_fish %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(usvi_fish_length_summary)

usvi_invert_length_summary <- lengths_usvi_invert %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(usvi_invert_length_summary)


# 12. Figures: Boxplots of total trajectory and mean interannual compositional stability across models ----

#adk
plot_adk_tl_metric_comp <- ggplot(lengths_adk, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (adk)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_adk_tl_metric_comp.png",
  plot = plot_adk_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_adk_mean_metric_comp <- ggplot(lengths_adk, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (adk)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_adk_mean_metric_comp.png",
  plot = plot_adk_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)

#AIMS
plot_aims_tl_metric_comp <- ggplot(aims_lengths, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (AIMS)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_aims_tl_metric_comp.png",
  plot = plot_aims_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_aims_mean_metric_comp <- ggplot(aims_lengths, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (AIMS)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_aims_mean_metric_comp.png",
  plot = plot_aims_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)

#bex_ae
plot_bex_ae_tl_metric_comp <- ggplot(lengths_bex_ae, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (bex_ae)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_bex_ae_tl_metric_comp.png",
  plot = plot_bex_ae_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_bex_ae_mean_metric_comp <- ggplot(lengths_bex_ae, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (bex_ae)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_bex_ae_mean_metric_comp.png",
  plot = plot_bex_ae_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)

#bex_he
plot_bex_he_tl_metric_comp <- ggplot(lengths_bex_he, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (bex_he)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_bex_he_tl_metric_comp.png",
  plot = plot_bex_he_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_bex_he_mean_metric_comp <- ggplot(lengths_bex_he, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (bex_he)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_bex_he_mean_metric_comp.png",
  plot = plot_bex_he_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)

#bex_se
plot_bex_se_tl_metric_comp <- ggplot(lengths_bex_se, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (bex_se)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_bex_se_tl_metric_comp.png",
  plot = plot_bex_se_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_bex_se_mean_metric_comp <- ggplot(lengths_bex_se, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (bex_se)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_bex_se_mean_metric_comp.png",
  plot = plot_bex_se_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)

#CDR
plot_cdr_tl_metric_comp <- ggplot(lengths_cdr, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (CDR)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_cdr_tl_metric_comp.png",
  plot = plot_cdr_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_cdr_mean_metric_comp <- ggplot(lengths_cdr, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (CDR)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_cdr_mean_metric_comp.png",
  plot = plot_cdr_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)

#gce
plot_gce_tl_metric_comp <- ggplot(lengths_gce, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (gce)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_gce_tl_metric_comp.png",
  plot = plot_gce_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_gce_mean_metric_comp <- ggplot(lengths_gce, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (gce)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_gce_mean_metric_comp.png",
  plot = plot_gce_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


#JRN
plot_jrn_tl_metric_comp <- ggplot(lengths_jrn, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (JRN)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_jrn_tl_metric_comp.png",
  plot = plot_jrn_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_jrn_mean_metric_comp <- ggplot(lengths_jrn, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (JRN)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_jrn_mean_metric_comp.png",
  plot = plot_jrn_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)

#KBS
plot_kbs_tl_metric_comp <- ggplot(lengths_kbs, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (KBS)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_kbs_tl_metric_comp.png",
  plot = plot_kbs_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)

plot_kbs_mean_metric_comp <- ggplot(lengths_kbs, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (KBS)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_kbs_mean_metric_comp.png",
  plot = plot_kbs_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)





#KNZ
plot_knz_tl_metric_comp <- ggplot(lengths_knz, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (KNZ)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_knz_tl_metric_comp.png",
  plot = plot_knz_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_knz_mean_metric_comp <- ggplot(lengths_knz, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (KNZ)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_knz_mean_metric_comp.png",
  plot = plot_knz_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


#MCR Fish
plot_mcr_invert_tl_metric_comp <- ggplot(lengths_mcr_fish, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (mcr_fish)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_mcr_fish_tl_metric_comp.png",
  plot = plot_mcr_fish_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_mcr_fish_mean_metric_comp <- ggplot(lengths_mcr_fish, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (mcr_fish)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_mcr_fish_mean_metric_comp.png",
  plot = plot_mcr_fish_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)



#MCR Invert
plot_mcr_invert_tl_metric_comp <- ggplot(lengths_mcr_invert, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (mcr_invert)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_mcr_invert_tl_metric_comp.png",
  plot = plot_mcr_invert_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_mcr_invert_mean_metric_comp <- ggplot(lengths_mcr_invert, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (mcr_invert)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_mcr_invert_mean_metric_comp.png",
  plot = plot_mcr_invert_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


#NTL Madison
plot_ntl_madison_tl_metric_comp <- ggplot(lengths_ntl_madison, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (NTL Madison)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_ntl_madison_tl_metric_comp.png",
  plot = plot_ntl_madison_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_ntl_madison_mean_metric_comp <- ggplot(lengths_ntl_madison, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (NTL Madison)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_ntl_madison_mean_metric_comp.png",
  plot = plot_ntl_madison_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)

#NTL trout
plot_ntl_trout_tl_metric_comp <- ggplot(lengths_ntl_trout, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (NTL trout)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_ntl_trout_tl_metric_comp.png",
  plot = plot_ntl_trout_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_ntl_trout_mean_metric_comp <- ggplot(lengths_ntl_trout, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (NTL trout)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_ntl_trout_mean_metric_comp.png",
  plot = plot_ntl_trout_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)

#sbc Fish
plot_sbc_invert_tl_metric_comp <- ggplot(lengths_sbc_fish, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (sbc_fish)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_sbc_fish_tl_metric_comp.png",
  plot = plot_sbc_fish_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_sbc_fish_mean_metric_comp <- ggplot(lengths_sbc_fish, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (sbc_fish)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_sbc_fish_mean_metric_comp.png",
  plot = plot_sbc_fish_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)



#sbc Invert
plot_sbc_invert_tl_metric_comp <- ggplot(lengths_sbc_invert, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (sbc_invert)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_sbc_invert_tl_metric_comp.png",
  plot = plot_sbc_invert_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_sbc_invert_mean_metric_comp <- ggplot(lengths_sbc_invert, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (sbc_invert)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_sbc_invert_mean_metric_comp.png",
  plot = plot_sbc_invert_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


#usvi Fish
plot_usvi_invert_tl_metric_comp <- ggplot(lengths_usvi_fish, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (usvi_fish)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_usvi_fish_tl_metric_comp.png",
  plot = plot_usvi_fish_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_usvi_fish_mean_metric_comp <- ggplot(lengths_usvi_fish, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (usvi_fish)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_usvi_fish_mean_metric_comp.png",
  plot = plot_usvi_fish_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)



#usvi Invert
plot_usvi_invert_tl_metric_comp <- ggplot(lengths_usvi_invert, aes(x = method, y = total_trajectory, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "Total trajectory length", x = "Distance method",
       title = "Compositional stability across distance methods (usvi_invert)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_usvi_invert_tl_metric_comp.png",
  plot = plot_usvi_invert_tl_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)


plot_usvi_invert_mean_metric_comp <- ggplot(lengths_usvi_invert, aes(x = method, y = mean_segment_length, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1) +
  facet_wrap(~trophic, scales = "free_y") +
  labs(y = "mean segment length", x = "Distance method",
       title = "Compositional stability across distance methods (usvi_invert)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave(
  filename = "figures/cta/plot_usvi_invert_mean_metric_comp.png",
  plot = plot_usvi_invert_mean_metric_comp,
  width = 10,
  height = 7,
  dpi = 600
)

  # End of script ----