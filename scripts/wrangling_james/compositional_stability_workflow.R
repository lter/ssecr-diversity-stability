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

# 6. Load harmonized data for producer and consumer assemblages ----

# KNZ
#producer data
knz_prod_wide <- readr::read_csv(here::here("data/wide_output_minimize", "knz_producers_wide_sub.csv"))
knz_con_wide  <- readr::read_csv(here::here("data/wide_output_minimize", "knz_consumers_wide_sub.csv"))

#KBS
kbs_prod_wide <- read.csv(here::here("data/wide_output_minimize", "kbs_producers_wide_sub.csv"))
kbs_con_wide <- read.csv(here::here("data/wide_output_minimize", "kbs_consumers_wide_sub.csv"))

#SBC 
sbc_invert_prod_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_invert_producers_wide_sub.csv"))
sbc_invert_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_invert_consumers_wide_sub.csv"))
sbc_fish_prod_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_fish_producers_wide_sub.csv"))
sbc_fish_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_fish_consumers_wide_sub.csv"))

# AIMS #
aims_con_wide <- read.csv(here::here("data/wide_output_minimize", "aims_consumers_wide_sub.csv"))
aims_prod_wide <- read.csv(here::here("data/wide_output_minimize", "aims_producers_wide_sub.csv"))

# KBS #
kbs_prod_wide <- read.csv(here::here("data/wide_output_minimize", "kbs_producers_wide_sub.csv"))
kbs_con_wide <- read.csv(here::here("data/wide_output_minimize", "kbs_consumers_wide_sub.csv"))

# CDR OLD FIELD #
cdr_of_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_of_producers_wide_sub.csv"))
cdr_of_con_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_of_consumers_wide_sub.csv"))


# 7. Diagnostic check that all plots surveyed have harmonized site-year combinations ----

# KNZ Match
matched <- match_site_years(knz_prod_wide, knz_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "knz_producer_consumer_year_matching.csv"))
knz_prod_matched <- matched$prod
knz_con_matched  <- matched$con

# KBS Match
matched <- match_site_years(kbs_prod_wide, kbs_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "kbs_producer_consumer_year_matching.csv"))
kbs_prod_matched <- matched$prod
kbs_con_matched  <- matched$con

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

# AIMS Match
matched <- match_site_years(aims_prod_wide, aims_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "aims_producer_consumer_year_matching.csv"))
aims_prod_matched <- matched$prod
aims_con_matched  <- matched$con

# CDR Match
matched <- match_site_years(cdr_of_prod_wide, cdr_of_con_wide)
save_diag(matched$diagnostics, here::here("diagnostics", "cdr_of_producer_consumer_year_matching.csv"))
cdr_of_prod_matched <- matched$prod
cdr_of_con_matched  <- matched$con

# 8. Run analysis pipeline----
# Methods to run
methods <- c("bray", "jaccard", "hellinger", "chord")

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


# SBC 
# can use either fish/invert matched producer file (identical)
for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("sbc_producer_invert_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "sbc", "producer", m)
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
  fig_path <- here::here("figures", "cta", paste0("sbc_consumer_invert_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "sbc", "consumer", m)
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

for (m in methods) {
  fig_path <- here::here("figures", "cta", paste0("sbc_consumer_fish_trajectory_", m, ".png"))
  output_dir <- here::here("outputs", "cta", "sbc", "consumer", m)
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

glimpse(cdr_of_con_matched)
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

# Load composition stability length files ----
# Helper function to load and annotate
load_lengths <- function(path, trophic, method) {
  read_csv(path) %>%
    mutate(
      trophic = trophic,
      method = method
    )
}


knz_lengths <- bind_rows(
  load_lengths(here("outputs/cta/knz/consumer/bray/knz_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/knz/consumer/jaccard/knz_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/knz/consumer/chord/knz_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/knz/consumer/hellinger/knz_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/knz/producer/bray/knz_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/knz/producer/jaccard/knz_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/knz/producer/chord/knz_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/knz/producer/hellinger/knz_lengths_hellinger.csv"), "producer", "hellinger")
)


kbs_lengths <- bind_rows(
  load_lengths(here("outputs/cta/kbs/consumer/bray/kbs_lengths_bray.csv"), "consumer", "bray"),
  load_lengths(here("outputs/cta/kbs/consumer/jaccard/kbs_lengths_jaccard.csv"), "consumer", "jaccard"),
  load_lengths(here("outputs/cta/kbs/consumer/chord/kbs_lengths_chord.csv"), "consumer", "chord"),
  load_lengths(here("outputs/cta/kbs/consumer/hellinger/kbs_lengths_hellinger.csv"), "consumer", "hellinger"),
  load_lengths(here("outputs/cta/kbs/producer/bray/kbs_lengths_bray.csv"), "producer", "bray"),
  load_lengths(here("outputs/cta/kbs/producer/jaccard/kbs_lengths_jaccard.csv"), "producer", "jaccard"),
  load_lengths(here("outputs/cta/kbs/producer/chord/kbs_lengths_chord.csv"), "producer", "chord"),
  load_lengths(here("outputs/cta/kbs/producer/hellinger/kbs_lengths_hellinger.csv"), "producer", "hellinger")
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


# Summary table
length_summary <- knz_lengths %>%
  group_by(trophic, method) %>%
  summarise(
    mean_length = mean(total_trajectory, na.rm = TRUE),
    sd_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  )
print(length_summary)

plot_knz_tl_metric_comp <- ggplot(knz_lengths, aes(x = method, y = total_trajectory, fill = method)) +
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
  plot = plot_knz_stability_metrics,
  width = 10,
  height = 7,
  dpi = 600
)


plot_knz_mean_metric_comp <- ggplot(knz_lengths, aes(x = method, y = mean_segment_length, fill = method)) +
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
  plot = plot_knz_stability_metrics,
  width = 10,
  height = 7,
  dpi = 600
)

# ---- 0) quick safety checks & settings ----
# Ensure the metric column exists and is numeric
stopifnot("mean_segment_length" %in% names(knz_lengths))
knz_lengths <- knz_lengths %>%
  mutate(
    method = factor(method, levels = c("bray", "hellinger", "chord", "jaccard")), # consistent ordering
    trophic = factor(trophic)
  )

# Vector of methods used (ordered)
methods <- levels(knz_lengths$method)
# helper: number of surveys per plot = number of non-NA segments + 1
knz_lengths <- knz_lengths %>%
  rowwise() %>%
  mutate(
    n_segments = sum(!is.na(c_across(starts_with("S")))),
    n_surveys = n_segments + 1
  ) %>%
  ungroup()

# ---- 1) summary table (per trophic x method) ----
length_summary <- knz_lengths %>%
  group_by(trophic, method) %>%
  summarise(
    n_plots = n_distinct(plot),                     
    mean_length = mean(mean_segment_length, na.rm = TRUE),
    sd_length   = sd(mean_segment_length, na.rm = TRUE),
    median_length = median(mean_segment_length, na.rm = TRUE),
    mean_total_traj_length = mean(total_trajectory, na.rm = TRUE),
    sd_total_traj_length   = sd(total_trajectory, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(trophic, method)

length_summary %>% print(n = Inf)

# Save summary for records
dir.create("outputs/analysis/knz", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(length_summary, "outputs/analysis/knz/knz_mean_segment_length_summary_by_method.csv")

# ---- 2) helper: wide, complete-case transformation per trophic ----
make_wide_complete <- function(df, trophic_level) {
  df %>%
    filter(trophic == trophic_level) %>%
    select(plot, method, mean_segment_length) %>%
    pivot_wider(names_from = method, values_from = mean_segment_length) %>%
    arrange(plot)
}



ggplot(kbs_lengths, aes(x = method, y = total_trajectory, fill = method)) +
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
  filename = "figures/cta/kbs_stability_metrics.png",
  plot = p,
  width = 10,
  height = 7,
  dpi = 300
)

  # End of script ----





















# AIMS #
aims_con_wide <- read.csv(here::here("data/wide_output_minimize", "aims_consumers_wide_sub.csv"))
aims_prod_wide <- read.csv(here::here("data/wide_output_minimize", "aims_producers_wide_sub.csv"))

# KBS #
kbs_prod_wide <- read.csv(here::here("data/wide_output_minimize", "kbs_producers_wide_sub.csv"))
kbs_con_wide <- read.csv(here::here("data/wide_output_minimize", "kbs_consumers_wide_sub.csv"))

# CDR OLD FIELD #
cdr_of_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_of_producers_wide_sub.csv"))
cdr_of_con_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_of_consumers_wide_sub.csv"))

# # CDR BIODIVERSITY #
# # numbers describe the initial diversity of producers in each plot
# cdr_biodiv_1_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_1_producers_wide_sub.csv"))
# cdr_biodiv_1_con_wide <-  read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_1_consumers_wide_sub.csv"))
# cdr_biodiv_2_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_2_producers_wide_sub.csv"))
# cdr_biodiv_2_con_wide <-  read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_2_consumers_wide_sub.csv"))
# cdr_biodiv_4_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_4_producers_wide_sub.csv"))
# cdr_biodiv_4_con_wide <-  read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_4_consumers_wide_sub.csv"))
# cdr_biodiv_8_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_8_producers_wide_sub.csv"))
# cdr_biodiv_8_con_wide <-  read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_8_consumers_wide_sub.csv"))
# cdr_biodiv_16_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_16_producers_wide_sub.csv"))
# cdr_biodiv_16_con_wide <-  read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_16_consumers_wide_sub.csv"))

# # GCE #
# gce_prod_wide <- read.csv(here::here("data/wide_output_minimize", "gce_producers_wide_sub.csv"))
# gce_con_wide <- read.csv(here::here("data/wide_output_minimize", "gce_consumers_wide_sub.csv"))

#USVI #
usvi_fish_prod_wide <- read.csv(here::here("data/wide_output_minimize", "usvi_fish_producers_wide_sub.csv"))
usvi_fish_con_wide <- read.csv(here::here("data/wide_output_minimize", "usvi_fish_consumers_wide_sub.csv"))
usvi_invert_prod_wide <- read.csv(here::here("data/wide_output_minimize", "usvi_invert_producers_wide_sub.csv"))
usvi_invert_con_wide <- read.csv(here::here("data/wide_output_minimize", "usvi_invert_consumers_wide_sub.csv"))

#SBC #
sbc_invert_prod_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_invert_producers_wide_sub.csv"))
sbc_invert_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_invert_consumers_wide_sub.csv"))
sbc_fish_prod_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_fish_producers_wide_sub.csv"))
sbc_fish_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_fish_consumers_wide_sub.csv"))

# MCR #
mcr_invert_prod_wide <- read.csv(here::here("data/wide_output_minimize", "mcr_invert_producers_wide_sub.csv"))
mcr_invert_wide <- read.csv(here::here("data/wide_output_minimize", "mcr_invert_consumers_wide_sub.csv"))
mcr_fish_prod_wide <- read.csv(here::here("data/wide_output_minimize", "mcr_fish_producers_wide_sub.csv"))
mcr_fish_wide <- read.csv(here::here("data/wide_output_minimize", "mcr_fish_consumers_wide_sub.csv"))


# Filtering Complete 0's ----

mcr_invert_wide = mcr_invert_wide |>
  filter(year != 2024)

mcr_invert_prod_wide = mcr_invert_prod_wide |>
  filter(year != 2024)

mcr_fish_wide = mcr_fish_wide |>
  filter(year != 2024)

mcr_fish_prod_wide = mcr_fish_prod_wide |>
  filter(year != 2024)

sbc_fish_wide = sbc_fish_wide |> 
  filter(plot != "AHND")

sbc_fish_prod_wide = sbc_fish_prod_wide |> 
  filter(plot != "AHND")

sbc_invert_wide = sbc_invert_wide |> 
  filter(plot != "AHND")

sbc_invert_prod_wide = sbc_invert_prod_wide |> 
  filter(plot != "AHND")

# Run CTA Pipeline ----

#KNZ Producers
run_cta_pipeline(
  df = knz_prod_wide,
  site_id = "knz",
  fig_path = here("figures/CTA/wide_output_minimize", "knz_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/producer", "knz_producer_legnths_bray.csv"))

#KNZ Consumers
run_cta_pipeline(
  df = knz_con_wide,
  site_id = "knz",
  fig_path = here("figures/CTA/wide_output_minimize", "knz_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/consumer", "knz_consumer_legnths_bray.csv"))


#AIMS Producers
run_cta_pipeline(
  df = aims_prod_wide,
  site_id = "aims",
  fig_path = here("figures/CTA/wide_output_minimize", "aims_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/producer", "aims_producer_legnths_bray.csv"))

#AIMS Consumers
run_cta_pipeline(
  df = aims_con_wide,
  site_id = "aims",
  fig_path = here("figures/CTA/wide_output_minimize", "aims_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/consumer", "aims_consumer_legnths_bray.csv"))


#KBS Producers
run_cta_pipeline(
  df = kbs_prod_wide,
  site_id = "kbs",
  fig_path = here("figures/CTA/wide_output_minimize", "kbs_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/producer", "kbs_producer_legnths_bray.csv"))

#KBS Consumers
run_cta_pipeline(
  df = kbs_con_wide,
  site_id = "kbs",
  fig_path = here("figures/CTA/wide_output_minimize", "kbs_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/consumer", "kbs_consumer_legnths_bray.csv"))

#CDR Oldfield Producers
run_cta_pipeline(
  df = cdr_of_prod_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/wide_output_minimize", "cdr_of_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/producer", "cdr_of_producer_legnths_bray.csv"))

#CDR Oldfield Consumer
run_cta_pipeline(
  df = cdr_of_con_wide,
  site_id = "cdr_of",
  fig_path = here("figures/CTA/wide_output_minimize", "cdr_of_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/consumer", "cdr_of_consumer_legnths_bray.csv"))

# #CDR Biodiv 1 Producers
# run_cta_pipeline(
#   df = cdr_biodiv_1_prod_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_1_producer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/producer", "cdr_biodiv_1_producer_legnths_bray.csv"))
# 
# #CDR Biodiv 1 Consumers
# run_cta_pipeline(
#   df = cdr_biodiv_1_con_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_1_consumer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/consumer", "cdr_biodiv_1_consumer_legnths_bray.csv"))
# 
# #CDR Biodiv 2 Producers
# run_cta_pipeline(
#   df = cdr_biodiv_2_prod_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_2_producer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/producer", "cdr_biodiv_2_producer_legnths_bray.csv"))
# 
# #CDR Biodiv 2 Consumers
# run_cta_pipeline(
#   df = cdr_biodiv_2_con_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_2_consumer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/consumer", "cdr_biodiv_2_consumer_legnths_bray.csv"))
# 
# #CDR Biodiv 4 Producers
# run_cta_pipeline(
#   df = cdr_biodiv_4_prod_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_4_producer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/producer", "cdr_biodiv_4_producer_legnths_bray.csv"))
# 
# #CDR Biodiv 4 Consumers
# run_cta_pipeline(
#   df = cdr_biodiv_4_con_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_4_consumer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/consumer", "cdr_biodiv_4_consumer_legnths_bray.csv"))
# 
# #CDR Biodiv 8 Producers
# run_cta_pipeline(
#   df = cdr_biodiv_8_prod_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_8_producer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/producer", "cdr_biodiv_8_producer_legnths_bray.csv"))
# 
# #CDR Biodiv 8 Consumers
# run_cta_pipeline(
#   df = cdr_biodiv_8_con_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_8_consumer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/consumer", "cdr_biodiv_8_consumer_legnths_bray.csv"))
# 
# #CDR Biodiv 16 Producers
# run_cta_pipeline(
#   df = cdr_biodiv_16_prod_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_16_producer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/producer", "cdr_biodiv_16_producer_legnths_bray.csv"))
# 
# #CDR Biodiv 16 Consumers
# run_cta_pipeline(
#   df = cdr_biodiv_16_con_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_16_consumer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/consumer", "cdr_biodiv_16_consumer_legnths_bray.csv"))

#GCE Producers
# run_cta_pipeline(
#   df = gce_prod_wide,
#   site_id = "gce",
#   fig_path = here("figures/CTA/wide_output_minimize", "gce_producer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/producer", "gce_producer_legnths_bray.csv"))
# # 
# #GCE Consumers
# run_cta_pipeline(
#   df = gce_con_wide,
#   site_id = "gce",
#   fig_path = here("figures/CTA/GCE", "gce_consumer_trajectory_plot.png"),
#   table_path = here("tables/GCE/consumer", "gce_consumer_legnths_bray.csv"))


#USVI Fish Producers
run_cta_pipeline(
  df = usvi_fish_prod_wide,
  site_id = "usvi",
  fig_path = here("figures/CTA/wide_output_minimize", "usvi_fish_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/producer", "usvi_fish_producer_legnths_bray.csv"))

#USVI Fish Consumers
run_cta_pipeline(
  df = usvi_fish_con_wide,
  site_id = "usvi",
  fig_path = here("figures/CTA/wide_output_minimize", "usvi_fish_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/consumer", "usvi_fish_consumer_legnths_bray.csv"))

#USVI Invert Producers
run_cta_pipeline(
  df = usvi_invert_prod_wide,
  site_id = "usvi",
  fig_path = here("figures/CTA/wide_output_minimize", "usvi_invert_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/producer", "usvi_invert_producer_legnths_bray.csv"))

#USVI Invert Consumers
run_cta_pipeline(
  df = usvi_invert_con_wide,
  site_id = "usvi",
  fig_path = here("figures/CTA/wide_output_minimize", "usvi_invert_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/consumer", "usvi_invert_consumer_legnths_bray.csv"))


# SBC Fish Producers
run_cta_pipeline(
  df = sbc_fish_prod_wide,
  site_id = "sbc",
  fig_path = here("figures/CTA/wide_output_minimize", "sbc_fish_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/producer", "sbc_fish_producer_legnths_bray.csv"))

#SBC Fish Consumers
run_cta_pipeline(
  df = sbc_fish_wide,
  site_id = "sbc",
  fig_path = here("figures/CTA/wide_output_minimize", "sbc_fish_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/consumer", "sbc_fish_consumer_legnths_bray.csv"))

#SBC Invert Producers
run_cta_pipeline(
  df = sbc_invert_prod_wide,
  site_id = "sbc",
  fig_path = here("figures/CTA/wide_output_minimize", "sbc_invert_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/producer", "sbc_invert_producer_legnths_bray.csv"))

#SBC Invert Consumers
run_cta_pipeline(
  df = sbc_invert_wide,
  site_id = "sbc",
  fig_path = here("figures/CTA/wide_output_minimize", "sbc_invert_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/consumer", "sbc_invert_consumer_legnths_bray.csv"))


#MCR Fish Producers
run_cta_pipeline(
  df = mcr_fish_prod_wide,
  site_id = "mcr",
  fig_path = here("figures/CTA/wide_output_minimize", "mcr_fish_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/producer", "mcr_fish_producer_legnths_bray.csv"))

#MCR Fish Consumers
run_cta_pipeline(
  df = mcr_fish_wide,
  site_id = "mcr",
  fig_path = here("figures/CTA/wide_output_minimize", "mcr_fish_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/consumer", "mcr_fish_consumer_legnths_bray.csv"))

#MCR Invert Producers
run_cta_pipeline(
  df = mcr_invert_prod_wide,
  site_id = "mcr",
  fig_path = here("figures/CTA/wide_output_minimize", "mcr_invert_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/producer", "mcr_invert_producer_legnths_bray.csv"))

# #MCR Invert Consumers
run_cta_pipeline(
  df = mcr_invert_wide,
  site_id = "mcr",
  fig_path = here("figures/CTA/wide_output_minimize", "mcr_invert_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_minimize/consumer", "mcr_invert_consumer_legnths_bray.csv"))