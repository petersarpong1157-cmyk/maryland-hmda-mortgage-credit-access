# Maryland HMDA: Spatial autocorrelation robustness
# Requires sf, spdep, readr, dplyr

library(sf)
library(spdep)
library(readr)
library(dplyr)

tract_file <- 'JREL_extension_results/figures/Figure_4_Tract_Data.csv'
shp_file <- 'JREL_extension_results/figures/census_tract_boundaries/tl_2025_24_tract.shp'

stopifnot(file.exists(tract_file), file.exists(shp_file))

tract <- read_csv(tract_file, show_col_types = FALSE,
                  col_types = cols(GEOID = col_character(), .default = col_guess()))
md <- st_read(shp_file, quiet = TRUE)
spatial_data <- left_join(md, tract, by = 'GEOID') |>
  filter(!is.na(minority_pct)) |>
  arrange(GEOID) |>
  st_transform(5070) |>
  st_make_valid()

stopifnot(nrow(spatial_data) == 1433)

run_moran <- function(queen) {
  nb <- poly2nb(spatial_data, queen = queen)
  lw <- nb2listw(nb, style = 'W', zero.policy = TRUE)
  set.seed(2026)
  mc <- moran.mc(spatial_data$minority_pct, lw,
                 nsim = 999, alternative = 'greater',
                 zero.policy = TRUE)
  data.frame(
    Weights = if (queen) 'Queen contiguity' else 'Rook contiguity',
    Tracts = nrow(spatial_data),
    Isolated = sum(card(nb) == 0),
    Mean_Neighbors = mean(card(nb)),
    Moran_I = unname(mc$statistic),
    Permutation_P = mc$p.value
  )
}

results <- bind_rows(run_moran(TRUE), run_moran(FALSE))
print(results)

write_csv(results,
  'JREL_extension_results/spatial_statistics/JREL_Moran_Robustness.csv')
