# 2018 U.S. Asphalt Emissions - choropleth map
# Reproducible script: download EPA data, clean, and visualize.

# --- 1. Package management ---
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, readxl, usmap)

# --- 2. Directory logic ---
for (dir in c("data", "plots")) {
  if (!dir.exists(dir)) dir.create(dir)
}

# --- 3. Download ---
url <- "https://pasteur.epa.gov/uploads/10.23719/1531683/AP_2018_State_County_Inventory.xlsx"
dest <- "data/AP_2018_State_County_Inventory.xlsx"

if (!file.exists(dest)) {
  tryCatch(
    download.file(url, dest, mode = "wb"),
    error = function(e) {
      stop("Download failed: ", e$message,
           "\nPlease check the EPA URL and your internet connection.")
    }
  )
} else {
  message("Data file already exists, skipping download.")
}

# --- 4. Read and clean ---
emissions_data <- tryCatch(
  readxl::read_excel(dest, sheet = "Output - State", .name_repair = "unique_quiet") |>
    dplyr::rename(
      state = "State",
      emissions_per_capita = "Total kg/person"
    ) |>
    dplyr::mutate(
      # Convert emissions to numeric; suppressWarnings ignores NAs from
      # non-numeric placeholders (e.g. "NA"), leaving missing states for the map.
      emissions_per_capita = suppressWarnings(as.numeric(as.character(emissions_per_capita))),
      state = tolower(state)
    ),
  error = function(e) {
    stop("Failed to read Excel file: ", e$message,
         "\nThe sheet name or column layout may have changed; check the EPA file.")
  }
)

# --- 5. Visualization ---
median_emissions <- median(emissions_data$emissions_per_capita, na.rm = TRUE)

p <- usmap::plot_usmap(data = emissions_data, values = "emissions_per_capita") +
  ggplot2::scale_fill_gradient2(
    low = "darkgreen",
    mid = "yellow",
    high = "red",
    midpoint = median_emissions,
    na.value = "grey90"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.background = ggplot2::element_rect(fill = "white", color = NA)
  ) +
  ggplot2::labs(
    title = "Per Capita Asphalt Emissions by State (2018)",
    subtitle = "Total kilograms per person based on EPA State County Inventory data.",
    caption = "Source: EPA (2023) | DOI: 10.1039/D3EA00066D",
    fill = "Emissions (kg/person)"
  ) +
  ggplot2::theme(legend.position = "right",
                plot.caption = ggplot2::element_text(hjust = 0))

# --- 6. Export ---
tryCatch(
  ggplot2::ggsave("plots/asphalt_emissions_map.png", p,
                  width = 10, height = 7, dpi = 300, bg = "white"),
  error = function(e) {
    stop("Failed to save plot: ", e$message)
  }
)

message("Done. Map saved to plots/asphalt_emissions_map.png")
