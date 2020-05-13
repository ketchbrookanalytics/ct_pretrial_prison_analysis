library(tidyverse)
library(RSocrata)
library(data.table)
library(glue)

# No scientific notation
options(scipen = 999)

# You can use the following line to download the data directly from the Socrata API, but it is slow
# data <- RSocrata::read.socrata("https://data.ct.gov/resource/b674-jy6w.csv")

# Create a tibble called 'data'
data <- data.table::fread("pretrial_data.csv") %>%
  tibble::as_tibble() %>% 
  # Parse the date from the 'download_date' and 'latest_admission_date' column variables
  dplyr::mutate(
    download_date = as.Date(substr(download_date, 1, 10)), 
    latest_admission_date = as.Date(substr(latest_admission_date, 1, 10))
  ) %>% 
  # Only keep the most recent upload of the data to the repository 
  # (data is uploaded daily, causing a lot of redundancy for analysis purposes)
  dplyr::filter(download_date == max(download_date)) %>% 
  # Keep only the desired columns by name
  dplyr::select(
    identifier, 
    latest_admission_date, 
    race, 
    gender, 
    age, 
    bond_amount, 
    offense, 
    facility, 
    detainer
  ) %>% 
  # Remove any duplicates in the 'identifier' column variable (the unique ID for 
  # each inmate), keeping the rest of the columns in the result
  dplyr::distinct(
    identifier, 
    .keep_all = T
  ) %>% 
  # Keep only observations where the inmate is not being detained for a special 
  # reason, on behalf of another state or the Fed, etc.
  dplyr::filter(detainer == "NONE")

# Define keywords found in the 'offense' column variable that we consider to be
# indicative that the offense was violent
violent_offenses <- c(
  "ASSAULT", 
  "INJURY", 
  "MURDER", 
  "MANSLAUGHTER", 
  "STRANGULATION", 
  "UNLAWFUL RESTRAINT", 
  "KIDNAPPING", 
  "ROBBERY", 
  "BURGLARY", 
  "HOME INVASION"
)

# Create a function for text filtering given a vector of strings
# We'll map this against the "offense" column variable in the dataframe to remove violent offenses
to_drop <- function(fixed_string, text) {
  !stringr::str_detect(text, stringr::fixed(fixed_string, ignore_case = TRUE))
}

# Create a new tibble called 'nonviolent_tbl', using the 'data' tibble
nonviolent_tbl <- data %>% 
  # Drop any observations where the offense is in the 'violent_offenses' character vector
  # Note: this uses partial matching not full matching
  dplyr::filter(
    violent_offenses %>%
      purrr::map(~ to_drop(.x, text = offense)) %>%
      purrr::pmap_lgl(all)
  )

# View summary statistics of the number of nonviolent inmates by race
nonviolent_tbl %>% 
  dplyr::count(race)

# View summary statistics of the number of nonviolent inmates age 65+
nonviolent_tbl %>% 
  dplyr::filter(age >= 65)

# View summary statistics on the most common types of nonviolent offenses
nonviolent_tbl %>% 
  dplyr::count(offense) %>% 
  dplyr::arrange(dplyr::desc(n))


# Plots -------------------------------------------------------------------

# Create the plot signature
signature <- "Michael Thomas, Ketchbrook Analytics, May 2020."

# Create the first footnote
anot1 <- "⁺ Ethnicity-Based Risk follows the research conducted by New York City and published on the CDC website on April 22, 2020, titled \"COVID-19 in Racial and Ethnic Minority Groups\"."

# Create the second footnote
anot2 <- paste0(
  "* Nonviolent offenses were composed of all offense types except the following: ", 
  paste(violent_offenses, collapse = ", "), 
  "."
)

# Create a plot using the 'nonviolent_tbl' tibble
p <- nonviolent_tbl %>% 
  # Remove the 4 observations where the value for the 'race' column variable is "AMER IND"
  # since we do not have any information on COVID-19 mortaliaty rates for this ethnicity
  dplyr::filter(race != "AMER IND") %>% 
  # Create a new column variable called 'Ethnicity-Based Risk' that uses the following logic:
  #   1. If the value for 'race' is "BLACK", then "Highest Risk"
  #   2. If the value for 'race' is "HISPANIC", then "Highest Risk"
  #   3. Otherwise (i.e., if the value for 'race' is "WHITE" or "ASIAN"), "Normal Risk"
  # Note: this is based upon the CDC article in footnote 1
  dplyr::mutate(
    `Ethnicity-Based Risk` = factor(dplyr::case_when(
      race == "BLACK" ~ "Highest Risk", 
      race == "HISPANIC" ~ "Higher Risk", 
      TRUE ~ "Normal Risk"
    )
  )) %>% 
  # Ensure that the values for 'Ethnic-Based Risk' are ordered appropriately for chart legend
  dplyr::mutate(`Ethnicity-Based Risk` = forcats::fct_relevel(
    `Ethnicity-Based Risk`, 
    "Highest Risk", 
    "Higher Risk", 
    "Normal Risk" 
  )) %>% 
  ggplot2::ggplot() + 
  # Use age as the variable to create the histogram on
  # Color the border on each bar white
  # Fill the color inside each bar based upon the value for `Ethinicity-Based Risk`
  ggplot2::aes(
    age, 
    fill = `Ethnicity-Based Risk`
  ) + 
  # Format the histogram aesthetics 
  ggplot2::geom_histogram(
    alpha = 0.9, 
    binwidth = 2, 
    size = 1.2, 
    color = "white"
  ) + 
  # Choose colors for filling the bars
  ggplot2::scale_fill_manual(values=c("#FF372C", "#E69F00", "#56B4E9")) + 
  # Set breaks for the x-axis
  ggplot2::scale_x_continuous(breaks = seq(0, 70, 10)) + 
  # Add a vertical dotted line indicating age 65
  ggplot2::geom_vline(
    xintercept = 65, 
    linetype = "dashed", 
    size = 1.5
  ) + 
  # Add labels for the x-axis, y-axis, title, subtitle, and footnotes
  ggplot2::labs(
    x = "Age of Inmate", 
    y = "Number of Inmates Awaiting Trial", 
    title = "COVID-19 Ethnicity-Based Risk⁺ by Age", 
    subtitle = "Within Connecticut Prison Population Awaiting Trial for Nonviolent* Offenses", 
    caption = glue::glue(
      signature, 
      "", 
      anot1, 
      anot2, 
      .sep = "\n"
    )
  ) + 
  # Set background and text aesthetics
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "#3D6098"), 
    panel.background = ggplot2::element_rect(fill = "#3D6098"), 
    legend.background = ggplot2::element_rect(fill = "#3D6098"), 
    text = ggplot2::element_text(color = "white"), 
    axis.text = ggplot2::element_text(color = "white")
  ) + 
  # Add annotation describing significance of vertical dotted line
  ggplot2::annotate(
    geom = "text", 
    color = "white", 
    size = 2.5, 
    x = 65.3, 
    y = 94.3, 
    label = "Individuals age 65+\nare at higher risk of\ndying from COVID-19,\nper the CDC", 
    hjust = "left",
    fontface = "italic"
  )
  
# Save the plot to the current working directory
# ggplot2::ggsave(
#   filename = "covid19_nonviolent_inmate_risk_ct.png", 
#   plot = p
# )


