### GETTING THE LIBRARIES
if (!require(pacman)) #<1>
  install.packages(pacman)


pacman::p_load(tidyverse,
               openintro,
               gridExtra,
               tidytuesdayR,
               dplyr,
               dlookr,
               DMwR2,  # Data Mining with R functions
               GGally, # Pair-wise plots using ggplot2
               Hmisc,  # Data analysis
               formattable)   #<2>

### setting theme for ggplot2
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 14, base_family = "sans"))


# setting width of code output
options(width = 65)

# setting figure parameters for knitr
knitr::opts_chunk$set(
  fig.width = 8,        # 8" width
  fig.asp = 0.618,      # the golden ratio
  fig.retina = 4,       # dpi multiplier for displaying HTML output on retina
  fig.align = "center", # center align figures
  dpi = 350             # higher dpi, sharper image
)

tuesdata <- tidytuesdayR::tt_load('2021-09-07')

results <- tuesdata$results
driver_standings <- tuesdata$driver_standings
races <- tuesdata$races
drivers <- tuesdata$drivers


driver_results_df <- driver_standings |>
  left_join(races, by = "raceId") |>
  rename(driver_url = url) |>
  left_join(drivers, by = "driverId") |> filter( year >= 2020)

missing_count <- colSums(is.na(driver_results_df))

missing_count

calculateAge <- function(dob) {
  # Convert DOB to Date class
  dob <- as.Date(dob)
  
  # Calculate age
  age <- as.integer(difftime(Sys.Date(), dob, units = "days") / 365)
  
  return(age)
}

getRacePos <- function(position) {
  case_when(
    position == 1 ~ "WINNER",
    (position > 1 & position <=3) ~ "PODIUM",
    (position > 3 & position <=10) ~ "TOP10",
    TRUE ~ "Other"  # Default case when none of the above conditions are met
  )
}


driver_results_df <- driver_results_df |> mutate(
  drivers_age = calculateAge(driver_results_df$dob),
  race_pos = getRacePos(driver_results_df$position)
)

ggplot(driver_results_df, mapping = aes(x = code, y = drivers_age)) +
  geom_point(aes(color = race_pos, size=3), stat='identity') + 
  coord_flip()

