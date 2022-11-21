library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num = 6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ----
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

summary_data <- incarceration %>%
  filter(year == 2018) %>%
  mutate(black_jail_prop = (black_jail_pop / total_jail_pop) * 100) %>%
  mutate(black_jail_prop = round(black_jail_prop, 2)) %>%
  mutate(black_jail_prop = replace_na(black_jail_prop, 0)) %>%
  select(state, county_name, black_jail_prop)

summary_list <- list()

summary_list$errors <- summary_data %>%
  filter(black_jail_prop > 100) %>%
  nrow()

max_black_jail_prop <- summary_data %>%
  filter(black_jail_prop <= 100) %>%
  filter(black_jail_prop == max(black_jail_prop, na.rm = TRUE))

summary_list$max_location <- max_black_jail_prop %>%
  mutate(location = paste0(county_name, ", ", state)) %>%
  pull(location)


summary_list$max_value <- max_black_jail_prop %>%
  pull(black_jail_prop) %>%
  unique()

summary_list$max_value_count <- max_black_jail_prop %>%
  nrow()

summary_list$min_value <- summary_data %>%
  filter(black_jail_prop == min(black_jail_prop, na.rm = TRUE)) %>%
  pull(black_jail_prop) %>%
  unique()

summary_list$min_value_count <- summary_data %>%
  filter(black_jail_prop == min(black_jail_prop, na.rm = TRUE)) %>%
  nrow()

summary_list$average <- summary_data %>%
  filter(black_jail_prop <= 100) %>%
  summarize(avg_blk_jail_prop = mean(black_jail_prop, na.rm = TRUE)) %>%
  mutate(avg_blk_jail_prop = round(avg_blk_jail_prop, 2)) %>%
  pull()

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# Creates data on U.S. prison population growth
# This function generates the data that shows the growth of the U.S. prison
# population.
get_year_jail_pop <- function() {
  plot_data <- incarceration %>%
    group_by(year) %>%
    summarise(total_j_pop = sum(total_jail_pop, na.rm = T))
  return(plot_data)
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function() {
  plot <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(y = total_j_pop, x = year)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Total Jail Population"
    )
  return(plot)
}

plot_jail_pop_for_us()

## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_jail_pop_by_states <- function(states) {
  jail_data <- incarceration %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarize(total = sum(total_jail_pop, na.rm = TRUE))
  return(jail_data)
}

plot_jail_pop_by_states <- function(states) {
  plot <- ggplot(
    data = get_jail_pop_by_states(states),
    aes(
      x = year,
      y = total,
      color = state,
      group = state
    )
  ) +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Increase of U.S. State Jail Populations (1970-2018)",
      x = "Year",
      y = "State Jail Population",
      color = "State"
    )
  return(plot)
}


## Section 5  ----
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_black_pop_data <- function(year_choice) {
  data <- incarceration %>%
    filter(year == year_choice) %>%
    group_by(county_name) %>%
    mutate(pop_prop_black = black_pop_15to64 / total_pop_15to64) %>%
    mutate(black_jail_prop = black_jail_pop / total_jail_pop) %>%
    filter(black_jail_prop <= 1)
  return(data)
}

get_black_jail_scatter <- function(year_choice) {
  plot <- ggplot(get_black_pop_data(year_choice),
    mapping = aes(x = pop_prop_black, y = black_jail_prop)
  ) +
    geom_point() +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "% Black of U.S. County Populations and County Jail Populations",
      subtitle = "(County population aged 15-65, data as of 2018)",
      x = "County Population % Black",
      y = "County Jail Population % Black"
    )
  return(plot)
}

## Section 6  ----
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_black_jail_map_data <- function() {
  plot_data <- incarceration %>%
    filter(year == 2018) %>%
    mutate(black_jail_prop = black_jail_pop / total_jail_pop) %>%
    filter(black_jail_prop <= 1) %>%
    mutate(subregion = tolower(word(county_name))) %>%
    select(subregion, black_jail_prop) %>%
    left_join(map_data("county"), by = "subregion") %>%
    group_by(region) %>%
    summarize(mean_blk = mean(black_jail_prop)) %>%
    left_join(map_data("state"), by = "region")
  return(plot_data)
}

map_black_jail_prop <- function() {
  ggplot(get_black_jail_map_data()) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = mean_blk),
      color = "white",
      size = .1
    ) +
    coord_map() +
    scale_fill_continuous(
      low = "#132B43",
      high = "Red",
      labels = scales::percent
    ) +
    labs(
      title = "Average % Black of State Jail Population",
      fill = "Avg. % Black of Jail Population"
    ) +
    theme_bw() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin=unit(c(-0.30,0,0,0), "null")    )
}

map_black_jail_prop()
## Load data frame ----
