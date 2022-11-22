library(tidyverse)

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  stringsAsFactors = FALSE
)

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
# Data Summary
#----------------------------------------------------------------------------#

# This dataset takes in the incarceration data and calculates the percent of a
# county jail population that is black as of 2018
summary_data <- incarceration %>%
  filter(year == 2018) %>% # Filters for 2018 data
  mutate(black_jail_prop = (black_jail_pop / total_jail_pop) * 100) %>%
  # Create the percentage of county jail populations that is black
  mutate(black_jail_prop = round(black_jail_prop, 2)) %>%
  # Round black_jail_prop values
  mutate(black_jail_prop = replace_na(black_jail_prop, 0)) %>%
  # Replace NA values with 0
  select(state, county_name, black_jail_prop) # Remove extraneous columns

# This list holds the summary paragraph values in its elements
summary_list <- list() # Create list

# This element finds the number of errors in the black_jail_prop values
summary_list$errors <- summary_data %>%
  filter(black_jail_prop > 100) %>% # Find values exceeding 100%
  nrow() # Get the number of observations

# This sequence removes erroneous values in the summary dataframe
summary_data <- summary_data %>%
  filter(black_jail_prop <= 100) # Filter out values exceeding 100% (errors)

# This dataset filters for observations with the maximum percent black jail
# populations
max_black_jail_prop <- summary_data %>%
  filter(black_jail_prop == max(black_jail_prop, na.rm = TRUE))
# Filter for the rows with the maximum black_jail_prop value

# This element records the maximum percent black any U.S. county jail population
# is
summary_list$max_value <- max_black_jail_prop %>%
  pull(black_jail_prop) %>% # Pull max percent black value into a vector
  unique() # Remove duplicate values in vector, leaving only one value

# This element records the number of counties with the maximum percent black
# jail populations
summary_list$max_value_count <- max_black_jail_prop %>%
  nrow() # Get count of max black_jail_prop observations

# This element records the locations of the maximum percent black jail
# populations
summary_list$max_location <- max_black_jail_prop %>%
  mutate(location = paste0(county_name, ", ", state)) %>%
  # Create a column with the county name and state of observations
  pull(location) # Pull out locations into a vector

# This dataset filters for observations with the minimum percent black jail
# populations
min_summary <- summary_data %>%
  filter(black_jail_prop == min(black_jail_prop, na.rm = TRUE))
# Filter for the rows with the minimum black_jail_prop value

# This element records the minimum percent black any U.S. county jail population
# is
summary_list$min_value <- min_summary %>%
  pull(black_jail_prop) %>% # Pull out the min percent black value into vector
  unique() # Remove duplicates in vector, leaving only one value

# This element records the number of counties with the minimum percent black
# jail populations
summary_list$min_value_count <- min_summary %>%
  nrow() # Get count of min black_jail_prop observations

# This element records the average black percent of county jail populations in
# the U.S.
summary_list$average <- summary_data %>%
  summarize(avg_blk_jail_prop = mean(black_jail_prop, na.rm = TRUE)) %>%
  # Create average black_jail_prop value across all counties
  mutate(avg_blk_jail_prop = round(avg_blk_jail_prop, 2)) %>%
  # Round avg_blk_jail_prop
  pull() # Pull value into a vector

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#

# Create data on U.S. jail population growth from 1970 to 2018
#
# @description
# This function wrangles the incarceration.csv data, returning a dataframe of
# U.S. jail population for 1970 to 2018.
#
# Returns a data frame with U.S. jail population growth data.
get_year_jail_pop <- function() {
  plot_data <- incarceration %>% # Take in incarceration data
    group_by(year) %>%
    summarise(total_j_pop = sum(total_jail_pop, na.rm = TRUE))
  # Calculate the total jail population for a given year
  return(plot_data)
}

# Create a bar graph of U.S. jail population growth from 1970 to 2018
#
# @description
# This function gets data on U.S. jail population growth from 1970 to 2018 and
# plots it in a bar graph.
#
# Returns a bar graph of U.S. jail population growth from 1970 to 2018.
plot_jail_pop_for_us <- function() {
  jail_pop_plot <- ggplot(data = get_year_jail_pop()) + # Get data
    geom_col(mapping = aes(y = total_j_pop, x = year)) + # Create bar graph
    scale_y_continuous(labels = scales::comma) + # Add commas to y-axis values
    labs( # Create title and axis labels
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Total Jail Population"
    )
  return(jail_pop_plot)
}

## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
#----------------------------------------------------------------------------#

# Create data on U.S. state jail population growth from 1970 to 2018
#
# @description
# This function wrangles the incarceration.csv data, returning a dataframe with
# data on the change in U.S. state jail population growth from 1970 to 2018.
#
# @param states A vector of states to analyze.
#
# Returns a dataframe with U.S. state jail population growth data.
get_jail_pop_by_states <- function(states) {
  jail_data <- incarceration %>% # Take in incarceration dataframe
    filter(state %in% states) %>% # Filter out states not inputted
    group_by(year, state) %>% # Group data
    summarize(total = sum(total_jail_pop, na.rm = TRUE)) # Total jail pop
  return(jail_data)
}

# Create a line graph of U.S. staet jail population growth from 1970 to 2018
#
# @description
# This function gets data on U.S. state jail population growth from 1970 to 2018
# and plots it in a line graph.
#
# @param states A vector of states to analyze.
#
# Returns a line graph of U.S. state jail population growth from 1970 to 2018.
plot_jail_pop_by_states <- function(states) {
  plot <- ggplot(
    data = get_jail_pop_by_states(states), # Get data
    aes( # Assign each axis a value
      x = year,
      y = total,
      color = state,
      group = state
    )
  ) +
    geom_line() + # Create a line for each state
    scale_y_continuous(labels = scales::comma) + # Add commas to y-axis labels
    labs( # Add title, legend, and axis labels
      title = "Increase of U.S. State Jail Populations (1970-2018)",
      x = "Year",
      y = "State Jail Population",
      color = "State"
    )
  return(plot)
}

## Section 5  ----
#----------------------------------------------------------------------------#
# Black Representation in County Jail and Non-inmate Populations
#----------------------------------------------------------------------------#

# Create data on the percent black of U.S. counties and county jails
#
# @description
# This function wrangles the incarceration.csv data, returning a dataframe with
# the percent black of U.S. county and county jail populations for 2018.
#
# Returns a dataframe of U.S. counties with the percent black of each county
# and county jails.
get_black_pop_data <- function() {
  data <- incarceration %>% # Take in incarceration dataframe
    filter(year == 2018) %>% # Filter out years other than 2018
    group_by(county_name) %>%
    mutate(pop_prop_black = black_pop_15to64 / total_pop_15to64) %>%
    # Create the proportion of a county population that is black
    mutate(black_jail_prop = black_jail_pop / total_jail_pop) %>%
    # Create the proportion of a county jail population that is black
    filter(black_jail_prop <= 1) # Remove proportions more than 100%
  return(data)
}

# Create a scatter plot of the percent black of U.S. counties and county jails
#
# @description
# This function creates a scatter plot comparing the percent black of U.S.
# county and county jail populations.
#
# Returns a scatterplot showing the percent black of U.S. counties and county
# jails.
get_black_jail_scatter <- function() {
  plot <- ggplot(get_black_pop_data(), # Get data
    mapping = aes(x = pop_prop_black, y = black_jail_prop) # Assign variables
  ) +
    geom_point() + # Create scatterplot
    scale_x_continuous(labels = scales::percent) + # Label x axis with %s
    scale_y_continuous(labels = scales::percent) + # Label y axis with %s
    labs( # Add title, subtitle and x/y axis labels
      title = "% Black of U.S. County Populations and County Jail Populations",
      subtitle = "(County population aged 15-65, as of 2018)",
      x = "County Population % Black",
      y = "County Jail Population % Black"
    )
  return(plot)
}

## Section 6  ----
#----------------------------------------------------------------------------#
# Geographic Distribution of Black Representation in Jails
#----------------------------------------------------------------------------#

# Create map data with the average percent black of each U.S. state's jails
#
# @description
# This function wrangles the incarceration.csv and map data on U.S. counties and
# states, returning a dataframe with geographical data and the average percent
# black of each U.S. state's county jail populations as of 2018.
#
# Returns a dataframe of contiguous U.S. states with average percent black of
# state's county jails and geographical data on states.
get_black_jail_map_data <- function() {
  plot_data <- incarceration %>% # Take in incarceration dataframe
    filter(year == 2018) %>% # Filter for 2018
    mutate(black_jail_prop = black_jail_pop / total_jail_pop) %>%
    # Get proportion of county jail populations that is black
    filter(black_jail_prop <= 1) %>% # Removes proportions more than 100%
    mutate(subregion = tolower(word(county_name))) %>%
    # Create a column with the name of a county in lowercase
    select(subregion, black_jail_prop) %>% # Removes extraneous columns
    left_join(map_data("county"), by = "subregion") %>%
    # Join data to geographical county data by the shared subregion column
    group_by(region) %>% # Groups data by state
    summarize(mean_blk = mean(black_jail_prop)) %>%
    # Create a column with the mean black jail proportion of a state
    left_join(map_data("state"), by = "region")
  # Join the data to geographical state data by the region column, which was
  # added in the first join (the only way to attach region data easily)
  return(plot_data)
}

# Create a map with the average percent black of each U.S. state's jails
#
# @description
# This function creates a map of the contiguous U.S., with each state colored
# according to the average percent black of the state's county jail populations.
#
# Returns a choropleth map of the contiguous U.S..
map_black_jail_prop <- function() {
  ggplot(get_black_jail_map_data()) + # Get map data
    geom_polygon( # Create blank map, assign geographic variables
      mapping = aes(x = long, y = lat, group = group, fill = mean_blk),
      color = "white",
      size = .1
    ) +
    coord_map() +
    scale_fill_continuous( # Color states depending on avg black % in jails
      low = "#132B43",
      high = "Red",
      labels = scales::percent
    ) +
    labs( # Add title, subtitle, and legend label
      title = "Average % Black of State County Jail Populations",
      subtitle = "(As of 2018)",
      fill = "Avg. % Black of County Jail Populations"
    ) +
    theme_bw() +
    theme( # Remove extraneous plot elements
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(-0.30, 0, 0, 0), "null") # Crops plot whitespace
    )
}
