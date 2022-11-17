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
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#



## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  plot_data <- incarceration %>% 
    group_by(year) %>% 
    summarise(total_j_pop = sum(total_jail_pop, na.rm = T))
return(plot_data)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
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
      y = "State Jail Population"
    )

  return(plot)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

section5_fun <- function(year_choice) {
  data <- incarceration %>% 
    #filter(state ==state_choice) %>% 
    filter(year == year_choice) %>% 
    group_by(county_name) %>% 
    mutate(pop_prop_blk = black_pop_15to64 / total_pop_15to64) %>% 
    mutate(black_jail_prop = black_jail_pop / total_jail_pop)
}

section5_plot <- function(state_choice, year_choice) {
  plot_data <- section5_fun(state_choice, year_choice)
  
  ggplot(data = plot_data,
         mapping = aes(
           x = black_prison_pop,
           y = pop_prop_blk)
         ) + geom_point()
}

plot_data <- section5_fun(2018)

test_plot <- ggplot(data = plot_data) +
       geom_point(mapping = aes(
         x = pop_prop_blk,
         y = black_jail_prop)) + 
  scale_y_continuous(limits = c(0,1))


test_plot

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

section6_data <- function() {
  plot_data <- incarceration %>% 
    filter(year == 2018) %>% 
    mutate(black_jail_prop = black_jail_pop / total_jail_pop) %>% 
    mutate(subregion = tolower(word(county_name))) %>% 
    select(subregion, black_jail_prop)
  return(plot_data)
}

section6_plot <- function() {
  data = section6_data() +
    
}

test_data <- section6_data()

test_map_data <- map_data("county")

test_both <- left_join(test_data, test_map_data, by = "subregion")


test <- ggplot(map_data("county")) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = black_jail_prop,
    linewidth = .1
  ) +
  coord_map()

test
## Load data frame ---- 


