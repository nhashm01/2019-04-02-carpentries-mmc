library(here)
library(tidyverse)   
library(ggplot2)
install.packages("skimr")
library(skimr)

gapminder <- readr::read_csv(here("data/gapminder/raw/gapminder_data.csv"))


#large ugly way of doing means
mean(gapminder$gdpPercap[gapminder$continent =="Africa"])        
mean(gapminder$gdpPercap[gapminder$continent =="Americas"])   

year_country_gdp <- select(gapminder, year, country, gdpPercap)



#the pipes
year_country_gdp <- gapminder %>% # equaivalent of saying select(gapminder) so you don't have to keep saying gapminder
  filter(continent == "Europe") %>%
  select(year, country, gdpPercap) #now putting this lines output in the next argument (filter())

lifeExp_country_year_Africa <- gapminder %>%
  filter(continent == "Africa") %>%
  select(lifeExp, country, year)


year_country_gdp <- gapminder %>% 
  group_by(continent) %>%
  summarize(mean_val = mean(gdpPercap))

lifeExp_bycountry <- gapminder %>% 
  group_by(country) %>%
  summarize(mean_lifeExp = mean(lifeExp))

lifeExp_bycountry %>%
  filter(mean_lifeExp == min(mean_lifeExp) | mean_lifeExp == max(mean_lifeExp))



ggplot(data = gapminder, aes(x = year, y = lifeExp, color = continent)) +
  geom_line() +
  facet_wrap( ~ country)



str(gapminder)
skimr::skim(gapminder)


gap_wide <- read.csv("data/gapminder_wide.csv", stringsAsFactors = FALSE)
str(gap_wide)


gap_long <- gap_wide %>%
  gather(obstype_year, obs_values, starts_with('pop'),
         starts_with('lifeExp'), starts_with('gdpPercap'))
str(gap_long)

# Create gap_wide
gap_wide <- gapminder %>%
  gather(key = 'key', value = 'value', c('pop', 'lifeExp', 'gdpPercap')) %>%
  mutate(year_var = paste(key, year, sep = '_')) %>%
  select(country, continent, year_var, value) %>%
  spread(key = 'year_var', value = 'value')

gap_long <- gap_wide %>%
  gather(obstype_year, obs_values, starts_with('pop'),
         starts_with('lifeExp'), starts_with('gdpPercap'))


gap_long <- gap_wide %>% gather(obstype_year,obs_values,-continent,-country)
str(gap_long)
