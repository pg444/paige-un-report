library(tidyverse)
library(readr)
gapminder_data <- read_csv("data/gapminder_data.csv")

# Summarizing our Data
summarise(gapminder_data, averagelifeExp=mean(lifeExp))

gapminder_data %>% summarise(averagelifeExp=mean(lifeExp))
gapminder_data_summarised <- gapminder_data %>% 
  summarise(averagelifeExp=mean(lifeExp))

# Filtering our data using filter()
gapminder_data %>% 
  filter(year== 2007) %>%
  summarise(average=mean(lifeExp))
gapminder_data %>%
  filter(year == 1952) %>%
  summarize(average=mean(gdpPercap))

#finding earliest year in data
gapminder_data %>% summarise(first_year = min(year))

gapminder_data %>% filter(year == 1952) %>% 
  summarise(averagegdp=mean(gdpPercap))
#keep in mind this average doesn't reflect any countries that formed after 1952 

#grouping data, what is the mean life expectancy by year?
gapminder_data %>%
  group_by(year) %>%
  summarise(average=mean(lifeExp))


gapminder_data %>%
  group_by(continent) %>%
  summarise(average=mean(lifeExp), min = min(lifeExp))


# adding new columns with mutate() comma and enter to do new column

gapminder_data %>%
  mutate(gdp = pop * gdpPercap,
         popInMillions=pop/1000000)
# subset columns or change their order with select () you can get rid of all except the ones you enter or else subtract the ones you dont want depending on how many


gapminder_data %>%
  select(pop, year)


gapminder_data %>%
  select(-pop, -gdpPercap, -year)

gapminder_data %>%
  select(gdpPercap, everything())



# moving between long and wide data with pivot_wider() and pivot_longer()

gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)


# final data set for analysis
gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)
