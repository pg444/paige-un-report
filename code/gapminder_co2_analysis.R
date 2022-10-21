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



#data cleaning

library(readr)
read_csv("data/co2-un-data.csv" , skip = 1)


read_csv("data/co2-un-data.csv", skip = 2, 
         col_names = c("region", "country", "year",
                       "series", "value", "footnotes", "source"))
read_csv("data/co2-un-data.csv" , skip = 1)%>%
  rename(country = ...2)


read_csv("data/co2-un-data.csv", skip = 1)%>%
  rename_all(tolower)
co2_emissions_dirt <- read_csv("data/co2-un-data.csv", skip = 2, 
                               col_names = c("region", "country", "year",
                                             "series", "value", "footnotes", "source"))

#practicing select

co2_emissions_dirt %>% 
  select(country, year, series, value)%>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value)%>%
  #number of observations per year
  count(year)

co2_emissions <- co2_emissions_dirt %>% 
  select(country, year, series, value)%>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value)%>%
  filter(year== 2005) %>%
  select(-year)

# joining data frames

df <- inner_join(gapminder_data_2007, co2_emissions)

#find not joined things
anti_join(gapminder_data_2007, co2_emissions, by = "country")

co2_emissions <- read_csv("data/co2-un-data.csv",
                          skip = 2,
                          col_names=c("region", "country", "year", "series", "value", "footnotes", "source"))%>%
  select(country, year, series, value)%>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value)%>%
  filter(year== 2005) %>%
  select(-year)%>% 
  mutate(country= recode(country,
                         "Bolivia (Plurin. State of)" = "Bolivia", 
                         "United States of America" = "United States",
                         "Venezuela (Boliv. Rep. of)" = "Venezuela"))
# a second antijoin

anti_join(gapminder_data_2007, co2_emissions, by = "country")

gapminder_data_2007 <- read_csv("data/gapminder_data.csv")%>%
  filter(year == 2007 & continent == "Americas") %>%
  select (-year, -continent)%>%
  mutate(country = recode(country, "Puerto Rico" = "United States"))

anti_join(gapminder_data_2007, co2_emissions, by = "country")

gapminder_data_2007 <- read_csv("data/gapminder_data.csv")%>%
  filter(year == 2007 & continent == "Americas") %>%
  select (-year, -continent)%>%
  mutate(country = recode(country, "Puerto Rico" = "United States"))%>%
  group_by(country)%>%
  summarise(lifeExp = sum(lifeExp*pop)/sum(pop),
            gdpPercap=sum(gdpPercap * pop) /sum(pop),
            pop = sum(pop))
inner_join(gapminder_data_2007, co2_emissions, by = "country")


gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by = "country")

gapminder_co2 %>%
  mutate(region = if_else(country== "Canada" | 
                            country == "United States" | 
                            country == "Mexico", "north", "south"))

write_csv(gapminder_co2, "data/gapminder_co2.csv")


#analyzing combined data

ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita_emissions))+
  geom_point() +
  labs(x= "GDP Per Capita", y = "co2 emitted per capita", title = "There is a strong association between nation's GDP \nand the amount of co2 it produces")

#fit a line to our data
ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita_emissions))+
  geom_point() +
  labs(x= "GDP Per Capita", y = "co2 emitted per capita", title = "There is a strong association between nation's GDP \nand the amount of co2 it produces")+
  geom_smooth()

#fit a straight line to the data

ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita_emissions))+
  geom_point() +
  labs(x= "GDP Per Capita", y = "co2 emitted per capita", title = "There is a strong association between nation's GDP \nand the amount of co2 it produces")+
  geom_smooth(method = "lm")
