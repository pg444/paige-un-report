library(tidyverse)
gapminder1997 <- read_csv("gapminder_1997.csv")
#Plotting!
ggplot(data = gapminder1997)+
  aes(x=gdpPercap)+
  labs(x="GDP Per Capita")+
  aes(y=lifeExp)+
    labs(y="Life Expectancy")+
  geom_point()+
  labs(title="Do People in Wealthy Countries Live Longer?")+
  aes(color= continent)+
  scale_color_brewer(palette = "Set1")+
  aes(size=pop/1000000)+
  labs(size= "Population in Millions")
ggplot(data=gapminder1997)+
  aes(x=gdpPercap,y=lifeExp,color=continent,size=pop/1000000)+
  geom_point()+
  scale_color_brewer(palette="Set1")+
  labs(x="GDP Per Capita", y="Life Expectancy", title="Do People in Wealthy Countries Live Longer", size="Population in Millions")
read.csv("gapminder_data.csv")
gapminderdata <- read.csv("gapminder_data.csv")
ggplot(data=gapminderdata)+
  aes(x=year, y=lifeExp, color=continent)+
  geom_point()
str(gapminderdata)
ggplot(data=gapminder_1997, aes(x=continent, y=lifeExp))+
  geom_violin(aes(fill=continent))+
  geom_jitter(alpha=0.7)


ggplot(gapminder1997)+
  aes(x=lifeExp)+
  geom_histogram()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5, hjust=1))


ggplot(gapminder1997)+
  aes(x=gdpPercap, y=lifeExp)+
  geom_point()+
  facet_wrap(vars(continent))

ggplot(gapminder1997)+
  aes(x=gdpPercap, y=lifeExp)+
  geom_point()+
  facet_grid(rows=vars(continent))

ggsave("awesomeplot.jpg", width=6, height=4)
violin_plot<-ggplot(data=gapminder1997)+
  aes(x=continent, y=lifeExp)+
  geom_violin(aes(fill=continent))
violin_plot
violin_plot+theme_bw()

violin_plot <- violin_plot +theme_bw()
ggsave(violin_plot,
       filename="awesomeviolinplot.jpg", width =6, height =4)

install.packages(c("gganimate", "gifski"))
library(gganimate)
library(gifski)


staticHansPlot <- ggplot(data=gapminderdata)+
  aes(x=log(gdpPercap), y=lifeExp, size=pop/1000000, color=continent)+
  geom_point(alpha=0.5)+
  scale_color_brewer(palette="Set1")+
  labs(x="GDP Per Capita", y= "Life Expectancy", color= "Continent", size= "population in millions")+
  theme_classic()

staticHansPlot

animatedHansPlot<-staticHansPlot+
  transition_states(year, transition_length = 1, state_length = 1)+
  ggtitle("{closest_state")

anim_save("hansAnimatedPlot.gif",
          plot=animatedHansPlot,
          renderer = gifski())
