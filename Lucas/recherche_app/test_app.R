library(shiny)
library(tidyverse)
library(tidyr)

#### La base de données comporte plusieurs soucis : 
  #### * Aprés 2016 elle est peu fiable
  #### * Les jeux sont séparés en platform. Càd que nous avons des duplications de jeux. A rassembler pour potentiel usage


df <- read_csv('vgsales.csv')

df <- df  %>% 
  mutate(Platform = as.factor(Platform)) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Genre = as.factor(Genre)) %>% 
  mutate(Publisher = as.factor(Publisher))

sum(is.na(df$Year))

fct_count(df$Genre[df$Year=="1995"], sort = T)

fct_count(df$Publisher, sort = T, prop = T)[1:10,]

fct_count(df$Platform, sort = T)


### Graphique représentant le nombre de jeu sortit pour le top 10 des 'publisher' dans une année 
### Ce graphique serait un peu plus pertinent si on enlevé le doublons name
df %>% 
  filter(Year == "1995") %>% 
  select(everything()) %>%
  summarize(fct_count(Genre, sort = T)) %>%
  slice(1:10) %>%
  ggplot() + 
  aes(x=fct_reorder(f, n), y = n) + 
  geom_col() + 
  coord_flip()


# df %>% 
#   filter(Year == "2015") %>% 
#   select(everything()) %>%
#   ggplot() + aes(x= fct_lump(fct_rev(fct_infreq(Publisher)), 10)) + geom_bar() +
#   coord_flip()

###Graphique représentant le nombre de genre dans une années
df %>% 
  filter(Year == "2012") %>% 
  select(everything()) %>% 
  summarize(fct_count(Genre, sort = T)) %>%
  ggplot() + 
  aes(x=fct_reorder(f, n), y = n) + 
  geom_col() + 
  coord_flip()

### Graphique repésentant le nombre de vente par publisher en une annee

df %>% 
  filter(Year == "1995") %>% 
  select(everything()) %>% 
  group_by(Publisher) %>%
  summarize(Total_sales = sum(Global_Sales))%>%
  arrange(desc(Total_sales)) %>%
  slice(1:10) %>%
  ggplot() + 
  aes(x=fct_reorder(Publisher, Total_sales), y = Total_sales) + 
  geom_col() + 
  coord_flip()

### Graphique repésentant le nombre de vente par Platform en une annee
###Graphique intéractif vidéo serait optimal
df %>% 
  filter(Year == "2012") %>% 
  select(everything()) %>% 
  group_by(Platform) %>%
  summarize(Total_sales = sum(Global_Sales))%>%
  arrange(desc(Total_sales)) %>%
  ggplot() + 
  aes(x=fct_reorder(Platform, Total_sales), y = Total_sales) + 
  geom_col() + 
  coord_flip()

### Graphique repésentant le nombre de vente par genre en une annee

df %>% 
  filter(Year == "2010") %>% 
  select(everything()) %>% 
  group_by(Genre) %>%
  summarize(Total_sales = sum(Global_Sales))%>%
  arrange(desc(Total_sales)) %>%
  setNames(c('Truc', 'Total_sales'))
  ggplot() + 
  aes(x=fct_reorder(Truc, Total_sales), y = Total_sales) + 
  geom_col() + 
  coord_flip()

### Graphique repésentant le top 10 des jeux vendues au cours d'une annee

df %>% 
  filter(Year == "2011") %>% 
  select(everything()) %>% 
  group_by(Name) %>%
  summarize(Total_sales = sum(Global_Sales))%>%
  arrange(desc(Total_sales)) %>%
  slice(1:10) %>% 
  ggplot() + 
  aes(x=fct_reorder(Name, Total_sales), y = Total_sales) + 
  geom_col() + 
  coord_flip()

levels(df$Year)
liste = list()
for(truc in levels(df$Platform)){
  liste <- c(liste, truc = truc)
}
liste

list("nana" = "nini")
truc = T
tablo <- df %>%
  filter(as.numeric(as.character(Year)) >= 1988)
  if(truc == T) { tablo <- tablo %>% filter(Platform == "PS3")}

  # filter(as.numeric(as.character(Year)) <= 2000) %>%
  # filter(Platform == "PS3") %>%
  # slice(1:3)

df %>% 
  filter(Platform == "PS3") %>%
  as.numeric(as.character(df$Year[df$Platform=="PS3"]))



min(na.omit(as.numeric(as.character(df$Year[df$Platform=="PS3"]))))
