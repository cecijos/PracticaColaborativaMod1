
library(tidyverse)
library(e1071)
#Useful Function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#Importar Data
players <- read.csv("C:/RMASTER/players_fifa22.csv",encoding = "UTF-8")
teams <- read.csv("C:/RMASTER/teams_fifa22.csv",encoding = "UTF-8")
summary(players)
str(players)
#Histograma de edad de jugadores
ggplot(data = players, aes(Age)) +
  geom_histogram(breaks=seq(0, 60, by=2), colour = "black", fill = "dodgerblue")
#Comprobando simetria
skewness(players$Age)
#Centrando grafico
mean(players$Age)

#Histograma de altura de jugadores
ggplot(data = players, aes(Height)) +
  geom_histogram(breaks=seq(150, 215, by=2), colour = "black", fill = "dodgerblue")
#Comprobando simetria
skewness(players$Height)
#Centrando grafico
mean(players$Height)


#Pais con mas jugadores
players$Nationality <- as.factor(players$Nationality)
#Top 10
top10_countries <- players %>%
  group_by(Nationality) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  slice(1:10)
#Graficos de paises con mayor represetacion
ggplot(top10_countries,aes(x=factor(Nationality),y=n))+
  geom_col(color='black',fill='dodgerblue')+
  labs(title = "Distribucion de nacionalidades en FIFA 22", x = "Pais", y = "Total")

#Mejores futbolistas variable Overall
players %>%
  select(Name, Overall) %>% 
  arrange(desc(Overall)) %>% 
  slice(1:10)
#Jugadores con mejores estadisticas
players %>%
  select(Name, Overall, TotalStats) %>% 
  arrange(desc(TotalStats)) %>% 
  slice(1:10)


#Juntamos el archivo teams con players

names(teams)[2] <- 'Club'

players <- players %>% full_join(teams, by="Club")
names(players)[9] <- 'Overall'

#Jugadores mejores overall por liga
Overall_league <- players  %>%  
  filter(Overall >= 85) %>% 
  group_by(League) %>%
  tally() %>% 
  arrange(desc(n))




#Jugadores mejores estadisticas por liga
beststats_league <-  players  %>%  
  filter(TotalStats >= 2100) %>% 
  group_by(League) %>%
  tally() %>% 
  arrange(desc(n))%>% 
  slice(1:5)

#Representacion grafica Mejores Overall liga
ggplot(Overall_league,aes(x=factor(League),y=n))+
  geom_col(color='black',fill='dodgerblue')+
  labs(title = "Ligas con mayores Overall (85+)", x = "Ligas", y = "Total")

#Representacion grafica Mejores estadisticas liga
ggplot(beststats_league,aes(x=factor(League),y=n))+
  geom_col(color='black',fill='dodgerblue')+
  labs(title = "Ligas con mejores jugadores por estadisticas", x = "Ligas", y = "Total")


#Clubs con jugadores overall >=85
Overall85_club <- players  %>%  
  filter(Overall >= 85) %>% 
  group_by(Club) %>%
  tally() %>% 
  arrange(desc(n)) %>% 
  slice(1:10)
#Clubs con mejores estadisticas
beststats_club <-  players  %>%  
  filter(TotalStats >= 2000) %>% 
  group_by(Club) %>%
  tally() %>% 
  arrange(desc(n))%>% 
  slice(1:10)

#Representacion grafica Mejores estadisticas equipos
ggplot(beststats_club,aes(x=factor(Club),y=n))+
  geom_col(color='black',fill='dodgerblue')+
  labs(title = "Equipos con mejores estadisticas", x = "Clubs", y = "Total")

#Representacion grafica Mejores oveall equipos
ggplot(Overall85_club,aes(x=factor(Club),y=n))+
  geom_col(color='black',fill='dodgerblue')+
  labs(title = "Equipos con mejor Overall", x = "Clubs", y = "Total")
