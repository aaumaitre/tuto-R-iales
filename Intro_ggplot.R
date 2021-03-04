#librerias
library(tidyverse) #incluye ggplot2

#cargamos nuestros datos
sw <- starwars


##############################
#La sintaxis de ggplot2 ######
##############################

ggplot(data = , aes(x =, y =, ...))+
  geom_*()

data%>%
ggplot(aes(x =, y =, ...))+
  geom_*()

# Data: el data.frame/tibble que estés usando

# Dentro de *aes*, incluimos las partes del 
# gráfico que queramos mapear con VARIABLES

# Con geom_*, le estás diciendo a ggplot cómo 
# quieres representar estas variables: puntos, 
# líneas, barras...

# Todas las geoms disponibles: https://ggplot2.tidyverse.org/reference/

# En ggplot, conectamos las capas con el símbolo +


############################
## Nuestro primer gráfico: un scatterplot
############################

sw %>%
  ggplot(aes(x = height, y = mass))+
  geom_point()


# Qué más podemos hacer con este gráfico?

# quitar outliers 
sw %>%
  filter(mass < 500)%>%
  ggplot(aes(x = height, y = mass))+
  geom_point()

# añadir texto
sw %>%
  filter(mass < 500)%>%
  ggplot(aes(x = height, y = mass))+
  geom_point()+
  labs(x = "Altura (cm)",
       y = "Masa corporal (kg)",
       title = "Nuestro primer ggplot",
       subtitle = "Relación entre altura y masa corporal en Star Wars",
       caption = "Datos: starwars dataset")

# cambiar los cortes de los ejes
sw %>%
  filter(mass < 500)%>%
  ggplot(aes(x = height, y = mass))+
  geom_point()+
  labs(x = "Altura (cm)",
       y = "Masa corporal (kg)",
       title = "Nuestro primer ggplot",
       subtitle = "Relación entre altura y masa corporal en Star Wars",
       caption = "Datos: starwars dataset")+
  scale_x_continuous(breaks = seq(50, 250, by = 25))


# añadir variables agrupando por color
sw %>%
  filter(mass < 500)%>%
  ggplot(aes(x = height, y = mass, color = gender))+
  geom_point()+
  labs(x = "Altura (cm)",
       y = "Masa corporal (kg)",
       title = "Nuestro primer ggplot",
       subtitle = "Relación entre altura y masa corporal en Star Wars",
       caption = "Datos: starwars dataset")+
  scale_x_continuous(breaks = seq(50, 250, by = 25))

# introducir cambios a escalas discretas
# scale_*_discrete vs scale_*_manual 

sw %>%
  filter(mass < 500, 
         !is.na(gender))%>%
  ggplot(aes(x = height, y = mass, color = gender))+
  geom_point(size = 2)+
  labs(x = "Altura (cm)",
       y = "Masa corporal (kg)",
       color = "Género:",
       title = "Nuestro primer ggplot",
       subtitle = "Relación entre altura y masa corporal en Star Wars",
       caption = "Datos: starwars dataset")+
  scale_x_continuous(breaks = seq(50, 250, by = 25))+
  scale_color_manual(labels = c("Femenino", "Masculino"),
                     values = c("#8F448B", "#3A705D"))

# podemos incluso añadir otro geom :)

sw %>%
  filter(mass < 500, 
         !is.na(gender))%>%
  ggplot(aes(x = height, y = mass, color = gender))+
  geom_point(size = 2)+
  geom_smooth(se= FALSE, method = "lm")+
  labs(x = "Altura (cm)",
       y = "Masa corporal (kg)",
       color = "Género:",
       title = "Nuestro primer ggplot",
       subtitle = "Relación entre altura y masa corporal en Star Wars",
       caption = "Datos: starwars dataset")+
  scale_x_continuous(breaks = seq(50, 250, by = 25))+
  scale_color_manual(labels = c("Femenino", "Masculino"),
                     values = c("#8F448B", "#3A705D"))

# añadir temas
# temas pre-hechos: https://ggplot2.tidyverse.org/reference/ggtheme.html
# componentes de los temas: https://ggplot2.tidyverse.org/reference/theme.html

sw %>%
  filter(mass < 500, 
         !is.na(gender))%>%
  ggplot(aes(x = height, y = mass, color = gender))+
  geom_point(size = 2)+
  geom_smooth(se= FALSE, method = "lm")+
  labs(x = "Altura (cm)",
       y = "Masa corporal (kg)",
       color = "Género:",
       title = "Nuestro primer ggplot",
       subtitle = "Relación entre altura y masa corporal en Star Wars",
       caption = "Datos: starwars dataset")+
  scale_x_continuous(breaks = seq(50, 250, by = 25))+
  scale_color_manual(labels = c("Femenino", "Masculino"),
                     values = c("#8F448B", "#3A705D"))+
  theme_minimal()+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  ggsave("sw.png", height = 5, width = 6, type = "cairo")

# Y ahora hablemos de guardar gráficos...

#######################
### Gráficos de barras: altura media por especie
#######################

sw%>%
  group_by(species)%>%
  summarize(avg_height = mean(height, na.rm = TRUE))%>%
  ggplot(aes(x= species, y= avg_height))+
  geom_col()


# Podemos hacer que esto sea legible?

sw%>%
  filter(!is.na(species))%>%
  group_by(species)%>%
  summarize(avg_height = mean(height, na.rm = TRUE))%>%
  ggplot(aes(x= avg_height, y= reorder(species, avg_height)))+
  geom_col()

# Qué más podemos añadir?

sw%>%
  filter(!is.na(species))%>%
  group_by(species)%>%
  summarize(avg_height = mean(height, na.rm = TRUE))%>%
  ggplot(aes(x= avg_height, y= reorder(species, avg_height)))+
  geom_col(fill = "#E15D8A", width = .2)+
  geom_point(size = 3, color = "#E15D8A")+
  labs(y = NULL,
       x = "Altura media (cm)",
       title = "Altura media por especie")+
  theme_minimal()
