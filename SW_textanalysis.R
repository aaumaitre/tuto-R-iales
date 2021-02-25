#librerias que vamos a usar
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(extrafont)

#cargando los datos (episodios iv, v y vi)
guiones <- list.files(pattern='*.txt', recursive = TRUE)%>%
map_df(~read.table(.))

guiones$dialogue <- as.character(guiones$dialogue)

# Pasando a formato tidy
guiones_tidy <- guiones%>%
  unnest_tokens(word, dialogue)

# Qué palabras se repiten más?
guiones_tidy%>%
  group_by(word)%>%
  count(sort = TRUE)

# Quitemos "stop words"
final_data <- guiones_tidy%>%
  anti_join(stop_words)

#Volvemos a mirar qué se repite...
final_data%>%
  group_by(word)%>%
  count(sort = TRUE)

# Podemos visualizar estas palabras:
final_data%>%
  group_by(word)%>%
  count(sort = TRUE)%>%
  ungroup()%>%
  top_n(10)%>%
  ggplot(aes(x = n, y  = reorder(word, n)))+
  geom_col(fill = "#E89E10", width = .5, color = "white")+
  labs(title = "Palabras más repetidas en\nla primera trilogía de Star Wars",
       x = "Número de veces que se repite",
       y = NULL)+
  theme(plot.title = element_text(family = "Star Jedi", color = "#E89E10",
                                  hjust = .5, size = 15),
        text = element_text(color = "white", face = "bold"),
        axis.text = element_text(color = "white", face = "bold"),
        plot.background = element_rect(fill = "black"),
        panel.background =element_rect(fill = "black") ,
        panel.grid = element_line(color = "grey10", size = .3))+
  ggsave("SW/mostfrequent.png", width = 7, height = 5, type = "cairo")

# Alto en el camino para guardar el tema que acabamos de crear
theme_sw <-   theme(plot.title = element_text(family = "Star Jedi", color = "#E89E10",
                                              hjust = .5, size = 15),
                    text = element_text(color = "white", face = "bold"),
                    axis.text = element_text(color = "white", face = "bold"),
                    plot.background = element_rect(fill = "black"),
                    panel.background =element_rect(fill = "black") ,
                    panel.grid = element_line(color = "grey10", size = .3))

# ¿Y si queremos hacer una nube de palabras?
wc <- final_data%>%
  group_by(word)%>%
  count(sort = TRUE)

wordcloud2(data = wc, 
           color = "#E89E10", backgroundColor = "black",
           size = .5)

wordcloud2(data = wc, 
           figPath="SW/dv2.png", color = "black", size = .6)


# Ahora, podemos mirar qué palabras repite más _cada personaje_
by_character <- final_data%>%
  filter(character %in% c("LUKE", "LEIA", "HAN", "VADER", "BEN", "YODA"))%>%
  group_by(character, word)%>%
  summarize(ntimes = n())%>%
  top_n(10, ntimes)


by_character%>%
  ggplot(aes(ntimes, word))+
  geom_col()+
  facet_wrap(~character, scales = "free_y")


by_character%>%
  mutate(word = reorder_within(word, ntimes, character))%>%
  ggplot(aes(x = ntimes, y = word))+
  geom_col(fill = "#E89E10", width = .5, color = "white")+
  facet_wrap(~character, scales = "free_y")+
  scale_y_reordered() +
  labs(x = "Número de veces que se repite",
       y = NULL,
       title = "Palabras más repetidas por cada personaje")+
  theme_sw+
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "#E89E10", face = "bold", size = 12))+
  ggsave("SW/bycharacter.png", width = 9, height = 6)
  
#¿Quién es el más _hablador_ de nuestros personajes?

guiones_tidy%>%
  group_by(character)%>%
  count(sort = TRUE)%>%
  ungroup()%>%
  top_n(10)%>%
  ggplot(aes(x = n, y = reorder(character, n)))+
  geom_col(fill = "#E89E10", width = .5, color = "white")+
  labs(title = "Los personajes que más hablan",
       y = NULL,
       x = "Número de palabras en toda la saga")+
  theme_sw+
   ggsave("SW/habladores.png", width = 7, height = 5)
