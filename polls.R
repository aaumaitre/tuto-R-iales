library(tidyverse) #manipulación de datos + visualización
library(rvest) #web scraping
library(janitor) #modificar nombres de columnas
library(zoo) #para rolling means
library(ggrepel) #etiquetas del gráfico
library(ariamsita) #theme del gráfico
library(extrafont) #usar fuentes en ggplot2

# Web con la que queremos trabajar
url <- "https://es.wikipedia.org/wiki/Elecciones_a_la_Asamblea_de_Madrid_de_2021#Encuestas"

# Descarga del texto de la tabla
table <- url %>% 
  read_html()%>%
  html_nodes('table') %>% 
  `[[`(4) %>%
  html_text()

# Separando por celdas
texto_tabla <- strsplit(table, "\n") %>% 
  `[[`(1) 

# Quitando entradas vacías
texto_tabla <- texto_tabla[texto_tabla != ""]

# "Rehaciendo" nuestro vector para que tenga la forma adecuada
# (como los headers de los partidos son imágenes, necesitamos
# añadirlos a mano)
head_texto <- texto_tabla[1:4]
partidos <- c("psoe", "pp", "cs", "mm", "vox", "ps")
tail_texto <- texto_tabla[-c(1:4)]
full_texto <- c(head_texto, partidos, tail_texto)

#eliminando las entradas demasiado largas (no son encuestas!)
full_texto2 <- subset(full_texto, nchar(full_texto) <=59)


# Creando una tabla final
encuestas <- matrix(full_texto2, ncol = 11,  byrow = TRUE) %>% 
             as.data.frame(stringsAsFactors = FALSE) %>% 
            row_to_names(row_number = 1) %>% 
  filter(row_number() <= n()-4)

# Arreglando últimos problemas
# - Convertir las fechas a formato "Date"
# "Alargar" la tabla
# - Limpiar la cenda con el % de voto

encuestas_clean <- encuestas%>%
  mutate(date0 = gsub(".*[–-]", "", `Trabajo de campo`),  #eliminamos la parte de la fecha que viene antes de guiones
         date = as.Date(date0,format = "%d %B %Y")) %>% #cambiamos de formato
  dplyr::select(5:10, 13) %>% # nos quedamos con las columnas de fecha y partidos
  pivot_longer(-date, names_to = "partido", values_to = "value")%>% #alargamos la tabla
  mutate(value = as.numeric(str_extract(value, pattern = "\\d+[.]\\d{1}"))) #nos quedamos solo con el primer decimal


# ¿Cómo hallamos las medias?
encuestas_clean <- encuestas_clean%>%
    arrange(date)%>%
    group_by(partido)%>%
    mutate(roll =  rollapply(c(rep(NA, 9), value), width=10, FUN=function(v) mean(v, na.rm=T)))
  
# Df con los puntos finales de las medias (para poner etiquetas de texto)
  final_points <- encuestas_clean%>%
    filter(date == max(date, na.rm = TRUE))%>%
    head(6)
  

# Gráfico final
  encuestas_clean%>%
    ggplot(aes(date, value/100, color = partido))+
    geom_point(alpha = .6)+
    geom_line(aes(y = roll/100))+
    geom_label_repel(data = final_points, 
                     aes(label = paste(round(roll,1), "%"), x = date+ 5), 
                     hjust = 0, size = 2, fill = "white",fontface = "bold",
                     xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))+
    scale_color_manual(values = c("#FF7E36", "#355E4C","#0195D7",  
                                  "#8A448C", "#EE6655",  "#57BA33"))+
    scale_x_date(expand = expand_scale(c(0, 0.1))) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L))+
    guides(color = FALSE)+
    labs(x = "Fecha", y = "Estimación de voto (%)",
         title = "Promedio de encuestas para la Comunidad de Madrid",
         caption = "Ariane Aumaitre - @ariamsita (Datos: Wikipedia)")+
    theme_ariamsita()+
    theme(axis.title.y = element_text(angle = 90),
          text = element_text(family = "Roboto Condensed"),
          plot.title = element_text(size = 14))+
    ggsave("encuestas/promedio.png", width = 6, height = 4.5, type = "cairo")
  
  
  
  