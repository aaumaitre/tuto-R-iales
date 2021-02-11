#libraries
library(tidyverse)
library(sf)

#shapefiles at the local level
# https://opendata.esri.es/datasets/53229f5912e04f1ba6dddb70a5abeb72_0

#using sf to read the shapefile in a tidy format
municipios <- st_read("mapas/shapefile/Municipios_IGN.shp")


#data on poverty (viene de https://www.ine.es/experimental/atlas/exp_atlas_tab.htm#)
ine_poverty <- read_csv2("mapas/datos_pobreza.csv")

ine_clean <- ine_poverty%>%
  filter(!str_detect(Lugar, 'distrito|sección'))%>%
  separate(Lugar, into = c("ine_cod", "municipio"), sep = 5)%>%
  transmute(CODIGOINE = ine_cod,
            perc = as.numeric(str_replace(perc, ",", ".")))



#joining data
local_plot <- municipios%>%
  left_join(ine_clean, by = "CODIGOINE")

rm(ine_clean, ine_poverty, municipios)


local_plot%>%
  ggplot(aes(fill = perc))+
  geom_sf(color = "white", size = 0.1)+
  ggsave("mapas/prueba.png")



# ¿Que hacemos ahora?
# 1. Acercar las Islas Canarias
# Para ello, hacemos algo parecido a: https://rpubs.com/dieghernan/beautifulmaps_II

canarias <- local_plot%>%
  filter(CODNUT1 == "ES7")

canarias_new <- st_sf(st_drop_geometry(canarias),
                      geometry = st_geometry(canarias) + c(4, 8))

st_crs(canarias_new) <- st_crs(local_plot)

local_plot2 <- rbind(  local_plot %>%
                         filter(CODNUT1 != "ES7"),
                       canarias_new)

 # Comprobamos que estén en su nuevo sitio:
local_plot2%>%
  ggplot(aes(fill = perc))+
  geom_sf(color = "white", size = 0.1)+
  ggsave("mapas/prueba.png", height = 5, width = 9, type = "cairo")


# 4. Hacer más bonito el gráfico en general

local_plot2%>%
  ggplot(aes(fill = perc))+
  geom_sf(color = "white", size = 0.1)+
  geom_segment(aes(x = -8, y = 35, xend = -8, yend = 38), color = "grey60")+
  geom_segment(aes(x = -15, y = 38, xend = -8, yend = 38), color = "grey60")+
  scale_fill_distiller(palette = "RdPu", trans = "reverse")+
  ggsave("mapas/prueba.png", height = 4, width = 9, type = "cairo")



# Como lo hacemos bonito??

theme_maps <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "ivory1", color = NA),
      panel.background = element_rect(fill = "ivory1", color = NA),
      legend.background = element_rect(fill = "ivory1", color = NA),
      panel.border = element_blank(),
      plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
      legend.title = element_text(color = "grey40", size = 8),
      legend.text = element_text(color = "grey40", size = 7, hjust = 0),
      legend.position = c(0.7, 0.07),
      legend.text.align = 0,
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.spacing = unit(c(2,0.2,.2,0.2), "cm"))
}


#mapa final!

local_plot2%>%
  ggplot(aes(fill = perc))+
  geom_sf(color = "white", size = 0.1)+
  geom_segment(aes(x = -8, y = 35, xend = -8, yend = 38), color = "grey60")+
  geom_segment(aes(x = -15, y = 38, xend = -8, yend = 38), color = "grey60")+
  scale_fill_distiller(palette = "RdPu", trans = "reverse", 
                       labels = function(x) paste0(x, "%"))+
  guides(fill = guide_legend(direction = "horizontal",
                             keyheight = unit(2, units = "mm"),
                             label.position = "bottom"))+
  labs(title = "Distribución territorial de la pobreza",
       subtitle = "% de personas por debajo del 60% de la renta mediana por municipio",
       fill = NULL)+
  theme_maps()+
  ggsave("mapas/final.png", height = 5, width = 9, type = "cairo")





