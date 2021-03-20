################
## 1. Librerías
################
library(tidyverse)
library(ariamsita) #devtools::install_github("aaumaitre/ariamsita")
library(extrafont)
library(spatstat)

##########
# 2. Cargando los datos
#########

p <- read.csv("esudb19p.csv")
r <- read.csv("esudb19r.csv")

per <- left_join(r, p, by =  c('RB030' = 'PB030'))%>%
  mutate(id_hh = as.integer(str_sub(RB030,1,-3)))

d <- read.csv("esudb19d.csv")
h <- read.csv("esudb19h.csv")

hog <- left_join(d,h, by =  c('DB030' = 'HB030'))

ecv19 <- left_join(per, hog, by = c("id_hh" = "DB030"))

# Si queréis guardar el archivo:
#save(ecv19, file = "ecv19.RData")
#load("ecv19.RData")

##############################
## 3. Limpieza de datos
# Vamos a filtrar para quedarnos sólo con las mujeres entre 24 y 65 años
# Y a crear algunas variables nuevas
#############################


ecv_brecha <- ecv19%>%
  mutate(pesos = RB050,
         ingresos_anuales = PY010N,
         edad = (2019 - RB080),
         child = ifelse(edad <18, 1,0),
         empleo = ifelse(PL031 %in% 1:4, 1, 0),
         tcompleto = ifelse(PL031 %in% c(1,3), 1, 0),
         women = ifelse(RB090 == 2, "Mujeres", "Hombres"),
         act = case_when(
           PL111A =="a" ~ "Agricultura, ganadería, silvicultura y pesca",
           PL111A == "b" ~ "Industrias extractivas",
           PL111A == "c" ~ "Industria manufacturera",
           PL111A == "d" ~ "Suministro de energía eléctrica, gas, etc.",
           PL111A == "e" ~ "Suministro de agua. Gestión residuos",
           PL111A == "f" ~ "Construcción",
           PL111A == "g" ~ "Comercio, reparación de vehículos de motor",
           PL111A == "h" ~ "Transporte y almacenamiento",
           PL111A == "i" ~ "Hostelería",
           PL111A == "j" ~ "Información y comunicaciones",
           PL111A == "k" ~ "Actividades financieras y de seguros",
           PL111A == "l" ~ "Actividades inmobiliarias",
           PL111A == "m" ~ "Actividades profesionales, científicas y técnicas",
           PL111A == "n" ~ "Actividades administrativas y servicios auxiliares",
           PL111A == "o" ~ "Administración Pública y Defensa. Seguridad Social",
           PL111A == "p" ~ "Educación",
           PL111A == "q" ~ "Actividades sanitarias y de servicios sociales",
           PL111A == "r" ~ "Actividades artísticas, recreativas",
           PL111A == "s" ~ "Otros servicios",
           PL111A == "t" ~ "Hogares como empleadores de personal doméstico",
           PL111A == "u" ~ "Organismos extraterritoriales, no consta"))%>%
  group_by(id_hh)%>%
  mutate(nch = sum(child), #numero de niños
         kids = ifelse(max(child)>0, "Hijos <18 en el hogar", "Sin hijos"))%>% #variable binaria sobre si hay niños en el hogar
  ungroup()%>%
  filter(edad %in% 24:65)


###########################################
## Distintas formas de calcular la brecha
############################################

#0. Determinamos el tema que usaremos para los gráficos
theme_set(theme_ariamsita()+
            theme(axis.title.y = element_text(angle = 90),
                  legend.position = "top",
                  text = element_text(family = "Roboto Condensed"),
                  plot.title = element_text(size = 16),
                  strip.text = element_text(face = "bold")))

# 1. Para toda la muestra (personas 24-65 años)
ecv_brecha%>%
  filter(empleo == 1, tcompleto == 1)%>%
  group_by(women)%>%
  summarize(sueldo_medio = weighted.mean(ingresos_anuales, pesos, na.rm = TRUE))


# 2. Sólo entre personas empleadas
ecv_brecha%>%
  filter(empleo == 1)%>%
  group_by(women)%>%
  summarize(sueldo_medio = weighted.mean(ingresos_anuales, pesos, na.rm = TRUE))


#interludio: histograma

ecv_brecha%>%
  filter(ingresos_anuales %in% 100:75000)%>%
  ggplot(aes(x = ingresos_anuales, fill = women))+
  geom_histogram()+
  facet_wrap(~women, nrow = 2)+
  scale_fill_manual(values = c("green4", "darkorchid3"))+
  scale_x_continuous(labels = scales::dollar_format(suffix = "€", prefix = ""))+
  labs(y = NULL,
       x = "Ingresos medios anuales",
       title = "Distribución de ingresos desglosada por género",
       caption = "@ariamsita - Datos: ECV 2019")+
  guides(fill = FALSE, color = FALSE)+
  ggsave("brecha_hist.png", width = 5, height = 6, type = "cairo")


# 3. La brecha según si hay hijos en el hogar

euro <- function(x){
  paste(format(x, big.mark = ".", digits = 0, scientific = FALSE), "€")
  }

ecv_brecha%>%
  filter(empleo == 1)%>%
  group_by(kids, women)%>%
  summarize(sueldo_medio = weighted.mean(ingresos_anuales, pesos, na.rm = TRUE))
  ggplot(aes(x = sueldo_medio, y = women, fill = women, color = women))+
  geom_col(width = .6)+
  facet_wrap(~kids, nrow = 2)+
  geom_text(aes(label = euro(sueldo_medio)),
            hjust = 1.2, color = "white")+
  scale_fill_manual(values = c("#7FC57F", "#CC97E5"))+
  scale_color_manual(values = c("green4", "darkorchid3"))+
  scale_x_continuous(labels = scales::dollar_format(suffix = "€", prefix = ""))+
  labs(y = NULL,
       x = "Ingresos medios",
       title = "La brecha salarial\nentre personas que trabajan",
       caption = "@ariamsita - Datos: ECV 2019")+
  guides(fill = FALSE, color = FALSE)+
  ggsave("brecha_sal.png", width = 5, height = 6, type = "cairo")


# 4. La brecha según sector de actividad

ecv_brecha%>%
  filter(empleo == 1,
         !is.na(act))%>%
  group_by(act, women)%>%
  summarize(sueldo_medio = weighted.mean(ingresos_anuales, pesos, na.rm = TRUE))%>%
  ggplot(aes(x = sueldo_medio, y = reorder(act, sueldo_medio), fill = women, color = women))+
  geom_col(width = .6, position = "dodge")+
  scale_fill_manual(values = c("#7FC57F", "#CC97E5"))+
  scale_color_manual(values = c("green4", "darkorchid3"))+
  scale_x_continuous(labels = scales::dollar_format(suffix = "€", prefix = ""))+
  labs(y = NULL,
       x = "Ingresos medios",
       title = "La brecha salarial\npor sector de ctividad",
       caption = "@ariamsita - Datos: ECV 2019")+
  guides(fill = FALSE, color = FALSE)


# 5. ¿Por qué no representar solo la diferencia?

ecv_brecha%>%
  filter(empleo == 1,
         !is.na(act))%>%
  group_by(act, women)%>%
  summarize(sueldo_medio = weighted.mean(ingresos_anuales, pesos, na.rm = TRUE))%>%
  pivot_wider(id_cols = act, names_from = women, values_from = sueldo_medio)%>%
  mutate(dif = Hombres - Mujeres,
         brecha = ifelse(dif <=0, 1, 0))%>%
  ggplot(aes(x=dif, y = reorder(act, dif), fill = factor(brecha)))+
  geom_col()+
  labs(y = NULL,
       x = "Ingresos medios",
       title = "La brecha salarial\npor sector de ctividad",
       caption = "@ariamsita - Datos: ECV 2019")+
  guides(fill = FALSE)
