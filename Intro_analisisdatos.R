#librerias
library(tidyverse)

#cargamos nuestros datos
sw <- starwars

#¿Qué aspecto tienen?
head(sw)
str(sw)
summary(sw$height)

#########################
## Hora de limpiar datos
########################

#1. observaciones: filter() nos devuelve las filas que cumplen las condiciones
sw_filtered <- filter(sw, homeworld == "Tatooine")

# Operadores que pueden resultarnos útiles: ==, >, <, >=, <=, !=

# Muy importante: el operador %in% !
planetas <- c("Tatooine", "Alderaan")

sw_filtered2 <- filter(sw, homeworld %in% planetas, hair_color == "blond")

# También útil: is.na() para deshacernos de missing values
sw_filtered3 <- filter(sw, !is.na(hair_color))

# Cuando tenemos varias condiciones, es útil conocer 
# los operadores & ("y") y | ("o")
sw_filtered4<- filter(sw, hair_color == "blond" | eye_color == "blue")

# Añadiendo más condiciones:
filter(sw,
       (hair_color == "blond" & eye_color == "blue")|
         (hair_color == "brown" & eye_color == "brown"))

#########################
### Interludio: the pipe %>%
############################

# %>% puede leerse como "y a continuación", y nos permite
# encadenar múltiples operaciones en nuestro código

# Así escribimos código más simple, legible y con menos repeticiones

# La línea:
# sw_filtered2 <- filter(sw, homeworld %in% c("Tatooine", "Alderaan"))
# Es lo mismo que:

sw_filtered5 <- sw%>%
  filter(homeworld %in% c("Tatooine", "Alderaan"))

##########################
### 3. Más funciones: select()
##########################

# Utilizamos select() para guardar sólo aquellas variables que nos interesan

# Podemos seleccionar por nombre de columna:
sw%>%
  select(name, height)

# Por posición:
sw%>%
  select(1:3)

# Todas las columnas menos una:
sw%>%
  select(-films)

# E incluso quitar varias:
sw%>%
  select(-c(8:14))

##########################
### 4. Encadenando operaciones y aprendiendo arrange()
##########################

# arrange() ordena las observaciones en función de una o más variables
# podemos usarla en orden ascendiente o descendiente

sw_arranged <- sw%>%
  filter(sex == "male")%>%
  select(name, height)%>%
  arrange(desc(height))


head(sw_arranged)


##########################
### 5. Podemos crear nuevas variables con mutate()
##########################

sw_imc <- sw%>%
  select(name, height, mass)%>%
  mutate(imc = mass/((height/100)^2))%>%
  arrange(desc(imc))

# FUnciones que pueden resultar útiles dentro de mutate:

#ifelse(condición, verdadero, falso) nos devuelve una columna con dos 
# valores (V/F) en función de una condición

sw_imc <- sw%>%
  select(name, height, mass)%>%
  mutate(imc = mass/((height/100)^2),
         sobrepeso = ifelse(imc >= 25, 1, 0))

#case_when es similar, pero nos permite añadir más condicones
sw_imc <- sw%>%
  select(name, height, mass)%>%
  mutate(imc = mass/((height/100)^2),
         sobrepeso = ifelse(imc >= 25, 1, 0),
         categoria = case_when(imc < 18.5 ~ "Bajo Peso",
                               imc >= 18.5 & imc <25 ~ "Normal",
                               imc >= 25 ~ "Sobrepeso"))

# Paquetes a explorar: stringr para character vectors, forcats para
# factores, lubridate para fechas...


##########################
### 6. Trabajando con grupos: group_by()
##########################

# group_by() no produce ningún output, pero todas las operaciones que
# realices a continuación se llevarán a cabo _dentro de esos grupos_

# Una vez has agrupado, puedes utilizar mutate() si quieres guardar
# todas las observaciones, o bien summarize() si quieres "resumirlas"

# RECUERDA hacer ungroup() si quieres volver a trabajar con los datos sin agrupar

# summarize() - hallemos la altura media según especie

sw_summarized <- sw%>%
  group_by(species)%>%
  summarize(sp_height = mean(height, na.rm = TRUE))

# Pero recuerda que podríamos usar mutate
sw_grouped <- sw%>%
  select(name, species, height)%>%
  group_by(species)%>%
  mutate(sp_height = mean(height, na.rm = TRUE))



# Más funciones: https://dplyr.tidyverse.org/reference/index.html
# Y si queréis profundizar: https://r4ds.had.co.nz/