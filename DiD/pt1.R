library(tidyverse)
library(readxl)

clean_year <- function(data) {
  data %>%
    filter(Años__ESTANDAR >= 2018, 
           Años__ESTANDAR <= 2023) %>%
    select(País__ESTANDAR, Años__ESTANDAR, value)
}

#reading in the data
tasa_desempleo <- read_excel("~/Desktop/Nibia/DiD/data/tasa_desempleo.xlsx") %>% 
  filter(Años__ESTANDAR >= 2018, 
         Años__ESTANDAR <= 2023) %>%
  select(País__ESTANDAR, Años__ESTANDAR, value) %>%
  rename("tasa_medio_desempleo" = "value")

tasa_ansiedad_empleo <- read_excel("~/Desktop/Nibia/DiD/data/tasa_de_ansiedad_de_perder_empleo.xlsx")%>%  
  clean_year() %>%
  rename("tasa_ansiedad_desempleo" = "value")


tasa_inmigracion <- read_excel("~/Desktop/Nibia/DiD/data/tasa_inmigracion.xlsx") %>%
  clean_year() %>%
  rename("tasa_inmigracion_1000" = "value")



tasa_pobreza <- read_excel("~/Desktop/Nibia/DiD/data/tasa_pobreza.xlsx") %>%
  clean_year() %>%
  rename("tasa_pobreza" = "value")





LatAmSocial1 <- full_join(tasa_pobreza, tasa_ansiedad_empleo, 
                         by = c("País__ESTANDAR", "Años__ESTANDAR"))

LatAmSocial2 <- full_join(LatAmSocial1, tasa_desempleo, 
                         by = c("País__ESTANDAR", "Años__ESTANDAR"))

LatAmSocial3 <- full_join(LatAmSocial2, tasa_inmigracion, 
                         by = c("País__ESTANDAR", "Años__ESTANDAR"))




