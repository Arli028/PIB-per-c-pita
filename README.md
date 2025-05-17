# PIB-per-c-pita
 (DATOS SOCIOECONÓMICOS) describe los datos de desarrollo por país como la esperanza de vida, PIB,poblacion.
#Gapmider (DATOS SOCIOECONÓMICOS) describe los datos de desarrollo por país como la esperanza de vida, PIB,poblacion.

install.packages("gapminder")
library(gapminder)
data(gapminder)

#Cargar librerías necesarias
library(readr)
library(dplyr)
library(ggplot2)

#Revisar el dataset
print("explorando el dataset cargado...")

#ver las primeras filas 
print("primeras 6 filas del dataset")
head(gapminder)

#Información general del dataset
print("Información general del dataset:")
str(gapminder)

#Resumen estadístico general
print("informacion estadistico general")
summary(gapminder)

#Identificar valores faltante NA
print("verificar si hay valores NA")
colSums(is.na(gapminder))
#No se encontraron NA ya que el dataset está limpio.


#Transformar y manipular datas con dplyr
gap2002<- gapminder %>%
filter(year==2002)%>%
  mutate(gdp_total= gdpPercap*pop)

#Calcular medidas de tendencia central 
print("Media y mediana del PIB per cápita por continente en 2002")
gap2002 %>%
  group_by(continent)%>%
  summarise(media_gdp=mean(gdpPercap), mediana_gdp=median(gdpPercap))


#Visualizaciones con ggplot2
print("diagrama de dispersión PIB per cápita vs esperanza de vida")
ggplot(gap2002, aes(x=gdpPercap, y=lifeExp, color=continent, size = pop))+geom_point(alpha=0.7)+ scale_x_log10() 
labs(title = "PIB per cápita vs Esperanza de vida (2002)", x= "PIB per cápita (log)", y = "Esperanza de vida")+ theme_minimal()


#Esperanza de vida en América Latina
gapminder %>%
  filter(country %in% c("Mexico", "Argentina", "Brazil", "Chile", "Peru")) %>%
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_line(size = 1.2) +
  labs(title = "Evolución de la esperanza de vida en América Latina",
       x = "Año", y = "Esperanza de vida") +
  theme_minimal()
