library(tidyverse)
library(tmap)
library(tidygeocoder)
library(tmaptools)
library(mapview)
library(leaflet)
library(sf)
library(ggmap)
library(gganimate)
library(transformr)
library(ggthemes)
library(osmdata)
library(forcats)
library(gifski)
install.packages("gifski")
install.packages("av")
install.packages("gganimate")
library(av)

# Analises


# 1. Abertura de estações por ano


dados %>% 
  group_by(ano) %>% 
  count() %>% 
  arrange(desc(n)) %>%
  ggplot(aes(x = ano, y = n))+
  geom_col()+
  theme_minimal() %>% 
  scale_y_continuous(breaks = seq(0,14,1))

str(dados)

# 2. Relação entre abertura de estação e ano eleitoral 


eleicoes_estaduais <- c(1974,1978,1982,1986,1990,1994,1998,2002,2006,2010,2014,2018,2022)

dados_eleicoes <- dados %>% 
  group_by(ano) %>% 
  count() %>% 
  arrange(desc(n))

dados_eleicoes <- dados_eleicoes %>% mutate(eleicao = ifelse(ano %in% eleicoes_estaduais, 'sim','nao'))

dados_eleicoes %>% group_by(eleicao) %>% summarise(lancamentos = sum(n))


dados_eleicoes %>% filter(ano >= 1994) %>% group_by(eleicao) %>% summarise(lancamentos = sum(n))


# 3. GGANIMATE


# Precisa arrumar localização corinthians itaquera, santo amaro, largo treze, borba gato,
# AAcd, santos imigrantes, tamanduateí, vila prudente, São Lucas, Oratório, camilo haddad, 
# Vila tolstoi, vila união, jardim planalto,patriarca vila ré, guilhermina esperança
# carrão,belem, bressre moca, anhangabau, republica, 



# santo amaro não mudou, 



# 3. GGANIMATE


# LATITUDE LONGITUDE


lat_longs <- dados %>% tidygeocoder::geocode(address = Endereco,method = 'arcgis', lat = lat, long = lon)




leaflet(lat_longs) %>% addTiles() %>% addMarkers(popup = ~as.character(Estacao), label = ~as.character(Estacao))




# ÁREA DE TESTE



lat_longs$id_estacao <- paste(lat_longs$ano, lat_longs$Estacao)



us <- c(left = -46.79, bottom = -23.69, right = -46.44, top = -23.45)
mapa_sp <- get_stamenmap(us, zoom = 14, maptype = "toner-lite")

ggmap(mapa_sp)
ggmap(teste)

teste <- get_stamenmap(
  bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
  zoom = 10, maptype = "terrain")


install.packages("ggmap")

# abaixo o CÓDIGO PERFEITO(QUASE)
mapa_ggmap <- ggmap(mapa_sp) +
  geom_point(data = lat_longs,aes(x = lon, y = lat, group = id_estacao),
             alpha = 0.7,
             size = 6,
             color = '#F9633A')+
  labs(
    title = "Evolução das inaugurações das estações de metro em São Paulo",
    subtitle = "Ano: {next_state}",
    x= "Longitude",
    y= "Latitude",
    caption = "") +
  cowplot::theme_cowplot(font_family = "Source Sans Pro",
                         font_size = 12) +
  theme(legend.position = "none",
        axis.title = element_text(size = 18, hjust = 0.5),
        plot.title = element_text(size = 26, hjust = 0.5),
        plot.subtitle = element_text(size = 22, hjust = 0.5)) +
  transition_states(id_estacao) +
  ease_aes('linear')+
  shadow_mark(color="black")


animate(mapa_ggmap, fps = 1,height = 1150, width = 1150, renderer = gifski_renderer())





lat_longs$id_estacao <- paste(lat_longs$ano, lat_longs$Estacao)


lat_longs %>% view()

dados %>% view()
