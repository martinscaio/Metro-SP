library(tidyverse)
library(tidygeocoder)
library(leaflet)
library(gganimate)
library(ggthemes)
library(osmdata)
library(forcats)
library(OpenStreetMap)
library(latex2exp)




dados <- read_csv2("C:\\Users\\Caio\\Desktop\\Projeto Metro SP\\endereco_metro.csv")





# Vamos extrair as latitudes e longitudes


lat_longs <- dados %>% tidygeocoder::geocode(address = Endereco,method = 'arcgis', lat = lat, long = lon)


# Consegui coletar as latitudes/longitudes mas algumas não estão exatamente corretas
# As vezes pega a LAT/LON de 1km de distancia, e como sou perfeccionista vou ajustar alguns na mão




lat_longs <- lat_longs %>% mutate(lat = ifelse(round(lat,5) == -23.66376, -23.655834,lat),
                                  lon = ifelse(round(lon,5) == -46.72623,-46.722366,lon),
                                  lat = ifelse(round(lat,5) == -23.65388, -23.654383,lat),
                                  lon = ifelse(round(lon,5) == -46.70783,-46.710461,lon),
                                  lat = ifelse(round(lat,5) == -23.63171, -23.633214, lat),
                                  lon = ifelse(round(lon,5) == -46.69122,-46.692991,lon),
                                  lat = ifelse(round(lat,5) == -23.61821, -23.618433, lat),
                                  lon = ifelse(round(lon,5) == -46.68345,-46.681776,lon),
                                  lat = ifelse(round(lat,5) == -23.61051, -23.606273, lat),
                                  lon = ifelse(round(lon,5) == -46.48524,-46.507357,lon),
                                  lat = ifelse(round(lat,5) == -23.60271, -23.603161, lat),
                                  lon = ifelse(round(lon,5) == -46.51849,-46.515702,lon),
                                  lat = ifelse(round(lat,5) == -23.58298, -23.600948, lat),
                                  lon = ifelse(round(lon,5) == -46.56446,-46.527397,lon),
                                  lat = ifelse(round(lat,5) == -23.5947, -23.595840, lat),
                                  lon = ifelse(round(lon,5) == -46.53867,-46.537549,lon),
                                  lat = ifelse(round(lat,5) == -23.58663, -23.589279, lat),
                                  lon = ifelse(round(lon,5) == -46.54660,-46.544654,lon),
                                  lat = ifelse(round(lat,5) == -23.58253,-23.582199, lat),
                                  lon = ifelse(round(lon,5) == -46.55989,-46.561450,lon),
                                  lat = ifelse(round(lat,5) == -23.58433, -23.585175, lat),
                                  lon = ifelse(round(lon,5) == -46.57849,-46.582655,lon),
                                  lat = ifelse(round(lat,5) == -23.56125, -23.542354, lat),
                                  lon = ifelse(round(lon,5) == -46.45498,-46.469442,lon),
                                  lat = ifelse(round(lat,5) == -23.52971, -23.531388, lat),
                                  lon = ifelse(round(lon,5) == -46.50432,-46.501769,lon),
                                  lat = ifelse(round(lat,5) == -23.52973,-23.529433,lat),
                                  lon = ifelse(round(lon,5) == -46.51449,-46.516407,lon),
                                  lat = ifelse(round(lat,5) == -23.55652,-23.538064,lat),
                                  lon = ifelse(round(lon,5) == -46.6305,-46.564232,lon),
                                  lat = ifelse(round(lat,5) == -23.54548,-23.543349,lat),
                                  lon = ifelse(round(lon,5) == -46.59801,-46.589853,lon),
                                  lat = ifelse(round(lat,5) == -23.54291,-23.546884,lat),
                                  lon = ifelse(round(lon,5) == -46.61107,-46.607655,lon),
                                  lat = ifelse(round(lat,5) == -23.54861,-23.549411,lat),
                                  lon = ifelse(round(lon,5) == -46.62603,-46.625527,lon),
                                  lat = ifelse(round(lat,5) == -23.55939,-23.563489,lat),
                                  lon = ifelse(round(lon,5) == -46.65897,-46.653890,lon),
                                  lat = ifelse(round(lat,5) == -23.53561,-23.533951,lat),
                                  lon = ifelse(round(lon,5) == -46.65345,-46.655618,lon),
                                  lat = ifelse(round(lat,5) == -23.5466,-23.547649,lat),
                                  lon = ifelse(round(lon,5) == -46.63802,-46.638947,lon),
                                  lat = ifelse(round(lat,5) == -23.54314,-23.544174,lat),
                                  lon = ifelse(round(lon,5) == -46.64388,-46.642660,lon),
                                  lat = ifelse(round(lat,5) == -23.54349,-23.547335,lat),
                                  lon = ifelse(round(lon,5) == -46.61692,-46.615628,lon),
                                  lat = ifelse(round(lat,5) == -23.53864,-23.540651,lat),
                                  lon = ifelse(round(lon,5) == -46.56768,-46.576349,lon),
                                  lat = ifelse(round(lat,5) == -23.53475,-23.532098,lat),
                                  lon = ifelse(round(lon,5) == -46.53475,-46.530796,lon),
                                  lat = ifelse(round(lat,5) == -23.53169,-23.533714,lat),
                                  lon = ifelse(round(lon,5) == -46.53006,-46.542496,lon))




write_excel_csv2(lat_longs,"C:\\Users\\Caio\\Desktop\\Projeto Metro SP\\dados_prontosQUARTO.csv")


lat_longs <- read_csv2("C:\\Users\\Caio\\Desktop\\Projeto Metro SP\\dados_prontosQUARTO.csv")


# Analises


# 1. Abertura de estações por ano


dados %>% 
  group_by(ano) %>% 
  count() %>% 
  arrange(desc(n)) %>%
  ggplot(aes(x = ano, y = n))+
  geom_col(fill = "darkorange1")+
  scale_y_continuous(breaks = seq(0,14,2))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme_minimal()+
  labs(x = NULL, y = NULL, title = "Quantidade de estações de metrô inauguradas por ano")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))



# 2. Relação entre abertura de estação e ano eleitoral 


eleicoes_estaduais <- c(1974,1978,1982,1986,1990,1994,1998,2002,2006,2010,2014,2018,2022)

dados_eleicoes <- dados %>% 
  group_by(ano) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(eleicao = ifelse(ano %in% eleicoes_estaduais, 'Ano de Eleição','Não é ano de Eleição'))


dados_eleicoes %>% 
  group_by(eleicao) %>% 
  summarise(lancamentos = sum(n)) %>% 
  ggplot(aes(x = eleicao, y = lancamentos))+
  geom_col()




dados_eleicoes %>% 
  filter(ano >= 1994) %>% 
  group_by(eleicao) %>% 
  summarise(lancamentos = sum(n)) %>% 
  ggplot(aes(x = eleicao, y = lancamentos))+
  geom_col(fill = "#41A1C8")+
  geom_text(aes(label = lancamentos),size = 5,color = "white",fontface = "bold" ,position = position_stack(vjust = 0.5))+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# # 3. GGANIMATE






# LEAFLET


leaflet(lat_longs) %>% addTiles() %>% addMarkers(popup = ~as.character(Estacao), label = ~as.character(Estacao))


# Mapa GIF


upperLeft <- c(-23.4490,-46.8258)
lowerRight <- c(-23.7127,-46.4327)

map <- openmap(upperLeft=upperLeft,
               lowerRight=lowerRight,
               type="osm", zoom = 12)



map2 <- openproj(map)



lat_longs$id_estacao <- paste(lat_longs$ano, lat_longs$Estacao)


caption_line1 <- TeX("Feito por Caio Martins")
caption_line2 <- TeX("Twitter: @Mcaio3")
caption_line3 <- TeX("Github: martinscaio")

# scale 3 zoom 13
mapa_osm <- OpenStreetMap::autoplot.OpenStreetMap(map2, scale = 3, zoom = 13) +
  geom_point(data = lat_longs,aes(x = lon, y = lat, group = id_estacao),
             alpha = 0.6,
             size = 5,
             color = '#6260EE')+
  labs(
    title = "Evolução das inaugurações das estações de metrô em São Paulo",
    subtitle = "Ano: {next_state}",
    x= "",
    y= "",
    caption = paste(caption_line1, caption_line2, caption_line3, sep = "\n")) +
  cowplot::theme_cowplot(font_family = "Source Sans Pro",
                         font_size = 12) +
  theme(legend.position = "none",
        axis.title = element_text(size = 18, hjust = 0.5),
        plot.title = element_text(size = 26, hjust = 0.5),
        plot.subtitle = element_text(size = 22, hjust = 0.5),
        plot.margin = margin(5, 5, 5, 5),
        axis.text = element_blank()) +
  transition_states(id_estacao) +
  ease_aes('linear')+
  shadow_mark(color="black")




gif <- animate(mapa_osm, fps = 1,height = 1600, width = 1600,res = 150, renderer = gifski_renderer("metro.gif"))

