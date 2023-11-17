library(tidyverse)
library(tidygeocoder)
library(leaflet)
library(gganimate)
library(ggthemes)
library(osmdata)
library(forcats)
library(OpenStreetMap)
library(latex2exp)
library(sf)




# dados do caio
dados <- read_csv2("C:\\Users\\Caio\\Desktop\\Projeto Metro SP\\endereco_metro.csv")


lat_longs <- read_csv2("C:\\Users\\Caio\\Desktop\\Projeto Metro SP\\dados_prontosQUARTO.csv")


# DADOS DO CEM - CENTRO DE ESTUDOS DA METRÓPOLE



metro_CEM <- read_sf("C:/Users/Caio/Desktop/Dados_METRO_CEM/MetE_2021_CEM.shp")


metro <- metro_CEM %>% filter(OPERACAO != "CPTM")

# quero pegar a coluna de cor da linha par o meu df



lat_longs <- lat_longs %>% mutate(NM_EST_C = Estacao,
                     NM_EST_C = str_remove_all(NM_EST_C, "Estação "),
                     NM_EST_C = abjutils::rm_accent(NM_EST_C),
                     NM_EST_C = str_replace_all(NM_EST_C, "-", " "),
                     NM_EST_C = toupper(NM_EST_C),
                     NM_EST_C = str_remove_all(NM_EST_C, "(METRO DE SAO PAULO)"),
                     NM_EST_C = str_remove_all(NM_EST_C, "(SAO PAULO)"),
                     NM_EST_C = str_remove_all(NM_EST_C, "\\(\\)"))







lat_longs <- lat_longs %>% mutate(NM_EST_C = case_when(
  NM_EST_C == "JARDIM  AYRTON SENNA" ~ "JARDIM SAO PAULO AYRTON SENNA",
  NM_EST_C == "HOSPITAL " ~ "HOSPITAL SAO PAULO",
  NM_EST_C == " MORUMBI" ~ "SAO PAULO MORUMBI",
  NM_EST_C == "PARAISO " ~ "PARAISO",
  NM_EST_C == "PENHA " ~ "PENHA",
  NM_EST_C == "REPUBLICA " ~ "REPUBLICA",
  NM_EST_C == "SANTA CRUZ " ~ "SANTA CRUZ",
  NM_EST_C == "SAO BENTO " ~ "SAO BENTO",
  TRUE ~ NM_EST_C
))







lat_longs <- lat_longs %>% left_join(metro, by = "NM_EST_C")


lat_longs <- lat_longs %>% select(Estacao,Endereco,value,ano,lat,lon,NM_LIN,COR_LIN, INICIO)

# editei via interface gráfica pois mais rapido e pq descobri essa função hj O.O 
lat_longs <- edit(lat_longs)






write_excel_csv2(lat_longs,"C:\\Users\\Caio\\Desktop\\Projeto Metro SP\\dados_prontosQUARTO_2.csv")






# ÁREA DE TESTE




lat_longs <- read_csv2("C:\\Users\\Caio\\Desktop\\Projeto Metro SP\\dados_prontosQUARTO_2.csv")



lat_longs %>% view()


upperLeft <- c(-23.4490,-46.8258)
lowerRight <- c(-23.7127,-46.4327)

map <- openmap(upperLeft=upperLeft,
               lowerRight=lowerRight,
               type="osm", zoom = 12)



map2 <- openproj(map)



lat_longs$id_estacao <- paste(lat_longs$INICIO,",",lat_longs$Estacao,",","Linha:",lat_longs$COR_LIN)


#caption_line1 <- TeX("Feito por Caio Martins")
#caption_line2 <- TeX("Twitter: @Mcaio3")
#caption_line3 <- TeX("Github: martinscaio")




lat_longs <- lat_longs %>%
  mutate(linha = case_when(
    NM_LIN == 1 ~ "Linha 1",
    NM_LIN == 2 ~ "Linha 2",
    NM_LIN == 3 ~ "Linha 3",
    NM_LIN == 4 ~ "Linha 4",
    NM_LIN == 5 ~ "Linha 5",
    NM_LIN == 15 ~ "Linha 15",
    TRUE ~ "LINHAINEXISTENTE"  # Defina uma cor padrão para casos não correspondentes
  ))



# scale 3 zoom 13
mapa_osm <- OpenStreetMap::autoplot.OpenStreetMap(map2, scale = 3, zoom = 13) +
  geom_point(data = lat_longs,aes(x = lon, y = lat, color = linha),
             alpha = 0.9,
             size = 5)+
  scale_color_manual(values = c("Linha 1" = "#0e539e", "Linha 2" = "#217f62", "Linha 3" = "#e7483c", "Linha 4" = "#fcd43a", "Linha 5" = "#76509c", "Linha 15" = "#868d90"))+
  labs(
    title = "Evolução das inaugurações das estações de metrô em São Paulo",
    subtitle = "Ano: {next_state}",
    x= "",
    y= "")+
    #caption = paste(caption_line1, caption_line2, caption_line3, sep = "\n")) +
  cowplot::theme_cowplot(font_family = "Source Sans Pro",
                         font_size = 12) +
  theme(legend.position = "none",
        axis.title = element_text(size = 18, hjust = 0.5),
        plot.title = element_text(size = 26, hjust = 0.5),
        plot.subtitle = element_text(size = 22, hjust = 0.5),
        plot.margin = margin(5, 5, 5, 5),
        axis.text = element_blank()) +
  transition_states(id_estacao,
                    transition_length = 2,
                    state_length = 1) +
  ease_aes('linear')+
  shadow_mark()




animate(mapa_osm, fps = 1,height = 1600, width = 1600,res = 150, renderer = gifski_renderer("metro.gif"))

lat_longs$cor_estacao

lat_longs %>% select(Estacao, INICIO) %>% arrange(INICIO) %>% view()












library(highcharter)




lat_longs %>% 
  group_by(INICIO) %>% 
  count() %>% 
  ggplot(aes(x = INICIO, y = n)) +
  geom_col(fill = "orange")+
  labs(x = "",
       y = "")+
  theme_minimal()+
  scale_y_continuous(breaks = seq(0,14,2))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 18))


lat_longs %>% 
  group_by(INICIO) %>% 
  count() %>% 
  hchart('column', hcaes(x = 'INICIO', y = 'n'), name = "Nº de Estações inauguradas") %>% 
  hc_xAxis(title = list(text = ""), labels = list(visible = FALSE), tickInterval = 1) %>% 
  hc_yAxis(title = list(text = ""), labels = list(visible = FALSE)) %>% 
  hc_plotOptions(column = list(pointWidth = 20))


lat_longs %>% 
  group_by(INICIO) %>% 
  count() %>% view()



