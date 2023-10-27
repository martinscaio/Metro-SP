

library(tidyverse)
library(rvest)
library(httr)





# EXTRAINDO NOME DAS ESTAÇÕES



url <- "https://pt.wikipedia.org/wiki/Categoria:Estações_do_Metrô_de_São_Paulo"

estacoes <- url %>% read_html() %>% html_nodes('li') %>% html_text()


estacoes <- estacoes[53:168]







# EXTRAINDO OS LINKS PRA CADA ESTAÇÃO



links <- url %>% read_html() %>% html_nodes('a') %>% html_attr('href')


links <- links[65:180]




# ADICIONANDO UMA PARTE DA URL




links <- paste0("https://pt.wikipedia.org", links)



# Eliminando uma linha "Lista de pátios do metro"


estacoes <- estacoes %>% as_data_frame() %>%  filter(!row_number() %in% c(58)) %>% rename('Estacao' = value)





# EXTRAINDO O ENDEREÇO DAS ESTAÇÕES ---------------------------------


lista <- c()

for (i in links){
  
  
  url_provisoria <- i
  
  resultado <- url_provisoria %>% read_html() %>% html_nodes('td') %>% html_text()
  
  lista <- append(lista, resultado)
  
  Sys.sleep(3)
  
}






# Função para adicionar linhas faltantes num index especifico (obg stackoverflow)




insertRow <- function(data, new_row, r) {
  data_new <- rbind(data[1:r, ],            
                    new_row,                
                    data[- (1:r), ])        
  rownames(data_new) <- 1:nrow(data_new)    
  return(data_new)
}




# LIMPANDO E EXTRAINDO OS ENDEREÇOS DAS ESTAÇÕES


resultado_final <- lista %>% as_data_frame()



checar <- resultado_final %>% 
  filter(
    str_detect(value, 'Av.| av.|Avenida|avenida|Rua|rua|RUA') &
      !str_detect(value, 'Logradouros|Edificações|Allianz Parque| Avenida Brasil Avenida') &
      !str_detect(value, 'Ponte Estaiada') &
      !str_detect(value, 'Promon') &
      !str_detect(value, 'concreto') &
      !str_detect(value, 'GreenFeld') &
      !str_detect(value, 'partir') &
      !str_detect(value, 'Avenida Cruzeiro do Sul e a estação') &
      !str_detect(value, 'Entrada da Estação na Avenida Paulista.') &
      !str_detect(value, 'Rua dos Jequitibás, s/n, Jabaquara, São Paulo, SP') &
      !str_detect(value, 'Avenida Engenheiro Armando de Arruda Pereira, 2654 Jabaquara, São Paulo') &
      !str_detect(value, 'Av. Arquiteto Vilanova Artigas, S/Nº Sapopemba, São Paulo') &
      !str_detect(value, 'Estação PaulistaAcesso do lado par da Rua da Consolação.') &
      !str_detect(value, 'Av. Adélia Chohfi, 100 São Mateus, São Paulo')
  )








# 2 endereços duplicados por algum motivo

checar <- checar %>% filter(!row_number() %in% c(102, 104))





# ADICIONANDO ALGUNS ENDEREÇOS FALTANTES 


index <- 1                                             

newrow <- c('Rua Pedro de Toledo, 1.601 - Indianópolis, São Paulo - SP')                                
checar <- insertRow(checar, newrow, index)





index <- 15                                             

newrow <- c('Av. Santo Amaro, 5665 - Santo Amaro')                                
checar <- insertRow(checar, newrow, index)






index <- 16                                             

newrow <- c('Praça Agente Cícero, s/n.º, Brás')                                
checar <- insertRow(checar, newrow, index)







index <- 36                                             

newrow <- c('Av. Ibirapuera, 3170 - Indianópolis')                                
checar <- insertRow(checar, newrow, index)







index <- 56                                            

newrow <- c('Praça da Liberdade, 133, Sé')                                
checar <- insertRow(checar, newrow, index)





index <- 58                                            

newrow <- c('Praça Marechal Deodoro, s/n.º Santa Cecília')                                
checar <- insertRow(checar, newrow, index)






index <- 74                                            

newrow <- c('Praça da Árvore, 39, Saúde')                                
checar <- insertRow(checar, newrow, index)





index <- 78                                            

newrow <- c('Largo Santa Cecília, s/nº, Santa Cecília')                                
checar <- insertRow(checar, newrow, index)






index <- 87                                            

newrow <- c('Largo São Bento, 109 - Sé')                                
checar <- insertRow(checar, newrow, index)






index <- 95                                           

newrow <- c('Praça da Sé, s/nº, Sé')                                
checar <- insertRow(checar, newrow, index)





index <- 108                                           

newrow <- c('Praça Américo Jacomino, 30, Pinheiros')                                
checar <- insertRow(checar, newrow, index)







enderecos <- checar


enderecos <- enderecos %>% rename('Endereco' = value)



# Juntando as informações de Estações e seus Endereços

dados <- cbind(estacoes, enderecos)




# EXTRAINDO AS DATAS DE INAUGURAÇÃO DE CADA ESTAÇÃO




data <- resultado_final %>% filter(str_detect(value, '\\d{1,2} de [a-zA-Z]+ de \\d{4}'))



data <- data %>% filter(!str_detect(value, 'Este artigo|Marcelo Accioly'))





data <- data %>% filter(!row_number() %in% c(2, 4,5,7,9,11,12,13,15,17,18,19,21,23,26,28,29,31,34,36,40,42,44,
                                             45,47,49,51,52,54,55,57,59,61,64,67,69,70,72,73,75,77,78,80,82,83,84,85,
                                             88,90,91,92,93,95,97,99,100,101,103,104,106,107,108,110,113,115,117,118,
                                             119,120,122,123,124,125,127,130,132,134,135,137,138,
                                             139,140,142,143, 145,147,148,151,153,154,155,157,160,161,163,166,167,169,
                                             170,171,173,175,177,179,180,182,184,186,188,190,191,193,194,196))





# Faltando 3 datas de inauguração

index <- 14                                             

newrow <- c('28 de março de 2011')                                
data <- insertRow(data, newrow, index)



index <- 26                                             

newrow <- c('2 de março de 2018')                                
data <- insertRow(data, newrow, index)


index <- 61                                             

newrow <- c('30 de março de 2006')                                
data <- insertRow(data, newrow, index)






# TERMINANDO DE ARRUMAR OS DADOS



# eliminando as estações que ainda não inauguraram (20/10/2023)

dados <- dados %>% filter(!Estacao %in% c('Estação 14 Bis',
                                          'Estação Anália Franco',
                                          'Estação Água Branca',
                                          'Estação Morumbi',
                                          'Estação Boa Esperança',
                                          'Estação Brasilândia',
                                          'Estação Brooklin Paulista',
                                          'Estação Chucri Zaidan',
                                          'Estação Congonhas',
                                          'Estação FAAP-Pacaembu',
                                          'Estação Freguesia do Ó',
                                          'Estação Guilherme Giorgi',
                                          'Estação Itaberaba',
                                          'Estação Jacu-Pêssego',
                                          'Estação Washington Luís',
                                          'Estação João Paulo I',
                                          'Estação Orfanato',
                                          'Estação Perdizes',
                                          'Estação PUC–Cardoso de Almeida',
                                          'Estação Santa Clara',
                                          'Estação Santa Isabel',
                                          'Estação Santa Marina',
                                          'Estação SESC-Pompeia',
                                          'Estação Vila Cardoso',
                                          'Estação Vila Cordeiro',
                                          'Estação Vila Formosa',
                                          'Estação Aricanduva',
                                          'Estação Bela Vista (Metrô de São Paulo)',
                                          'Estação Vereador José Diniz',
                                          'Estação FAAP–Pacaembu',
                                          'Estação Washington Luís (Metrô de São Paulo)',
                                          'Estação Santa Clara (Metrô de São Paulo)',
                                          'Estação Santa Isabel (Metrô de São Paulo)'))







# TENTEI ALTERAR ALGUNS ENDEREÇOS PARA FACILITAR A COLETA DE LATITUDE/LONGITUDE

dados <- dados %>% 
  mutate(Endereco = dplyr::case_when(Endereco == 'Rua Formosa, s/nº (próxima à Praça da Bandeira), República\n'~ 'Rua Formosa, s/nº, República',
                                     Endereco == 'Rua do Hipódromo, s/nº, Brás (lado norte) e Mooca (lado sul).\n' ~ 'Rua do Hipódromo, 773 , Brás',
                                     Endereco == 'Av. Paulista, s/nº Bela Vista  (sentido Consolação) Vila Mariana  (sentido Paraíso)\n' ~ 'Av. Paulista, 447 - Paraíso, São Paulo',
                                     Endereco == 'Av. Vital Brasil × Rua Pirajussara, Butantã\n' ~ 'Av. Vital Brasil, 427, Butantã',
                                     Endereco == 'Av. Jornalista Roberto Marinho × Av. Santo Amaro, Campo Belo\n' ~ 'Av. Jornalista Roberto Marinho, 1661, Brooklin, São Paulo',
                                     Endereco == 'Radial Leste X Rua Apucarana, Tatuapé\n' ~ 'Av. Radial Leste, 3560, Tatuapé, São Paulo',
                                     Endereco == 'Rua Vergueiro, Vila Mariana\n' ~ 'Rua Vergueiro, 3807, Vila Mariana',
                                     Endereco == 'Av. Paulista, s/nº, Consolação\n' ~ 'Av. Paulista, 2163 - Cerqueira César, São Paulo',
                                     Endereco == 'Rua Fradique Coutinho × Rua dos Pinheiros, Pinheiros\n' ~ 'Rua dos Pinheiros, 623 - Pinheiros, São Paulo',
                                     Endereco == 'Rua da Consolação × Rua Piauí, Consolação\n' ~ 'Rua da Consolação, 1051, Consolação, São Paulo',
                                     Endereco == 'Confluência da Av. Padre José Maria com a Rua Barão do Rio Branco Santo Amaro\n' ~ 'Av. Padre José Maria, 62, Santo Amaro, São Paulo',
                                     Endereco == 'Av. Ibirapuera × Av. Divino Salvador, Moema\n' ~ 'Av. Ibirapuera, 2179, Ibirapuera, São Paulo',
                                     Endereco == 'Av. Auro Soares de Moura Andrade (sul) x Rua Jorn. Aloysio Biondi (norte), s/n - Barra Funda\n' ~ 'Avenida Mario de Andrade, 815, Barra Funda, São Paulo',
                                     Endereco == 'Rua da Figueira, s/nº, Sé\n' ~ 'Rua da Figueira, 3, Sé, São Paulo',
                                     Endereco == 'Avenida Conde de Frontin, s/n, Vila Matilde\n' ~ 'Rua Alvinópolis, 178 - Vila Beatriz, São Paulo',
                                     Endereco == 'Av. Dr. Ricardo Jafet, s/nº, Cursino\n' ~ 'Av. Dr. Ricardo Jafet, 2318, Cursino, São Paulo',
                                     Endereco == 'Rua Catiguá, s/nº, Tatuapé\n' ~ 'Rua Melo Freire, 2620, Tatuapé, São Paulo',
                                     Endereco == 'Av. Paulista, s/nº, Jardim Paulista\n' ~ 'Av. Paulista, 1293, São Paulo',
                                     Endereco == 'Avenida Professor Francisco Morato × Rua Heitor dos Prazeres, s/nºVila Sônia\n' ~ 'Av. Prof. Francisco Morato, 4009 - Vila Sonia, São Paulo',
                                     Endereco == 'Praça da Sé, s/nº, Sé' ~ 'Praça da Sé - Sé, São Paulo - SP, 01310-200',
                                     Endereco == 'Av. Guido Caloi, 2221 (Linha 5)Av. das Nações Unidas (Marginal Pinheiros), s/n (Linha 9)\n' ~ 'Avenida Guido Caloi - 2221, Santo Amaro, São Paulo',
                                     Endereco == 'Av. Dr. Antonio Maria Laet, 100,Tucuruvi\n' ~ 'Rua Paranabi, 228, Tucuruvi, Sâo Paulo',
                                     Endereco == 'Av. das Belezas, 880, Vila Andrade\n'~ 'Rua Miguel da Silva, 84, Vila Prel, São Paulo',
                                     Endereco == 'Rua Vergueiro, 505 - Vila Mariana\n' ~ 'Rua Vergueiro, 2229,Vila Mariana, São Paulo',
                                     Endereco == 'Rua Vergueiro, 1456, Vila Mariana\n' ~ 'Rua Doutor Eduardo Amaro, 2, Vila Mariana, São Paulo',TRUE ~ Endereco))




# JUNTANDO O DF DAS DATAS COM O DF DAS ESTAÇÕES/INAUGURAÇÕES



dados <- dados %>% cbind(data)



# nova versão só roda isso abaixo



dados <- dados %>% 
  mutate(ano = str_extract_all(value, '\\d{4}')) %>% 
  unnest(ano) %>% 
  filter(!row_number() %in% c(6,12,14,26,31,39,48,56,61,63,64,68,72,75,80))





# Arrumando alguns nomes


dados <- dados %>% 
  mutate(Estacao = stringr::str_replace_all(Estacao,pattern = "Terminal Intermodal", replacement = "Estação"))






# Os dados estão prontos para analise. Vamos exportá-lo



write_excel_csv2(dados,"C:\\Users\\Caio\\Desktop\\Projeto Metro SP\\endereco_metro.csv")

