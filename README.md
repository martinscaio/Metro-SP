# Metro-SP

## Mapeando as aberturas das estações de metrô em SP com um GIF

Recentemente deparei-me no twitter com uma publicação que mostrava a evolução da extensão metroviária entre duas cidades. O objetivo da publicação era demonstrar como uma das cidades desenvolveu o dobro de extensão da rede metroviária em um menor intervalo de tempo. Não lembro muito bem quais eram as cidades, mas acredito que uma delas era Seul, a capital da Coreia do Sul.

Junto a isso, relembrei de um post do cientista de dados Sillas Gonzaga em que [mapeava a abertura das escolas municipais em SP com um GIF](sillasgonzaga.com/post/mapeando-a-abertura-de-escolas-municipais-em-sao-paulo-ao-longo-dos-anos/). A junção das duas coisas trouxe-me a ideia: por que não fazer o mesmo com as estações de metrô na cidade de SP ?!

O mapeamento das escolas é uma questão mais pertinente, afinal, será que o governo prioriza a abertura de escolas em áreas com déficit de vagas ou em áreas menos centrais ?

É evidente que no caso do metrô todos conhecem a localização das estações (o próprio nome já indica). Mas não deixa de ser curioso poder visualizar em um GIF a evolução da rede metroviária na cidade de SP.

Portanto, este artigo irá mostrar a evolução da rede metroviária na cidade de SP com um GIF.

## Metodologia

Para mapear a evolução das inaugurações das estações de metrô é necessário ter em mãos não só o ano da inauguração, como também o endereço das estações.

Confesso que não encontrei em dados públicos ambas as informações. A solução utilizada aqui foi raspar as informações no site do Wikipedia. O Wikipedia tem uma página que mostra todas as estações da cidade de São Paulo. Fiz um Scrapper pra coletar o nome dessas estações, o link da página de cada estação, a data de inauguração e o endereço.

Todo o processo desde a raspagem de dados até a plotagem foi feita utilizando o R e suas dependências.

Disclaimer: somente após terminar o scrapper que descobri que o [CEM (Centro de Estudos da Métropole)](centrodametropole.fflch.usp.br/pt-br) disponibiliza dados referentes as estações de metrô/CPTM.

Atenção: o GIF da evolução da rede metroviária de SP segue a ordem de inauguração das estações com base no ano. No entanto, é importante notar que, dentro do mesmo ano, a sequência das estações pode não refletir sua cronologia exata de lançamento ao longo do ano. Portanto, a ordem de aparição das estações no GIF é baseado exclusivamente no ano de inauguração, o que pode não representar a ordem exata de lançamento ao longo desse período, pois não estou considerando dia/mês.
