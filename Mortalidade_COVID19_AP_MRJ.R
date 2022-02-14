############################
# Universidade Federal do Rio de Janeiro

# Instituto de Estudos em Saúde Coletiva

# Análise Comparativa dos óbitos por COVID-19 no município do Rio de Janeiro

# Autores: Matheus Santos Matos, Mariana da Silva Costa, Lana dos Santos Meijinhos,
# Paula Cristina Pungartinik e Gabriela Almeida Chaves dos Santos.

# Orientadores: Antonio José Leal Costa, Natália Santana Paiva e Ana Paula Razal Dalvi

# Este script trata sobre os dados coletados no site Data.Rio, do Instituto Pereira Passos,
# da prefeitura da cidade do Rio de Janeiro, acerca dos óbitos por COVID-19 município do RJ e
# nas Áreas de Planejamento dispostas na cidade, além da taxa bruta de mortalidade do município,
# das taxas específicas de mortalidade por sexo e faixa etária para a cidade do RJ e da taxa
# padronizada de mortalidade segundo Áreas de Planejamento de Saúde da cidade do RJ.

# Local de estudo: Município do Rio de Janeiro

# Período de análise: 19 de Março de 2020 e 30 de Setembro de 2021
############################

############################# 0 - CHAMAR OS PACOTES NA BIBLIOTECA para
#                                 exercer as funções necessárias para rodar
#                                 a análise.

# Para chamar os pacotes, usar a função "library" e colocar entre parênteses o
# nome do pacote

library(tidyverse)
library(readxl)
library(PHEindicatormethods)
library(ggplot2)
library(MetBrewer)

############################# 1 - Chamar o diretório do computador, cujo qual 
#                                 está alocado os arquivos necessários para 
#                                 utilizar no script.

# Usar a função "setwd" e inserir o endereço do diretório entre parênteses para chamá-lo ou
# ir no caminho Session >> Set Working Directory >> Choose...


############################# 2 - IMPORTAR BASE DE DADOS QUE UTILIZAREI NA ANÁLISE 

# Escolher um nome para atribuir o arquivo baixado, neste caso será "covid_mrj".
# Em seguida, usar a função "read.csv, para baixar o arquivo em csv, e inserir
# entre parênteses o nome do arquivo salvo no diretório e os demais argumentos necessários.

covid_mrj <- read.csv("db_PainelRioCovid.csv", sep=",", na.strings=c(""," ", "NA"))

# Baixar o arquivo que contém os dados sobre a população por Área Progrmática de Saúde.
# Utilizar a função "read_excel", já que a extensão do arquivo é "xlsx".

# É necessário atribuir essa função a um vetor, neste caso é "pop.ap.total".

# Inserir entre parênteses o nome do arquivo como está no diretório, além dos
# comandos "sheet", para identificar a planilha e "range", para identificar as
# linhas e colunas de interesse.

pop.ap.total <- read_excel("Populações por AP 2.xlsx",  sheet = "População APs",
                  range = "A17:D27")

pop.ap.total <- pop.ap.total[, -c(2,3)] # excluindo colunas indesejadas


########################################################## 
# TAXA BRUTA DE MORTALIDADE DO MRJ
##########################################################

# Para o cálculo da Taxa Bruta de Mortalidade do município do Rio de Janeiro (MRJ),
# deve-se atribuir o número de óbitos por COVID-19 no múnicípio ao vetor "obitos.mrj".

# Em seguida, atribui-se ao vetor "pop.total.mrj" o número da população do
# biênio 2020-2021, que será utilizada no cálculo.

obitos.mrj <- c(33866)

pop.total.mrj <- c(13678694) # A população utilizada foi o dobro da estimativa 
                             # populacional para o ano de 2020, devido a ausência
                             # da estimativa populacional para o ano de 2021,
                             # além do fato de que se assumiu que a diferença
                             # populacional de um ano para o outro foi pequena.

# Para juntar os vetores em uma só tabela, deve-se utilizar a função bind_cols e
# em seguida inserir os vetores entre parênteses.
# Deve-se atribuir essa função a um novo vetor, neste caso chamado "TBM.mrj"

TBM.mrj <- bind_cols(obitos.mrj, pop.total.mrj)

colnames(TBM.mrj)[1:2]<-c('óbitos', 'popMRJ' ) # renomeando colunas

# Para finalmente calcular a Taxa Bruta de Mortalidade, deve-se criar uma nova coluna,
# para a tabela que contém os dados de óbitos e de população, a fim de inserir as taxas
# calculadas.

TBM.mrj$TBM <- phe_rate(TBM.mrj, óbitos, popMRJ, type = "full", confidence = 0.95, multiplier = 100000)


########################################################## 
# TAXA BRUTA DE MORTALIDADE POR AP
##########################################################

# Antes de obter as taxas padronizadas, deve-se calcular a Taxa Bruta de Mortalidade por AP

# Primeiro atribuo o banco covid_mrj ao vetor de mesmo nome e uso a função mutate
# e o comando recode para, dentro da variável "ap_residencia_estadia" transformar em 
# ignorados os dados sem informação sobre ap de residência do indivíduo falecido

covid_mrj <- covid_mrj %>%
  mutate(ap_residencia_estadia = recode(ap_residencia_estadia, `N/D` = "Ignorado"))

########## df com óbitos por ap

# Para filtrar apenas para os dados de óbitos o banco "covid_mrj", deve-se utilizar
# a função filter e entre parênteses especificar a variável de interesse (evolução)
# e a informação de interesse (óbito).

# OBS.: o nome da variável e o nome da informação que consta na linha deve estar
# escrito da mesma forma que consta no banco. Devido a isso os nomes estão dispostos
# como "evolucao" e "Ã³bito"

obito.ap <- covid_mrj %>%
  filter(evolucao == "Ã³bito")

# Agrupando as informações sobre as APS utilizando a função "group_by" e a 
# variável "ap_residência_estadia"

obito.ap <- obito.ap %>%
  group_by(ap_residencia_estadia) %>%
  tally

obito.ap <- obito.ap[(-11),] # excluindo linha de ignorados dos dados de óbitos
                             # do vetor "obito.ap"

# jutando óbitos com pop para calcular a TBM

TBM.ap <- bind_cols(obito.ap, pop.ap.total) # Formando uma tabela com os dois vetores

TBM.ap <- TBM.ap[, -c(3)] # excluindo colunas indesejadas da tabela criada
                          
# calculando a TBM por 100 mil

# Para isso, é necessário criar uma nova variável na tabela, que se chamará "TBM"

# Utiliza-se a função "round" para a realização do cálculo

TBM.ap$TBM <- round(TBM.ap$n / TBM.ap$Total * 100000, 1)


########################################################## 
# TAXA BRUTA DE MORTALIDADE POR SEXO
##########################################################

##Ajeitando Feminino e Masculino
# Renomeando as linhas que identificam os sexos masculino e feminino

# Tudo que iniciar com F ou f recebe Feminino
# tudo que iniciar om M ou m recebe Masculino

covid_mrj$sexo[str_sub(covid_mrj$sexo, 1,1) == "F" | str_sub(covid_mrj$sexo, 1,1) == "f"] <- "Feminino"
covid_mrj$sexo[str_sub(covid_mrj$sexo, 1,1) == "M" | str_sub(covid_mrj$sexo, 1,1) == "m"] <- "Masculino"
covid_mrj$sexo[covid_mrj$sexo != "Feminino" & covid_mrj$sexo != "Masculino"] <- "Ignorado"

# Verificando se de fato ocorreu a mudança
# Para isso utilizo a função "table" e entre parênteses identifico o banco e a variável
table(covid_mrj$sexo)

# Filtrando no banco a informação dos óbitos e atribuindo os dados filtrados ao
# vetor "obitos.sexo"
obitos.sexo <- covid_mrj %>%
  filter(evolucao == "Ã³bito")
  
# Agrupando as informações de sexo e óbitos
# Para isso utiliza-se o vetor contendo as informações de óbito utilizando-se
# como base a variável sexo

obitos.sexo <- obitos.sexo %>%
  group_by(sexo) %>%
  tally

# jutando óbitos com população por sexo para calcular a TBM

# Para isso, atribuo ao vetor pop.sexo o arquivo que contém a informação sobre
# a população, indicando a planilha e as colunas e linhas de interesse, por meio
# dos comandos sheet e range

pop.sexo <- read_excel("Populações por AP 2.xlsx",  sheet = "pop sexo",
                       range = "A01:B03")

# Juntando os vetores em uma só tabela por meio da função bind_cols
TBM.sexo<- bind_cols(obitos.sexo, pop.sexo)

TBM.sexo<- TBM.sexo[, -c(3)] # excluindo colunas indesejadas

# Finalmente, realizando o cálculo da TBM
TBM.sexo$TBM <- phe_rate(TBM.sexo, n, pop, type = "full", confidence = 0.95, multiplier = 100000)


########################################################## 
# TAXA BRUTA DE MORTALIDADE POR FX ETARIA
##########################################################

# Visualizando a variável no banco
table(covid_mrj$faixa_etaria)

# Utilizando a função mutate para recodificar as linhas com falta de informação
# sobre a idade e transformálas em ignorados.
covid_mrj <- covid_mrj %>%
  mutate(faixa_etaria = recode(faixa_etaria, `N/D` = "Ignorado"), 
         faixa_etaria = recode(faixa_etaria, `#N/D` = "Ignorado"))

# Transformando as linhas com "NA" em ignorados.
covid_mrj$faixa_etaria[is.na(covid_mrj$faixa_etaria)] <- "Ignorado"

# Utilizando a função "mutate" e o comando "recode" para reajustar a faixa etária
# Entre a crase estão as faixas etárias contidas no banco e entre as aspas 
# as faixas etárias novas

covid_mrj <- covid_mrj %>%
  mutate(faixa_etaria = recode(faixa_etaria, `De 0 a 9` = "De 0 a 19"),
         faixa_etaria = recode(faixa_etaria, `De 10 a 19` = "De 0 a 19"),
         faixa_etaria = recode(faixa_etaria, `De 20 a 29` = "De 20 a 39"),
         faixa_etaria = recode(faixa_etaria, `De 30 a 39` = "De 20 a 39"),
         faixa_etaria = recode(faixa_etaria, `De 40 a 49` = "De 40 a 59"),
         faixa_etaria = recode(faixa_etaria, `De 50 a 59` = "De 40 a 59"),
         faixa_etaria = recode(faixa_etaria, `De 90 a 99` = "De 90 ou +"),
         faixa_etaria = recode(faixa_etaria, `Maior de 100` = "De 90 ou +"))

# Óbitos por fx etaria

# Atribuindo ao vetor obitos.fxetaria as informações de óbitos da variável evolução
# filtrada do banco covid_mrj

obitos.fxetaria <- covid_mrj %>%
  filter(evolucao == "Ã³bito")

# Agrupando as informações de óbito e faixa etária por meio da função group_by
obitos.fxetaria <- obitos.fxetaria %>%
  group_by(faixa_etaria) %>%
  tally

# juntando obitos.fxetaria com pop do MRJ por fx etaria

#Atribuindo o arquivo com as populações por faixa etária ao vetor pop.fxetaria
pop.fxetaria <- read_excel("Populações por AP 2.xlsx",  sheet = "População APs",
                           range = "A03:L11")

pop.fxetaria <- pop.fxetaria[, -c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)] # excluindo colunas indesejadas
pop.fxetaria <- pop.fxetaria[(-8),] #excluindo linha de total

# Agrupando as informações de óbitos, faixa etária e população por faixa etária
# por meio da função bind_cols
TBM.fxetaria <- bind_cols(obitos.fxetaria, pop.fxetaria)


TBM.fxetaria <- TBM.fxetaria[, -c(3)] # excluindo colunas indesejadas


# calculando a TBM por 100 mil 

TBM.fxetaria$TBM <- round(TBM.fxetaria$n / TBM.fxetaria$MRJ * 100000, 1)


########################################################## 
# TAXA PADRONIZADA DE MORTALIDADE 
##########################################################

# criando df de óbitos por AP segundo faixa etaria  

# Transformando a variável ap_residencia_estadia do banco covid_mrj em uma variável
# de fator.
covid_mrj$ap_residencia_estadia <- factor(covid_mrj$ap_residencia_estadia)

# Filtrando a evolução de casos para óbito do banco covid_mrj e atribuindo essas
# informações ao vetor obitos.ap.fxetaria

obitos.ap.fxetaria <- covid_mrj %>%
  filter(evolucao == "Ã³bito")

# Agrupando as informações de óbito, ap e faixa etária utilizando a função group_by
# e as variáveis ap_residencia_estadia e faixa_etaria, atribuindo o comando ao vetor
# obitos.ap.residencia, que contém as informações sobre os óbitos

obitos.ap.fxetaria<- obitos.ap.fxetaria %>%
  group_by(ap_residencia_estadia, faixa_etaria, .drop=FALSE) %>%
  tally

obitos.ap.fxetaria <- obitos.ap.fxetaria[c(-71,-72, -73,-74, -75,-76,-77),] #excluindo linhas de ignorado


# juntando obitos.ap.fxetaria com pop por fx etaria

ap.pop.fxetaria <- read_excel("ap_pop_fxetaria_TPM_2.xlsx", sheet = "Planilha1",
                              range = "A01:C71")

# Juntando os dados de óbitos, ap, faixa etária e população por meio da função bind_cols
TPM.aux <- bind_cols(obitos.ap.fxetaria, ap.pop.fxetaria)

# verificar se estão na mesma ordem
View(TPM.aux) # sim, estão

TPM.aux <- TPM.aux[, -c(4,5)] # excluindo colunas indesejadas

# criando vetor com a pop mrj

popmrj <- c(1520197, 2157159, 1860127, 684377, 389762, 173382, 46338)

names(popmrj)<- c('0 a 19', '20 a 39', '40 a 59', '60 a 69', '70 a 79', '80 a 89' ,'90 ou mais')

popmrj <- rep(popmrj, 10) 

# colocando a população padrão em TPM.aux

TPM.aux$popmrj <- bind_cols(popmrj)

colnames(TPM.aux)[1:5]<-c('AP', 'Faixa etária', 'Óbitos', 'pop', 'popMRJ' ) # renomeando colunas

# calculando TPM

names(TPM.aux)

TPM <- TPM.aux %>%
  group_by(AP)%>% # 
  phe_dsr(x = Óbitos, n = pop, type = "standard", 
          stdpop = popMRJ, stdpoptype = "field")

TPM

