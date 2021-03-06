############################
# Universidade Federal do Rio de Janeiro

# Instituto de Estudos em Sa�de Coletiva

# An�lise Comparativa dos �bitos por COVID-19 no munic�pio do Rio de Janeiro

# Autores: Matheus Santos Matos, Mariana da Silva Costa, Lana dos Santos Meijinhos,
# Paula Cristina Pungartinik e Gabriela Almeida Chaves dos Santos.

# Orientadores: Antonio Jos� Leal Costa, Nat�lia Santana Paiva e Ana Paula Razal Dalvi

# Este script trata sobre os dados coletados no site Data.Rio, do Instituto Pereira Passos,
# da prefeitura da cidade do Rio de Janeiro, acerca dos �bitos por COVID-19 munic�pio do RJ e
# nas �reas de Planejamento dispostas na cidade, al�m da taxa bruta de mortalidade do munic�pio,
# das taxas espec�ficas de mortalidade por sexo e faixa et�ria para a cidade do RJ e da taxa
# padronizada de mortalidade segundo �reas de Planejamento de Sa�de da cidade do RJ.

# Local de estudo: Munic�pio do Rio de Janeiro

# Per�odo de an�lise: 19 de Mar�o de 2020 e 30 de Setembro de 2021
############################

############################# 0 - CHAMAR OS PACOTES NA BIBLIOTECA para
#                                 exercer as fun��es necess�rias para rodar
#                                 a an�lise.

# Para chamar os pacotes, usar a fun��o "library" e colocar entre par�nteses o
# nome do pacote

library(tidyverse)
library(readxl)
library(PHEindicatormethods)
library(ggplot2)
library(MetBrewer)

############################# 1 - Chamar o diret�rio do computador, cujo qual 
#                                 est� alocado os arquivos necess�rios para 
#                                 utilizar no script.

# Usar a fun��o "setwd" e inserir o endere�o do diret�rio entre par�nteses para cham�-lo ou
# ir no caminho Session >> Set Working Directory >> Choose...


############################# 2 - IMPORTAR BASE DE DADOS QUE UTILIZAREI NA AN�LISE 

# Escolher um nome para atribuir o arquivo baixado, neste caso ser� "covid_mrj".
# Em seguida, usar a fun��o "read.csv, para baixar o arquivo em csv, e inserir
# entre par�nteses o nome do arquivo salvo no diret�rio e os demais argumentos necess�rios.

covid_mrj <- read.csv("db_PainelRioCovid.csv", sep=",", na.strings=c(""," ", "NA"))

# Baixar o arquivo que cont�m os dados sobre a popula��o por �rea Progrm�tica de Sa�de.
# Utilizar a fun��o "read_excel", j� que a extens�o do arquivo � "xlsx".

# � necess�rio atribuir essa fun��o a um vetor, neste caso � "pop.ap.total".

# Inserir entre par�nteses o nome do arquivo como est� no diret�rio, al�m dos
# comandos "sheet", para identificar a planilha e "range", para identificar as
# linhas e colunas de interesse.

pop.ap.total <- read_excel("Popula��es por AP 2.xlsx",  sheet = "Popula��o APs",
                  range = "A17:D27")

pop.ap.total <- pop.ap.total[, -c(2,3)] # excluindo colunas indesejadas


########################################################## 
# TAXA BRUTA DE MORTALIDADE DO MRJ
##########################################################

# Para o c�lculo da Taxa Bruta de Mortalidade do munic�pio do Rio de Janeiro (MRJ),
# deve-se atribuir o n�mero de �bitos por COVID-19 no m�nic�pio ao vetor "obitos.mrj".

# Em seguida, atribui-se ao vetor "pop.total.mrj" o n�mero da popula��o do
# bi�nio 2020-2021, que ser� utilizada no c�lculo.

obitos.mrj <- c(33866)

pop.total.mrj <- c(13678694) # A popula��o utilizada foi o dobro da estimativa 
                             # populacional para o ano de 2020, devido a aus�ncia
                             # da estimativa populacional para o ano de 2021,
                             # al�m do fato de que se assumiu que a diferen�a
                             # populacional de um ano para o outro foi pequena.

# Para juntar os vetores em uma s� tabela, deve-se utilizar a fun��o bind_cols e
# em seguida inserir os vetores entre par�nteses.
# Deve-se atribuir essa fun��o a um novo vetor, neste caso chamado "TBM.mrj"

TBM.mrj <- bind_cols(obitos.mrj, pop.total.mrj)

colnames(TBM.mrj)[1:2]<-c('�bitos', 'popMRJ' ) # renomeando colunas

# Para finalmente calcular a Taxa Bruta de Mortalidade, deve-se criar uma nova coluna,
# para a tabela que cont�m os dados de �bitos e de popula��o, a fim de inserir as taxas
# calculadas.

TBM.mrj$TBM <- phe_rate(TBM.mrj, �bitos, popMRJ, type = "full", confidence = 0.95, multiplier = 100000)


########################################################## 
# TAXA BRUTA DE MORTALIDADE POR AP
##########################################################

# Antes de obter as taxas padronizadas, deve-se calcular a Taxa Bruta de Mortalidade por AP

# Primeiro atribuo o banco covid_mrj ao vetor de mesmo nome e uso a fun��o mutate
# e o comando recode para, dentro da vari�vel "ap_residencia_estadia" transformar em 
# ignorados os dados sem informa��o sobre ap de resid�ncia do indiv�duo falecido

covid_mrj <- covid_mrj %>%
  mutate(ap_residencia_estadia = recode(ap_residencia_estadia, `N/D` = "Ignorado"))

########## df com �bitos por ap

# Para filtrar apenas para os dados de �bitos o banco "covid_mrj", deve-se utilizar
# a fun��o filter e entre par�nteses especificar a vari�vel de interesse (evolu��o)
# e a informa��o de interesse (�bito).

# OBS.: o nome da vari�vel e o nome da informa��o que consta na linha deve estar
# escrito da mesma forma que consta no banco. Devido a isso os nomes est�o dispostos
# como "evolucao" e "óbito"

obito.ap <- covid_mrj %>%
  filter(evolucao == "óbito")

# Agrupando as informa��es sobre as APS utilizando a fun��o "group_by" e a 
# vari�vel "ap_resid�ncia_estadia"

obito.ap <- obito.ap %>%
  group_by(ap_residencia_estadia) %>%
  tally

obito.ap <- obito.ap[(-11),] # excluindo linha de ignorados dos dados de �bitos
                             # do vetor "obito.ap"

# jutando �bitos com pop para calcular a TBM

TBM.ap <- bind_cols(obito.ap, pop.ap.total) # Formando uma tabela com os dois vetores

TBM.ap <- TBM.ap[, -c(3)] # excluindo colunas indesejadas da tabela criada
                          
# calculando a TBM por 100 mil

# Para isso, � necess�rio criar uma nova vari�vel na tabela, que se chamar� "TBM"

# Utiliza-se a fun��o "round" para a realiza��o do c�lculo

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

# Verificando se de fato ocorreu a mudan�a
# Para isso utilizo a fun��o "table" e entre par�nteses identifico o banco e a vari�vel
table(covid_mrj$sexo)

# Filtrando no banco a informa��o dos �bitos e atribuindo os dados filtrados ao
# vetor "obitos.sexo"
obitos.sexo <- covid_mrj %>%
  filter(evolucao == "óbito")
  
# Agrupando as informa��es de sexo e �bitos
# Para isso utiliza-se o vetor contendo as informa��es de �bito utilizando-se
# como base a vari�vel sexo

obitos.sexo <- obitos.sexo %>%
  group_by(sexo) %>%
  tally

# jutando �bitos com popula��o por sexo para calcular a TBM

# Para isso, atribuo ao vetor pop.sexo o arquivo que cont�m a informa��o sobre
# a popula��o, indicando a planilha e as colunas e linhas de interesse, por meio
# dos comandos sheet e range

pop.sexo <- read_excel("Popula��es por AP 2.xlsx",  sheet = "pop sexo",
                       range = "A01:B03")

# Juntando os vetores em uma s� tabela por meio da fun��o bind_cols
TBM.sexo<- bind_cols(obitos.sexo, pop.sexo)

TBM.sexo<- TBM.sexo[, -c(3)] # excluindo colunas indesejadas

# Finalmente, realizando o c�lculo da TBM
TBM.sexo$TBM <- phe_rate(TBM.sexo, n, pop, type = "full", confidence = 0.95, multiplier = 100000)


########################################################## 
# TAXA BRUTA DE MORTALIDADE POR FX ETARIA
##########################################################

# Visualizando a vari�vel no banco
table(covid_mrj$faixa_etaria)

# Utilizando a fun��o mutate para recodificar as linhas com falta de informa��o
# sobre a idade e transform�las em ignorados.
covid_mrj <- covid_mrj %>%
  mutate(faixa_etaria = recode(faixa_etaria, `N/D` = "Ignorado"), 
         faixa_etaria = recode(faixa_etaria, `#N/D` = "Ignorado"))

# Transformando as linhas com "NA" em ignorados.
covid_mrj$faixa_etaria[is.na(covid_mrj$faixa_etaria)] <- "Ignorado"

# Utilizando a fun��o "mutate" e o comando "recode" para reajustar a faixa et�ria
# Entre a crase est�o as faixas et�rias contidas no banco e entre as aspas 
# as faixas et�rias novas

covid_mrj <- covid_mrj %>%
  mutate(faixa_etaria = recode(faixa_etaria, `De 0 a 9` = "De 0 a 19"),
         faixa_etaria = recode(faixa_etaria, `De 10 a 19` = "De 0 a 19"),
         faixa_etaria = recode(faixa_etaria, `De 20 a 29` = "De 20 a 39"),
         faixa_etaria = recode(faixa_etaria, `De 30 a 39` = "De 20 a 39"),
         faixa_etaria = recode(faixa_etaria, `De 40 a 49` = "De 40 a 59"),
         faixa_etaria = recode(faixa_etaria, `De 50 a 59` = "De 40 a 59"),
         faixa_etaria = recode(faixa_etaria, `De 90 a 99` = "De 90 ou +"),
         faixa_etaria = recode(faixa_etaria, `Maior de 100` = "De 90 ou +"))

# �bitos por fx etaria

# Atribuindo ao vetor obitos.fxetaria as informa��es de �bitos da vari�vel evolu��o
# filtrada do banco covid_mrj

obitos.fxetaria <- covid_mrj %>%
  filter(evolucao == "óbito")

# Agrupando as informa��es de �bito e faixa et�ria por meio da fun��o group_by
obitos.fxetaria <- obitos.fxetaria %>%
  group_by(faixa_etaria) %>%
  tally

# juntando obitos.fxetaria com pop do MRJ por fx etaria

#Atribuindo o arquivo com as popula��es por faixa et�ria ao vetor pop.fxetaria
pop.fxetaria <- read_excel("Popula��es por AP 2.xlsx",  sheet = "Popula��o APs",
                           range = "A03:L11")

pop.fxetaria <- pop.fxetaria[, -c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)] # excluindo colunas indesejadas
pop.fxetaria <- pop.fxetaria[(-8),] #excluindo linha de total

# Agrupando as informa��es de �bitos, faixa et�ria e popula��o por faixa et�ria
# por meio da fun��o bind_cols
TBM.fxetaria <- bind_cols(obitos.fxetaria, pop.fxetaria)


TBM.fxetaria <- TBM.fxetaria[, -c(3)] # excluindo colunas indesejadas


# calculando a TBM por 100 mil 

TBM.fxetaria$TBM <- round(TBM.fxetaria$n / TBM.fxetaria$MRJ * 100000, 1)


########################################################## 
# TAXA PADRONIZADA DE MORTALIDADE 
##########################################################

# criando df de �bitos por AP segundo faixa etaria  

# Transformando a vari�vel ap_residencia_estadia do banco covid_mrj em uma vari�vel
# de fator.
covid_mrj$ap_residencia_estadia <- factor(covid_mrj$ap_residencia_estadia)

# Filtrando a evolu��o de casos para �bito do banco covid_mrj e atribuindo essas
# informa��es ao vetor obitos.ap.fxetaria

obitos.ap.fxetaria <- covid_mrj %>%
  filter(evolucao == "óbito")

# Agrupando as informa��es de �bito, ap e faixa et�ria utilizando a fun��o group_by
# e as vari�veis ap_residencia_estadia e faixa_etaria, atribuindo o comando ao vetor
# obitos.ap.residencia, que cont�m as informa��es sobre os �bitos

obitos.ap.fxetaria<- obitos.ap.fxetaria %>%
  group_by(ap_residencia_estadia, faixa_etaria, .drop=FALSE) %>%
  tally

obitos.ap.fxetaria <- obitos.ap.fxetaria[c(-71,-72, -73,-74, -75,-76,-77),] #excluindo linhas de ignorado


# juntando obitos.ap.fxetaria com pop por fx etaria

ap.pop.fxetaria <- read_excel("ap_pop_fxetaria_TPM_2.xlsx", sheet = "Planilha1",
                              range = "A01:C71")

# Juntando os dados de �bitos, ap, faixa et�ria e popula��o por meio da fun��o bind_cols
TPM.aux <- bind_cols(obitos.ap.fxetaria, ap.pop.fxetaria)

# verificar se est�o na mesma ordem
View(TPM.aux) # sim, est�o

TPM.aux <- TPM.aux[, -c(4,5)] # excluindo colunas indesejadas

# criando vetor com a pop mrj

popmrj <- c(1520197, 2157159, 1860127, 684377, 389762, 173382, 46338)

names(popmrj)<- c('0 a 19', '20 a 39', '40 a 59', '60 a 69', '70 a 79', '80 a 89' ,'90 ou mais')

popmrj <- rep(popmrj, 10) 

# colocando a popula��o padr�o em TPM.aux

TPM.aux$popmrj <- bind_cols(popmrj)

colnames(TPM.aux)[1:5]<-c('AP', 'Faixa et�ria', '�bitos', 'pop', 'popMRJ' ) # renomeando colunas

# calculando TPM

names(TPM.aux)

TPM <- TPM.aux %>%
  group_by(AP)%>% # 
  phe_dsr(x = �bitos, n = pop, type = "standard", 
          stdpop = popMRJ, stdpoptype = "field")

TPM

