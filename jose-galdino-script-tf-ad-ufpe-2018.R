# Script para replicação do artigo

# Instalando pacotes essenciais para a analise

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")

# Subindo os bancos de dados

library(readr)
NMC_5_0 <- read_csv("~/Acadêmico/UFPE/Doutorado/Semestres/2018.2/Analíse de Dados - Davi Moreira/Bibliografia/Smooth-Sailing/NMC_5_0.csv", 
                    col_types = cols(cinc = col_number(), 
                                     milex = col_number(), year = col_number()))
View(NMC_5_0)

library(readr)
brasil_pib <- read_csv("~/Acadêmico/UFPE/Doutorado/Semestres/2018.2/Analíse de Dados - Davi Moreira/Artigo final/Bancos/brasil_pib.csv", 
                       col_types = cols(`Brasil - PIB - paridade do poder de compra (PPC) - US$ - Banco Mundial- World Development Indicators (WDI) - WDI_PIBPPCBRA` = col_number(), 
                                        Data = col_number()))
View(brasil_pib)

# Mudando os nomes das variaveis para unir os bancos atraves de "year"

names(brasil_pib)[names(brasil_pib) == 'Brasil - PIB - paridade do poder de compra (PPC) - US$ - Banco Mundial- World Development Indicators (WDI) - WDI_PIBPPCBRA'] <- 'pib'

names(brasil_pib)[names(brasil_pib) == 'Data'] <- 'year'

View(brasil_pib)

# Editando os bancos em relacao ao intervalo temporal (1980 - 2011)

library(tidyverse)
library(dplyr)
library(ggplot2)

brasil <- select(NMC_5_0,
                 stateabb, 
                 milex,
                 cinc,
                 year)
View(brasil)
brasil <- brasil%>%filter(stateabb=="BRA")
View(brasil)
brasil <- brasil%>%filter(year >= 1980)
brasil

View(brasil_pib)
pib <- select(brasil_pib, -(X3))
View(pib)
pib <- pib%>%filter(year <= 2011)
View(pib)

# Unindo os bancos através da variavel "year"

completo <- full_join(brasil, pib, by = "year")

View(completo)

# Analise exploratoria dos dados

summary(completo)
head(completo)
summary(completo$pib)
summary(completo$cinc)
summary(completo$milex)

# Graficos para analise exploratoria

# Evolução do PIB ao longo do tempo

ggplot(data = completo, aes(x = year, y = pib))+
  geom_line(color = "#00AFBB", size = 2)+
  labs(x = "Anos", y = "PIB")
  

# Evolução do investimento no setor militar  

ggplot(data = completo, aes(x = year, y = milex))+
  geom_line(color = "#00AFBB", size = 2)+
  labs(x = "Anos", y = "Gastos nas forças armadas")

# Evolução das capacidades nacionais no Brasil

ggplot(data = completo, aes(x = year, y = cinc))+
  geom_line(color = "#00AFBB", size = 2)+
  labs(x = "Anos", y = "ICCN")

# Analise dos dados - rodando o modelo 

# Modelo de regressão bivariado 1 - PIB como VI e ICCN como VD

lm1 <- lm(cinc ~ log(pib), data = completo)

summary(lm1)

plot(lm1)


# Modelo 2 - PIB como VI e Milex como VD

lm2 <- lm(milex ~ log(pib), data = completo)

summary(lm2)

Plot(lm2)