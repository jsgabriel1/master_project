#Instalando os pacotes 
install.packages("tidyverse")
install.packages("effectsize")
library(readxl)
library(effectsize)
library(tidyverse)

#Importando as planilhas
br_n2<-read_excel("br_final.xlsx")
ti<-read_excel("tabela_final_ind.xlsx")
quilo<--read_excel("tabela_final_quilo.xlsx")

glimpse(br_n2)

d1<-ti%>%
  filter(NM_REGIAO=="Norte")
d2<-br_n2%>%
  filter(regiao=="NORTE")

d3<-d1$saude_leishmaniose_tegumentar
d4<-d2$saude_leishmaniose_tegumentar

#d-COHEN geral 
resultado <- cohens_d(d3,d4)
media_ti<-mean(d3, na.rm=TRUE)
media_regiao<-mean(d4, na.rm=TRUE)

# Exibir resultados com mais casas decimais
cat("Média TI/Quilombo:", sprintf("%.10f", media_ti), "\n")
cat("Média Região:", sprintf("%.10f", media_regiao), "\n\n")

# Mostrar resultado com mais casas decimais no IC
print(format(resultado, digits = 15))


