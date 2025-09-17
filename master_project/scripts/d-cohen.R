#Instalando o pacote 
install.packages("effectsize")
library(readxl)
library(effectsize)

#Importando as planilhas

br_n2<-read_excel("br_final.xlsx")
ti<-read.csv("tabela_final_ind1.csv", dec=",")
quilo<-read.csv("tabela_final_quilo1.csv", dec=",")

glimpse(br_n2)

d1<-ti%>%
  filter(NM_REGIAO=="Nordeste")
d2<-br_n2%>%
  filter(regiao=="NORDESTE")

d3<-d1$seca_segali
d4<-d2$seca_segali

#d-COHEN geral 
resultado <- cohens_d(d3,d4)
media_ti<-mean(d3)
media_regiao<-mean(d4)

# Exibir o resultado
cat("Média TI/Quilombo:", media_ti,"|", "Média região:", media_regiao)
print(resultado)


