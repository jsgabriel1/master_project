#Importando o Tidyverse
install.packages("tidyverse")
install.packages("readxl")
library(tidyverse)
library(readxl)

#Carregreadr_example()#Carregando os dados
dados<-read.csv("teste.csv")
ti<-read_excel("tabela_final_ind.xlsx")

#Gerando o gráfico
ggplot(dados, aes(x=d_cohen, y=nivel2, color=grupo))+
  geom_point(position = position_dodge(width = 1),size=2)+
  geom_errorbar(aes(xmin=ic_inf, xmax=ic_sup), position = position_dodge(width = 1))+
  theme_classic()+
  geom_vline(xintercept = 0, linetype="dashed")+
  labs(title = "North region",
       x = "Effect size",
       y=NULL)+
  labs(color="Land type",)+
  scale_color_brewer(palette = "Dark2")
