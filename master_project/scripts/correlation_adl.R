#INSTALANDO E CARREGANDO OS PACOTES NECESSÁRIOS
install.packages("readxl")
install.packages("tidyverse")
install.packages("patchwork")

library(tidyverse)
library(readxl)
library(patchwork)

#IMPORTANDO AS PLANILHAS
quilo<-read_excel("tabela_final_quilo.xlsx")

#AJUSTANDO AS COLUNAS COM NOMES PADRONIZADOS E ABREVIAÇÕES QUE VÃO AJUDAR NO LAYOUT DA MATRIZ DE CORRELAÇÃO + MATRIZ DE CORRELAÇÃO SOMENTE PARA A  REGIÃO NORTE
glimpse(quilo)

quilo<-quilo%>%
  rename("DRO-HS"=seca_rischidro, "DRO-FS"=seca_segali, "FLOOD-FS"=chuva_segali, "CC-ES"=segener_acesso, "ELEC-VAR"=segener_disp, "CC-MAL"=saude_malaria, "CC-LV"=saude_leishmaniose_visceral, "CC-LT"= saude_leishmaniose_tegumentar, "FLOOD-RISK"=inudacoes_des_hidro, "LANDSLIDE-RISK"=deslizamento_des_hidro )

quilo_regiao<-quilo%>%
  filter(NM_REGIAO=="Norte")

quilo_regiao_num<-quilo_regiao%>%
  select(-name,-NM_REGIAO)

#DATA FRAME SOMENTE COM VALORES
df <- quilo_regiao_num  

corr_mat <- cor(df)

cor_long <- as.data.frame(as.table(corr_mat)) %>%
  rename(var1 = Var1, var2 = Var2, corr = Freq)  

north<-ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +                # Tiles coloridos
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA", midpoint = 0) + # Escala divergente
  geom_text(aes(label = round(corr, 2)), size = 3) + # Mostrar valores arredondados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # Ajustar texto do eixo x
  labs(title = "North Region Afro-descendant Lands (Brazil)", fill = "Correlation")+
  ylab(NULL)+
  xlab(NULL)
north

ggsave(filename = "cor_adl_north.jpeg", plot = north, dpi = 600, width = 8, height = 6, units = "in")


######PARA A REGIÃO NORDESTE######

#IMPORTANDO AS PLANILHAS
quilo<-read_excel("tabela_final_quilo.xlsx")

#AJUSTANDO AS COLUNAS COM NOMES PADRONIZADOS E ABREVIAÇÕES QUE VÃO AJUDAR NO LAYOUT DA MATRIZ DE CORRELAÇÃO + MATRIZ DE CORRELAÇÃO SOMENTE PARA A  REGIÃO NORDESTE
glimpse(quilo)

quilo<-quilo%>%
  rename("DRO-HS"=seca_rischidro, "DRO-FS"=seca_segali, "FLOOD-FS"=chuva_segali, "CC-ES"=segener_acesso, "ELEC-VAR"=segener_disp, "CC-MAL"=saude_malaria, "CC-LV"=saude_leishmaniose_visceral, "CC-LT"= saude_leishmaniose_tegumentar, "FLOOD-RISK"=inudacoes_des_hidro, "LANDSLIDE-RISK"=deslizamento_des_hidro )

quilo_regiao<-quilo%>%
  filter(NM_REGIAO=="Nordeste")

quilo_regiao_num<-quilo_regiao%>%
  select(-name,-NM_REGIAO)

#DATA FRAME SOMENTE COM VALORES
df <- quilo_regiao_num  

cor_mat <- cor(df)

cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, corr = Freq)

northeast<-ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +                # Tiles coloridos
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA", midpoint = 0) + # Escala divergente
  geom_text(aes(label = round(corr, 2)), size = 3) + # Mostrar valores arredondados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # Ajustar texto do eixo x
  labs(title = "Northeast Region Afro-descendant Lands (Brazil)", fill = "Correlation")+
  ylab(NULL)+
  xlab(NULL)
northeast

ggsave(filename = "cor_adl_northeast.jpeg", plot = northeast, dpi = 600, width = 8, height = 6, units = "in")


######PARA A REGIÃO SUDESTE######

#IMPORTANDO AS PLANILHAS
quilo<-read_excel("tabela_final_quilo.xlsx")

#AJUSTANDO AS COLUNAS COM NOMES PADRONIZADOS E ABREVIAÇÕES QUE VÃO AJUDAR NO LAYOUT DA MATRIZ DE CORRELAÇÃO + MATRIZ DE CORRELAÇÃO SOMENTE PARAA  REGIÃO SUDESTE
glimpse(quilo)

quilo<-quilo%>%
  rename("DRO-HS"=seca_rischidro, "DRO-FS"=seca_segali, "FLOOD-FS"=chuva_segali, "CC-ES"=segener_acesso, "ELEC-VAR"=segener_disp, "CC-MAL"=saude_malaria, "CC-LV"=saude_leishmaniose_visceral, "CC-LT"= saude_leishmaniose_tegumentar, "FLOOD-RISK"=inudacoes_des_hidro, "LANDSLIDE-RISK"=deslizamento_des_hidro )

quilo_regiao<-quilo%>%
  filter(NM_REGIAO=="Sudeste")

quilo_regiao_num<-quilo_regiao%>%
  select(-name,-NM_REGIAO)

#DATA FRAME SOMENTE COM VALORES
df <- quilo_regiao_num  

cor_mat <- cor(df)

cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, corr = Freq)

southeast<-ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +                # Tiles coloridos
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA", midpoint = 0) + # Escala divergente
  geom_text(aes(label = round(corr, 2)), size = 3) + # Mostrar valores arredondados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # Ajustar texto do eixo x
  labs(title = "Southeast Region Afro-escendant Lands (Brazil)", fill = "Correlation")+
  ylab(NULL)+
  xlab(NULL)
southeast

ggsave(filename = "cor_adl_southeast.jpeg", plot = southeast, dpi = 600, width = 8, height = 6, units = "in")


######PARA A REGIÃO CENTRO-OESTE######

#IMPORTANDO AS PLANILHAS
quilo<-read_excel("tabela_final_quilo.xlsx")

#AJUSTANDO AS COLUNAS COM NOMES PADRONIZADOS E ABREVIAÇÕES QUE VÃO AJUDAR NO LAYOUT DA MATRIZ DE CORRELAÇÃO + MATRIZ DE CORRELAÇÃO SOMENTE PARA A REGIÃO CENTRO-OESTE

glimpse(quilo)

quilo<-quilo%>%
  rename("DRO-HS"=seca_rischidro, "DRO-FS"=seca_segali, "FLOOD-FS"=chuva_segali, "CC-ES"=segener_acesso, "ELEC-VAR"=segener_disp, "CC-MAL"=saude_malaria, "CC-LV"=saude_leishmaniose_visceral, "CC-LT"= saude_leishmaniose_tegumentar, "FLOOD-RISK"=inudacoes_des_hidro, "LANDSLIDE-RISK"=deslizamento_des_hidro )

quilo_regiao<-quilo%>%
  filter(NM_REGIAO=="Centro-oeste")

quilo_regiao_num<-quilo_regiao%>%
  select(-name,-NM_REGIAO)

#DATA FRAME SOMENTE COM VALORES
df <- quilo_regiao_num  

cor_mat <- cor(df)

cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, corr = Freq)

cw<-ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +                # Tiles coloridos
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA", midpoint = 0) + # Escala divergente
  geom_text(aes(label = round(corr, 2)), size = 3) + # Mostrar valores arredondados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # Ajustar texto do eixo x
  labs(title = "Central-West Afro-descendant Lands (Brazil)", fill = "Correlation")+
  ylab(NULL)+
  xlab(NULL)
cw

ggsave(filename = "cor_adl_cw.jpeg", plot = cw, dpi = 600, width = 8, height = 6, units = "in")


######PARA A REGIÃO SUL######

#IMPORTANDO AS PLANILHAS
quilo<-read_excel("tabela_final_quilo.xlsx")

#AJUSTANDO AS COLUNAS COM NOMES PADRONIZADOS E ABREVIAÇÕES QUE VÃO AJUDAR NO LAYOUT DA MATRIZ DE CORRELAÇÃO + MATRIZ DE CORRELAÇÃO SOMENTE PARa A REGIÃO SUL

glimpse(quilo)

quilo<-quilo%>%
  rename("DRO-HS"=seca_rischidro, "DRO-FS"=seca_segali, "FLOOD-FS"=chuva_segali, "CC-ES"=segener_acesso, "ELEC-VAR"=segener_disp, "CC-MAL"=saude_malaria, "CC-LV"=saude_leishmaniose_visceral, "CC-LT"= saude_leishmaniose_tegumentar, "FLOOD-RISK"=inudacoes_des_hidro, "LANDSLIDE-RISK"=deslizamento_des_hidro )

quilo_regiao<-quilo%>%
  filter(NM_REGIAO=="Sul")

quilo_regiao_num<-quilo_regiao%>%
  select(-name,-NM_REGIAO)

#DATA FRAME SOMENTE COM VALORES
df <- quilo_regiao_num  

cor_mat <- cor(df)

cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, corr = Freq)

south<-ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +                # Tiles coloridos
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA", midpoint = 0) + # Escala divergente
  geom_text(aes(label = round(corr, 2)), size = 3) + # Mostrar valores arredondados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # Ajustar texto do eixo x
  labs(title = "South Region Afro-descendant Lands (Brazil)", fill = "Correlation")+
  ylab(NULL)+
  xlab(NULL)
south

ggsave(filename = "cor_adl_south.jpeg", plot =south, dpi = 600, width = 8, height = 6, units = "in")


######PARA O BRASIL GERAL######
quilo<-read_excel("tabela_final_quilo.xlsx")

#AJUSTANDO AS COLUNAS COM NOMES PADRONIZADOS E ABREVIAÇÕES QUE VÃO AJUDAR NO LAYOUT DA MATRIZ DE CORRELAÇÃO + MATRIZ DE CORRELAÇÃO PARA TODO O BRASIL

glimpse(quilo)

quilo<-quilo%>%
  rename("DRO-HS"=seca_rischidro, "DRO-FS"=seca_segali, "FLOOD-FS"=chuva_segali, "CC-ES"=segener_acesso, "ELEC-VAR"=segener_disp, "CC-MAL"=saude_malaria, "CC-LV"=saude_leishmaniose_visceral, "CC-LT"= saude_leishmaniose_tegumentar, "FLOOD-RISK"=inudacoes_des_hidro, "LANDSLIDE-RISK"=deslizamento_des_hidro )

quilo_regiao_num<-quilo%>%
  select(-name,-NM_REGIAO)

#DATA FRAME SOMENTE COM VALORES
df <- quilo_regiao_num  

cor_mat <- cor(df)

cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, corr = Freq)

br<-ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +                # Tiles coloridos
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA", midpoint = 0) + # Escala divergente
  geom_text(aes(label = round(corr, 2)), size = 3) + # Mostrar valores arredondados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # Ajustar texto do eixo x
  labs(title = "Afro-descendat Lands (Brazi)", fill = "Correlation")+
  ylab(NULL)+
  xlab(NULL)
br

ggsave(filename = "cor_adl_br.jpeg", plot =br, dpi = 600, width = 8, height = 6, units = "in")
