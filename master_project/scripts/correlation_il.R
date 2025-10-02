#INSTALANDO E CARREGANDO OS PACOTES NECESSÁRIOS
install.packages("readxl")
install.packages("tidyverse")
install.packages("patchwork")

library(tidyverse)
library(readxl)
library(patchwork)

#IMPORTANDO AS PLANILHAS
ti<-read_excel("tabela_final_ind.xlsx")

#AJUSTANDO AS COLUNAS COM NOMES PADRONIZADOS E ABREVIAÇÕES QUE VÃO AJUDAR NO LAYOUT DA MATRIZ DE CORRELAÇÃO + MATRIZ DE CORRELAÇÃO SOMENTE PARAA  REGIÃO NORTE
glimpse(ti)

ti<-ti%>%
  rename("DRO-HS"=seca_rischidro, "DRO-FS"=seca_segali, "FLOOD-FS"=chuva_segali, "CC-ES"=segener_acesso, "ELEC-VAR"=segener_disp, "CC-MAL"=saude_malaria, "CC-LV"=saude_leishmaniose_visceral, "CC-LT"= saude_leishmaniose_tegumentar, "FLOOD-RISK"=inudacoes_des_hidro, "LANDSLIDE-RISK"=deslizamento_des_hidro )

ti_regiao<-ti%>%
  filter(NM_REGIAO=="Norte")

ti_regiao_num<-ti_regiao%>%
  select(-name,-NM_REGIAO)

#DATA FRAME SOMENTE COM VALORES
df <- ti_regiao_num  

corr_mat <- cor(df)

cor_long <- as.data.frame(as.table(corr_mat)) %>%
  rename(var1 = Var1, var2 = Var2, corr = Freq)  

north<-ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +                # Tiles coloridos
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA", midpoint = 0) + # Escala divergente
  geom_text(aes(label = round(corr, 2)), size = 3) + # Mostrar valores arredondados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # Ajustar texto do eixo x
  labs(title = "North Region Indigenous Lands (Brazil)", fill = "Correlation")+
  ylab(NULL)+
  xlab(NULL)
north

ggsave(filename = "cor_il_north.jpeg", plot = north, dpi = 600, width = 8, height = 6, units = "in")


######PARA A REGIÃO NORDESTE######

ti<-read_excel("tabela_final_ind.xlsx")

#AJUSTANDO OS DADOS 
glimpse(ti)

#AJUSTANDO AS COLUNAS COM NOMES PADRONIZADOS E ABREVIAÇÕES QUE VÃO AJUDAR NO LAYOUT DA MATRIZ DE CORRELAÇÃO + MATRIZ DE CORRELAÇÃO SOMENTE PARAA  REGIÃO NORDESTE

ti<-ti%>%
  rename("DRO-HS"=seca_rischidro, "DRO-FS"=seca_segali, "FLOOD-FS"=chuva_segali, "CC-ES"=segener_acesso, "ELEC-VAR"=segener_disp, "CC-MAL"=saude_malaria, "CC-LV"=saude_leishmaniose_visceral, "CC-LT"= saude_leishmaniose_tegumentar, "FLOOD-RISK"=inudacoes_des_hidro, "LANDSLIDE-RISK"=deslizamento_des_hidro )

ti_regiao<-ti%>%
  filter(NM_REGIAO=="Nordeste")

ti_regiao_num<-ti_regiao%>%
  select(-name,-NM_REGIAO)

#DATA FRAME SOMENTE COM VALORES
df <- ti_regiao_num 

cor_mat <- cor(df)

cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, corr = Freq)

northeast<-ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +                # Tiles coloridos
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA", midpoint = 0) + # Escala divergente
  geom_text(aes(label = round(corr, 2)), size = 3) + # Mostrar valores arredondados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # Ajustar texto do eixo x
  labs(title = "Northeast Region Indigenous Lands (Brazil)", fill = "Correlation")+
  ylab(NULL)+
  xlab(NULL)
northeast

ggsave(filename = "cor_il_northeast.jpeg", plot = northeast, dpi = 600, width = 8, height = 6, units = "in")


######PARA A REGIÃO SUDESTE######

ti<-read_excel("tabela_final_ind.xlsx")

#AJUSTANDO AS COLUNAS COM NOMES PADRONIZADOS E ABREVIAÇÕES QUE VÃO AJUDAR NO LAYOUT DA MATRIZ DE CORRELAÇÃO + MATRIZ DE CORRELAÇÃO SOMENTE PARAA  REGIÃO SUDESTE
glimpse(ti)

ti<-ti%>%
  rename("DRO-HS"=seca_rischidro, "DRO-FS"=seca_segali, "FLOOD-FS"=chuva_segali, "CC-ES"=segener_acesso, "ELEC-VAR"=segener_disp, "CC-MAL"=saude_malaria, "CC-LV"=saude_leishmaniose_visceral, "CC-LT"= saude_leishmaniose_tegumentar, "FLOOD-RISK"=inudacoes_des_hidro, "LANDSLIDE-RISK"=deslizamento_des_hidro )

ti_regiao<-ti%>%
  filter(NM_REGIAO=="Sudeste")

ti_regiao_num<-ti_regiao%>%
  select(-name,-NM_REGIAO,-`ELEC-VAR`)

#DATA FRAME SOMENTE COM VALORES
df <- ti_regiao_num 

cor_mat <- cor(df)

cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, corr = Freq)

southeast<-ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +                # Tiles coloridos
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA", midpoint = 0) + # Escala divergente
  geom_text(aes(label = round(corr, 2)), size = 3) + # Mostrar valores arredondados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # Ajustar texto do eixo x
  labs(title = "Southeast Region Indigenous Lands (Brazil)", fill = "Correlation")+
  ylab(NULL)+
  xlab(NULL)
southeast

ggsave(filename = "cor_il_southeast.jpeg", plot = southeast, dpi = 600, width = 8, height = 6, units = "in")


######PARA A REGIÃO CENTRO-OESTE######

ti<-read_excel("tabela_final_ind.xlsx")

#AJUSTANDO AS COLUNAS COM NOMES PADRONIZADOS E ABREVIAÇÕES QUE VÃO AJUDAR NO LAYOUT DA MATRIZ DE CORRELAÇÃO + MATRIZ DE CORRELAÇÃO SOMENTE PARAA  REGIÃO CENTRO-OESTE
glimpse(ti)

ti<-ti%>%
  rename("DRO-HS"=seca_rischidro, "DRO-FS"=seca_segali, "FLOOD-FS"=chuva_segali, "CC-ES"=segener_acesso, "ELEC-VAR"=segener_disp, "CC-MAL"=saude_malaria, "CC-LV"=saude_leishmaniose_visceral, "CC-LT"= saude_leishmaniose_tegumentar, "FLOOD-RISK"=inudacoes_des_hidro, "LANDSLIDE-RISK"=deslizamento_des_hidro )

ti_regiao<-ti%>%
  filter(NM_REGIAO=="Centro-oeste")

ti_regiao_num<-ti_regiao%>%
  select(-name,-NM_REGIAO,-`ELEC-VAR`)

#DATA FRAME SOMENTE COM VALORES
df <- ti_regiao_num  

cor_mat <- cor(df)

cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, corr = Freq)

cw<-ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +                # Tiles coloridos
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA", midpoint = 0) + # Escala divergente
  geom_text(aes(label = round(corr, 2)), size = 3) + # Mostrar valores arredondados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # Ajustar texto do eixo x
  labs(title = "Central-West Region Indigenous Lands (Brazil)", fill = "Correlation")+
  ylab(NULL)+
  xlab(NULL)
cw

ggsave(filename = "cor_il_cw.jpeg", plot = cw, dpi = 600, width = 8, height = 6, units = "in")


######PARA A REGIÃO SUL######

ti<-read_excel("tabela_final_ind.xlsx")

#AJUSTANDO AS COLUNAS COM NOMES PADRONIZADOS E ABREVIAÇÕES QUE VÃO AJUDAR NO LAYOUT DA MATRIZ DE CORRELAÇÃO + MATRIZ DE CORRELAÇÃO SOMENTE PARAA  REGIÃO SUL
glimpse(ti)

ti<-ti%>%
  rename("DRO-HS"=seca_rischidro, "DRO-FS"=seca_segali, "FLOOD-FS"=chuva_segali, "CC-ES"=segener_acesso, "ELEC-VAR"=segener_disp, "CC-MAL"=saude_malaria, "CC-LV"=saude_leishmaniose_visceral, "CC-LT"= saude_leishmaniose_tegumentar, "FLOOD-RISK"=inudacoes_des_hidro, "LANDSLIDE-RISK"=deslizamento_des_hidro )

ti_regiao<-ti%>%
  filter(NM_REGIAO=="Sul")

ti_regiao_num<-ti_regiao%>%
  select(-name,-NM_REGIAO,-`ELEC-VAR`)

#DATA FRAME SOMENTE COM VALORES
df <- ti_regiao_num  

cor_mat <- cor(df)

cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, corr = Freq)

south<-ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +                # Tiles coloridos
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA", midpoint = 0) + # Escala divergente
  geom_text(aes(label = round(corr, 2)), size = 3) + # Mostrar valores arredondados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # Ajustar texto do eixo x
  labs(title = "South Region Indigenous Lands (Brazil)", fill = "Correlation")+
  ylab(NULL)+
  xlab(NULL)
south

ggsave(filename = "cor_il_south.jpeg", plot =south, dpi = 600, width = 8, height = 6, units = "in")


######PARA O BRASIL GERAL######

ti<-read_excel("tabela_final_ind.xlsx")

#AJUSTANDO AS COLUNAS COM NOMES PADRONIZADOS E ABREVIAÇÕES QUE VÃO AJUDAR NO LAYOUT DA MATRIZ DE CORRELAÇÃO + MATRIZ DE CORRELAÇÃO PARA TODO O BRASIL

glimpse(ti)

ti<-ti%>%
  rename("DRO-HS"=seca_rischidro, "DRO-FS"=seca_segali, "FLOOD-FS"=chuva_segali, "CC-ES"=segener_acesso, "ELEC-VAR"=segener_disp, "CC-MAL"=saude_malaria, "CC-LV"=saude_leishmaniose_visceral, "CC-LT"= saude_leishmaniose_tegumentar, "FLOOD-RISK"=inudacoes_des_hidro, "LANDSLIDE-RISK"=deslizamento_des_hidro )

ti_regiao_num<-ti%>%
  select(-name,-NM_REGIAO)

#DATA FRAME SOMENTE COM VALORES
df <- ti_regiao_num  

cor_mat <- cor(df)

cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, corr = Freq)

br<-ggplot(cor_long, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +                # Tiles coloridos
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA", midpoint = 0) + # Escala divergente
  geom_text(aes(label = round(corr, 2)), size = 3) + # Mostrar valores arredondados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + # Ajustar texto do eixo x
  labs(title = "Indigenous Lands (Brazi)", fill = "Correlation")+
  ylab(NULL)+
  xlab(NULL)
br

ggsave(filename = "cor_il_br.jpeg", plot =br, dpi = 600, width = 8, height = 6, units = "in")

