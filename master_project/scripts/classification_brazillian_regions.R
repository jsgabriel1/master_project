#Carregando os pacotes
library(readxl)
library(tidyverse)
library(writexl)

#Importando as bases 
seca_risc_hidro<-read_excel("AdaptaBrasil_recursos_hidricos_indice_de_risco_de_impacto_para_seca_BR_municipio_2015.xlsx")

#Criando nova planilha com as classificações
final<-seca_risc_hidro%>%
  select(nome, classe)

final<-final%>%
  rename("NM_REGIAO"=classe)
    
classificar_regiao <- function(sigla_estado) {
  if(grepl("AC", sigla_estado) || grepl("AP", sigla_estado) || grepl("AM", sigla_estado) || 
     grepl("PA", sigla_estado) || grepl("RO", sigla_estado) || grepl("RR", sigla_estado) || grepl("TO", sigla_estado)) {
    return("NORTE")
  } else if(grepl("AL", sigla_estado) || grepl("BA", sigla_estado) || grepl("CE", sigla_estado) || 
            grepl("MA", sigla_estado) || grepl("PB", sigla_estado) || grepl("PE", sigla_estado) || 
            grepl("PI", sigla_estado) || grepl("RN", sigla_estado) || grepl("SE", sigla_estado)) {
    return("NORDESTE")
  } else if(grepl("DF", sigla_estado) || grepl("GO", sigla_estado) || grepl("MS", sigla_estado) || 
            grepl("MT", sigla_estado)) {
    return("CENTRO-OESTE")
  } else if(grepl("ES", sigla_estado) || grepl("MG", sigla_estado) || grepl("RJ", sigla_estado) || 
            grepl("SP", sigla_estado)) {
    return("SUDESTE")
  } else if(grepl("PR", sigla_estado) || grepl("RS", sigla_estado) || grepl("SC", sigla_estado)) {
    return("SUL")
  } else {
    return("Desconhecido")  # Caso não seja encontrado, retorna 'Desconhecido'
  }
}

final <- final %>%
  mutate(regiao = sapply(nome, classificar_regiao))%>%
  select(-NM_REGIAO)

# Exibindo o resultado
print(final)

#Importante todas as tabelas com dados do Adaptabrasil. seca_risc_hidro já usei anteriormente
seca_risc_hidro<-read_excel("AdaptaBrasil_recursos_hidricos_indice_de_risco_de_impacto_para_seca_BR_municipio_2015.xlsx")
seca_risc_hidro<-seca_risc_hidro%>%
  select(nome, valor)

seca_segali<-read_excel("AdaptaBrasil_seguranca_alimentar_indice_de_risco_de_impacto_para_seca_BR_municipio_2015.xlsx")
seca_segali<-seca_segali%>%
  select(nome, valor)

chuva_segali<-read_excel("AdaptaBrasil_seguranca_alimentar_indice_de_risco_de_impacto_para_a_chuva_BR_municipio_2015.xlsx")
chuva_segali<-chuva_segali%>%
  select(nome, valor)

segener_acesso<-read_excel("AdaptaBrasil_seguranca_energetica_acesso_BR_municipio_2019.xlsx")
segener_acesso<-segener_acesso%>%
  select(nome, valor)

segener_disp<-read_excel("AdaptaBrasil_seguranca_energetica_disponiblidade_BR_municipio_2019.xlsx")
segener_disp<-segener_disp%>%
  select(nome, valor)

saude_malaria<-read_excel("AdaptaBrasil_saude_malaria_BR_municipio_2020.xlsx")
saude_malaria<-saude_malaria%>%
  select(nome, valor)

saude_leishmaniose_visceral<-read_excel("AdaptaBrasil_saude_leishmaniose_visceral_BR_municipio_2020.xlsx")
saude_leishmaniose_visceral<-saude_leishmaniose_visceral%>%
  select(nome, valor)

saude_leishmaniose_tegumentar<-read_excel("AdaptaBrasil_saude_leishmaniose_tegumentar_americana_BR_municipio_2020.xlsx")
saude_leishmaniose_tegumentar<-saude_leishmaniose_tegumentar%>%
  select(nome, valor)

inudacoes_des_hidro<-read_excel("AdaptaBrasil_desastres_geo-hidrologicos_indice_de_risco_para_inundacoes_enxurradas_e_alagamentos_BR_municipio_2015.xlsx")
inudacoes_des_hidro<-inudacoes_des_hidro%>%
  select(nome, valor)

deslizamento_dez_hidro<-read_excel("AdaptaBrasil_desastres_geo-hidrologicos_indice_de_risco_para_deslizamento_de_terra_BR_municipio_2015.xlsx")
deslizamento_dez_hidro<-deslizamento_dez_hidro%>%
  select(nome, valor)

#Juntando todas as tabelas de acordo com a coluna em comum "nome"  
br_n2 <- final %>%
  full_join(seca_risc_hidro, by = "nome") %>%
  full_join(seca_segali, by = "nome") %>%
  full_join(chuva_segali, by = "nome") %>%
  full_join(segener_acesso, by = "nome") %>%
  full_join(segener_disp, by = "nome") %>%
  full_join(saude_malaria, by = "nome") %>%
  full_join(saude_leishmaniose_visceral, by = "nome") %>%
  full_join(saude_leishmaniose_tegumentar, by = "nome") %>%
  full_join(inudacoes_des_hidro, by = "nome") %>%
  full_join(deslizamento_dez_hidro, by = "nome")

# Verificando a estrutura do dataframe final
glimpse(br_n2)

br_n2 <- br_n2 %>%
  relocate(regiao, .after = valor.y.y.y.y.y) %>%
  rename(
    seca_rischidro = valor.x,
    seca_segali = valor.y,
    chuva_segali = valor.x.x,
    segener_acesso = valor.y.y,
    segener_disp = valor.x.x.x,
    saude_malaria = valor.y.y.y,
    saude_leishmaniose_visceral = valor.x.x.x.x,
    saude_leishmaniose_tegumentar = valor.y.y.y.y,
    inundacoes_des_hidro = valor.x.x.x.x.x,
    deslizamento_dez_hidro = valor.y.y.y.y.y
  )
  
write_xlsx(br_n2, "br_final.xlsx")
  
  
  
  
 


