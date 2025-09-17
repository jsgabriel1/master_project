# Carrega o pacote necessário
install.packages("dplyr")
library(dplyr)

# Lê os dados do CSV (separador ponto e vírgula, usado geralmente em arquivos do Excel no Brasil)
dados_quilo <- read.csv2("bigtab_quilo.csv", stringsAsFactors = FALSE)

# Converte a coluna 'area_km2' para numérica
dados_quilo$area_km2 <- as.numeric(dados_quilo$area_km2)

# Calcula as médias ponderadas para as variáveis
tabela_final <- dados_quilo %>%
  group_by(name) %>%
  summarise(
    seca_rischidro = weighted.mean(seca_rischidro, area_km2, na.rm = TRUE),
    seca_segali = weighted.mean(seca_segali, area_km2, na.rm = TRUE),
    chuva_segali = weighted.mean(chuva_segali, area_km2, na.rm = TRUE),
    segener_acesso = weighted.mean(segener_acesso, area_km2, na.rm = TRUE),
    segener_disp = weighted.mean(segener_disp, area_km2, na.rm = TRUE),
    saude_malaria = weighted.mean(saude_malaria, area_km2, na.rm = TRUE),
    saude_leishmaniose_visceral = weighted.mean(saude_leishmaniose_visceral, area_km2, na.rm = TRUE),
    saude_leishmaniose_tegumentar = weighted.mean(saude_leishmaniose_tegumentar, area_km2, na.rm = TRUE),
    inudacoes_des_hidro = weighted.mean(inundacoes_des_hidro, area_km2, na.rm = TRUE),
    deslizamento_des_hidro = weighted.mean(deslizamento_des_hidro, area_km2, na.rm = TRUE)
  )

# Exibe o resultado final
print(tabela_final)

# Exportar para CSV
#write.csv(tabela_final, "tabela_final.csv", row.names = FALSE)

# Carregar o pacote openxlsx para exportar para XLSX
#install.packages("openxlsx")
#library(openxlsx)

# Exportar para XLSX
#write.xlsx(tabela_final, "tabela_final_quilo.xlsx")