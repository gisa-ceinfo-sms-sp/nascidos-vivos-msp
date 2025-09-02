# ==============================================================================
# SCRIPT DE PROCESSAMENTO E VISUALIZAÇÃO DE DADOS DE NASCIDOS VIVOS
#
# Instituição: Secretaria Municipal da Saúde de São Paulo
# Departamento: Núcleo de Geoprocessamento e Informação Socioambiental (GISA) / Coordenação de Epidemiologia e Informação (CEInfo)
# Coordenador: Marcelo Antunes Failla
# Repositório: https://github.com/gisa-ceinfo-sms-sp
# Parceiro de Programação: Gemini AI
# Data da versão: 02 de setembro de 2025
#
# Objetivo:
# Reestruturar um sistema de mapeamento e análise de dados de nascidos vivos,
# preservando todas as saídas (tabelas e mapas) do código original, mas com
# uma estrutura mais modular, legível e manutenível.
#
# ==============================================================================

#### 1. Configuração Inicial: Instalação e Carregamento de Bibliotecas, Definição de Caminhos ####

# --- 1.1. Instalação de Pacotes (Descomente e execute apenas se necessário) ---
# install.packages("ggrepel")
# install.packages("writexl")
# install.packages("ggplot2")
# install.packages("sf")
# install.packages("RColorBrewer")
# install.packages("grid")
# install.packages("stringr")
# install.packages("ggmap")
# install.packages("leaflet")
# install.packages("leaflet.extras")
# install.packages("plotly")
# install.packages("spatstat")
# install.packages("shiny")
# # install.packages("geocodehere") # Removido por não ser utilizado e gerar erro
# install.packages("htmlwidgets")
# install.packages("dplyr")
# install.packages("classInt")
# install.packages("readxl")
# install.packages("ggspatial")
# install.packages("cowplot")
# install.packages("magick")
# install.packages("tmap")
# install.packages("ggimage")
# install.packages("spData")
# # install.packages("maptools") # Removido por não ser utilizado e gerar erro
# install.packages("tidyverse")
# install.packages("gridExtra")




# --- 1.2. Carregamento das Bibliotecas ---
# A ordem pode ser importante para evitar conflitos de funções (ex: dplyr e sf)
library(tidyverse)    # Para manipulação de dados (dplyr, tidyr, etc.)
library(sf)           # Para trabalhar com dados espaciais (shapefiles)
library(readxl)       # Para ler arquivos .xlsx
library(writexl)      # Para escrever arquivos .xlsx
library(ggplot2)      # Para criação de mapas estáticos (base para ggspatial, ggrepel)
library(ggspatial)    # Para adicionar elementos espaciais a mapas ggplot (seta norte, escala)
library(ggrepel)      # Para evitar sobreposição de rótulos em mapas
library(RColorBrewer) # Para paletas de cores
library(classInt)     # Para calcular intervalos de classes para mapas coropléticos
library(stringr)      # Para manipulação de strings (ex: wrap de rótulos)
library(leaflet)      # Para criação de mapas dinâmicos
library(leaflet.extras) # Para funcionalidades extras do Leaflet (ex: heatmaps)
library(htmlwidgets)  # Para salvar mapas Leaflet como HTML
library(htmltools)    # Para manipular HTML no Leaflet
library(cowplot)      # Para combinar plots (se necessário, ou ajustar para gridExtra)
library(gridExtra)    # Para arranjar múltiplos gráficos em uma grade
# As bibliotecas abaixo parecem não ter sido usadas diretamente no código fornecido ou
# suas funcionalidades são cobertas por outras já carregadas. Mantendo por precaução,
# mas podemos remover na refatoração final se não houver uso explícito.
library(grid)
library(ggmap)
library(plotly)
library(spatstat)
library(shiny)
# library(geocodehere) # Removido
library(png)
library(magick)
library(tmap)
library(ggimage)
library(spData)
# library(maptools) # Removido


# --- 1.3. Definição dos Caminhos dos Arquivos e Pastas de Saída ---

# Caminhos dos arquivos de entrada
PATH_SHAPE_AAUBS <- "//seulocal//dados//AAUBS_MSP_2024_UTM_SIRGAS2000_23S.shp"
PATH_SHAPE_STSUBS <- "//seulocal//dados//STS_2020_WGS84.shp"
PATH_SHAPE_CRSSUBS <- "//seulocal//dados//CRS_2020_WGS84.shp"
PATH_SHAPE_IPVS <- "//seulocal//dados//MalhaUniverso_MSP_IPVS2010.shp" #DESCOMPACTE o arquivo ZIPado antes !
PATH_DADOS_NV <- "//seulocal//dados//modelo_dados_FALSOS_p_script_SINASC_RNRISCO_revGISA_AAUBS_NAO_INTERPRETAR.xlsx"

# Caminho base para salvar os outputs (tabelas e mapas estáticos)
BASE_OUTPUT_PATH <- "//seulocal//saidas//mensal"

# Caminho base para salvar os outputs DE TESTES (tabelas e mapas estáticos)
#BASE_OUTPUT_PATH <- "//seulocal//SCRIPTS_MODELO_R/nascidos_vivos_risco/"

# Caminhos de imagens para o mapa dinâmico
URL_BANNER_MAPA <- "https://raw.githubusercontent.com/gisa-ceinfo-sms-sp/dados/refs/heads/main/banner_NV_webmap_jan2025.png"
URL_FAVICON_MAPA <- "https://raw.githubusercontent.com/gisa-ceinfo-sms-sp/dados/refs/heads/main/Brasao_PMSP_reduzido_favicon.png"

# --- Fim da Configuração Inicial ---




#### 2. Leitura e Pré-processamento dos Dados ####

# Função para ler e realizar o pré-processamento inicial dos dados
ler_e_preparar_dados <- function(path_shape_aubs, path_shape_sts, path_shape_crs, path_shape_ipvs, path_dados_nv) {
  
  message("Lendo shapefiles...")
  # Ler o shapefile das Áreas de Abrangência de UBS (AAUBS)
  dados_shape_AAUBS <- st_read(path_shape_aubs)
  
  # Ler os shapefiles de STS, CRS e IPVS e transformar para WGS84 (EPSG: 4326)
  shapeSTSUBSwgs84 <- st_transform(st_read(path_shape_sts), crs = 4326)
  shapeCRSSUBSwgs84 <- st_transform(st_read(path_shape_crs), crs = 4326)
  shapeIPVS_raw <- st_transform(st_read(path_shape_ipvs), crs = 4326)
  
  message("Lendo tabela de Nascidos Vivos (NV)...")
  # Ler a tabela de nascidos vivos
  dados_NV <- read_excel(path_dados_nv)
  
  message("Aplicando filtros iniciais aos dados NV...")
  # Filtrar registros não vazios para CNES
  dados_NV_AAUBS <- dados_NV %>%
    filter(!is.na(CNES), CNES != "<Null>")
  
  # Filtrar RN de risco
  dados_NV_risco_AAUBS <- dados_NV_AAUBS %>%
    filter(!is.na(RN_RISCO), RN_RISCO == "S")
  
  # Filtrar Anomalias Congênitas
  dados_NV_AC_AAUBS <- dados_NV_AAUBS %>%
    filter(!is.na(IDANOMAL), IDANOMAL == "1")
  
  
  message("Padronizando nomes de colunas e preparando dados de IPVS...")
  # Padronizar nomes de colunas no shapefile AAUBS
  colnames(dados_shape_AAUBS)[colnames(dados_shape_AAUBS) == "STS_UBS"] <- "STSUBS"
  colnames(dados_shape_AAUBS)[colnames(dados_shape_AAUBS) == "CRS_UBS"] <- "CRSUBS"
  
  # Criar a tabela de correspondência IPVS
  tabela_correspondencia_ipvs <- data.frame(
    ipvs = c(1, 2, 3, 4, 5, 6, 7, 9),
    descr_ipvs = c("Baixíssima", "Muito Baixa", "Baixa", "Média", "Alta (urbanos)", "Muito Alta", "Alta (rurais)", "NC")
  )
  
  # Associar descrições IPVS ao shapefile IPVS
  shapeIPVS_raw$descr_ipvs <- tabela_correspondencia_ipvs$descr_ipvs[match(shapeIPVS_raw$v10, tabela_correspondencia_ipvs$ipvs)]
  
  # Filtrar o shapefile IPVS para excluir valores "9", "0" e nulos
  shapeIPVS_filtrado <- shapeIPVS_raw %>%
    filter(!(NUMIPVS %in% c("9", "0", NA)))
  
  # Retornar todos os dataframes processados
  return(list(
    dados_shape_AAUBS = dados_shape_AAUBS,
    shapeSTSUBSwgs84 = shapeSTSUBSwgs84,
    shapeCRSSUBSwgs84 = shapeCRSSUBSwgs84,
    shapeIPVS_filtrado = shapeIPVS_filtrado,
    tabela_correspondencia_ipvs = tabela_correspondencia_ipvs,
    dados_NV = dados_NV, # Manter o original também, se necessário para outras operações
    dados_NV_AAUBS = dados_NV_AAUBS,
    dados_NV_risco_AAUBS = dados_NV_risco_AAUBS,
    dados_NV_AC_AAUBS = dados_NV_AC_AAUBS
  ))
}

# --- Execução da função de leitura e pré-processamento ---
# Chama a função e armazena os resultados em uma lista 'dados_brutos_processados'
dados_brutos_processados <- ler_e_preparar_dados(
  PATH_SHAPE_AAUBS,
  PATH_SHAPE_STSUBS,
  PATH_SHAPE_CRSSUBS,
  PATH_SHAPE_IPVS,
  PATH_DADOS_NV
)















#### 3. Agregação e Cálculo de Métricas (REVISADO PARA NOMES DE ARQUIVOS DESCRITIVOS) ####

# Função para calcular frequências e percentuais a partir dos dados NV
calcular_frequencias_e_percentuais <- function(dados_NV, dados_NV_AAUBS, dados_NV_risco_AAUBS, dados_NV_AC_AAUBS, tabela_correspondencia_ipvs) {
  
  message("Calculando frequências de NV, RN de Risco e Anomalias Congênitas...")
  
  # FREQ NV - Contar número de ocorrências agrupadas por CNES
  freqNV <- dados_NV_AAUBS %>%
    group_by(CNES, NOMEUBS, STS_UBS, CRS_UBS) %>%
    summarise(NUMERODN = n(), .groups = 'drop')
  
  # FREQ RN RISCO - Contar o número de ocorrências agrupadas por CNES
  freqRNRISCO_raw <- dados_NV_risco_AAUBS %>%
    group_by(CNES, NOMEUBS, STS_UBS, CRS_UBS) %>%
    summarise(RN_RISCO = n(), .groups = 'drop')
  
  # FREQ RN ANOMALIAS CONG?NITAS - Contar o número de ocorrências agrupadas por CNES
  freqNVAC_raw <- dados_NV_AC_AAUBS %>%
    group_by(CNES, NOMEUBS, STS_UBS, CRS_UBS) %>%
    summarise(IDANOMAL = n(), .groups = 'drop')
  
  
  message("Unindo frequências e calculando percentuais...")
  
  # Unindo freqNV_CNES com freqRNRISCO
  freqRNRISCO_combinado <- freqNV %>%
    left_join(freqRNRISCO_raw, by = c("CNES", "NOMEUBS", "STS_UBS", "CRS_UBS")) %>%
    mutate(RN_RISCO = ifelse(is.na(RN_RISCO), 0, RN_RISCO)) %>%
    rename(NUMERODN_NV = NUMERODN)
  
  # Unindo freqNV_CNES com freqNVAC
  freqNVAC_combinado <- freqNV %>%
    left_join(freqNVAC_raw, by = c("CNES", "NOMEUBS", "STS_UBS", "CRS_UBS")) %>%
    mutate(IDANOMAL = ifelse(is.na(IDANOMAL), 0, IDANOMAL)) %>%
    rename(NUMERODN_NV = NUMERODN)
  
  
  # Calcular a taxa de RN de Risco e converter para num?rico
  freqRNRISCO_final <- freqRNRISCO_combinado %>%
    mutate(percRNRISCO = as.numeric((RN_RISCO / NUMERODN_NV) * 100)) %>%
    mutate(percRNRISCO = round(percRNRISCO, 2)) %>%
    mutate(RNRisco = RN_RISCO, PercRNRisco = percRNRISCO)
  
  
  # Calcular a taxa de Anomalias Cong?nitas e converter para num?rico
  freqNVAC_final <- freqNVAC_combinado %>%
    mutate(percNVAC = as.numeric((IDANOMAL / NUMERODN_NV) * 100)) %>%
    mutate(percNVAC = round(percNVAC, 2)) %>%
    mutate(freqNVAC = NUMERODN_NV)
  
  
  message("Calculando frequências de NV e RN de Risco por IPVS...")
  # FREQ NV por IPVS
  freqNV_IPVS <- dados_NV_AAUBS %>%
    group_by(CNES, NOMEUBS, STS_UBS, CRS_UBS, IPVS) %>%
    summarise(NUMERODN = n(), .groups = 'drop') %>%
    mutate(descr_ipvs = tabela_correspondencia_ipvs$descr_ipvs[match(IPVS, tabela_correspondencia_ipvs$ipvs)])
  
  
  # FREQ RN RISCO por IPVS
  freqRNRISCO_IPVS <- dados_NV_risco_AAUBS %>%
    group_by(CNES, NOMEUBS, STS_UBS, CRS_UBS, IPVS) %>%
    summarise(RN_RISCO = n(), .groups = 'drop') %>%
    mutate(descr_ipvs = tabela_correspondencia_ipvs$descr_ipvs[match(IPVS, tabela_correspondencia_ipvs$ipvs)])
  
  
  message("Criando campos dinâmicos de data e nomes de arquivos...")
  data_atual <- format(Sys.Date(), "%d/%m/%Y")
  
  MESANO_NASC_STR <- as.character(dados_NV$DTNASC[1])
  MESANO_NASC <- as.Date(MESANO_NASC_STR, format = "%d%m%Y")
  
  if(is.na(MESANO_NASC)) {
    primeira_data_valida_str <- as.character(dados_NV$DTNASC[!is.na(dados_NV$DTNASC)][1])
    MESANO_NASC <- as.Date(primeira_data_valida_str, format = "%d%m%Y") 
    if (is.na(MESANO_NASC)) {
      warning("Não foi possível determinar a data de competência a partir de DTNASC. Usando a data atual.")
      MESANO_NASC <- Sys.Date()
    }
  }
  
  COMP_MES <- format(MESANO_NASC, "%m")
  COMP_ANO <- format(MESANO_NASC, "%Y")
  COMP_MESANO <- paste0(COMP_MES, "-", COMP_ANO)
  
  # --- Adicionar definições de nomes de arquivo XLSX para tabelas descritivas aqui ---
  nomeexcel_tabdescr_MSP <- paste0("Descritiva_NV_RNRISCO_SEG_MSP_",COMP_MESANO,".xlsx")
  nomeexcel_tabdescr_CRS <- paste0("Descritiva_NV_RNRISCO_SEG_CRS_",COMP_MESANO,".xlsx")
  nomeexcel_tabdescr_CRS_STS <- paste0("Descritiva_NV_RNRISCO_SEG_CRS_e_STS_",COMP_MESANO,".xlsx")
  
  nomeexcel_tabdescrAC_MSP<- paste0("Descritiva_NV_AC_SEG_MSP_",COMP_MESANO,".xlsx")
  nomeexcel_tabdescrAC_CRS<- paste0("Descritiva_NV_AC_SEG_CRS_",COMP_MESANO,".xlsx")
  nomeexcel_tabdescrAC_CRS_STS<- paste0("Descritiva_NV_AC_SEG_CRS_e_STS_",COMP_MESANO,".xlsx")
  # --- Fim da adição de nomes de arquivo ---
  
  
  # Transformar CNES para caractere com 6 dígitos, preenchendo com zeros à esquerda
  freqRNRISCO_final$CNES <- sprintf("%06s", freqRNRISCO_final$CNES)
  freqNVAC_final$CNES <- sprintf("%06s", freqNVAC_final$CNES)
  freqNV_IPVS$CNES <- sprintf("%06s", freqNV_IPVS$CNES)
  freqRNRISCO_IPVS$CNES <- sprintf("%06s", freqRNRISCO_IPVS$CNES)
  
  return(list(
    freqRNRISCO_final = freqRNRISCO_final,
    freqNVAC_final = freqNVAC_final,
    freqNV_IPVS = freqNV_IPVS,
    freqRNRISCO_IPVS = freqRNRISCO_IPVS,
    data_atual = data_atual,
    COMP_MES = COMP_MES,
    COMP_ANO = COMP_ANO,
    COMP_MESANO = COMP_MESANO,
    # --- Incluir os novos nomes de arquivo no retorno ---
    nomeexcel_tabdescr_MSP = nomeexcel_tabdescr_MSP,
    nomeexcel_tabdescr_CRS = nomeexcel_tabdescr_CRS,
    nomeexcel_tabdescr_CRS_STS = nomeexcel_tabdescr_CRS_STS,
    nomeexcel_tabdescrAC_MSP = nomeexcel_tabdescrAC_MSP,
    nomeexcel_tabdescrAC_CRS = nomeexcel_tabdescrAC_CRS,
    nomeexcel_tabdescrAC_CRS_STS = nomeexcel_tabdescrAC_CRS_STS
    # --- Fim da inclusão ---
  ))
}

# --- Execução da função de cálculo de frequências e percentuais ---
resultados_metricas <- calcular_frequencias_e_percentuais(
  dados_brutos_processados$dados_NV, 
  dados_brutos_processados$dados_NV_AAUBS,
  dados_brutos_processados$dados_NV_risco_AAUBS,
  dados_brutos_processados$dados_NV_AC_AAUBS,
  dados_brutos_processados$tabela_correspondencia_ipvs
)


#### 4. Preparação de Dados Espaciais e Tabulares para Saída (REVISADO - Removidos nomes de arqs descritivos) ####

# Função para combinar dados e preparar dataframes para sa?da
preparar_dados_para_saida <- function(dados_shape_AAUBS, freqRNRISCO_final, freqNVAC_final, resultados_metricas) {
  
  message("Relacionando dados de frequência com shapefiles de AAUBS...")
  
  # Garantir que o CNES no shapefile também seja caractere com 6 d?gitos para jun??o
  dados_shape_AAUBS$CNES <- sprintf("%06s", as.character(dados_shape_AAUBS$CNES))
  
  # Unir dados de RN Risco ao shapefile de AAUBS
  dados_combinados <- dados_shape_AAUBS %>%
    left_join(freqRNRISCO_final, by = "CNES") # Usar a coluna CNES para jun??o
  
  # Unir dados de Anomalias Cong?nitas ao shapefile de AAUBS
  dados_combinados3 <- dados_shape_AAUBS %>%
    left_join(freqNVAC_final, by = "CNES") # Usar a coluna CNES para jun??o
  
  
  message("Criando subsets de dados combinados por CRS (Coordenadoria Regional de Sa?de)...")
  # Subsets para dados de RN Risco por CRS
  dados_combinados_CRSNorte <- subset(dados_combinados, CRSUBS == 'NORTE')
  dados_combinados_CRSSUL <- subset(dados_combinados, CRSUBS == 'SUL')
  dados_combinados_CRSSUDESTE <- subset(dados_combinados, CRSUBS == 'SUDESTE')
  dados_combinados_CRSLESTE <- subset(dados_combinados, CRSUBS == 'LESTE')
  dados_combinados_CRSCENTRO <- subset(dados_combinados, CRSUBS == 'CENTRO')
  dados_combinados_CRSOESTE <- subset(dados_combinados, CRSUBS == 'OESTE')
  
  # Subsets para dados de Anomalias Cong?nitas por CRS
  dados_combinados_CRSNorte3 <- subset(dados_combinados3, CRSUBS == 'NORTE')
  dados_combinados_CRSSUL3 <- subset(dados_combinados3, CRSUBS == 'SUL')
  dados_combinados_CRSSUDESTE3 <- subset(dados_combinados3, CRSUBS == 'SUDESTE')
  dados_combinados_CRSLESTE3 <- subset(dados_combinados3, CRSUBS == 'LESTE')
  dados_combinados_CRSCENTRO3 <- subset(dados_combinados3, CRSUBS == 'CENTRO')
  dados_combinados_CRSOESTE3 <- subset(dados_combinados3, CRSUBS == 'OESTE')
  
  
  message("Selecionando e renomeando campos para sa?da final...")
  # Selecionando e renomeando os campos para a tabela principal (RN Risco)
  campos_desejados <- dados_combinados[, c("CNES", "NOMEUBS.x", "STSUBS", "CRSUBS", "AREA_KM2", "PERCESF", "EAB", "NUMERODN_NV", "RN_RISCO", "percRNRISCO","geometry")]
  names(campos_desejados) <- c("CNES", "NOMEUBS", "STSUBS", "CRSUBS", "AREA_KM2", "PERC_ESF", "EAB", "FREQ_NV", "FREQ_RN_RISCO", "Perc_RN_RISCO", "geometry")
  
  # Selecionando e renomeando os campos para a tabela de Anomalias Cong?nitas
  campos_desejados3 <- dados_combinados3[, c("CNES", "NOMEUBS.x", "STSUBS", "CRSUBS", "AREA_KM2", "PERCESF", "EAB", "NUMERODN_NV", "IDANOMAL", "percNVAC","geometry")]
  names(campos_desejados3) <- c("CNES", "NOMEUBS", "STSUBS", "CRSUBS", "AREA_KM2", "PERC_ESF", "EAB", "FREQ_NV", "freqNVAC", "Perc_NVAC", "geometry")
  
  
  # Definir nomes de arquivos Excel din?micos
  # NOTA: Os nomes dos arquivos descritivos est?o agora em resultados_metricas
  nomeexcel_tab_msp <- paste0("Tabela_Freq_NV_RNRISCO_", resultados_metricas$COMP_MESANO, ".xlsx")
  nomeexcel_tab_crs_norte <- paste0("Tabela_Freq_NV_RNRISCO_CRS_Norte_", resultados_metricas$COMP_MESANO, ".xlsx")
  nomeexcel_tab_crs_leste <- paste0("Tabela_Freq_NV_RNRISCO_CRS_Leste_", resultados_metricas$COMP_MESANO, ".xlsx")
  nomeexcel_tab_crs_oeste <- paste0("Tabela_Freq_NV_RNRISCO_CRS_Oeste_", resultados_metricas$COMP_MESANO, ".xlsx")
  nomeexcel_tab_crs_sudeste <- paste0("Tabela_Freq_NV_RNRISCO_CRS_Sudeste_", resultados_metricas$COMP_MESANO, ".xlsx")
  nomeexcel_tab_crs_sul <- paste0("Tabela_Freq_NV_RNRISCO_CRS_Sul_", resultados_metricas$COMP_MESANO, ".xlsx")
  nomeexcel_tab_crs_centro <- paste0("Tabela_Freq_NV_RNRISCO_CRS_Centro_", resultados_metricas$COMP_MESANO, ".xlsx")
  
  # Ajustes para nomes dos arquivos de anomalias cong?nitas
  nomeexcel_tab_msp_ac <- paste0("Tabela_Freq_NV_AC_", resultados_metricas$COMP_MESANO, ".xlsx") # Novo
  nomeexcel_tab_crs_norte_ac <- paste0("Tabela_Freq_NV_AC_CRS_Norte_", resultados_metricas$COMP_MESANO, ".xlsx") # Novo
  nomeexcel_tab_crs_leste_ac <- paste0("Tabela_Freq_NV_AC_CRS_Leste_", resultados_metricas$COMP_MESANO, ".xlsx") # Novo
  nomeexcel_tab_crs_oeste_ac <- paste0("Tabela_Freq_NV_AC_CRS_Oeste_", resultados_metricas$COMP_MESANO, ".xlsx") # Novo
  nomeexcel_tab_crs_sudeste_ac <- paste0("Tabela_Freq_NV_AC_CRS_Sudeste_", resultados_metricas$COMP_MESANO, ".xlsx") # Novo
  nomeexcel_tab_crs_sul_ac <- paste0("Tabela_Freq_NV_AC_CRS_Sul_", resultados_metricas$COMP_MESANO, ".xlsx") # Novo
  nomeexcel_tab_crs_centro_ac <- paste0("Tabela_Freq_NV_AC_CRS_Centro_", resultados_metricas$COMP_MESANO, ".xlsx") # Novo
  
  
  return(list(
    dados_combinados = dados_combinados,
    dados_combinados3 = dados_combinados3, # para anomalias congenitas
    dados_combinados_CRSNorte = dados_combinados_CRSNorte,
    dados_combinados_CRSSUL = dados_combinados_CRSSUL,
    dados_combinados_CRSSUDESTE = dados_combinados_CRSSUDESTE,
    dados_combinados_CRSLESTE = dados_combinados_CRSLESTE,
    dados_combinados_CRSCENTRO = dados_combinados_CRSCENTRO,
    dados_combinados_CRSOESTE = dados_combinados_CRSOESTE,
    dados_combinados_CRSNorte3 = dados_combinados_CRSNorte3, # para anomalias congenitas
    dados_combinados_CRSSUL3 = dados_combinados_CRSSUL3,
    dados_combinados_CRSSUDESTE3 = dados_combinados_CRSSUDESTE3,
    dados_combinados_CRSLESTE3 = dados_combinados_CRSLESTE3,
    dados_combinados_CRSCENTRO3 = dados_combinados_CRSCENTRO3,
    dados_combinados_CRSOESTE3 = dados_combinados_CRSOESTE3,
    campos_desejados = campos_desejados,
    campos_desejados3 = campos_desejados3, # para anomalias congenitas
    nomeexcel_tab_msp = nomeexcel_tab_msp,
    nomeexcel_tab_crs_norte = nomeexcel_tab_crs_norte,
    nomeexcel_tab_crs_leste = nomeexcel_tab_crs_leste,
    nomeexcel_tab_crs_oeste = nomeexcel_tab_crs_oeste,
    nomeexcel_tab_crs_sudeste = nomeexcel_tab_crs_sudeste,
    nomeexcel_tab_crs_sul = nomeexcel_tab_crs_sul,
    nomeexcel_tab_crs_centro = nomeexcel_tab_crs_centro,
    nomeexcel_tab_msp_ac = nomeexcel_tab_msp_ac,
    nomeexcel_tab_crs_norte_ac = nomeexcel_tab_crs_norte_ac,
    nomeexcel_tab_crs_leste_ac = nomeexcel_tab_crs_leste_ac,
    nomeexcel_tab_crs_oeste_ac = nomeexcel_tab_crs_oeste_ac,
    nomeexcel_tab_crs_sudeste_ac = nomeexcel_tab_crs_sudeste_ac,
    nomeexcel_tab_crs_sul_ac = nomeexcel_tab_crs_sul_ac,
    nomeexcel_tab_crs_centro_ac = nomeexcel_tab_crs_centro_ac
  ))
}

# --- Execução da função de preparação de dados para sa?da ---
dados_para_saida <- preparar_dados_para_saida(
  dados_brutos_processados$dados_shape_AAUBS,
  resultados_metricas$freqRNRISCO_final,
  resultados_metricas$freqNVAC_final,
  resultados_metricas # Agora passamos resultados_metricas para ter acesso ao COMP_MESANO
)


#### 5. Salvamento das Tabelas XLSX (REVISADO - FINALIZADO) ####

# Função para criar diretórios e salvar todas as tabelas em formato XLSX
salvar_tabelas_xlsx <- function(base_output_path, resultados_metricas, dados_para_saida, dados_brutos_processados) {
  
  message("Criando estrutura de diretórios para saídas...")
  
  # Caminho para salvar os arquivos Excel do MSP
  caminho_excel_msp <- file.path(base_output_path, resultados_metricas$COMP_MESANO, "MSP")
  caminho_excel_msp <- stringr::str_trim(caminho_excel_msp)
  if (!dir.exists(caminho_excel_msp)) {
    dir.create(caminho_excel_msp, recursive = TRUE)
    message(paste("Pasta criada:", caminho_excel_msp))
  }
  
  # Caminho para salvar os arquivos Excel das CRS
  caminho_pasta_crs <- file.path(base_output_path, resultados_metricas$COMP_MESANO, "CRS")
  caminho_pasta_crs <- stringr::str_trim(caminho_pasta_crs)
  if (!dir.exists(caminho_pasta_crs)) {
    dir.create(caminho_pasta_crs, recursive = TRUE)
    message(paste("Pasta criada:", caminho_pasta_crs))
  }
  
  message("Preparando dados para a tabela final com informações de naturalidade...")
  
  # Calculando as contagens para cada condição e adicionando colunas adicionais - NV
  nv_maes_estrang_AAUBS <- dados_brutos_processados$dados_NV_AAUBS %>%
    group_by(CNES) %>%
    summarise(
      totalnv = n(),
      maes_brasileiras = sum(!is.na(CODUFNATU)),
      maes_estrangeiras = sum(NATURALMAE != 1 & !is.na(NATURALMAE)),
      maes_bolivianas = sum(NATURALMAE == 33, na.rm = TRUE),
      maes_sem_natu_inf = sum(is.na(NATURALMAE) & is.na(CODUFNATU)),
      .groups = 'drop'
    ) %>%
    mutate(
      perc_estrangeiras = (maes_estrangeiras / totalnv),
      perc_bolivianas = (maes_bolivianas / totalnv)
    )
  
  # Calculando as contagens para cada condição e adicionando colunas adicionais - NV RN Risco
  rnrisco_nv_maes_estrang_AAUBS <- dados_brutos_processados$dados_NV_risco_AAUBS %>%
    group_by(CNES) %>%
    summarise(
      totalnv = n(),
      maes_brasileiras = sum(!is.na(CODUFNATU)),
      maes_estrangeiras = sum(NATURALMAE != 1 & !is.na(NATURALMAE)),
      maes_bolivianas = sum(NATURALMAE == 33, na.rm = TRUE),
      maes_sem_natu_inf = sum(is.na(NATURALMAE) & is.na(CODUFNATU)),
      .groups = 'drop'
    ) %>%
    mutate(
      perc_estrangeiras = (maes_estrangeiras / totalnv),
      perc_bolivianas = (maes_bolivianas / totalnv)
    )
  
  # Multiplicando os resultados por 100 para percentuais
  nv_maes_estrang_AAUBS$perc_estrangeiras <- nv_maes_estrang_AAUBS$perc_estrangeiras * 100
  nv_maes_estrang_AAUBS$perc_bolivianas <- nv_maes_estrang_AAUBS$perc_bolivianas * 100
  rnrisco_nv_maes_estrang_AAUBS$perc_estrangeiras <- rnrisco_nv_maes_estrang_AAUBS$perc_estrangeiras * 100
  rnrisco_nv_maes_estrang_AAUBS$perc_bolivianas <- rnrisco_nv_maes_estrang_AAUBS$perc_bolivianas * 100
  
  # Selecionando apenas as variáveis desejadas dos dataframes
  variaveis_nv_maes <- nv_maes_estrang_AAUBS %>%
    select(CNES, maes_brasileiras, maes_estrangeiras, maes_bolivianas, perc_estrangeiras, perc_bolivianas)
  
  variaveis_rnrisco_nv_maes <- rnrisco_nv_maes_estrang_AAUBS %>%
    select(CNES, maes_brasileiras, maes_estrangeiras, maes_bolivianas, perc_estrangeiras, perc_bolivianas)
  
  # Adicionando prefixos às colunas para evitar conflitos de nomes no join
  variaveis_nv_maes <- variaveis_nv_maes %>%
    rename_with(~ paste0("nv_", .x), -CNES)
  
  variaveis_rnrisco_nv_maes <- variaveis_rnrisco_nv_maes %>%
    rename_with(~ paste0("rnrisco_nv_", .x), -CNES)
  
  # Garantir que CNES seja caractere com 6 dígitos para junção
  variaveis_nv_maes$CNES <- sprintf("%06s", variaveis_nv_maes$CNES)
  variaveis_rnrisco_nv_maes$CNES <- sprintf("%06s", variaveis_rnrisco_nv_maes$CNES)
  
  # Construção de campos_desejados_final (já corrigida no passo anterior)
  campos_desejados_final <- dados_para_saida$campos_desejados %>%
    st_drop_geometry() %>%
    left_join(variaveis_nv_maes, by = "CNES") %>%
    left_join(variaveis_rnrisco_nv_maes, by = "CNES") %>%
    left_join(
      dados_para_saida$campos_desejados3 %>% st_drop_geometry() %>% select(CNES, freqNVAC, Perc_NVAC),
      by = "CNES"
    )
  
  # Separando os dados para salvamento por CRS (utilizando o campos_desejados_final)
  campos_desejados_final_CRSNorte <- subset(campos_desejados_final, CRSUBS == 'NORTE')
  campos_desejados_final_CRSSUL <- subset(campos_desejados_final, CRSUBS == 'SUL')
  campos_desejados_final_CRSSUDESTE <- subset(campos_desejados_final, CRSUBS == 'SUDESTE')
  campos_desejados_final_CRSLESTE <- subset(campos_desejados_final, CRSUBS == 'LESTE')
  campos_desejados_final_CRSCENTRO <- subset(campos_desejados_final, CRSUBS == 'CENTRO')
  campos_desejados_final_CRSOESTE <- subset(campos_desejados_final, CRSUBS == 'OESTE')
  
  
  message("Salvando tabelas XLSX para o MSP...")
  # Salvando dados do MSP
  write_xlsx(campos_desejados_final, file.path(caminho_excel_msp, dados_para_saida$nomeexcel_tab_msp))
  
  # Criando e salvando tabelas descritivas para MSP e CRS
  message("Calculando e salvando tabelas descritivas...")
  
  # MSP (Nascidos Vivos de Risco)
  descritivaMSP <- dados_para_saida$campos_desejados %>%
    st_drop_geometry() %>%
    summarise(
      QTDE_UBS = n(),
      NV_sum = sum(`FREQ_NV`, na.rm = TRUE),
      NVRISCO_sum = sum(`FREQ_RN_RISCO`, na.rm = TRUE)
    ) %>%
    mutate(PERCRNRISCO = (NVRISCO_sum / NV_sum) * 100)
  # AQUI ESTÁ A LINHA COM PROBLEMA NO SEU OUTPUT
  write_xlsx(descritivaMSP, file.path(caminho_excel_msp, resultados_metricas$nomeexcel_tabdescr_MSP))
  
  # AC MSP (Anomalias Congênitas)
  descritivaACMSP <- dados_para_saida$campos_desejados3 %>%
    st_drop_geometry() %>%
    summarise(
      QTDE_UBS = n(),
      NV_sum = sum(`FREQ_NV`, na.rm = TRUE),
      NVAC_sum = sum(`freqNVAC`, na.rm = TRUE)
    ) %>%
    mutate(percNVAC = (NVAC_sum / NV_sum) * 100)
  write_xlsx(descritivaACMSP, file.path(caminho_excel_msp, resultados_metricas$nomeexcel_tabdescrAC_MSP))
  
  
  # SEG CRS (Nascidos Vivos de Risco)
  descritivaCRS <- dados_para_saida$campos_desejados %>%
    st_drop_geometry() %>%
    group_by(CRSUBS) %>%
    summarise(
      QTDE_UBS = n(),
      NV_sum = sum(`FREQ_NV`, na.rm = TRUE),
      NVRISCO_sum = sum(`FREQ_RN_RISCO`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(PERCRNRISCO = (NVRISCO_sum / NV_sum) * 100)
  write_xlsx(descritivaCRS, file.path(caminho_excel_msp, resultados_metricas$nomeexcel_tabdescr_CRS))
  
  
  # AC SEG CRS (Anomalias Congênitas)
  descritivaACCRS <- dados_para_saida$campos_desejados3 %>%
    st_drop_geometry() %>%
    group_by(CRSUBS) %>%
    summarise(
      QTDE_UBS = n(),
      NV_sum = sum(`FREQ_NV`, na.rm = TRUE),
      NVAC_sum = sum(`freqNVAC`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(percNVAC = (NVAC_sum / NV_sum) * 100)
  write_xlsx(descritivaACCRS, file.path(caminho_excel_msp, resultados_metricas$nomeexcel_tabdescrAC_CRS))
  
  
  # SEG CRS E STS (Nascidos Vivos de Risco)
  descritivaCRSSTS <- dados_para_saida$campos_desejados %>%
    st_drop_geometry() %>%
    group_by(CRSUBS, STSUBS) %>%
    summarise(
      QTDE_UBS = n(),
      NV_sum = sum(`FREQ_NV`, na.rm = TRUE),
      NVRISCO_sum = sum(`FREQ_RN_RISCO`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(PERCRNRISCO = (NVRISCO_sum / NV_sum) * 100)
  write_xlsx(descritivaCRSSTS, file.path(caminho_excel_msp, resultados_metricas$nomeexcel_tabdescr_CRS_STS))
  
  # AC SEG CRS E STS (Anomalias Congênitas)
  descritivaACCRSSTS <- dados_para_saida$campos_desejados3 %>%
    st_drop_geometry() %>%
    group_by(CRSUBS, STSUBS) %>%
    summarise(
      QTDE_UBS = n(),
      NV_sum = sum(`FREQ_NV`, na.rm = TRUE),
      NVAC_sum = sum(`freqNVAC`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(percNVAC = (NVAC_sum / NV_sum) * 100)
  write_xlsx(descritivaACCRSSTS, file.path(caminho_excel_msp, resultados_metricas$nomeexcel_tabdescrAC_CRS_STS))
  
  
  
  message("Salvando tabelas XLSX para as CRS...")
  # Salvando dados das CRS
  write_xlsx(campos_desejados_final_CRSNorte, file.path(caminho_pasta_crs, dados_para_saida$nomeexcel_tab_crs_norte))
  write_xlsx(campos_desejados_final_CRSLESTE, file.path(caminho_pasta_crs, dados_para_saida$nomeexcel_tab_crs_leste))
  write_xlsx(campos_desejados_final_CRSOESTE, file.path(caminho_pasta_crs, dados_para_saida$nomeexcel_tab_crs_oeste))
  write_xlsx(campos_desejados_final_CRSSUDESTE, file.path(caminho_pasta_crs, dados_para_saida$nomeexcel_tab_crs_sudeste))
  write_xlsx(campos_desejados_final_CRSSUL, file.path(caminho_pasta_crs, dados_para_saida$nomeexcel_tab_crs_sul))
  write_xlsx(campos_desejados_final_CRSCENTRO, file.path(caminho_pasta_crs, dados_para_saida$nomeexcel_tab_crs_centro))
  
  # Salvando dados das CRS para Anomalias Congênitas (usando nomeexcel_tab_crs_NOME_ac)
  write_xlsx(campos_desejados_final_CRSNorte, file.path(caminho_pasta_crs, dados_para_saida$nomeexcel_tab_crs_norte_ac))
  write_xlsx(campos_desejados_final_CRSLESTE, file.path(caminho_pasta_crs, dados_para_saida$nomeexcel_tab_crs_leste_ac))
  write_xlsx(campos_desejados_final_CRSOESTE, file.path(caminho_pasta_crs, dados_para_saida$nomeexcel_tab_crs_oeste_ac))
  write_xlsx(campos_desejados_final_CRSSUDESTE, file.path(caminho_pasta_crs, dados_para_saida$nomeexcel_tab_crs_sudeste_ac))
  write_xlsx(campos_desejados_final_CRSSUL, file.path(caminho_pasta_crs, dados_para_saida$nomeexcel_tab_crs_sul_ac))
  write_xlsx(campos_desejados_final_CRSCENTRO, file.path(caminho_pasta_crs, dados_para_saida$nomeexcel_tab_crs_centro_ac))
  
  
  message("Todas as tabelas foram salvas com sucesso!")
}


# --- Execução da função de salvamento de tabelas XLSX ---
salvar_tabelas_xlsx(
  BASE_OUTPUT_PATH,
  resultados_metricas,
  dados_para_saida,
  dados_brutos_processados
)




#### 6. Geração de Mapas Estáticos (PNG/PDF) - REVISADO PELA QUINTA VEZ (Ajuste Tamanho Rótulo) ####

# Fun??o auxiliar para gerar mapas coropl?ticos
gerar_mapa_coropletico <- function(data, fill_var, title, fill_legend_title, filename,
                                   data_atual, comp_mesano,
                                   num_classes = 5, style = "jenks", palette = "RdPu",
                                   add_labels = FALSE, label_var = NULL) {
  
  valores_sem_na <- na.omit(fill_var)
  
  if (length(valores_sem_na) == 0 || max(valores_sem_na) == min(valores_sem_na)) {
    warning(paste("Não há variação ou dados suficientes para criar classes para", filename, ". Pulando mapa."))
    return(NULL)
  }
  
  intervalos <- classInt::classIntervals(valores_sem_na, n = num_classes, style = style)
  paleta_cores <- RColorBrewer::brewer.pal(num_classes, palette)
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = data, aes(fill = fill_var), color = "black") +
    ggplot2::scale_fill_gradientn(
      colors = paleta_cores,
      breaks = intervalos$brks,
      labels = scales::number_format(scale = 1, decimal.mark = ","),
      na.value = "gray",
      limits = c(min(intervalos$brks), max(intervalos$brks))
    ) +
    ggplot2::labs(
      title = paste0(title, "\nno mês: ", comp_mesano),
      fill = fill_legend_title,
      caption = paste0("Fonte: SINASC/CEInfo/CIS/SERMAP, Secretaria Municipal da Saúde de São Paulo\n",
                       "Elaboração: SMS-SP/SERMAP/CIS/CEInfo/GISA em ", data_atual)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = 10, unit = "pt")),
      plot.title = ggplot2::element_text(lineheight = 1.1)
    )
  
  if (add_labels && !is.null(label_var)) {
    data_wgs84_labels <- sf::st_transform(data, crs = 4326)
    centroides_labels <- sf::st_centroid(data_wgs84_labels)
    data_labels_df <- as.data.frame(sf::st_coordinates(centroides_labels))
    data_labels_df$label_text <- data_wgs84_labels[[label_var]]
    data_labels_df$CNES <- data_wgs84_labels$CNES
    
    data_labels_df <- data_labels_df %>% dplyr::filter(!is.na(label_text))
    
    p <- p + ggrepel::geom_text_repel(data = data_labels_df,
                                      aes(x = X, y = Y, label = stringr::str_wrap(label_text, width = 16)),
                                      size = 0.6 * .pt, # AQUI ESTÁ O AJUSTE: Reduzido de 1.0 para 0.6 (experimente outros valores como 0.5 ou 0.7)
                                      color = "black",
                                      box.padding = unit(0.35, "lines"),
                                      point.padding = unit(0.5, "lines"),
                                      segment.color = 'grey50',
                                      min.segment.length = 0,
                                      force = 1,
                                      max.overlaps = Inf
    )
  }
  
  p <- p +
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                                      height = unit(1, "cm"), width = unit(1, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering) +
    ggspatial::annotation_scale(location = "br", width_hint = 0.1,
                                height = unit(0.2, "cm"), pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
    ggplot2::coord_sf(crs = 4326, datum = 4326, expand = FALSE)
  
  ggsave(filename = filename, plot = p, width = 29.7, height = 21, units = 'cm', dpi = 300)
  message(paste("Mapa coroplético salvo:", filename))
}


# Fun??o auxiliar para gerar mapas de s?mbolos proporcionais (sem altera??es)
gerar_mapa_simbolos_proporcionais <- function(data, size_var, title, size_legend_title, filename,
                                              data_atual, comp_mesano,
                                              base_shape_wgs84) {
  
  data_filtered <- data %>%
    filter(!is.na(!!sym(size_var)), !!sym(size_var) > 0)
  
  if (nrow(data_filtered) == 0) {
    warning(paste("Não há dados válidos para criar classes para o mapa de símbolos proporcionais para", filename, ". Pulando mapa."))
    return(NULL)
  }
  
  target_crs <- sf::st_crs(4326)
  
  data_wgs84 <- sf::st_transform(data_filtered, target_crs)
  base_shape_wgs84_transformed <- sf::st_transform(base_shape_wgs84, target_crs)
  
  centroides <- sf::st_centroid(data_wgs84)
  centroides_df <- sf::st_coordinates(centroides)
  
  dados_centroides_plot <- data.frame(
    VALOR_SIZE = data_wgs84[[size_var]],
    LON = centroides_df[, "X"],
    LAT = centroides_df[, "Y"]
  )
  
  dados_centroides_plot <- dados_centroides_plot %>%
    dplyr::filter(!is.na(VALOR_SIZE), is.finite(VALOR_SIZE))
  
  if (max(dados_centroides_plot$VALOR_SIZE) == min(dados_centroides_plot$VALOR_SIZE)) {
    breaks_size <- c(min(dados_centroides_plot$VALOR_SIZE))
  } else {
    breaks_size <- classInt::classIntervals(dados_centroides_plot$VALOR_SIZE, n = 5, style = "equal")$brks
  }
  
  breaks_legend <- unique(round(breaks_size))
  if (length(breaks_legend) > 1 && breaks_legend[1] == breaks_legend[2]) {
    breaks_legend <- breaks_legend[-1]
  }
  if (length(breaks_legend) == 0) breaks_legend <- 1
  
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = base_shape_wgs84_transformed, color = "black", fill = NA) +
    ggplot2::geom_point(data = dados_centroides_plot, aes(x = LON, y = LAT, size = VALOR_SIZE),
                        color = "red", alpha = 0.5) +
    ggplot2::scale_size_continuous(range = c(0.2, 8),
                                   breaks = breaks_legend,
                                   labels = scales::number_format(scale = 1, decimal.mark = ","),
                                   guide = ggplot2::guide_legend(title = size_legend_title)) +
    ggplot2::labs(
      title = paste0(title, "\nno mês: ", comp_mesano),
      caption = paste0("Fonte: SINASC/CEInfo/CIS/SERMAP, Secretaria Municipal da Saúde de São Paulo\n",
                       "Elaboração: SMS-SP/SERMAP/CIS/CEInfo/GISA em ", data_atual)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(t = 10, unit = "pt")),
      plot.title = ggplot2::element_text(lineheight = 1.1)
    ) +
    ggplot2::coord_sf(crs = 4326, datum = 4326, expand = FALSE)
  
  ggplot2::ggsave(filename = filename, plot = p, width = 29.7, height = 21, units = 'cm', dpi = 300)
  message(paste("Mapa de s?mbolos proporcionais salvo:", filename))
}


# Fun??o principal para orquestrar a gera??o de todos os mapas est?ticos (sem altera??es)
gerar_todos_mapas_estaticos <- function(base_output_path, resultados_metricas, dados_para_saida, dados_brutos_processados) {
  
  message("Iniciando a geração de mapas estáticos...")
  
  caminho_mapas_msp <- file.path(base_output_path, resultados_metricas$COMP_MESANO, "MSP")
  caminho_mapas_crs <- file.path(base_output_path, resultados_metricas$COMP_MESANO, "CRS")
  
  
  # --- Mapas para o MSP (sem rótulos de UBS) ---
  
  # 1. Frequ?ncia NV - Coropl?tico MSP
  filename_freq_nv_msp <- file.path(caminho_mapas_msp, paste0("Freq_NV_segAAUBS_MSP_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados,
    fill_var = dados_para_saida$dados_combinados$NUMERODN_NV,
    title = "Nascidos Vivos de parturientes residentes no\nMunicípio de São Paulo",
    fill_legend_title = "Frequência NV",
    filename = filename_freq_nv_msp,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 6, palette = "PuBu", style = "jenks"
  )
  
  # 2. Frequ?ncia NV - S?mbolos Proporcionais MSP
  filename_simb_nv_msp <- file.path(caminho_mapas_msp, paste0("Freq_NV_segAAUBS_MSP_SimbProporc_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_simbolos_proporcionais(
    data = dados_para_saida$dados_combinados,
    size_var = "NUMERODN_NV",
    title = "Nascidos Vivos de parturientes residentes\nno Município de São Paulo",
    size_legend_title = "Frequência de NV",
    filename = filename_simb_nv_msp,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    base_shape_wgs84 = dados_brutos_processados$dados_shape_AAUBS
  )
  
  # 3. Frequ?ncia RN Risco - Coropl?tico MSP
  filename_freq_rn_risco_msp <- file.path(caminho_mapas_msp, paste0("Freq_RNRisco_segAAUBS_MSP_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados,
    fill_var = dados_para_saida$dados_combinados$RN_RISCO,
    title = "Nascidos Vivos de risco segundo residência da Parturiente\nno Município de São Paulo",
    fill_legend_title = "Frequência NV\nde Alto Risco",
    filename = filename_freq_rn_risco_msp,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 9, palette = "Reds", style = "quantile"
  )
  
  # 4. Frequ?ncia RN Risco - S?mbolos Proporcionais MSP
  filename_simb_rn_risco_msp <- file.path(caminho_mapas_msp, paste0("Freq_RNRisco_segAAUBS_MSP_SimbProporc_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_simbolos_proporcionais(
    data = dados_para_saida$dados_combinados,
    size_var = "RN_RISCO",
    title = "Nascidos Vivos de risco segundo residência da Parturiente\nno Município de São Paulo",
    size_legend_title = "Freq de NV Risco",
    filename = filename_simb_rn_risco_msp,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    base_shape_wgs84 = dados_brutos_processados$dados_shape_AAUBS
  )
  
  # 5. Percentual RN Risco - Coropl?tico MSP
  filename_perc_rn_risco_msp <- file.path(caminho_mapas_msp, paste0("Perc_RNRisco_segAAUBS_MSP_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados,
    fill_var = dados_para_saida$dados_combinados$percRNRISCO,
    title = "Nascidos Vivos de risco segundo residência da Parturiente\nno Município de São Paulo",
    fill_legend_title = "Percentual de NV\nde alto risco (Jenks)",
    filename = filename_perc_rn_risco_msp,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks"
  )
  
  # 6. Percentual Anomalias Cong?nitas - Coropl?tico MSP
  filename_perc_nv_ac_msp <- file.path(caminho_mapas_msp, paste0("Perc_NV_AC_segAAUBS_MSP_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados3,
    fill_var = dados_para_saida$dados_combinados3$percNVAC,
    title = "Nascidos Vivos com Anomalia Congênita segundo residência\nda Parturiente no Município de São Paulo",
    fill_legend_title = "Percentual de NV\nc/Anomal. Congênita (Jenks)",
    filename = filename_perc_nv_ac_msp,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks"
  )
  
  # --- Mapas para cada CRS (COM Rótulos de UBS usando geom_text_repel) ---
  
  # CRS NORTE
  # Perc RN Risco CRS Norte
  filename_perc_rn_risco_crs_norte <- file.path(caminho_mapas_crs, paste0("Perc_RNrisco_segAAUBS_CRS_Norte_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados_CRSNorte,
    fill_var = dados_para_saida$dados_combinados_CRSNorte$percRNRISCO,
    title = "Nascidos Vivos de risco de parturientes\nresidentes na CRS Norte",
    fill_legend_title = "Percentual de NV\nde alto risco (Jenks)",
    filename = filename_perc_rn_risco_crs_norte,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks",
    add_labels = TRUE, label_var = "NOMEUBS.x"
  )
  
  # Perc AC CRS Norte
  filename_perc_nv_ac_crs_norte <- file.path(caminho_mapas_crs, paste0("Perc_NV_AC_segAAUBS_CRS_Norte_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados_CRSNorte3,
    fill_var = dados_para_saida$dados_combinados_CRSNorte3$percNVAC,
    title = "Nascidos Vivos com Anomalia Congênita de parturientes\nresidentes na CRS Norte",
    fill_legend_title = "Percentual de NV c/\nAnomal. Congênita (Jenks)",
    filename = filename_perc_nv_ac_crs_norte,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks",
    add_labels = TRUE, label_var = "NOMEUBS.x"
  )
  
  # CRS LESTE
  # Perc RN Risco CRS Leste
  filename_perc_rn_risco_crs_leste <- file.path(caminho_mapas_crs, paste0("Perc_RNrisco_segAAUBS_CRS_Leste_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados_CRSLESTE,
    fill_var = dados_para_saida$dados_combinados_CRSLESTE$percRNRISCO,
    title = "Nascidos Vivos de risco de parturientes\nresidentes na CRS LESTE",
    fill_legend_title = "Percentual de NV\nde alto risco (Jenks)",
    filename = filename_perc_rn_risco_crs_leste,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks",
    add_labels = TRUE, label_var = "NOMEUBS.x"
  )
  
  # Perc AC CRS Leste
  filename_perc_nv_ac_crs_leste <- file.path(caminho_mapas_crs, paste0("Perc_NV_AC_segAAUBS_CRS_Leste_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados_CRSLESTE3,
    fill_var = dados_para_saida$dados_combinados_CRSLESTE3$percNVAC,
    title = "Nascidos Vivos com Anomalia Congênita de parturientes\nresidentes na CRS LESTE",
    fill_legend_title = "Percentual de NV c/\nAnomal. Congênita (Jenks)",
    filename = filename_perc_nv_ac_crs_leste,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks",
    add_labels = TRUE, label_var = "NOMEUBS.x"
  )
  
  # CRS SUL
  # Perc RN Risco CRS Sul
  filename_perc_rn_risco_crs_sul <- file.path(caminho_mapas_crs, paste0("Perc_RNrisco_segAAUBS_CRS_Sul_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados_CRSSUL,
    fill_var = dados_para_saida$dados_combinados_CRSSUL$percRNRISCO,
    title = "Nascidos Vivos de risco de parturientes\nresidentes na CRS SUL",
    fill_legend_title = "Percentual de NV\nde alto risco (Jenks)",
    filename = filename_perc_rn_risco_crs_sul,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks",
    add_labels = TRUE, label_var = "NOMEUBS.x"
  )
  
  # Perc AC CRS Sul
  filename_perc_nv_ac_crs_sul <- file.path(caminho_mapas_crs, paste0("Perc_NV_AC_segAAUBS_CRS_Sul_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados_CRSSUL3,
    fill_var = dados_para_saida$dados_combinados_CRSSUL3$percNVAC,
    title = "Nascidos Vivos com Anomalia Congênita de parturientes\nresidentes na CRS SUL",
    fill_legend_title = "Percentual de NV c/\nAnomal. Congênita (Jenks)",
    filename = filename_perc_nv_ac_crs_sul,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks",
    add_labels = TRUE, label_var = "NOMEUBS.x"
  )
  
  # CRS SUDESTE
  # Perc RN Risco CRS Sudeste
  filename_perc_rn_risco_crs_sudeste <- file.path(caminho_mapas_crs, paste0("Perc_RNrisco_segAAUBS_CRS_Sudeste_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados_CRSSUDESTE,
    fill_var = dados_para_saida$dados_combinados_CRSSUDESTE$percRNRISCO,
    title = "Nascidos Vivos de risco de parturientes\nresidentes na CRS SUDESTE",
    fill_legend_title = "Percentual de NV\nde alto risco (Jenks)",
    filename = filename_perc_rn_risco_crs_sudeste,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks",
    add_labels = TRUE, label_var = "NOMEUBS.x"
  )
  
  # Perc AC CRS Sudeste
  filename_perc_nv_ac_crs_sudeste <- file.path(caminho_mapas_crs, paste0("Perc_NV_AC_segAAUBS_CRS_Sudeste_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados_CRSSUDESTE3,
    fill_var = dados_para_saida$dados_combinados_CRSSUDESTE3$percNVAC,
    title = "Nascidos Vivos com Anomalia Congênita de parturientes\nresidentes na CRS SUDESTE",
    fill_legend_title = "Percentual de NV c/\nAnomal. Congênita (Jenks)",
    filename = filename_perc_nv_ac_crs_sudeste,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks",
    add_labels = TRUE, label_var = "NOMEUBS.x"
  )
  
  # CRS CENTRO
  # Perc RN Risco CRS Centro
  filename_perc_rn_risco_crs_centro <- file.path(caminho_mapas_crs, paste0("Perc_RNrisco_segAAUBS_CRS_Centro_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados_CRSCENTRO,
    fill_var = dados_para_saida$dados_combinados_CRSCENTRO$percRNRISCO,
    title = "Nascidos Vivos de risco de parturientes\nresidentes na CRS CENTRO",
    fill_legend_title = "Percentual de NV\nde alto risco (Jenks)",
    filename = filename_perc_rn_risco_crs_centro,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks",
    add_labels = TRUE, label_var = "NOMEUBS.x"
  )
  
  # Perc AC CRS Centro
  filename_perc_nv_ac_crs_centro <- file.path(caminho_mapas_crs, paste0("Perc_NV_AC_segAAUBS_CRS_Centro_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados_CRSCENTRO3,
    fill_var = dados_para_saida$dados_combinados_CRSCENTRO3$percNVAC,
    title = "Nascidos Vivos com Anomalia Congênita de parturientes\nresidentes na CRS CENTRO",
    fill_legend_title = "Percentual de NV c/\nAnomal. Congênita (Jenks)",
    filename = filename_perc_nv_ac_crs_centro,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks",
    add_labels = TRUE, label_var = "NOMEUBS.x"
  )
  
  # CRS OESTE
  # Perc RN Risco CRS Oeste
  filename_perc_rn_risco_crs_oeste <- file.path(caminho_mapas_crs, paste0("Perc_RNrisco_segAAUBS_CRS_Oeste_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados_CRSOESTE,
    fill_var = dados_para_saida$dados_combinados_CRSOESTE$percRNRISCO,
    title = "Nascidos Vivos de risco de parturientes\nresidentes na CRS OESTE",
    fill_legend_title = "Percentual de NV\nde alto risco (Jenks)",
    filename = filename_perc_rn_risco_crs_oeste,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks",
    add_labels = TRUE, label_var = "NOMEUBS.x"
  )
  
  # Perc AC CRS Oeste
  filename_perc_nv_ac_crs_oeste <- file.path(caminho_mapas_crs, paste0("Perc_NV_AC_segAAUBS_CRS_Oeste_", resultados_metricas$COMP_MESANO, ".png"))
  gerar_mapa_coropletico(
    data = dados_para_saida$dados_combinados_CRSOESTE3,
    fill_var = dados_para_saida$dados_combinados_CRSOESTE3$percNVAC,
    title = "Nascidos Vivos com Anomalia Congênita de parturientes\nresidentes na CRS OESTE",
    fill_legend_title = "Percentual de NV c/\nAnomal. Congênita (Jenks)",
    filename = filename_perc_nv_ac_crs_oeste,
    data_atual = resultados_metricas$data_atual,
    comp_mesano = resultados_metricas$COMP_MESANO,
    num_classes = 5, palette = "RdPu", style = "jenks",
    add_labels = TRUE, label_var = "NOMEUBS.x"
  )
  
  message("Geração de mapas estáticos concluída com sucesso!")
}

# --- Execução da função de geração de mapas estáticos ---
gerar_todos_mapas_estaticos(
  BASE_OUTPUT_PATH,
  resultados_metricas,
  dados_para_saida,
  dados_brutos_processados
)



#### 7. Geração do Mapa Dinâmico HTML Leaflet (REVISADO FINALÍSSIMO - LAYOUT ESTRUTURAL DEFINITIVO) ####

# Função para gerar o mapa dinâmico Leaflet
gerar_mapa_dinamico_leaflet <- function(resultados_metricas, dados_brutos_processados, url_banner, url_favicon) {
  
  message("Preparando dados para o mapa dinâmico Leaflet (fidelidade ao original)...")
  
  # Descompactar os dados processados para facilitar o acesso
  dados_NV_AAUBS <- dados_brutos_processados$dados_NV_AAUBS
  dados_NV_risco_AAUBS <- dados_brutos_processados$dados_NV_risco_AAUBS
  shapeAAUBSwgs84 <- dados_brutos_processados$dados_shape_AAUBS %>% sf::st_transform(crs = 4326)
  shapeSTSUBSwgs84 <- dados_brutos_processados$shapeSTSUBSwgs84
  shapeCRSSUBSwgs84 <- dados_brutos_processados$shapeCRSSUBSwgs84
  shapeIPVS_filtrado <- dados_brutos_processados$shapeIPVS_filtrado
  
  dados_NV_AAUBS_maes_adol <- dados_NV_AAUBS %>% dplyr::filter(IDADEMAE >= 0 & IDADEMAE <= 19)
  dados_NV_AAUBS_maes_estrang <- dados_NV_AAUBS %>% dplyr::filter(NATURALMAE != 1)
  dados_NV_AAUBS_pre_termo <- dados_NV_AAUBS %>% dplyr::filter(GESTACAO %in% c(1, 2, 3, 4))
  dados_NV_AAUBS_anomal <- dados_NV_AAUBS %>% dplyr::filter(IDANOMAL == 1)
  dados_NV_AAUBS_baixo_peso <- dados_NV_AAUBS %>% dplyr::filter(PESO > 0 & PESO < 2500)
  dados_NV_AAUBS_prenatal_incomp <- dados_NV_AAUBS %>% dplyr::filter(CONSULTAS %in% c(1, 2, 3))
  dados_NV_cesario <- dados_NV_AAUBS %>% dplyr::filter(PARTO == 2)
  dados_NV_estabSUS <- dados_NV_AAUBS %>% dplyr::filter(ESTAB_SUS == 'PÚBLICO')
  
  message("Configurando paleta de cores para IPVS...")
  descricoes_ipvs_ordenadas <- c("Baixíssima", "Muito Baixa", "Baixa", "Média", "Alta (urbanos)", "Muito Alta", "Alta (rurais)", "NC")
  
  colors_ipvs_ordenadas <- c("Baixíssima" = "#1a9641", "Muito Baixa" = "#a6d96a", "Baixa" = "#ffffbf", "Média" = "#fdae61", "Alta (urbanos)"= "#f46d43", "Muito Alta" = "#d73027", "Alta (rurais)" = "#7f0000", "NC" = "#bdbdbd")
  
  shapeIPVS_filtrado$IPVS_Desc <- factor(dados_brutos_processados$tabela_correspondencia_ipvs$descr_ipvs[match(shapeIPVS_filtrado$v10, dados_brutos_processados$tabela_correspondencia_ipvs$ipvs)], levels = descricoes_ipvs_ordenadas)
  
  pal_ipvs <- leaflet::colorFactor(palette = colors_ipvs_ordenadas, domain = descricoes_ipvs_ordenadas, ordered = TRUE)
  
  message("Criando o mapa Leaflet e adicionando camadas na ordem correta...")
  
  mapa_leaflet_final <- leaflet::leaflet(options = leaflet::leafletOptions(title = "Nascidos Vivos no Município de São Paulo")) %>%
    setView(lng = -46.6138637, lat = -23.6495082, zoom = 11) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Mapa-base de vias (cinza) | Carto DB") %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "Mapa-base de vias | OpenStreetMap") %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Imagem de satélite | Esri World Imagery") %>%
    
    
    leaflet::addPolygons(data = shapeIPVS_filtrado, color = "black", weight = 0.2, fillOpacity = 0.6, fillColor = ~pal_ipvs(IPVS_Desc), popup = ~htmltools::htmlEscape(paste("Vulnerabilidade: ", IPVS_Desc)), group = "Vulnerabilidade Social (IPVS)") %>%
    
    leaflet::addPolygons(data = shapeCRSSUBSwgs84, color = "black", weight = 3.2, fillOpacity = 0.0, fillColor = NA, label = ~CRS, group = "Coordenadoria Regional de Saúde (CRS)") %>%
    
    leaflet::addPolygons(data = shapeSTSUBSwgs84, color = "black", weight = 2.0, fillOpacity = 0.0, fillColor = NA, label = ~STS, group = "Supervisão Técnica de Saúde (STS)") %>%
    
    leaflet::addPolygons(data = shapeAAUBSwgs84, color = "black", weight = 1.2, fillOpacity = 0.0, fillColor = NA, label = ~NOMEUBS, group = "Área de Abrangência de UBS (AAUBS)") %>%
    
    leaflet.extras::addHeatmap(lng = dados_NV_AAUBS$LONSIRGAS, lat = dados_NV_AAUBS$LATSIRGAS, blur = 42, max = 0.5, group = "Mapa de calor - Nascidos Vivos Gerais") %>%
    
    leaflet.extras::addHeatmap(lng = dados_NV_risco_AAUBS$LONSIRGAS, lat = dados_NV_risco_AAUBS$LATSIRGAS, blur = 42, max = 0.5, group = "Mapa de calor - Nascidos Vivos de Risco") %>%
    
    leaflet::addCircleMarkers(data = dados_NV_risco_AAUBS, lng = ~LONSIRGAS, lat = ~LATSIRGAS, radius = 8, fillOpacity = 0.5, color = "#810f7c", stroke = FALSE, group = "Nascidos Vivos de Risco") %>%
    leaflet::addCircleMarkers(data = dados_NV_AAUBS_pre_termo, lng = ~LONSIRGAS, lat = ~LATSIRGAS, radius = 4, fillOpacity = 0.5, color = "#f16913", stroke = FALSE, group = "Pré-Termo (<37 semanas)") %>%
    
    leaflet::addCircleMarkers(data = dados_NV_AAUBS_baixo_peso, lng = ~LONSIRGAS, lat = ~LATSIRGAS, radius = 5, fillOpacity = 0.5, color = "#d7301f", stroke = FALSE, group = "Baixo Peso (<2500g)") %>%
    
    leaflet::addCircleMarkers(data = dados_NV_AAUBS_anomal, lng = ~LONSIRGAS, lat = ~LATSIRGAS, radius = 7, fillOpacity = 0.6, color = "#67000d", stroke = FALSE, group = "Presença de Anomalia") %>%
    
    leaflet::addCircleMarkers(data = dados_NV_AAUBS_prenatal_incomp, lng = ~LONSIRGAS, lat = ~LATSIRGAS, radius = 4, fillOpacity = 0.6, color = "#7a0177", stroke = FALSE, group = "Pré-natal incompleto (<7 consultas)") %>%
    
    leaflet::addCircleMarkers(data = dados_NV_AAUBS_maes_adol, lng = ~LONSIRGAS, lat = ~LATSIRGAS, radius = 4, fillOpacity = 0.6, color = "#dd3497", stroke = FALSE, group = "Parturientes Adolescentes (<=19 anos)") %>%
    
    leaflet::addCircleMarkers(data = dados_NV_AAUBS_maes_estrang, lng = ~LONSIRGAS, lat = ~LATSIRGAS, radius = 4, fillOpacity = 0.6, color = "#8c510a", stroke = FALSE, group = "Parturientes Estrangeiras") %>%
    
    leaflet::addCircleMarkers(data = dados_NV_cesario, lng = ~LONSIRGAS, lat = ~LATSIRGAS, radius = 4.5, fillOpacity = 0.4, color = "#7a0177", stroke = FALSE, group = "Parto Cesário") %>%
    
    leaflet::addCircleMarkers(data = dados_NV_estabSUS, lng = ~LONSIRGAS, lat = ~LATSIRGAS, radius = 4.5, fillOpacity = 0.4, color = "#08519c", stroke = FALSE, group = "Parto em Estabelecimento SUS") %>%
    
    leaflet::addCircleMarkers(data = dados_NV_AAUBS, lng = ~LONSIRGAS, lat = ~LATSIRGAS, radius = 3, fillOpacity = 0.6, color = "#737373", label = ~NUMERODN, stroke = FALSE,
                              popup = ~lapply(seq_len(nrow(dados_NV_AAUBS)), function(i) {
                                RN_Risco_val <- ifelse(dados_NV_AAUBS$RN_RISCO[i] == "S", "Sim", ifelse(dados_NV_AAUBS$RN_RISCO[i] == "N", "Não", "Ignorado"))
                                Anomalia_val <- ifelse(dados_NV_AAUBS$IDANOMAL[i] == "1", "Sim", ifelse(dados_NV_AAUBS$IDANOMAL[i] == "2", "Não", ""))
                                Anomalia_CID_val <- ifelse(dados_NV_AAUBS$IDANOMAL[i] != "", paste(" - CID:", dados_NV_AAUBS$CODANOMAL[i]), "")
                                Tipo_parto_val <- ifelse(dados_NV_AAUBS$PARTO[i] == "1", "Vaginal", ifelse(dados_NV_AAUBS$PARTO[i] == "2", "Cesário", ifelse(dados_NV_AAUBS$PARTO[i] == "9", "Ignorado", "")))
                                Parto_SUS_val <- ifelse(dados_NV_AAUBS$ESTAB_SUS[i] == "S", "Sim", "Não")
                                Brasileira_val <- ifelse(dados_NV_AAUBS$CODUFNATU[i] != "", "Sim", ifelse(is.na(dados_NV_AAUBS$NATURALMAE[i]) || dados_NV_AAUBS$NATURALMAE[i] == "", "Não", ""))
                                CorRaca_val <- ifelse(dados_NV_AAUBS$RACACORMAE[i] == "1", "Branca", ifelse(dados_NV_AAUBS$RACACORMAE[i] == "2", "Preta", ifelse(dados_NV_AAUBS$RACACORMAE[i] == "3", "Amarela", ifelse(dados_NV_AAUBS$RACACORMAE[i] == "4", "Parda", ifelse(dados_NV_AAUBS$RACACORMAE[i] == "5", "Indígena", ifelse(dados_NV_AAUBS$RACACORMAE[i] == "9", "Ignorado", ""))))))
                                Escolaridade_val <- ifelse(dados_NV_AAUBS$ESCMAE[i] == "1", "Nenhum", ifelse(dados_NV_AAUBS$ESCMAE[i] == "2", "1 a 3 anos", ifelse(dados_NV_AAUBS$ESCMAE[i] == "3", "4 a 7 anos", ifelse(dados_NV_AAUBS$ESCMAE[i] == "4", "8 a 11 anos", ifelse(dados_NV_AAUBS$ESCMAE[i] == "5", "12 e mais", ifelse(dados_NV_AAUBS$ESCMAE[i] == "9", "Ignorado", ""))))))
                                Gestacao_val <- ifelse(dados_NV_AAUBS$GESTACAO[i] == "1", "Menos de 22", ifelse(dados_NV_AAUBS$GESTACAO[i] == "2", "22 a 27", ifelse(dados_NV_AAUBS$GESTACAO[i] == "3", "28 a 31", ifelse(dados_NV_AAUBS$GESTACAO[i] == "4", "32 a 36", ifelse(dados_NV_AAUBS$GESTACAO[i] == "5", "37 a 41", ifelse(dados_NV_AAUBS$GESTACAO[i] == "6", "42 ou mais", ifelse(dados_NV_AAUBS$GESTACAO[i] == "9", "Ignorado", "")))))))
                                
                                data_formatada <- format(as.Date(as.character(dados_NV_AAUBS$DTNASC[i]), format = "%d%m%Y"), "%d/%m/%Y")
                                hora_formatada <- format(strptime(as.character(dados_NV_AAUBS$HORANASC[i]), format = "%H%M"), "%H:%M")
                                hora_com_h <- paste(hora_formatada, "h", sep = "")
                                
                                to_windows1252_safe <- function(s) {
                                  if(is.character(s)) {
                                    tryCatch(
                                      iconv(s, from = "UTF-8", to = "windows-1252//TRANSLIT//IGNORE"),
                                      error = function(e) {
                                        warning(paste("Erro de convers?o de codifica??o:", s, "-", e$message))
                                        return(s)
                                      }
                                    )
                                  } else {
                                    return(s)
                                  }
                                }
                                
                                htmltools::HTML(paste0(
                                  "<b>Nº DNV:</b> ", htmltools::htmlEscape(dados_NV_AAUBS$NUMERODN[i]), "<br/>",
                                  "<b>Nome da Parturiente:</b> ", htmltools::htmlEscape(to_windows1252_safe(dados_NV_AAUBS$NOMEMAE[i])), "<br/>",
                                  "<b>Nº SUS da Parturiente:</b> ", htmltools::htmlEscape(dados_NV_AAUBS$NUMSUSMAE[i]), "<br/>",
                                  "<b>Idade da Parturiente:</b> ", htmltools::htmlEscape(dados_NV_AAUBS$IDADEMAE[i]), "<br/>",
                                  "<b>Brasileira:</b> ", htmltools::htmlEscape(to_windows1252_safe(Brasileira_val)), "<br/>",
                                  "<b>Raça/cor da Parturiente:</b> ", htmltools::htmlEscape(to_windows1252_safe(CorRaca_val)), "<br/>",
                                  "<b>Escolaridade (anos de estudo):</b> ", htmltools::htmlEscape(to_windows1252_safe(Escolaridade_val)), "<br/>",
                                  "<b>Endereço residencial:</b> ", htmltools::htmlEscape(to_windows1252_safe(dados_NV_AAUBS$ENDRES[i])), "<br/>",
                                  "<b>Número:</b> ", htmltools::htmlEscape(dados_NV_AAUBS$NUMRES[i]), "<br/>",
                                  "<b>Complemento:</b> ", htmltools::htmlEscape(to_windows1252_safe(dados_NV_AAUBS$COMPLRES[i])), "<br/>",
                                  "<b>Distrito:</b> ", htmltools::htmlEscape(to_windows1252_safe(dados_NV_AAUBS$BAIRES[i])), "<br/>",
                                  "<b>CEP residencial:</b> ", htmltools::htmlEscape(dados_NV_AAUBS$CEPRES[i]), "<br/>",
                                  "<b>Coordenadoria Regional de Saúde:</b> ", htmltools::htmlEscape(to_windows1252_safe(dados_NV_AAUBS$CRS_UBS[i])), "<br/>",
                                  "<b>Supervisão Técnica de Saúde:</b> ", htmltools::htmlEscape(to_windows1252_safe(dados_NV_AAUBS$STS_UBS[i])), "<br/>",
                                  "<b>Área de abrangência:</b> ", htmltools::htmlEscape(to_windows1252_safe(dados_NV_AAUBS$NOMEUBS[i])), "<br/>",
                                  "<b>Semanas de gestação:</b> ", htmltools::htmlEscape(to_windows1252_safe(Gestacao_val)), "<br/>",
                                  "<b>Mês de gestação de Início do PN:</b> ", htmltools::htmlEscape(to_windows1252_safe(paste(dados_NV_AAUBS$MESPRENAT[i],"º"))), "<br/>",
                                  "<b>Consultas PN:</b> ", htmltools::htmlEscape(dados_NV_AAUBS$CONSULTAS[i]), "<br/>",
                                  "<b>Qtde filho(s) vivo(s):</b> ", htmltools::htmlEscape(dados_NV_AAUBS$QTDFILVIVO[i]), "<br/>",
                                  "<b>Qtde filho(s) morto(s):</b> ", htmltools::htmlEscape(dados_NV_AAUBS$QTDFILMORT[i]), "<br/>",
                                  "<b>Tipo de parto:</b> ", htmltools::htmlEscape(to_windows1252_safe(Tipo_parto_val)), "<br/>",
                                  "<b>Parto em estab.SUS:</b> ", htmltools::htmlEscape(to_windows1252_safe(Parto_SUS_val)), "<br/>",
                                  "<b>Peso ao nascer:</b> ", htmltools::htmlEscape(paste(dados_NV_AAUBS$PESO[i], " gramas")), "<br/>",
                                  "<b>APGAR no 1º minuto:</b> ", htmltools::htmlEscape(dados_NV_AAUBS$APGAR1[i]), "<br/>",
                                  "<b>APGAR no 5º minuto:</b> ", htmltools::htmlEscape(dados_NV_AAUBS$APGAR5[i]), "<br/>",
                                  "<b>Anomalia:</b> ", htmltools::htmlEscape(to_windows1252_safe(Anomalia_val)), " ", htmltools::htmlEscape(to_windows1252_safe(Anomalia_CID_val)), "<br/>",
                                  "<b>Data nascimento:</b> ", htmltools::htmlEscape(data_formatada), "<br/>",
                                  "<b>Hora nascimento:</b> ", htmltools::htmlEscape(hora_com_h)
                                ))
                              }),
                              group = "Nascidos Vivos (clique no ponto)") %>%
    
    
    # Adiciona controles de camada
    leaflet::addLayersControl(
      baseGroups = c("Mapa-base de vias (cinza) | Carto DB", "Mapa-base de vias | OpenStreetMap", "Imagem de satélite | Esri World Imagery"),
      
      
      
      
      overlayGroups = c(
        "Coordenadoria Regional de Saúde (CRS)",
        "Supervisão Técnica de Saúde (STS)",
        "Área de Abrangência de UBS (AAUBS)",
        "Mapa de calor - Nascidos Vivos Gerais",
        "Mapa de calor - Nascidos Vivos de Risco",
        "Nascidos Vivos (clique no ponto)",
        "Nascidos Vivos de Risco",
        "Pré-Termo (<37 semanas)",
        "Baixo Peso (<2500g)",
        "Presença de Anomalia",
        "Pré-natal incompleto (<7 consultas)",
        "Parturientes Adolescentes (<=19 anos)",
        "Parturientes Estrangeiras",
        "Parto Cesário",
        "Parto em Estabelecimento SUS",
        "Vulnerabilidade Social (IPVS)"
      ),
      options = leaflet::layersControlOptions(collapsed = FALSE),
      position = "topright" # POSICIONAMENTO DO CONTROLE DE CAMADAS
    ) %>%
    
    # Define visibilidade inicial (hideGroup)
    leaflet::hideGroup("Mapa de calor - Nascidos Vivos Gerais") %>%
    #leaflet::hideGroup("Nascidos Vivos de Risco") %>%
    leaflet::hideGroup("Pré-Termo (<37 semanas)") %>%
    leaflet::hideGroup("Baixo Peso (<2500g)") %>%
    leaflet::hideGroup("Presença de Anomalia") %>%
    leaflet::hideGroup("Pré-natal incompleto (<7 consultas)") %>%
    leaflet::hideGroup("Parturientes Adolescentes (<=19 anos)") %>%
    leaflet::hideGroup("Parturientes Estrangeiras") %>%
    leaflet::hideGroup("Vulnerabilidade Social (IPVS)") %>%
    leaflet::hideGroup("Parto em Estabelecimento SUS") %>%
    leaflet::hideGroup("Parto Cesário") %>%
    
    # Adiciona legenda do IPVS
    leaflet::addLegend(
      position = "bottomright", # POSICIONAMENTO DA LEGENDA IPVS
      colors = colors_ipvs_ordenadas,
      labels = descricoes_ipvs_ordenadas,
      title = "Índice Paulista de<br/>Vulnerabilidade Social (IPVS)",
      opacity = 1,
      group = "Vulnerabilidade Social (IPVS)"
    ) %>%
    htmlwidgets::onRender("
                           function(el, x) {
                           document.title = 'Nascidos Vivos no Município de São Paulo';
                           }
                           ")
  
  message("Adicionando elementos HTML (banner, rodapé, favicon) ao HTML final do mapa...")
  
  nome_mapa_dinamico <- paste0("Mapa_Dinamico_Nascidos_Vivos_MSP_SMS_SP_", resultados_metricas$COMP_MESANO, ".html")
  nome_mapa_dinamico <- stringr::str_trim(nome_mapa_dinamico)
  
  caminho_mapa_final <- file.path(BASE_OUTPUT_PATH, resultados_metricas$COMP_MESANO, "MSP", nome_mapa_dinamico)
  
  # Salva o mapa Leaflet puro primeiro
  htmlwidgets::saveWidget(widget = mapa_leaflet_final, file = caminho_mapa_final, selfcontained = TRUE)
  
  # LÊ o HTML salvo forçando a codificação windows-1252
  html_content <- readLines(con = caminho_mapa_final, warn = FALSE, encoding = "windows-1252")
  
  # Encontra a linha onde </head> está e insere o favicon e o meta charset.
  head_end_line <- which(grepl("</head>", html_content, ignore.case = TRUE))[1]
  if (!is.na(head_end_line)) {
    # CSS para ajustar posi??o de controles e legendas.
    # A altura do seu banner ? 'auto', o que dificulta um c?lculo exato.
    # A altura do rodap? com font-size 12px ? de cerca de 30-40px.
    # Se o banner tiver uns 100px de altura, o top dos controles tem que ser maior.
    # Vamos usar pixels para uma estimativa inicial, que voc? pode ajustar.
    css_custom_positions <- '
    <style>
      /* Ajusta a posi??o da ?rea de controle superior direita (onde est? o addLayersControl) */
      .leaflet-top.leaflet-right {
        top: 100px !important; /* Ajuste este valor (altura do banner + respiro) */
      }
      /* Ajusta a posi??o da ?rea de controle inferior esquerda (onde est? a legenda IPVS) */
      .leaflet-bottom.leaflet-left {
        bottom: 50px !important; /* Ajuste este valor (altura do rodap? + respiro) */
      }
      /* Se a legenda do Leaflet tivesse um ID ou classe espec?fica, poder?amos ser mais espec?ficos,
         mas .leaflet-bottom.leaflet-left j? ? o cont?iner direto. */
    </style>
    '
    html_content <- c(
      html_content[1:(head_end_line-1)],
      '<meta charset="windows-1252">',
      paste0('<link rel="icon" href="', url_favicon, '" type="image/png">'),
      css_custom_positions, # INJETAR O CSS CUSTOMIZADO AQUI
      html_content[head_end_line:length(html_content)]
    )
  }
  
  # Encontra a linha onde <body> começa e insere o banner depois
  body_start_line <- which(grepl("<body", html_content, ignore.case = TRUE))[1]
  if (!is.na(body_start_line)) {
    banner_html_code <- paste0('<img src="', url_banner, '" style="width: 100%; height: auto; position: absolute; top: 0; left: 0; z-index: 1000;">')
    html_content <- c(
      html_content[1:body_start_line],
      banner_html_code,
      html_content[(body_start_line+1):length(html_content)]
    )
  }
  
  # Encontra a linha onde </body> começa e insere o rodapé antes
  body_end_line <- which(grepl("</body>", html_content, ignore.case = TRUE))[1]
  
  rodape_texto_base_raw <- sprintf(
    " | Competência dos dados (mês-ano): %s<br/>", resultados_metricas$COMP_MESANO
  )
  rodape_texto_parte2_raw <- paste0(
    " | Fontes: ",
    "<a href=\"https://www.prefeitura.sp.gov.br/cidade/secretarias/saude/epidemiologia_e_informacao/nascidos_vivos/\" target=\"_blank\">Nascidos vivos - SINASC / CEInfo / SMS-SP</a> | ",
    "<a href=\"https://ipvs.seade.gov.br/view/index.php\" target=\"_blank\">Índice Paulista de Vulnerabilidade Social 2010 - F. SEADE de São Paulo</a> | ",
    "<a href=\"https://www.prefeitura.sp.gov.br/cidade/secretarias/saude/epidemiologia_e_informacao/geoprocessamento_e_informacoes_socioambientais/index.php?p=265388\" target=\"_blank\">Camadas geográficas - Secretaria Municipal da Saúde de São Paulo - 2022</a><br/>",
    " | Elaboração: ",
    "<a href=\"https://www.prefeitura.sp.gov.br/cidade/secretarias/saude/epidemiologia_e_informacao/geoprocessamento_e_informacoes_socioambientais/\">Núcleo de Geoprocessamento e Informação Socioambiental - Coordenação de Epidemiologia e Informação - Secretaria Municipal da Saúde de São Paulo - 2024</a>"
  )
  
  to_windows1252_safe <- function(s) { # Fun??o auxiliar definida aqui
    if(is.character(s)) {
      tryCatch(
        iconv(s, from = "UTF-8", to = "windows-1252//TRANSLIT//IGNORE"),
        error = function(e) {
          warning(paste("Erro de convers?o de codifica??o:", s, "-", e$message))
          return(s)
        }
      )
    } else {
      return(s)
    }
  }
  
  rodape_texto_base_converted <- to_windows1252_safe(rodape_texto_base_raw)
  rodape_texto_parte2_converted <- to_windows1252_safe(rodape_texto_parte2_raw)
  
  rodape_html_full <- sprintf('
    <footer style="width: 100%%; font-size: 12px; background-color: #FFFFFF; color: #000000; margin: 0; padding: 0; border: none; position: absolute; bottom: 0; left: 0; right: 0;">
        <p style="width: 100%%; text-align: left;">%s%s</p>
    </footer>
    ', rodape_texto_base_converted, rodape_texto_parte2_converted)
  
  
  if (!is.na(body_end_line)) {
    html_content <- c(
      html_content[1:(body_end_line-1)],
      rodape_html_full,
      html_content[body_end_line:length(html_content)]
    )
  }
  
  writeLines(text = html_content, con = caminho_mapa_final)
  message(paste("Mapa dinâmico Leaflet salvo em:", caminho_mapa_final))
}


# --- Execução da função de geração do mapa dinâmico Leaflet ---
gerar_mapa_dinamico_leaflet(
  resultados_metricas = resultados_metricas,
  dados_brutos_processados = dados_brutos_processados,
  url_banner = URL_BANNER_MAPA,
  url_favicon = URL_FAVICON_MAPA
)

# --- Fim do passo 7 ---
