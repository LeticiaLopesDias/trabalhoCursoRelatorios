## Preparar dados de desmatamento em Unidades de Conservação

library(dplyr)

# Baixar dados das Unidades de Conservação

## Download
download.file(url = "http://dados.mma.gov.br/dataset/44b6dc8a-dc82-4a84-8d95-1b0da7c85dac/resource/fed217ff-fe75-4744-9663-0a06157d9c3b/download/cnuc_2022_01sem.csv",
              destfile = here::here("data-raw/cnuc.csv"), mode = "wb")

cnuc <- read.csv2("data-raw/cnuc.csv")

glimpse(cnuc)

cnuc_tratado <- cnuc %>%
  janitor::clean_names() %>%
  select(codigo_uc, esfera_administrativa,
         nome_da_uc,
         categoria_de_manejo,
         uf,
         area_soma_biomas)

# Não consegui baixar pelo R os dados de desmatamento, mas estão disponíveis no link:
# http://terrabrasilis.dpi.inpe.br/app/dashboard/deforestation/biomes/legal_amazon/increments

prodes <- read.csv2("data-raw/terrabrasilis_legal_amazon_20_6_2022_1658328157652.csv",
                    encoding = "UTF-8")
glimpse(prodes)

# Juntar dados de desmatamento com os dados de UCs
prodes_cnuc <- left_join(prodes, cnuc_tratado, by = c("consunit" = "nome_da_uc"))

# Algumas unidades não se uniram pelo nome
sem_codigo <- prodes_cnuc %>%
  filter(is.na(codigo_uc))

unique(sem_codigo$consunit)
# "ÁREA DE PROTEçãO AMBIENTAL DA BAIXADA MARANHENSE"
# "ÁREA DE PROTEçãO AMBIENTAL DE UPAON-AçU / MIRITIBA / ALTO PREGUIçAS"

cnuc %>%
  filter(UF == "MA") %>% View()
# grafia:
# ÁREA DE PROTEÇÃO AMBIENTAL DA BAIXADA MARANHENSE
# ÁREA DE PROTEÇÃO AMBIENTAL DE UPAON-AÇU / MIRITIBA / ALTO PREGUIÇAS


prodes <- prodes %>%
  mutate(consunit = case_when(consunit == "ÁREA DE PROTEçãO AMBIENTAL DA BAIXADA MARANHENSE" ~ "ÁREA DE PROTEÇÃO AMBIENTAL DA BAIXADA MARANHENSE",
                              consunit == "ÁREA DE PROTEçãO AMBIENTAL DE UPAON-AçU / MIRITIBA / ALTO PREGUIçAS" ~ "ÁREA DE PROTEÇÃO AMBIENTAL DE UPAON-AÇU / MIRITIBA / ALTO PREGUIÇAS",
                              TRUE ~ consunit))

# Juntar novamente
prodes_cnuc <- left_join(prodes, cnuc_tratado, by = c("consunit" = "nome_da_uc")) %>%
 rename(desmatamento_km2 = `area.km²`,
        uc = consunit,
        ano = year) %>%
  mutate(area_uc_km2 = as.double(stringr::str_replace_all(area_soma_biomas, "[.]", ""))/100) %>%
  select(-area_soma_biomas)

prodes_cnuc %>%
  filter(is.na(codigo_uc))
# Agora foi certinho

glimpse(prodes_cnuc)

writexl::write_xlsx(prodes_cnuc, "data/dados_desmatamento.xlsx")
