########## working diretory
setwd("~/Desktop/MSDE/DPV/project/DataWrangling")

# libraries
library (tidyverse)
library (dplyr)
library (easypackages)
library (readr)
library (tidyr)
library(readxl)
library(openxlsx)



#### import dataset - Produção de resíduos 
#(Em que países as famílias e as empresas produzem mais e menos lixo?) (ton)

waste_prod <- read_xlsx(("ProducaoTotalResiduos.xlsx"), col_names = TRUE)
waste_prod$AE04 <- as.numeric( waste_prod$AE04 )
               
# print columns
#waste_prod$Países
#waste_prod$Total2004
#waste_prod$Total2020
#waste_prod$AE2004         ----- JÁ NÃO TÊM ESTE NOME!!!! 
#waste_prod$AE2020
#waste_prod$AG2004
#waste_prod$AG2020

# turn data vertical
wp00 <- waste_prod %>% 
  select(countries, T00, AE00, AD00) %>%
  rename(total = T00 , res_atv_economica = AE00, res_agg_domestico = AD00 ) %>% 
  mutate(ano=2000)

wp01 <- waste_prod %>% 
  select(countries, T01, AE01, AD01) %>%
  rename(total = T01 , res_atv_economica = AE01, res_agg_domestico = AD01 ) %>% 
  mutate(ano=2001)

wp02 <- waste_prod %>% 
  select(countries, T02, AE02, AD02) %>%
  rename(total = T02 , res_atv_economica = AE02, res_agg_domestico = AD02 ) %>% 
  mutate(ano=2002)

wp03 <- waste_prod %>% 
  select(countries, T03, AE03, AD03) %>%
  rename(total = T03 , res_atv_economica = AE03, res_agg_domestico = AD03 ) %>% 
  mutate(ano=2003)

wp04 <- waste_prod %>% 
  select(countries, T04, AE04, AD04) %>%
  rename(total = T04 , res_atv_economica = AE04, res_agg_domestico = AD04 ) %>% 
  mutate(ano=2004)

wp05 <- waste_prod %>% 
  select(countries, T05, AE05, AD05) %>%
  rename(total = T05 , res_atv_economica = AE05, res_agg_domestico = AD05 ) %>% 
  mutate(ano=2005)

wp06 <- waste_prod %>%
  select(countries, T06, AE06, AD06) %>%
  rename(total = T06 , res_atv_economica = AE06, res_agg_domestico = AD06 ) %>% 
  mutate(ano=2006)

wp07 <- waste_prod %>% 
  select(countries, T07, AE07, AD07) %>%
  rename(total = T07 , res_atv_economica = AE07, res_agg_domestico = AD07 ) %>% 
  mutate(ano=2007)

wp08 <- waste_prod %>%
  select(countries, T08, AE08, AD08) %>%
  rename(total = T08 , res_atv_economica = AE08, res_agg_domestico = AD08 ) %>% 
  mutate(ano=2008)

wp09 <- waste_prod %>% 
  select(countries, T09, AE09, AD09) %>%
  rename(total = T09 , res_atv_economica = AE09, res_agg_domestico = AD09 ) %>% 
  mutate(ano=2009)

wp10 <- waste_prod %>%
  select(countries, T10, AE10, AD10) %>%
  rename(total = T10 , res_atv_economica = AE10, res_agg_domestico = AD10 ) %>% 
  mutate(ano=2010)

wp11 <- waste_prod %>% 
  select(countries, T11, AE11, AD11) %>%
  rename(total = T11 , res_atv_economica = AE11, res_agg_domestico = AD11 ) %>% 
  mutate(ano=2011)

wp12 <- waste_prod %>%
  select(countries, T12, AE12, AD12) %>%
  rename(total = T12 , res_atv_economica = AE12, res_agg_domestico = AD12 ) %>% 
  mutate(ano=2012)

wp13 <- waste_prod %>% 
  select(countries, T13, AE13, AD13) %>%
  rename(total = T13 , res_atv_economica = AE13, res_agg_domestico = AD13 ) %>% 
  mutate(ano=2013)

wp14 <- waste_prod %>%
  select(countries, T14, AE14, AD14) %>%
  rename(total = T14 , res_atv_economica = AE14, res_agg_domestico = AD14 ) %>% 
  mutate(ano=2014)

wp15 <- waste_prod %>% 
  select(countries, T15, AE15, AD15) %>%
  rename(total = T15 , res_atv_economica = AE15, res_agg_domestico = AD15 ) %>% 
  mutate(ano=2015)

wp16 <- waste_prod %>%
  select(countries, T16, AE16, AD16) %>%
  rename(total = T16 , res_atv_economica = AE16, res_agg_domestico = AD16 ) %>% 
  mutate(ano=2016)

wp17 <- waste_prod %>% 
  select(countries, T17, AE17, AD17) %>%
  rename(total = T17 , res_atv_economica = AE17, res_agg_domestico = AD17 ) %>% 
  mutate(ano=2017)

wp18 <- waste_prod %>%
  select(countries, T18, AE18, AD18) %>%
  rename(total = T18 , res_atv_economica = AE18, res_agg_domestico = AD18 ) %>% 
  mutate(ano=2018)

wp19 <- waste_prod %>% 
  select(countries, T19, AE19, AD19) %>%
  rename(total = T19 , res_atv_economica = AE19, res_agg_domestico = AD19 ) %>% 
  mutate(ano=2019)

wp20 <- waste_prod %>%
  select(countries, T20, AE20, AD20) %>%
  rename(total = T20 , res_atv_economica = AE20, res_agg_domestico = AD20 ) %>% 
  mutate(ano=2020)

wp21 <- waste_prod %>% 
  select(countries, T21, AE21, AD21) %>%
  rename(total = T21 , res_atv_economica = AE21, res_agg_domestico = AD21 ) %>% 
  mutate(ano=2021)

wp_vert <- bind_rows(wp00,wp01,wp02,wp03,wp04,wp05,wp06,wp07,wp08,wp09,wp10,wp11,wp12,wp13
                     ,wp14,wp15,wp16,wp17,wp18,wp19,wp20,wp21)
wp_vert <- wp_vert %>% 
  rename(TotalResiduos = total, ResiduosAtivEconomica = res_atv_economica, ResiduosAggDomesticos = res_agg_domestico, 
         Ano = ano, Countries = countries)

#### import dataset - Tx. de Reciclagem de Resíduos Municipais - 
#Em que países há maior e menor percentegam de lixo, produzido pelos municípios, que é reciclado?

rec_lw <- read_xlsx(("TaxaReciclagemResiduosMunicipais.xlsx"), col_names = TRUE)

#turn data vertical

rlw00 <- rec_lw%>% 
  select(countries, T00) %>%
  rename(total = T00)%>% 
  mutate(ano=2000)

rlw01 <- rec_lw%>% 
  select(countries, T01) %>%
  rename(total = T01)%>% 
  mutate(ano=2001)

rlw02 <- rec_lw%>% 
  select(countries, T02) %>%
  rename(total = T02)%>% 
  mutate(ano=2002)

rlw03 <- rec_lw%>% 
  select(countries, T03) %>%
  rename(total = T03)%>% 
  mutate(ano=2003)

rlw04 <- rec_lw%>% 
  select(countries, T04) %>%
  rename(total = T04)%>% 
  mutate(ano=2004)

rlw05 <- rec_lw%>% 
  select(countries, T05) %>%
  rename(total = T05)%>% 
  mutate(ano=2005)

rlw06 <- rec_lw%>% 
  select(countries, T06) %>%
  rename(total = T06)%>% 
  mutate(ano=2006)

rlw07 <- rec_lw%>% 
  select(countries, T07) %>%
  rename(total = T07)%>% 
  mutate(ano=2007)

rlw08 <- rec_lw%>% 
  select(countries, T08) %>%
  rename(total = T08)%>% 
  mutate(ano=2008)

rlw09 <- rec_lw%>% 
  select(countries, T09) %>%
  rename(total = T09)%>% 
  mutate(ano=2009)

rlw10 <- rec_lw%>% 
  select(countries, T10) %>%
  rename(total = T10)%>% 
  mutate(ano=2010)

rlw11 <- rec_lw%>% 
  select(countries, T11) %>%
  rename(total = T11)%>% 
  mutate(ano=2011)

rlw12 <- rec_lw%>% 
  select(countries, T12) %>%
  rename(total = T12)%>% 
  mutate(ano=2012)

rlw13 <- rec_lw%>% 
  select(countries, T13) %>%
  rename(total = T13)%>% 
  mutate(ano=2013)

rlw14 <- rec_lw%>% 
  select(countries, T14) %>%
  rename(total = T14)%>% 
  mutate(ano=2014)

rlw15 <- rec_lw%>% 
  select(countries, T15) %>%
  rename(total = T15)%>% 
  mutate(ano=2015)

rlw16 <- rec_lw%>% 
  select(countries, T16) %>%
  rename(total = T16)%>% 
  mutate(ano=2016)

rlw17 <- rec_lw%>% 
  select(countries, T17) %>%
  rename(total = T17)%>% 
  mutate(ano=2017)

rlw18 <- rec_lw%>% 
  select(countries, T18) %>%
  rename(total = T18)%>% 
  mutate(ano=2018)

rlw19 <- rec_lw%>% 
  select(countries, T19) %>%
  rename(total = T19)%>% 
  mutate(ano=2019)

rlw20 <- rec_lw%>% 
  select(countries, T20) %>%
  rename(total = T20)%>% 
  mutate(ano=2020)

rlw21 <- rec_lw%>% 
  select(countries, T21) %>%
  rename(total = T21)%>% 
  mutate(ano=2021)

rlw_vert <- bind_rows(rlw00, rlw01, rlw02, rlw03, rlw04, rlw05, rlw06, rlw07, rlw08,
                      rlw09, rlw10, rlw11, rlw12, rlw13, rlw14, rlw15, rlw16, rlw17, rlw18, rlw19, rlw20, rlw21)
rlw_vert <- rlw_vert %>% 
  rename(TaxaReciclagemResiduosMunicipais = total)

#### import dataset - Taxa de Utilização de Mat. Circular - Que países reutilizam maior e 
#menor percentagem de matérias-primas na sua economia? (%)

rec_waste <- read_xlsx(("TaxaUtilizacaoMaterialCircular.xlsx"), col_names = TRUE)

#turn data vertical

rc00 <- rec_waste %>% 
  select(countries, T00) %>%
  rename(total = T00)%>% 
  mutate(ano=2000)

rc01 <- rec_waste %>% 
  select(countries, T01) %>%
  rename(total = T01)%>% 
  mutate(ano=2001)

rc02 <- rec_waste %>% 
  select(countries, T02) %>%
  rename(total = T02)%>% 
  mutate(ano=2002)

rc03 <- rec_waste %>% 
  select(countries, T03) %>%
  rename(total = T03)%>% 
  mutate(ano=2003)

rc04 <- rec_waste %>% 
  select(countries, T04) %>%
  rename(total = T04)%>% 
  mutate(ano=2004)

rc05 <- rec_waste %>% 
  select(countries, T05) %>%
  rename(total = T05)%>% 
  mutate(ano=2005)

rc06 <- rec_waste %>% 
  select(countries, T06) %>%
  rename(total = T06)%>% 
  mutate(ano=2006)

rc07 <- rec_waste %>% 
  select(countries, T07) %>%
  rename(total = T07)%>% 
  mutate(ano=2007)

rc08 <- rec_waste %>% 
  select(countries, T08) %>%
  rename(total = T08)%>% 
  mutate(ano=2008)

rc09 <- rec_waste %>% 
  select(countries, T09) %>%
  rename(total = T09)%>% 
  mutate(ano=2009)

rc10 <- rec_waste %>% 
  select(countries, T10) %>%
  rename(total = T10)%>% 
  mutate(ano=2010)

rc11 <- rec_waste %>% 
  select(countries, T11) %>%
  rename(total = T11)%>% 
  mutate(ano=2011)

rc12 <- rec_waste %>% 
  select(countries, T12) %>%
  rename(total = T12)%>% 
  mutate(ano=2012)

rc13 <- rec_waste %>% 
  select(countries, T13) %>%
  rename(total = T13)%>% 
  mutate(ano=2013)

rc14 <- rec_waste %>% 
  select(countries, T14) %>%
  rename(total = T14)%>% 
  mutate(ano=2014)

rc15 <- rec_waste %>% 
  select(countries, T15) %>%
  rename(total = T15)%>% 
  mutate(ano=2015)

rc16 <- rec_waste %>% 
  select(countries, T16) %>%
  rename(total = T16)%>% 
  mutate(ano=2016)

rc17 <- rec_waste %>% 
  select(countries, T17) %>%
  rename(total = T17)%>% 
  mutate(ano=2017)

rc18 <- rec_waste %>% 
  select(countries, T18) %>%
  rename(total = T18)%>% 
  mutate(ano=2018)

rc19 <- rec_waste %>% 
  select(countries, T19) %>%
  rename(total = T19)%>% 
  mutate(ano=2019)

rc20 <- rec_waste %>% 
  select(countries, T20) %>%
  rename(total = T20)%>% 
  mutate(ano=2020)

rc21 <- rec_waste %>% 
  select(countries, T21) %>%
  rename(total = T21)%>% 
  mutate(ano=2021)

rc_vert <- bind_rows(rc00, rc01, rc02,rc03, rc04, rc05, rc06, rc07, rc08, rc09, rc10,
                     rc11, rc12, rc13, rc14, rc15, rc16, rc17, rc18, rc19, rc20, rc21)

rc_vert <- rc_vert %>% 
  rename(TaxaUtilizacaoMaterialCircular = total, Ano = ano, Countries = countries)

#### Import dataset - PIB - Quanta riqueza é criada? - (Euros-Milhares)

pib <- read_xlsx(("EuroPIB.xlsx"), col_names = TRUE)

#turn data vertical

pib00 <- pib %>% 
  select(countries, P00) %>% 
  rename(PIB = P00) %>% 
  mutate(ano=2000)

pib01 <- pib %>% 
  select(countries, P01) %>% 
  rename(PIB = P01) %>% 
  mutate(ano=2001)

pib02 <- pib %>% 
  select(countries, P02) %>% 
  rename(PIB = P02) %>% 
  mutate(ano=2002)

pib03 <- pib %>% 
  select(countries, P03) %>% 
  rename(PIB = P03) %>% 
  mutate(ano=2003)

pib04 <- pib %>% 
  select(countries, P04) %>% 
  rename(PIB = P04) %>% 
  mutate(ano=2004)

pib05 <- pib %>% 
  select(countries, P05) %>% 
  rename(PIB = P05) %>% 
  mutate(ano=2005)

pib06 <- pib %>% 
  select(countries, P06) %>% 
  rename(PIB = P06) %>% 
  mutate(ano=2006)

pib06 <- pib %>% 
  select(countries, P06) %>% 
  rename(PIB = P06) %>% 
  mutate(ano=2006)

pib07 <- pib %>% 
  select(countries, P07) %>% 
  rename(PIB = P07) %>% 
  mutate(ano=2007)

pib07 <- pib %>% 
  select(countries, P07) %>% 
  rename(PIB = P07) %>% 
  mutate(ano=2007)

pib08 <- pib %>% 
  select(countries, P08) %>% 
  rename(PIB = P08) %>% 
  mutate(ano=2008)

pib09 <- pib %>% 
  select(countries, P09) %>% 
  rename(PIB = P09) %>% 
  mutate(ano=2009)

pib10 <- pib %>% 
  select(countries, P10) %>% 
  rename(PIB = P10) %>% 
  mutate(ano=2010)

pib11 <- pib %>% 
  select(countries, P11) %>% 
  rename(PIB = P11) %>% 
  mutate(ano=2011)

pib12 <- pib %>% 
  select(countries, P12) %>% 
  rename(PIB = P12) %>% 
  mutate(ano=2012)

pib13 <- pib %>% 
  select(countries, P13) %>% 
  rename(PIB = P13) %>% 
  mutate(ano=2013)

pib14 <- pib %>% 
  select(countries, P14) %>% 
  rename(PIB = P14) %>% 
  mutate(ano=2014)

pib15 <- pib %>% 
  select(countries, P15) %>% 
  rename(PIB = P15) %>% 
  mutate(ano=2015)

pib16 <- pib %>% 
  select(countries, P16) %>% 
  rename(PIB = P16) %>% 
  mutate(ano=2016)

pib17 <- pib %>% 
  select(countries, P17) %>% 
  rename(PIB = P17) %>% 
  mutate(ano=2017)

pib18 <- pib %>% 
  select(countries, P18) %>% 
  rename(PIB = P18) %>% 
  mutate(ano=2018)

pib19 <- pib %>% 
  select(countries, P19) %>% 
  rename(PIB = P19) %>% 
  mutate(ano=2019)

pib20 <- pib %>% 
  select(countries, P20) %>% 
  rename(PIB = P20) %>% 
  mutate(ano=2020)

pib21 <- pib %>% 
  select(countries, P21) %>% 
  rename(PIB = P21) %>% 
  mutate(ano=2021)

pib_vert <- bind_rows(pib00, pib01, pib02, pib03, pib04, pib05, pib06, pib07, pib08, pib09, pib10,
                      pib11, pib12, pib13, pib14, pib15, pib16, pib17, pib18, pib19, pib20, pib21)

#### Import dataset - Taxa de crescimento real do PIB - 
#Quanto cresce a riqueza criada, a preços constantes? (Tx de variação (%))

tx_pib <- read_xlsx(("TaxaCrescimentoRealPIB.xlsx"), col_names = TRUE)

#turn data vertical

tx_pib00 <- tx_pib %>% 
  select(countries, T00) %>% 
  rename(TaxaCrescimentoPIB = T00) %>% 
  mutate(ano=2000)

tx_pib01 <- tx_pib %>% 
  select(countries, T01) %>% 
  rename(TaxaCrescimentoPIB = T01) %>% 
  mutate(ano=2001)

tx_pib02 <- tx_pib %>% 
  select(countries, T02) %>% 
  rename(TaxaCrescimentoPIB = T02) %>% 
  mutate(ano=2002)

tx_pib03 <- tx_pib %>% 
  select(countries, T03) %>% 
  rename(TaxaCrescimentoPIB = T03) %>% 
  mutate(ano=2003)

tx_pib04 <- tx_pib %>% 
  select(countries, T04) %>% 
  rename(TaxaCrescimentoPIB = T04) %>% 
  mutate(ano=2004)

tx_pib05 <- tx_pib %>% 
  select(countries, T05) %>% 
  rename(TaxaCrescimentoPIB = T05) %>% 
  mutate(ano=2005)

tx_pib06 <- tx_pib %>% 
  select(countries, T06) %>% 
  rename(TaxaCrescimentoPIB = T06) %>% 
  mutate(ano=2006)

tx_pib07 <- tx_pib %>% 
  select(countries, T07) %>% 
  rename(TaxaCrescimentoPIB = T07) %>% 
  mutate(ano=2007)

tx_pib08 <- tx_pib %>% 
  select(countries, T08) %>% 
  rename(TaxaCrescimentoPIB = T08) %>% 
  mutate(ano=2008)

tx_pib09 <- tx_pib %>% 
  select(countries, T09) %>% 
  rename(TaxaCrescimentoPIB = T09) %>% 
  mutate(ano=2009)

tx_pib10 <- tx_pib %>% 
  select(countries, T10) %>% 
  rename(TaxaCrescimentoPIB = T10) %>% 
  mutate(ano=2010)

tx_pib11 <- tx_pib %>% 
  select(countries, T11) %>% 
  rename(TaxaCrescimentoPIB = T11) %>% 
  mutate(ano=2011)

tx_pib12 <- tx_pib %>% 
  select(countries, T12) %>% 
  rename(TaxaCrescimentoPIB = T12) %>% 
  mutate(ano=2012)

tx_pib13 <- tx_pib %>% 
  select(countries, T13) %>% 
  rename(TaxaCrescimentoPIB = T13) %>% 
  mutate(ano=2013)

tx_pib13 <- tx_pib %>% 
  select(countries, T13) %>% 
  rename(TaxaCrescimentoPIB = T13) %>% 
  mutate(ano=2013)

tx_pib14 <- tx_pib %>% 
  select(countries, T14) %>% 
  rename(TaxaCrescimentoPIB = T14) %>% 
  mutate(ano=2014)

tx_pib15 <- tx_pib %>% 
  select(countries, T15) %>% 
  rename(TaxaCrescimentoPIB = T15) %>% 
  mutate(ano=2015)

tx_pib16 <- tx_pib %>% 
  select(countries, T16) %>% 
  rename(TaxaCrescimentoPIB = T16) %>% 
  mutate(ano=2016)

tx_pib17 <- tx_pib %>% 
  select(countries, T17) %>% 
  rename(TaxaCrescimentoPIB = T17) %>% 
  mutate(ano=2017)

tx_pib18 <- tx_pib %>% 
  select(countries, T18) %>% 
  rename(TaxaCrescimentoPIB = T18) %>% 
  mutate(ano=2018)

tx_pib19 <- tx_pib %>% 
  select(countries, T19) %>% 
  rename(TaxaCrescimentoPIB = T19) %>% 
  mutate(ano=2019)

tx_pib20 <- tx_pib %>% 
  select(countries, T20) %>% 
  rename(TaxaCrescimentoPIB = T20) %>% 
  mutate(ano=2020)

tx_pib21 <- tx_pib %>% 
  select(countries, T21) %>% 
  rename(TaxaCrescimentoPIB = T21) %>% 
  mutate(ano=2021)

tx_pib_vert <- bind_rows(tx_pib00, tx_pib01, tx_pib02, tx_pib03, tx_pib04, tx_pib05, tx_pib06, tx_pib07, tx_pib08, tx_pib09,
                         tx_pib10, tx_pib11, tx_pib12, tx_pib13, tx_pib14, tx_pib15, tx_pib16, tx_pib17, tx_pib18, tx_pib19, tx_pib20,
                         tx_pib21)


################################# FINAL DATASET ##########################################

ds_raw <- bind_cols(wp_vert, rc_vert, rlw_vert, pib_vert, tx_pib_vert)

ds <- ds_raw %>% 
  select(Countries...1, TotalResiduos, ResiduosAtivEconomica, ResiduosAggDomesticos, TaxaReciclagemResiduosMunicipais,
         TaxaUtilizacaoMaterialCircular, PIB, TaxaCrescimentoPIB, Ano...5) %>% 
  rename(Paises = Countries...1, Ano = Ano...5 )

head_line <- createStyle(halign = "center", textDecoration = "Bold")

write.xlsx(ds, "EconomiaCircular.xlsx",  headerStyle = head_line)


  
  