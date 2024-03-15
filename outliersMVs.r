setwd("~/Desktop/MSDE/DPV/project")

library(tidyr)
library(readxl)
library ( openintro )  
library ( ggplot2 ) 
library( GGally )
library( dplyr )

n_ds <- read_xlsx("EconomiaCircular.xlsx")
dim(n_ds)

n_ds <- n_ds %>% drop_na()
dim(n_ds)



# create detect outlier function
detect_outlier <- function(x) {
  
  # calculate first quantile
  Quantile1 <- quantile(x, probs=.25)
  
  # calculate third quantile
  Quantile3 <- quantile(x, probs=.75)
  
  # calculate inter quartile range
  IQR = Quantile3-Quantile1
  
  # return true or false
  x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
}

# create remove outlier function
remove_outlier <- function(dataframe,
                            columns=names(dataframe)) {
  
  # for loop to traverse in columns vector
  for (col in columns) {
    
    # remove observation if it satisfies outlier function
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  
  # return dataframe
  print("Remove outliers")
  print(dataframe)
}

#n_ds <- remove_outlier(ds,c('TotalResiduos','ResiduosAtivEconomica','ResiduosAggDomesticos','TaxaReciclagemResiduosMunicipais','TaxaUtilizacaoMaterialCircular','PIB','©'))
#dim(n_ds)



min_max <- function( x , new_min , new_max )
{
  if( missing (new_min ) ) new_min <- 0 ## set default value for the parameter
  if( missing (new_max ) ) new_max <- 50
  return( ( x - min(x) ) / (max( x )-min( x ) ) * ( new_max - new_min) + new_min )
}

n_ds <- n_ds %>% mutate (TotalResiduos = min_max( TotalResiduos , 0, 50), PIB = min_max( PIB , 0, 50))

ggplot(n_ds, aes(Paises, TaxaUtilizacaoMaterialCircular)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_bar(n_ds$TaxaUtilizacaoMaterialCircular)+
  coord_flip()

dsa <- n_ds[n_ds$Paises %in% c('PT - Portugal','ES - Espanha','NL - Países Baixos', 'LV - Letónia'),]
dsa <- dsa %>% mutate (TaxaUtilizacaoMaterialCircular = min_max( TaxaUtilizacaoMaterialCircular, 0, 100), PIB = min_max( PIB , 0, 100), TotalResiduos = min_max(TotalResiduos, 0 , 100))


ggplot( data= dsa, aes(x=Ano, y=TaxaUtilizacaoMaterialCircular)) +
  geom_line( aes ( colour=Paises ) , alpha = 0.5 ) +
  geom_line( aes ( y = PIB, colour=Paises ) , alpha = 0.5 ) +
  theme_light( base_size = 15 )


dsa <- dsa %>% mutate (TaxaUtilizacaoMaterialCircular = , PIB = min_max( PIB , 0, 100), TotalResiduos = min_max(TotalResiduos, 0 , 100))

dsa$logTC=log(dsa$TaxaUtilizacaoMaterialCircular)
dsa$logLP=log(dsa$TotalResiduos)

ggplot( data= dsa, aes(x=Ano, y=logTC)) +
  geom_line( aes ( colour=Paises ) , alpha = 0.5 ) +
  geom_line( aes ( y = logLP, colour=Paises ) , alpha = 0.5 ) +
  theme_light( base_size = 15 )
