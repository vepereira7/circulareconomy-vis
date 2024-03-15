setwd("~/Desktop/MSDE/DPV/project")

library(tidyr)
library(readxl)
library ( openintro )  
library ( ggplot2 ) 
library( GGally )
library( dplyr )
library(dplyr)
library(stringr)
library(gganimate)
n_ds <- read_xlsx("EconomiaCircular.xlsx")
dim(n_ds)

rep = c('BE - Bélgica'='BE - Belgium','DE - Alemanha'='DE - Germany','AT - Áustria'='AT - Austria',
        'BG - Bulgária'='BG - Bulgaria','CY - Chipre'='CY - Cyprus','HR - Croácia'='HR - Croatia',
        'DK - Dinamarca'='DK - Denmark','SK - Eslováquia'='SK - Slovakia','SI - Eslovénia'='SI - Slovenia',
        'ES - Espanha'='ES - Spain','EE - Estónia'='EE - Estonia','FI - Finlândia'='FI - Finland',
        'FR - França'='FR - France','GR - Grécia'='GR - Greece','HU - Hungria'='HU - Hungary',
        'IE - Irlanda'='IE - Ireland','IT - Itália'='IT - Italy','LV - Letónia'='LV - Latvia',
        'LT - Lituânia'='LT - Lithuania','LU - Luxemburgo'='LU - Luxembourg','MT - Malta'='MT - Malta',
        'NL - Países Baixos'='NL - Netherlands','PL - Polónia'='PL - Poland','PT - Portugal'='PT - Portugal',
        'CZ - República Checa'='CZ - Czech Republic','RO - Roménia'='RO - Romania','SE - Suécia'='SE - Sweden',
        'IS - Islândia'='IS - Iceland','NO - Noruega'='NO - Norway','UK - Reino Unido'='UK - United Kingdom',
        'CH - Suíça'='CH - Switzerland','UE27 (2020) - União Europeia 27 (desde 2020)'='EU 27')

n_ds$Paises <- str_replace_all(n_ds$Paises, rep)
n_ds$Paises[n_ds$Paises=="UE27 (2020) - União Europeia 27 (desde 2020)"] <-"EU 27"



n_ds <-filter(n_ds, rowSums(is.na(n_ds)) != ncol(n_ds))
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





n_ds <- n_ds[n_ds$Paises %in% c('PT - Portugal','BE - Belgium','AT - Austria','GR - Greece','EU 27'),]
n_ds <- n_ds[n_ds$Ano %in% c(2010,2012,2014,2016,2018,2020),]
#n_ds$LR = n_ds$TotalResiduos * (n_ds$TaxaUtilizacaoMaterialCircular/100)

n_ds$TotalResiduos[n_ds$Paises == 'EU 27'] = n_ds$TotalResiduos[n_ds$Paises == 'EU 27'] / 31

  

p1 <- ggplot(data=n_ds, aes(y=reorder(Paises,+TaxaUtilizacaoMaterialCircular), fill=Paises)) +
  geom_col(aes(x=TaxaUtilizacaoMaterialCircular),alpha=.8,color = 'black', show.legend = FALSE) +
  #geom_text(aes(x = TaxaUtilizacaoMaterialCircular,
                #label = as.factor(paste0(sprintf("%1.1f", TaxaUtilizacaoMaterialCircular),"%"))),
            #hjust = -0.1, nudge_x = -.5,size = 3, color = 'white') +
  transition_time(as.integer(Ano)) +
  theme_classic()+
  labs(x = 'Circularity rate (%)', y = 'Countries', caption = "Year : {frame_time}         based on data pordata.pt",
       title = paste("Circularity rate per year"),
       subtitle = paste("Created by Jorge Vieira, Vitor Pereira and João Fragão in: ", Sys.Date()-1)) + 
  #scale_fill_continuous(name = "Circularity rate (%)") +
  theme(plot.title = element_text(colour = 'black', face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(color = '#616161', hjust = 0.5),
  plot.background = element_rect ( fill = "grey95"), plot.caption = element_text(face = 'bold', size = 14, hjust = 0),
  axis.title.x = element_text(face = 'bold', hjust = 1), axis.title.y = element_text(face = 'bold'),
  legend.title = element_text(face = 'bold'))
p1


p2 <- p1

p2 <- animate( plot = p2 , nframes = 100 , fps = 30 , duration = 15 , width = 15, height = 10, units = "cm" ,
         end_pause = 30 , res = 150 )
p2

anim_save('totalwasteandCircRate.gif', p2)

