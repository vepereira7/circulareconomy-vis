library(tidyr)
library(readxl)
library ( openintro )  
library ( ggplot2 ) 
library( GGally )
library( dplyr )
library(gganimate)
library(ggrepel)
library(stringr)

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
        'CH - Suíça'='CH - Switzerland','EU'='EU 27')

n_ds$Paises <- str_replace_all(n_ds$Paises, rep)

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

n_ds <- remove_outlier(n_ds,c('TotalResiduos','ResiduosAtivEconomica',
                            'ResiduosAggDomesticos','TaxaReciclagemResiduosMunicipais',
                            'TaxaUtilizacaoMaterialCircular','PIB',
                            'TaxaCrescimentoPIB'))
dim(n_ds)

colnames(n_ds)[which(names(n_ds) == "Ano")] <- "Year"
#-------------------------------------
library(stringr)
library( ggthemes )
library(plotly)
n_ds$PIBCres <- ifelse(n_ds$TaxaCrescimentoPIB >= 0,'Increase','Decrease')
n_ds
n_ds <- n_ds[n_ds$Paises %in% c('PT - Portugal','SE - Sweden','FI - Finland', 'SI - Slovenia', 'RO - Romania'),]

p11 <- ggplot(n_ds,aes(x = Year, y = TaxaUtilizacaoMaterialCircular, col=PIBCres, group=Paises)) + scale_color_manual(values = c("#A61414", "#1793CD")) +
  scale_x_continuous("Year", labels = as.character(n_ds$Year), breaks = n_ds$Year) + 
  geom_text(aes(label=ifelse(Year==2010, Paises, "")), col="#000000", position = position_dodge(width = .9), vjust = -1)+
  geom_line() + 
  labs(title = "The impact of the circularity rate on the economy of a country (GPD)", subtitle = paste("Created by Jorge Vieira, Vitor Pereira and João Fragão in: ", Sys.Date()-1)) +
  labs(caption = "(based on data from pordata.pt)") +
  ylab("Circular material usage rate") +
  guides(color = guide_legend(title = "GPD Growth")) + 
  theme_economist() 

p11

ggsave("PlotTxUtCirPIBAno.png", p11, dpi=600)
