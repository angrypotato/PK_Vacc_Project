## Purpose: Source File For Necessary Libraries and Functions 

# Call Necessary Libraries ----

library('rgdal')
library('ggplot2')
library('dplyr')
library('rgeos')
library('formattable')
library('tidyr')
library('foreign')
library('haven')
library('caret')
library('gbm')
library('randomForest')
library('xtable')
library('devtools')
library('gbm')
library('randomForest')
library('lars')
library('mlbench')
library('caret')
library('Metrics')
library('htmltools')
library('caTools')
library('sp')
library('dismo')
library('readxl')


# Functions ----

## To Integrate Geographic Covariates With Either Tehsil or District Level Data ----
## F = Covariate Source File
## attr = Covariate Column Name - IE Poverty or Fertility
## res = Indicate Whether Tehsil or District Level Data - res = 1 for Tehsil, 2 for District

get_geovars <- function(f,attr,res){
  file <- as.data.frame(raster(f),xy=TRUE)
  coordinates(file)<- ~x +y
  proj4string(file) <- proj4string(tehsils_shp)
  pts2 <- over(file,tehsils_shp)
  names(file)[1] <- "Attr"
  binded <- cbind(pts2,file$Attr)
  print(names(binded))
  binded_df <- data.frame("District" = binded[,3], "Tehsil" = binded[,4], "Population" = binded[,9])
  binded_df<- binded_df %>% 
    mutate(Tehsil = toupper(Tehsil))
  binded_df<- binded_df %>% 
    mutate(District = toupper(District))
  binded_df[which(binded_df$Tehsil == "SAHIWAL" & binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"
  if(res == 1){
    binded_df <- data.frame(binded_df[complete.cases(binded_df),] %>% group_by(Tehsil) %>% summarise_each(funs(nz_mean)))
    binded_df <- data.frame(binded_df %>% group_by(Tehsil) %>% summarise_each(funs(nz_mean)))
    tehsils$attr <- 0
    tehsils$Tehsil <- tehsils$TEHSIL
    merged <- merge(tehsils, binded_df, by='Tehsil', all.x = T)
  }
  else{
    binded_df <- data.frame(binded_df[complete.cases(binded_df),] %>% group_by(District) %>% summarise_each(funs(nz_mean)))
    binded_df <- data.frame(binded_df %>% group_by(District) %>% summarise_each(funs(nz_mean)))
    districts$attr <- 0
    districts$District <- districts$DISTRICT
    merged <- merge(districts, binded_df, by='District')
    
  }
  merged$attr <- merged[,ncol(merged)]
  merged <- merged[,-c(ncol(merged)-2,ncol(merged)-1)]
  names(merged)[length(names(merged))] <- attr
  merged
}


## To Integrate Geographic Covariates With UC Level Data ----
## F = Covariate Source File
## attr = Covariate Column Name - IE Poverty or Fertility

get_geovars_uc <- function(f,attr){
  file <- as.data.frame(raster(f),xy=TRUE)
  coordinates(file)<- ~x +y
  # returns a spatialpoint dataframe
  proj4string(file) <- proj4string(uc_shp)  
  pts <- over(file,uc_shp)    
  names(file)[1] <- "Attr"
  binded <- cbind(pts,file$Attr)
  print(names(binded))
  binded_df <- data.frame("District" = binded[,2], "Tehsil" = binded[,3], "UC" = binded[,4], "Population" = binded[,20])
  binded_df <- binded_df %>% 
    mutate(Tehsil = toupper(Tehsil)) %>%
    mutate(District = toupper(District)) %>%
    mutate(UC = toupper(UC))
  
  binded_df <- data.frame(binded_df[complete.cases(binded_df),] %>% group_by(UC) %>% summarise_each(funs(nz_mean)))
  binded_d2 <- data.frame(binded_df %>% group_by(UC) %>% summarise_each(funs(nz_mean)))
  ucs$attr <- 0
  ucs$UC <- toupper(ucs$UC)
  merged <- merge(ucs, binded_df, by='UC')
  
  merged$attr <- merged[,ncol(merged)]
  merged <- merged[ ,-c(ncol(merged)-2,ncol(merged)-1)]
  names(merged)[length(names(merged))] <- attr
  merged
}


## Map Misspelled Names of Tehsils For Proper Joining of Data Sources ----

solve_name <- function(name){
  name <- toupper(name)
  if(name == "CHAK JUMRA"){
    name <- "CHAK JHUMRA"
  }
  if(grepl('khanpur|khan pur|kahn pur|kahnpur',name,ignore.case=TRUE)){
    name <- 'KHAN PUR'
  }
  if(name == "GUJAR KHAN TEHSIL"){
    name <- "GUJAR KHAN"
  }
  if(name == "GUJRANWALA SADDAR"){
    name <- "GUJRANWALA"
  }
  if(name == "JEHANIAN"){
    name <- "JAHANIAN"
  }
  if(name == "KAHUTA TEHSIL"){
    name <- "KAHUTA"
  }
  if(name == "MURREE TEHSIL"){
    name <- "MURREE"
  }
  if(name == "KOTLI SATTIAN TEHSIL"){
    name <- "KOTLI SATTIAN"
  }
  if(name == "KEHROR PACCA"){
    name <- "KARORPACCA"
  }
  if(name == "KOTLI SATTIAN TEHSIL"){
    name <- "KOTLI SATTIAN"
  }
  if(name == "NAUSHERA VIRKHAN"){
    name <- "NOWSHERA VIRKHAN"
  }
  if(name == "RAWALPINDI TEHSIL"){
    name <- "RAWALPINDI"
  }
  if(name == "TAXILA TEHSIL"){
    name <- "TAXILA"
  }
  if(name == "RANALA KHURD"){
    name <- "RENALA KHURD"
  }
  if(name == "DENA"){
    name <- "DINA"
  }
  if(name == "DUNIAPUR"){
    name <- "DUNYA PUR"
  }
  if(name == "ESA KHEL"){
    name <- "ISAKHEL"
  }
  if(name == "KALLURKOT"){
    name <- "KALUR KOT"
  }
  if(name == "KAHROR PACCA"){
    name <- "KARORPACCA"
  }
  if(name == "KOT ADU"){
    name <- "KOT ADDU"
  }
  if(name == "MURIDKEY"){
    name <- "MURIDKE"
  }
  if(name == "DUNIAPUR"){
    name <- "KOT ADDU"
  }
  if(name == "DUNIAPUR"){
    name <- "KOT ADDU"
  }
  if(name == "NOORPUR THAL"){
    name <- "NOORPUR"
  }
  if(name == "SAHIWAL CITY"){
    name <- "SAHIWAL_SAHIWAL"
  }
  if(name == "TAUNSA SHARIF"){
    name <- "TAUNSA"
  }
  if(name == "SAHIWAL CITY"){
    name <- "SAHIWAL_SAHIWAL"
  }
  if(name == "PIR MAHAL" || name=="PIRMAHAL"){
    name <- "TOBA TEK SINGH"
  }
  if(name == "SHUJA ABAD"){
    name <- "SHUJABAD"
  }
  # if(name == "QILA DEEDAR SINGH"){
  #  name <- "QILA DIDAR SINGH"
  # }
  if(name == "MINCHANABAD"){
    name <- "MINCHINABAD"
  }
  if(name == "KAROR LAL-E-SON"){
    name <- "KAROR LAL ESAN"
  }
  if(name == "DATA GUNJ BAKHSH TOWN"){
    name <- "LAHORE CITY"
  }
  if(name == "GULBERG"){
    name <- "LAHORE CITY"
  }
  if(name == "TAXILA TOWN"){
    name <- "TAXILA"
  }
  if(name == "GUJAR KHAN TOWN"){
    name <- "GUJAR KHAN"
  }
  if(name == "KOTLI SATTIAN TOWN"){
    name <- "KOTLI SATTIAN"
  }
  if(name == "KAHUTA TOWN"){
    name <- "KAHUTA"
  }
  if(name == 'KAMOKE TOWN'){
    name <- 'KAMOKE'
  }
  if(name == 'MUMTAZABAD TOWN' || name=="SHAH RUKN-E-ALAM TOWN" || name == "MOUSA PAK (SHAHEED) TOWN" || name=='SHAH RUKN-E-ALAM'){
    name <- 'MULTAN CITY'
  }
  if(name == 'QILA DEDAR SINGH TOWN'){
    name <- 'SHARAK PUR'
  }
  if(name == 'SHAHKOT'){
    name <- 'SHAH KOT'
  }
  if(name == 'AROOP TOWN'){
    name <- 'GUJRANWALA'
  }
  if(name == 'KOTMOMAN' || name=='KOT MOMEN'){
    name <- 'KOT MOMIN'
  }
  if(name == 'TOBA TEB SINGH'){
    name <- 'TOBA TEK SINGH'
  }
  if(name == 'BHOWANA'){
    name <- 'BHAWANA'
  }
  if(name=="LYALLPUR" || name=="LYALLPUR TOWN"){
    name <- 'FAISALABAD CITY'
  }
  if(name=="FAISALABAD"){
    name <- 'FAISALABAD CITY'
  }
  if(name == 'CHAK JHUMRA TOWN'){
    name <- 'CHAK JHUMRA'
  }
  if(name == "KALAR KAHAR"){
    name <- 'KALLAR KAHAR'
  }
  if(name == 'JARANWALA TOWN'){
    name <- 'JARANWALA'
  }
  if(name == "TANDLIANWALA TOWN"){
    name <- "TANDLIANWALA"
  }
  if(name == 'JALALPUR PIRWALA TOWN'){
    name <- "JALALPUR PIRWALA"
  } 
  if(name == 'SHUJABAD TOWN'){
    name <- 'SHUJABAD'
  }
  if(name == 'KOTADDU'){
    name <- 'KOT ADDU'
  }
  if(name == 'HASSAN ABDAL' || name=="HASAN ABDAL"){
    name <- 'HASSANABDAL'
  }
  if(name == 'KALLAR SYEDAN' || name=='KALLAR SAYADDAN'){
    name <- 'KALLAR SAYYEDAN'
  }
  if(name == "SHERSHAH TOWN" || name == 'SHER SHAH'){
    name <- "MULTAN SADDAR"
  }
  if(name == 'BOSAN TOWN' || name == 'BOSAN'){
    name <- 'KHANEWAL'
  }
  if(name == 'AHMADPUR SIAL'){
    name <- 'AHMEDPUR SIAL'
  }
  if(name == 'MURIDKAY'){
    name <- 'MURIDKE'
  }
  if(name == 'CANTONMENT AREA'){
    name <- 'LAHORE CANTT'
  }
  if(name == 'BAHAWALPUR SADAR'){
    name <- 'BAHAWALPUR'
  }
  if(name == 'KHAIRPUR TAMMEWALI'){
    name <- 'KHAIRPUR TAMEWALI'
  }
  if(name == 'FORTABBAS'){
    name <- 'FORT ABBAS'
  }
  if(name == 'LIAQUAT PUR'){
    name <- 'LIAQAT PUR'
  }
  if(name == 'R.Y. KHAN'){
    name <- 'RAHIM YAR KHAN'
  }
  if(name == 'D.G.KHAN'){
    name <- 'DERA GHAZI KHAN'
  }
  if(name == 'CHOUBARA'){
    name <- 'CHAUBARA'
  }
  if(name=="MUZAFFAR GARH"){
    name <- 'MUZAFFARGARH'
  }
  if(name=="ALLAMA IQBAL"){
    name <- 'LAHORE CITY'
  }
  if(name=="JINNAH"){
    name <- 'LAHORE CITY'
  }
  if(name=="LYALLPUR"){
    name <- 'FAISALABAD CITY'
  }
  if(name=="POTOHAR" || name=="PALLANDARI"){
    name<-"RAWALPINDI"
  }
  if(name == "QUAIDABAD"){
    name <- "QAIDABAD"
  }
  if(name == "KHIALI SHAHPUR"){
    name <- "GUJRANWALA"
  }
  if(name == "SAMMUNDRI"){
    name <- "SAMUNDARI"
  }
  if(name=="MADINA"){
    name<-"FAISALABAD CITY"
  }
  if(name == "TANDLIAN WALA"){
    name <- "TANDLIANWALA"
  }
  if(name == "T.T.SINGH"){
    name <- "TOBA TEK SINGH"
  }
  if(name == "ARUP"){
    name <- "WAZIRABAD"
  }
  if(name == 'NANDIPUR TOWN' || name == 'NANDI PUR' || name == 'QILA DIDAR SINGH'){
    name <- 'GUJRANWALA'
  }
  if(name == 'NOWSHERA VIRKAN' || name == "NOSHERA VIRKAN"){
    name <- 'NOWSHERA VIRKHAN'
  }
  if(name == "MANDI BAHAUDIN"){
    name <- "MANDI BAHAUDDIN"
  }
  if(name == 'AZIZ BHATTI TOWN'){
    name <- 'LAHORE CANTT'
  }
  if(name == 'DATA BAKSH TOWN'){
    name <- 'LAHORE CITY'
  }
  if(name == 'ALLAMA IQBAL TOWN' || name =='IQBAL TOWN' || name=='IQBAL TOWN_DUPLICATED_72'){
    name <- 'LAHORE CITY'
  }
  if(name == 'NISHTER TOWN' || name =='NISHTAR TOWN'){
    name <- 'LAHORE CANTT'
  }
  if(name == 'RAVI TOWN'){
    name <- 'GUJRANWALA CITY'
  }
  if(name == "SAMANABAD TOWN"){
    name <- "LAHORE CITY"
  }
  if(name == 'SHALIMAR TOWN' || name == 'SHALAMAR TOWN' || name == "WAHGA TOWN"){
    name <- 'LAHORE CITY'
  }
  if(name == "SANGLA HILL"){
    name <- "SHANGLA HILL"
  }
  if(name == "NANKANA SAHIB"){
    name <- "NANKANA SAHAB"
  }
  if(name == "SHARQ PUR SHARIF"){
    name <- "SHARAK PUR"
  }
  if(name == 'MUMTAZABAD TOWN' || name == 'MUMTAZABAD (MOUSA PAK)' || name=='SHAH RUKN-E-ALAM'){
    name <- 'MULTAN CITY'
  }
  if(name == 'DUNYAPUR'){
    name <- 'DUNYA PUR'
  }
  if(name == 'KAROR PAKKA' || name == 'KAROOR PACCA' || name=='KAROR PACCA'){
    name <- 'KARORPACCA'
  }
  #if(name == 'RAWAL TOWN'){
  #  name <- 'RAWALPINDI'
  # }
  if(name == 'TALAGANG'){
    name <- 'TALA GANG'
  }
  if(name == 'SHOR KOT'){
    name <- 'SHORKOT'
  }
  if(name == 'KHANPUR'){
    name <- 'KHAN PUR'
  }
  if(name=="JINNAH" || name=="JINNAH TOWN"){
    name <- 'LAHORE CITY'
  }
  if(name == 'ESSA KHEL'){
    name <- 'ISAKHEL'
  }
  if(name == "LIAQUATPUR"){
    name <- 'LIAQAT PUR'
  }
  if(name == 'SAHIWAL _DUPLICATED_13' || name=='SAHIWAL_DUPLICATED_137'){
    name <- 'SAHIWAL'
  }
  if(name == 'PAKPATTAN'){
    name <- 'PAK PATTAN'
  }
  if(name=="ARIF WALA"){
    name<-"ARIFWALA"
  }
  if(name=="RY KHAN"){
    name<-"RAHIM YAR KHAN"
  }
  if(name=="DG KHAN"){
    name<-"DERA GHAZI KHAN"
  }
  if(name == 'CHOBARA'){
    name <- 'CHAUBARA'
  }
  if(name == "SAMMUNDRI TOWN"){
    name <- "SAMUNDARI"
  }
  if(name == 'KAROR PAKKA' || name == 'KAROOR PACCA' || name=='KAROR'){
    name <- 'KARORPACCA'
  }
  if(name=="MADINA" || name=="MADINA TOWN"){
    name<-"FAISALABAD CITY"
  }
  #if(name=="ARIF WALA" || name==""){
  #name<-"ARIFWALA"
  #}
  if(name == '18 - HAZARI'){
    name <- '18-HAZARI'
  }
  if(name == 'NOWSHERA VIRKAN' || name == "NAUSHERA VIRKAN TOWN"){
    name <- 'NOWSHERA VIRKHAN'
  }
  if(name == "CANTONMENT"){
    name <- "LAHORE CANTT"
  }
  if(name == "KHIALLI SHAHPUR TOWN"){
    name <- "GUJRANWALA"
  }
  if(name == "WAZIRABAD TOWN"){
    name <- "WAZIRABAD"
  }
  if(name == "DATA GANJ BAKHSH TOWN" || name == "DATA GUNJ BUKSH"){
    name <- "LAHORE CITY"
  }
  if(name == "SHERSHAH TOWN" || name == 'SHER SHAH TOWN'){
    name <- "MULTAN SADDAR"
  }
  if(name=="POTOHAR TOWN"){
    name<-"RAWALPINDI"
  }
  if(name=="SAFDAR ABAD"){
    name <- "SAFDARABAD"
  }
  if(name == "SHARAQPUR"){
    name <- "SHARAK PUR"
  }
  if(name == "MURREE TOWN"){
    name <- "MURREE"
  }
  if(name == 'KALLAR SYEDAN TOWN'){
    name <- 'KALLAR SAYYEDAN'
  }
  if(name == 'NOOR PUR THAL' || name=='NURPUR THAL'){
    name <- 'NOORPUR'
  }
  if(name == 'KOTMOMAN' || name=='KOT MOMEN'){
    name <- 'KOT MOMIN'
  }
  if(name == "ISA KHEL"){
    name <- "ISAKHEL"
  }
  if(name == 'KOTADDU'){
    name <- 'KOT ADDU'
  }
  if(name == 'KALARKAHAR'){
    name <- 'KALLAR KAHAR'
  }
  if(name == 'KALLUR KOT'){
    name <- 'KALUR KOT'
  }
  if(name == 'HASSAN ABDAL' || name=="HASAN ABDAL"){
    name <- 'HASSANABDAL'
  }
  if(name == 'KALLAR SYEDAN' || name=='KALLAR SAYADDAN' || name == "KALLER SYEDAN"){
    name <- 'KALLAR SAYYEDAN'
  }
  if(name == "SHERSHAH TOWN" || name == 'SHER SHAH'){
    name <- "MULTAN SADDAR"
  }
  if(name == 'BOSAN TOWN' || name == 'BOSAN'){
    name <- 'KHANEWAL'
  }
  if(name == "MUREE"){
    name <- "MURREE"
  }
  if(name == 'AHMADPUR SIAL'){
    name <- 'AHMEDPUR SIAL'
  }
  if(name == 'MURIDKAY'){
    name <- 'MURIDKE'
  }
  if(name == 'CANTONMENT AREA'){
    name <- 'LAHORE CANTT'
  }
  if(name == 'BAHAWALPUR SADAR'){
    name <- 'BAHAWALPUR'
  }
  if(name == 'KHAIRPUR TAMMEWALI' || name == 'TAMEWALI'){
    name <- 'KHAIRPUR TAMEWALI'
  }
  if(name == 'FORTABBAS' || name == 'FORTABBASS'){
    name <- 'FORT ABBAS'
  }
  if(name == 'LIAQUAT PUR'){
    name <- 'LIAQAT PUR'
  }
  if(name == 'R.Y. KHAN'){
    name <- 'RAHIM YAR KHAN'
  }
  if(name == 'D.G.KHAN'){
    name <- 'DERA GHAZI KHAN'
  }
  if(name == 'CHOUBARA'){
    name <- 'CHAUBARA'
  }
  if(name == "JATAOI"){
    name <- "JATOI"
  }
  
  if(name=="MUZAFFAR GARH"){
    name <- 'MUZAFFARGARH'
  }
  if(name=="ALLAMA IQBAL"){
    name <- 'LAHORE CITY'
  }
  if(name=="JINNAH"){
    name <- 'LAHORE CITY'
  }
  if(name=="LYALLPUR"){
    name <- 'FAISALABAD CITY'
  }
  #if(name=='GULBERG'){
  #  name <- "GULBERG TOWN"
  #}
  if(name=="POTOHAR"){
    name<-"RAWALPINDI"
  }
  if(name=="KAHUTTA"){
    name<-"KAHUTA"
  }
  
  if(name == "QUAIDABAD" || name == "QAID BAD"){
    name <- "QAIDABAD"
  }
  if(name == "KHIALI SHAHPUR"){
    name <- "GUJRANWALA"
  }
  if(name == "SAMMUNDRI"){
    name <- "SAMUNDARI"
  }
  if(name=="MADINA"){
    name<-"FAISALABAD CITY"
  }
  if(name == "TANDLIAN WALA"){
    name <- "TANDLIANWALA"
  }
  if(name == "T.T.SINGH"){
    name <- "TOBA TEK SINGH"
  }
  if(name == "P.D KHAN"){
    name <- "PIND DADAN KHAN"
  }
  if(name == "ARUP"){
    name <- "WAZIRABAD"
  }
  if(name == 'NANDIPUR TOWN' || name == 'NANDI PUR' || name == 'QILA DIDAR SINGH'){
    name <- 'GUJRANWALA'
  }
  if(name == 'NOWSHERA VIRKAN' || name == "NOSHERA VIRKAN" || name=="NOWSHERA" || name == "NAUSHERA"){
    name <- 'NOWSHERA VIRKHAN'
  }
  if(name == "MANDI BAHAUDIN" || name=="MANDI BAHUDDIN"){
    name <- "MANDI BAHAUDDIN"
  }
  if(name == 'AZIZ BHATTI TOWN'){
    name <- 'LAHORE CANTT'
  }
  if(name == 'DATA BAKSH TOWN'){
    name <- 'LAHORE CITY'
  }
  if(name == 'ALLAMA IQBAL TOWN' || name =='IQBAL TOWN'){
    name <- 'LAHORE CITY'
  }
  if(name == 'NISHTER TOWN' || name =='NISHTAR TOWN'){
    name <- 'LAHORE CANTT'
  }
  if(name == 'RAVI TOWN'){
    name <- 'GUJRANWALA CITY'
  }
  
  if(name == "SAMANABAD TOWN"){
    name <- "LAHORE CITY"
  }
  if(name == 'SHALIMAR TOWN' || name == 'SHALAMAR TOWN' || name == "WAHGA TOWN" || name == "SHALIMAR"){
    name <- 'LAHORE CITY'
  }
  if(name == "SARAI ALAM GIR"){
    name <- "SARAI ALAMGIR"
  }
  if(name == "SANGLA HILL"){
    name <- "SHANGLA HILL"
  }
  if(name == "NANKANA SAHIB"){
    name <- "NANKANA SAHAB"
  }
  if(name == "SHARQ PUR SHARIF"){
    name <- "SHARAK PUR"
  }
  if(name == 'MUMTAZABAD TOWN' || name == 'MUMTAZABAD (MOUSA PAK)' || name=='SHAH RUKN-E-ALAM'){
    name <- 'MULTAN CITY'
  }
  if(name == 'DUNYAPUR'){
    name <- 'DUNYA PUR'
  }
  if(name == 'KAROR PAKKA' || name == 'KAROOR PACCA' || name=="KAROR PACCA"){
    name <- 'KARORPACCA'
  }
  #if(name == 'RAWAL TOWN'){
  # name <- 'RAWALPINDI'
  #}
  if(name == 'TALAGANG'){
    name <- 'TALA GANG'
  }
  if(name == 'SHOR KOT'){
    name <- 'SHORKOT'
  }
  if(name == 'ESSA KHEL'){
    name <- 'ISAKHEL'
  }
  if(name == 'SAHIWAL _DUPLICATED_13'){
    name <- 'SAHIWAL'
  }
  if(name == 'PAKPATTAN'){
    name <- 'PAK PATTAN'
  }
  if(name=="ARIF WALA"){
    name<-"ARIFWALA"
  } 
  if(name == "RWP CANTT" || name=="RAWALPINDI RURAL" || name == "RAWALPINDI CANTT" || name == "RAWALPINDI CITY" || name == "RWP RURAL" || name == "RWP CITY"){
    name <- "RAWALPINDI"
  }
  # if(name == "ISLAMABAD URBAN" || name == "ISLAMABAD RURAL" || name == "ISALAMABAD"){
  #name <- "RAWALPINDI"
  #}
  if(name == "SAMANABAD TOWN" || name == "SAMANABAD"){
    name <- "LAHORE CITY"
  }
  
  if(name == 'HASSAN ABDAL'){
    name <- 'HASSANABDAL'
  }
  if(name == 'FAISALABAD SADAR'){
    name <- 'FAISALABAD SADDAR'
  }
  if(name == 'GUJRANWALA SADAR'){
    name <- 'GUJRANWALA'
  }
  if(name == 'DUNYAPUR'){
    name <- 'DUNYA PUR'
  }
  if(name == 'DE-EX.AREA OF RAJANPUR'){
    name <- 'RAJANPUR'
  }
  if(name == 'DEPAL PUR'){
    name <- 'DEPALPUR'
  }
  if(name == 'HASSAN ABDAL'){
    name <- 'HASSANABDAL'
  }
  if(name == 'KALLAR SYEDAN'){
    name <- 'KALLAR SAYYEDAN'
  }
  if(name == 'KAROR LAL EASAN' || name == "KAROR LALISAN"){
    name <- 'KAROR LAL ESAN'
  }
  if(name == 'KAROR PAKKA'){
    name <- 'KARORPACCA'
  }
  if(name == 'RAVI TOWN' || name == 'RAVI'){
    name <- 'GUJRANWALA CITY'
  }
  if(name == 'LAHOR'){
    name <- 'LAHORE CITY'
  }
  if(name == 'D.G.KHAN' || name=='D.G.KHAN '){
    name <- 'DERA GAZI KHAN'
  }
  #if(name == 'RAWAL TOWN'){
  # name <- 'RAWALPINDI'
  #}
  if(name == 'QILA DEEDAR SINGH TOWN'){
    name <- 'SHARAK PUR' 
  }
  if(name == 'PIR MAHAL'){
    name <- 'TOBA TEK SINGH'
  }
  if(name == 'PASROOR'){
    name <- 'PASRUR'
  }
  if(name == 'PAKPATTAN'){
    name <- 'PAK PATTAN'
  }
  if(name == 'PASROOR'){
    name <- 'PASRUR'
  }
  if(name == 'NOWSHERA VIRKAN' || name == "NOSHERA VIRKAN"){
    name <- 'NOWSHERA VIRKHAN'
  }
  if(name == 'NOOR PUR THAL' || name == "NOOR PURTHAL"){
    name <- 'NOORPUR'
  }
  if(name == 'NISHTAR TOWN' || name == 'NISHTER' || name == 'NISHTER TOWN' || name == 'NISHTAR'){
    name <- 'LAHORE CANTT'
  }
  
  if(name == 'NANDIPUR TOWN' || name == 'NANDI PUR' || name=="NANDIPUR"){
    name <- 'GUJRANWALA'
  }
  if(name == 'MULTAN URBAN'){
    name <- 'MULTAN CITY'
  }
  if(name == "MULTAN SADAR" || name =='MULTAN RURAL'){
    name <- 'MULTAN SADDAR'
  }
  if(name == 'LIAQUAT PUR' || name=="LIAQATPUR"){
    name <- 'LIAQAT PUR'
  }
  if(name == 'LALLIAN'){
    name <- 'SAHIWAL'
  }
  if(name == 'KOTMOMAN'){
    name <- 'KOT MOMIN'
  }
  if(name == 'KOTADDU'){
    name <- 'KOT ADDU'
  }
  if(name == 'KAROR PAKKA '){
    name <- 'KARORPACCA'
  }
  if(name == 'PASROOR'){
    name <- 'PASRUR'
  }
  if(name == 'BAHAWALPUR SADAR'){
    name <- 'BAHAWALPUR'
  }
  if(name == 'WAGHA TOWN'){
    name <- 'LAHORE CANTT'
  }
  if(name == 'TRIBAL AREAS'){
    name <- 'RAZMAK'
  }
  if(name == 'TALAGANG'){
    name <- 'TALA GANG'
  }
  if(name == 'SHOR KOT'){
    name <- 'SHORKOT'
  }
  if(name == 'SHARAQPUR SHARIF'){
    name <- 'SHARAK PUR'
  }
  if(name == 'SHALIMAR TOWN' || name == 'DAJA GUNJ BUKSH'){
    name <- 'LAHORE CITY'
  }
  if(name == "CHISTIAN"){
    name <- "CHISHTIAN"
  }
  if(name == 'CHOA SAIDEN SHAH' || name == "CHOA SAIDDEN SHAH"){
    name <- 'CHOA SAIDAN SHAH'
  }
  if(name == 'CHICHAWATANI'){
    name <- 'SAHIWAL'
  }
  if(name == 'SHALAMAR'){
    name <- 'LAHORE CITY'
  }
  if(name == 'SAMUNDRI'){
    name <- 'SAMUNDARI'
  }
  if(name == 'ABDUL HAKIM'){
    name <- 'KABIRWALA'
  }
  if(name == 'AHMEDPUR EAST'){
    name <- 'AHMADPUR EAST'
  }
  if(name == 'AHMAD PUR SIAL'){
    name <- 'AHMEDPUR SIAL'
  }
  if(name == 'AROOP TOWN' || name == 'AROOP'){
    name <- 'GUJRANWALA'
  }
  if(name == 'TAUNSA '){
    name <- 'TAUNSA'
  }
  if(name == 'ATHARA HAZARI'){
    name <- '18-HAZARI'
  }
  
  if(name == 'AZIZ BHATTI TOWN' || name == 'AZIZ BHATTI'){
    name <- 'LAHORE CANTT'
  }
  if(name == 'AZIZ BHATI TOWN'){
    name <- 'LAHORE CANTT'
  }
  if(name == 'BAHAWALPUR SADDAR'){
    name <- 'BAHAWALPUR'
  }
  
  if(name == 'BHOWANA'){
    name <- 'BHAWANA'
  }
  if(name == 'BHERA'){
    name <- 'BHALWAL'
  }
  if(name == 'BOSAN TOWN'){
    name <- 'KHANEWAL'
  }
  if(name == 'CHAKWAL TOWN'){
    name <- 'CHAKWAL'
  }
  if(name == 'CHINOT'){
    name <- 'CHINIOT'
  }
  if(name == 'CHOA SAIDEN SHAH'){
    name <- 'CHOA SAIDAN SHAH'
  }
  if(name == 'CHOUBARA'){
    name <- 'CHAUBARA'
  }
  if(name == 'CHOLISTAN'){
    name <- 'BAHAWALPUR'
  }
  if(name == " SHARAQPUR SHARIF"){
    name <- "SHARAK PUR"
  }
  if(name == 'DATA GUNJ BAKSH TOWN' || name == "DATA GUNJ BUKSH"){
    name <- 'LAHORE CITY'
  }
  if(name == 'DATA GANJ BAKSH TOWN' || name == 'DATA GANJ BAKHSH TOWN'){
    name <- 'LAHORE CITY'
  }
  if(name == 'DE-EX.AREA OF RAJANPUR'){
    name <- 'DE-EXCLUDED AREA RAJANPUR'
  }
  if(name == 'IQBAL TOWN' || name == 'ALLAMA IQBAL'){
    name <- 'LAHORE CITY'
  }
  if(name == 'FAISALABAD SADAR'){
    name <- 'FAISALABAD SADDAR'
  }
  if(name == 'GUJRANWALA SADAR'){
    name <- 'GUJRANWALA'
  }
  if(name == "ALI PUR"){
    name <- "ALIPUR"
  }
  if(name == 'KHIYALI TOWN' || name == 'KHIYALI TOWN'){
    name <- 'GUJRANWALA'
  }
  if(name == 'MALIKWAL'){
    name <- 'MALAKWAL'
  }
  if(name == 'K.S PUR TOWN' || name == "KHANPUR"){
    name <- 'KHAN PUR'
  }
  if(name == 'MUMTAZABAD TOWN'){
    name <- 'MULTAN CITY'
  }
  if(name == 'PASROOR'){
    name <- 'PASRUR'
  }
  if(name == "SHOR KOT"){
    name <- "SHORKOT"
  }
  if(name == "WAHGA TOWN" || name=="WAHGA"){
    name <- "LAHORE CITY"
  }
  if(name == "TRIBAL AREA"){
    name <- "RAZMAK"
  }
  if(name == "SHAH RUKN-E-ALAM TOWN"){
    name <- "MULTAN CITY"
  }
  if(name == "SHERSHAH TOWN"){
    name <- "MULTAN SADDAR"
  }
  if(name == "SANGLA HILL"){
    name <- "SHANGLA HILL"
  }
  if(name == "RAWALPINDI CANTT"){
    name <- "RAWALPINDI"
  }
  if(name == "R Y KHAN" || name == "RY KHAN" || name == "RAHIMYAR KHAN"){
    name <- "RAHIM YAR KHAN"
  }
  if(name == "KOT CHUTTA"){
    name <- "DERA GHAZI KHAN"
  }
  if(name == "D.G.KHAN"){
    name <- "DERA GHAZI KHAN"
  }
  if(name == "LAHORE"){
    name <- "LAHORE CITY"
  }
  if(name == "PINDIGHEB"){
    name <- "PINDI GHEB"
  }
  if(name == "WAGHA"){
    name <- "LAHORE CANTT"
  }
  if(name == "CANTONMENT" || name=="CANTT"){
    name <- "LAHORE CANTT"
  }
  if(name == "FATEHJANG"){
    name <- "FATEH JANG"
  }
  if(name == "FEROZWALA"){
    name <- "FEROZEWALA"
  }
  if(name == 'D.G.KHAN' || name == "DERA GAZI KHAN"){
    name <- 'DERA GHAZI KHAN'
  }
  if(name == "DE-EX.AREA OF RAJANPUR"){
    name <- "DE-EXCLUDED AREA RAJANPUR"
  }
  toupper(name)
}


## Map Misspelled Names of Districts For Proper Joining of Data Sources ----

solve_district_name <- function(name){
  name <- toupper(name)
  if(name == "B. NAGAR"){
    name <- "BAHAWALNAGAR"
  }
  if(name == "RANALA KHURD"){
    name <- "RENALA KHURD"
  }
  if(name == "PAKPATTAN"){
    name <- "PAK PATTAN"
  }
  if(name == "M. BAHAUDIN"){
    name <- "MANDI BAHAUDDIN"
  }
  if(name == "N. SAHIB"){
    name <- "NANKANA SAHIB"
  }
  if(name == "M. GARH"){
    name <- "MUZAFFARGARH"
  }
  if(name == "TT SINGH"){
    name <- "TOBA TEK SINGH"
  }
  if(name == "LAHORE CITY"){
    name <- "LAHORE"
  }
  if(name == "FAISALABAD CITY"){
    name <- "FAISALABAD"
  }
  if(name == "DG KHAN"){
    name <- "DERA GHAZI KHAN"
  }
  if(name == "RY KHAN"){
    name <- "RAHIM YAR KHAN"
  }
  toupper(name)
}


## Remove UC observations with the same name ----
### should be applied after filtered to Punjab

solve_uc_name <- function(ucs) {
  name_list <- data.frame(table(ucs$UC))
  name_duplicate <- name_list[name_list$Freq > 1, ]
  name_dup_list <- name_duplicate$Var1
  uc_rmv_dup <- ucs[-which(ucs$UC %in% name_dup_list),] %>%
    mutate(UC = toupper(UC))
  uc_rmv_dup
}



## To Calculate the Mean Disregarding 0 ----

nz_mean <- function(x){
  mean(x[x!=0])
}







