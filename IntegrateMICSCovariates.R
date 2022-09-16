
## Purpose: To Extract More Demographic Covariates from the MICS Cluster Population Survey and Join Them With Districts and Tehsils 


# Call Source File for Required Libraries and Functions ----

source(file='PreRun.r')

library(foreign)
library(haven)


# read in and prepare data ---- 

## tehsil data ---

tehsils_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp")
tehsils <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp")
tehsils@data$id <- rownames(tehsils@data)
tehsils <- data.frame(tehsils)
tehsils <- tehsils[which(tehsils$PROVINCE == 'PUNJAB'),]
tehsils$TEHSIL <- sapply(tehsils$TEHSIL,solve_name)
tehsils <- tehsils[!(tehsils$TEHSIL %in% c('RAZMAK')),]   ### 136 obs left
tehsils[which(tehsils$TEHSIL == "SAHIWAL" & tehsils$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL"


## Read in Household, Womens and Children Under 5 Surveys ----

hh <- read.spss("VaccinationStudy/Data/Pakistan (Punjab)_MICS4_Datasets/Pakistan (Punjab) MICS 2011 SPSS Datasets/hh.sav")
wm <- read.spss("VaccinationStudy/Data/Pakistan (Punjab)_MICS4_Datasets/Pakistan (Punjab) MICS 2011 SPSS Datasets/wm.sav")
ch <- read.spss("VaccinationStudy/Data/Pakistan (Punjab)_MICS4_Datasets/Pakistan (Punjab) MICS 2011 SPSS Datasets/ch.sav")

### Redefine names of columns for sake of consistency between the 3 datasets

hh_df <- data.frame(TEHSIL=hh[[which(names(hh)=="HH1B")]],
                    urban=hh[[which(names(hh)=="HH6U")]],
                    electricity=hh[[which(names(hh)=="HC8A")]],
                    radio=hh[[which(names(hh)=="HC8B")]],
                    television=hh[[which(names(hh)=="HC8C")]],
                    mobile_phone=hh[[which(names(hh)=="HC9B")]])

wm_df <- data.frame(TEHSIL=wm[[which(names(wm)=="HH1B")]],
                    urban=wm[[which(names(wm)=="HH6U")]],
                    mothers_age=wm[[which(names(wm)=="WB2")]],
                    school_boolean=wm[[which(names(wm)=="WB3")]],
                    school_level=wm[[which(names(wm)=="WB4")]],
                    antenatal_care=wm[[which(names(wm)=="MN1")]])

ch_df <- data.frame(TEHSIL=ch[[which(names(ch)=="HH1B")]],
                    urban=ch[[which(names(ch)=="HH6U")]],
                    age=ch[[which(names(ch)=="AG2")]],
                    card=ch[[which(names(ch)=="IM2")]],
                    dtp_boolean=ch[[which(names(ch)=="IM11A")]],
                    dtp_number=ch[[which(names(ch)=="IM11B")]])

### Account for location naming discrepancies to match the names in tehsil and district data

hh_df$TEHSIL <- sapply(hh_df$TEHSIL,solve_name)
wm_df$TEHSIL <- sapply(wm_df$TEHSIL,solve_name)
ch_df$TEHSIL <- sapply(ch_df$TEHSIL,solve_name)

dost <- data.frame("DIST" = c("pakpattan","pakpattan","nishtar town","lahore city"))
dost$DIST <- sapply(dost$DIST,solve_name)

hh_df[(hh_df$TEHSIL == "SAHIWAL" & hh_df$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL"
wm_df[(wm_df$TEHSIL == "SAHIWAL" & wm_df$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL"
ch_df[(ch_df$TEHSIL == "SAHIWAL" & ch_df$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL"


### Recode Binary Survey Responses

recode <- function(x) {
  switch(as.character(x),
         'Yes' = 1,
         'No' = 0,
         'Missing' = NA,
         'DK' = NA,
         as.numeric(x)
  )
}



# functions for data cleaning and manipulation ----

## Function to recode variables of each survey dataset (factor to numeric)

recode_values <- function(df,y,res){
  if(res == 1){
    x <- "DISTRICT"
  }
  else{
    x <- "TEHSIL"
  }
  df <- df[complete.cases(df[,y]),]
  df[,y] <- as.numeric(unlist(sapply(df[,y],recode)))
  df <- df[complete.cases(df[,y]),]
  if(res == 1){
    df <- data.frame(df[,c(x,y)] %>% group_by(DISTRICT) %>% summarise_each(funs(mean)))
  }
  if(res == 2){
    df <- df[complete.cases(df$TEHSIL),]
    df <- data.frame(df[,c(x,y)] %>% group_by(TEHSIL) %>% summarise_each(funs(mean)))
  }
  df
}


## Function to attain binary variables from MICS dataset and integrate with Tehsil Data

get_var <- function(df1,df2,attr,res){
  if(res==1){
    x <- "DISTRICT"
  }
  else{
    x <- "TEHSIL"
  }
  df1[,attr] <- 0
  df <- merge(df1,recode_values(df2,attr,res),by=x, all.x = T)
  df[,attr]<- df[,ncol(df)]
  if(res==1){
    df <- df[,-c(ncol(df)-2,ncol(df)-1)]
  }
  df
}


## Function to attain multi-level categorical variables from MICS dataset and integrate with Tehsil Data

include_var <- function(df1,df2,attr,res){
  
  df1[,attr] <- 0
  if(NROW(df2[which(df2[,attr] == "Missing"),])>0){
    df2[which(df2[,attr] == "Missing"),][,attr] <- NA
  }
  df2 <- df2[complete.cases(df2[,attr]),]
  if(res == 1){
    x <- "DISTRICT"
    df2 <- data.frame(df2[,c(x,attr)] %>% group_by(DISTRICT) %>% summarise_each(funs(mean)))
  }
  else {
    x <- "TEHSIL"
    df2 <- data.frame(df2[,c(x,attr)] %>% group_by(TEHSIL) %>% summarise_each(funs(mean)))
  }
  df <- merge(df1,df2,by=x, all.x=T)
  df[,attr]<- df[,ncol(df)]
  if(res==1){
    df <- df[,-c(ncol(df)-2,ncol(df)-1)]
  }
  df
}


## Fix Childrens dataset DTP Data responses

ch_df[which(ch_df$dtp_number == "Missing"),]$dtp_number <- NA
ch_df$dtp_number <- as.numeric(ch_df$dtp_number)
ch_df[which(ch_df$dtp_number > 3),]$dtp_number <- 3


## Recode Education Field in MICS Womens Datasets and attain mode at the tehsil level

recode_education <- function(x) {
  switch(as.character(x),
         'Preschool' = 1,
         'Primary' = 2,
         'Middle' = 3,
         'Matric' = 4,
         'Above Matric' = 5,
         'Missing' = NA,
         as.numeric(x)
  )
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


include_edu <- function(df1,df2,attr) {
  df1[,attr] <- 0
  if(NROW(df2[which(df2[,attr] == "Missing"),])>0){
    df2[which(df2[,attr] == "Missing"),][,attr] <- NA
  }
  df2 <- df2[complete.cases(df2[,attr]),]
  
  x <- "TEHSIL"
  df2 <- data.frame(df2[,c(x,attr)] %>% group_by(TEHSIL) %>% summarise( edu_mode = getmode(school_level)))
  
  df <- merge(df1,df2,by=x, all.x=T)
  df[,attr]<- df[,ncol(df)]
  df
}




# summarize survey results to tehsil level ----

## Apply recoded education field  to MICS womens dataset

wm_df$school_level <- sapply(wm_df$school_level,recode_education)

## Recode Urbanicity Field in MICS Datasets

recode_urban <- function(x){
  
  
  switch(as.character(x),
         'Rural' = 0,
         'All Urban' = 1,
         'Missing' = NA,
         as.numeric(x)
  )
}


## Apply recoded urbanicity field to MICS datasets

hh_df$hh_urban <- sapply(hh_df$urban,recode_urban)
ch_df$ch_urban <- sapply(ch_df$urban,recode_urban)
wm_df$wm_urban <- sapply(wm_df$urban,recode_urban)
hh_df[(hh_df$hh_urban == 2),]$hh_urban <- 1
ch_df[(ch_df$ch_urban == 2),]$ch_urban <- 1
wm_df[(wm_df$wm_urban == 2),]$wm_urban <- 1


## Integrate Tehsil level data with Covariates from MICS using previously developed functions

### binary categorical vars
tehsils <- get_var(tehsils,hh_df,"radio",2)
tehsils <- get_var(tehsils,hh_df,"electricity",2)
tehsils <- get_var(tehsils,hh_df,"television",2)
tehsils <- get_var(tehsils,hh_df,"mobile_phone",2)
tehsils <- get_var(tehsils,ch_df,"dtp_boolean",2)
tehsils <- get_var(tehsils,ch_df,"card",2)
tehsils <- get_var(tehsils,wm_df,"school_boolean",2)
tehsils <- get_var(tehsils,wm_df,"antenatal_care",2)

### continuous
tehsils <- include_var(tehsils,ch_df,"age",2)
tehsils <- include_var(tehsils,wm_df,"mothers_age",2)
tehsils <- include_var(tehsils,ch_df,"dtp_number",2)

### urban_rural_ratio
tehsils <- get_var(tehsils,wm_df,"wm_urban",2)
tehsils <- get_var(tehsils,hh_df,"hh_urban",2)
tehsils <- get_var(tehsils,ch_df,"ch_urban",2)

tehsils$urban_to_rural <- (tehsils$ch_urban + tehsils$wm_urban + tehsils$hh_urban) /3

### educatin mode
tehsils <- include_edu(tehsils,wm_df,"school_level")

