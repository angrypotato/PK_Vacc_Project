## Purpose: To Extract Relevant Geographic and Demographic Covariates and Map Them To Tehsil and District Level Geographic Data 


# Call Source File for Required Libraries and Functions ----

source(file='PreRun.r')



# Get Geographic Data from Shapefile ----

## Tehsil (Town) level 
tehsils_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp")
tehsils <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp")
tehsils@data$id <- rownames(tehsils@data)

## UC level 
uc_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs@data$id <- rownames(ucs@data)

## Convert to Data Frames and Clean Names ----
tehsils <- data.frame(tehsils)
ucs <- data.frame(ucs)

### Isolate Punjab Data
tehsils <- tehsils[which(tehsils$PROVINCE == 'PUNJAB'),]
ucs <- ucs[which(ucs$PROVINCE == 'Punjab'),] %>%
  mutate(UC = toupper(UC))

### Clean Geographic Field Names
tehsils$TEHSIL <- sapply(tehsils$TEHSIL,solve_name)
ucs <- ucs[!duplicated(ucs$UC),]   ### instead of solve_uc_name

### Remove Tehsils that were Mistakenly Labelled as being in Punjab
tehsils <- tehsils[!(tehsils$TEHSIL %in% c('RAZMAK')),]   

### Sahiwal is the name of 2 tehsils, one in Sahiwal district and another outside, therefore lets name the sahiwal in the sahiwal district - SAHIWAL_SAHIWAL
tehsils[which(tehsils$TEHSIL == "SAHIWAL" & tehsils$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL"



# Join Fertility, Elevation, Poverty and Night Lights Covariates ----

tehsils <- get_geovars("VaccinationStudy/Data/PAK_births_pp_v2_2015.tif","fertility",1)
tehsils <- get_geovars("VaccinationStudy/Data/PAK_msk_alt/PAK_msk_alt.grd","elevation",1)
tehsils <- get_geovars("VaccinationStudy/Data/pak07povmpi.tif","poverty",1)
tehsils <- get_geovars("VaccinationStudy/Data/NLDI_2006_0p25_rev20111230.tif","night_lights",1)

ucs <- get_geovars_uc("VaccinationStudy/Data/PAK_births_pp_v2_2015.tif","fertility")
ucs <- get_geovars_uc("VaccinationStudy/Data/PAK_msk_alt/PAK_msk_alt.grd","elevation")
ucs <- get_geovars_uc("VaccinationStudy/Data/pak07povmpi.tif","poverty")


# Distance to Lakes/Rivers Covariate ----

river<-readOGR("VaccinationStudy/Data/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
river@data$id <- rownames(river@data)
river_lines.df <- fortify(river)

coordinates(river_lines.df)<- ~long +lat
proj4string(river_lines.df) <- proj4string(tehsils_shp)

river_pts <- over(river_lines.df, tehsils_shp)
river_lines.df$number <- 1
river_binded <- cbind(river_pts, river_lines.df$number)
river_binded_df <- data.frame("District" = river_binded[,3],"Tehsil" = river_binded[,4], "Population" = river_binded[,9])
river_binded_df<- river_binded_df %>% 
  mutate(Tehsil = toupper(Tehsil))
river_binded_df[which(river_binded_df$Tehsil == "SAHIWAL" & river_binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"

river_poptable <- table(river_binded_df$Tehsil)
river_pop_df <- data.frame("Tehsil" = names(river_poptable))
river_pop_df$rivers <- 0

for(i in 1:length(river_poptable)){
  river_name <- names(river_poptable)[i]
  river_cells <-river_binded_df[which(river_binded_df$Tehsil == river_name),]
  river_pop<- sum(river_cells$Population,na.rm=TRUE)
  river_pop_df[which(river_pop_df$Tehsil == river_name),]$rivers <- river_pop
}
river_pop_dfs <- river_pop_df[which(river_pop_df$Tehsil %in% tehsils$TEHSIL),]

tehsils$distance_to_lakes_rivers <- 0
for(i in 1:NROW(river_pop_dfs)){
  river_cell <- river_pop_dfs[i,]
  river_cell$Tehsil <- sapply(river_cell$Tehsil,solve_name)
  if(NROW(river_cell)>0){
    tehsils[which(tehsils$TEHSIL == toupper(river_cell$Tehsil)),]$distance_to_lakes_rivers <- river_cell$rivers
  }
  
}



# Distance to City Covariates ----

## Tehsil ----
load('VaccinationStudy/Data/distance_to_cities_data.Rdata')
dis_df <- as.data.frame(res,xy=TRUE) ##

coordinates(dis_df)<- ~x +y
proj4string(dis_df) <- proj4string(tehsils_shp)

dist_pts <- over(dis_df, tehsils_shp)
dist_binded <- cbind(dist_pts, dis_df$accessibility_to_cities_2015_v1.0)
dist_binded_df <- data.frame("District" = dist_binded[,3],"Tehsil" = dist_binded[,4], "Population" = dist_binded[,9])
dist_binded_df<- dist_binded_df %>% 
  mutate(Tehsil = toupper(Tehsil))

dist_binded_df[which(dist_binded_df$Tehsil == "SAHIWAL" & dist_binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"

dist_poptable <- table(dist_binded_df$Tehsil)
dist_pop_df <- data.frame("Tehsil" = names(dist_poptable))
dist_pop_df$distance_to_cities <- 0

for(i in 1:length(dist_poptable)){
  dist_name <- names(dist_poptable)[i]
  dist_cells <-dist_binded_df[which(dist_binded_df$Tehsil == dist_name),]
  dist_pop<- mean(dist_cells$Population,na.rm=TRUE)
  dist_pop_df[which(dist_pop_df$Tehsil == dist_name),]$distance_to_cities <- dist_pop
}
tehsils$distance_to_cities <- 0
for(i in 1:NROW(dist_pop_df)){
  dist_cell <- dist_pop_df[i,]
  dist_cell$Tehsil <- sapply(dist_cell$Tehsil,solve_name)
  if(NROW(tehsils[which(tehsils$TEHSIL == toupper(dist_cell$Tehsil)),]$distance_to_cities) > 0){
    tehsils[which(tehsils$TEHSIL == toupper(dist_cell$Tehsil)),]$distance_to_cities <- dist_cell$distance_to_cities
  }
}


## UC ----
load('VaccinationStudy/Data/distance_to_cities_data.Rdata')
dis_df <- as.data.frame(res,xy=TRUE) ##

coordinates(dis_df)<- ~x +y
proj4string(dis_df) <- proj4string(uc_shp)

dist_pts <- over(dis_df, uc_shp)
dist_binded <- cbind(dist_pts, dis_df$accessibility_to_cities_2015_v1.0)
dist_binded_df <- data.frame("District" = dist_binded[,2],"Tehsil" = dist_binded[,3], "UC" = dist_binded[ ,4], "Population" = dist_binded[,20])
dist_binded_df<- dist_binded_df %>% 
  mutate(UC = toupper(UC))

dist_poptable <- table(dist_binded_df$UC)
dist_pop_df <- data.frame("UC" = names(dist_poptable))
dist_pop_df$distance_to_cities <- 0

for(i in 1:length(dist_poptable)){
  dist_name <- names(dist_poptable)[i]
  dist_cells <-dist_binded_df[which(dist_binded_df$UC == dist_name),]
  dist_pop<- mean(dist_cells$Population,na.rm=TRUE)
  dist_pop_df[which(dist_pop_df$UC == dist_name),]$distance_to_cities <- dist_pop
}
ucs$distance_to_cities <- 0
for(i in 1:NROW(dist_pop_df)){
  dist_cell <- dist_pop_df[i,]
  if(NROW(ucs[which(ucs$UC == toupper(dist_cell$UC)),]$distance_to_cities) > 0){
    ucs[which(ucs$UC == toupper(dist_cell$UC)),]$distance_to_cities <- dist_cell$distance_to_cities
  }
}



# Extract Population Covariate ----
# Note: This file is very large and will take awhile to run

total_population <- read.csv(file = 'VaccinationStudy/Data/population_pak_2018-10-01.csv')
tehsils$Population <- 0
ucs$Population <- 0

coordinates(total_population)<- ~longitude +latitude

## tehsil ----
proj4string(total_population) <- proj4string(tehsils_shp)

tehsil_pts <- over(total_population, tehsils_shp)

total_binded <- cbind(tehsil_pts, total_population$population_2020)

total_binded_df <- data.frame("District" = total_binded[,3], "Tehsil" = total_binded[,4], "Population" =  total_binded[,9])
total_binded_df<- total_binded_df %>% 
  mutate(Tehsil = toupper(Tehsil))
total_binded_df<- total_binded_df %>% 
  mutate(District = toupper(District))

total_binded_df[which(total_binded_df$Tehsil == "SAHIWAL" & total_binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"

total_poptable <- table(total_binded_df$Tehsil)
total_pop_df <- data.frame("Tehsil" = names(total_poptable))
total_pop_df$population <- 0

for(i in 1:length(total_poptable)){
  total_name <- names(total_poptable)[i]
  total_cells <- total_binded_df[which(total_binded_df$Tehsil == total_name),]
  total_pop<- sum(total_cells$Population,na.rm=TRUE)
  total_pop_df[which(total_pop_df$Tehsil == total_name),]$population <- total_pop
}
tehsils$Population <- NA
total_pop_dfs <- total_pop_df[which(total_pop_df$Tehsil %in% tehsils$TEHSIL),]
for(i in 1:NROW(total_pop_dfs)){
  cell <- total_pop_dfs[i,]
  name <- solve_name(cell$Tehsil)
  tehsils[which(tehsils$TEHSIL == toupper(name)),]$Population <- cell$population
}


## UC ----
proj4string(total_population) <- proj4string(uc_shp)

uc_pts <- over(total_population, uc_shp)

total_binded <- cbind(uc_pts, total_population$population_2020)

total_binded_df <- data.frame("UC" = total_binded[,4], "Population" =  total_binded[,20])
total_binded_df<- total_binded_df %>% 
  mutate(UC = toupper(UC))

total_poptable <- table(total_binded_df$UC)
total_pop_df <- data.frame("UC" = names(total_poptable))
total_pop_df$population <- 0

for(i in 1:length(total_poptable)){
  total_name <- names(total_poptable)[i]
  total_cells <- total_binded_df[which(total_binded_df$UC == total_name),]
  total_pop<- sum(total_cells$Population,na.rm=TRUE)
  total_pop_df[which(total_pop_df$UC == total_name),]$population <- total_pop
}
ucs$Population <- NA
total_pop_dfs <- total_pop_df[which(total_pop_df$UC %in% ucs$UC),]

for(i in 1:NROW(total_pop_dfs)){
  cell <- total_pop_dfs[i,]
  ucs[which(ucs$UC == toupper(cell$UC)),]$Population <- cell$population
}



# Extract Child Population Covariate ----
# Note: This file is very large and will take awhile to run

child_population <- read.csv(file = 'VaccinationStudy/Data/PAK_children_under_five_2019-08-03.csv')
coordinates(child_population)<- ~longitude +latitude

## tehsil ----
proj4string(child_population) <- proj4string(tehsils_shp)

pts <- over(child_population, tehsils_shp)

binded <- cbind(pts, child_population$population)

binded_df <- data.frame("District" = binded[,3], "Tehsil" = binded[,4], "Population" = binded[,9])
binded_df<- binded_df %>% 
  mutate(Tehsil = toupper(Tehsil))
binded_df<- binded_df %>% 
  mutate(District = toupper(District))

binded_df[which(binded_df$Tehsil == "SAHIWAL" & binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"
poptable <- table(binded_df$Tehsil)
pop_df <- data.frame("Tehsil" = names(poptable))
pop_df$population <- 0

for(i in 1:length(poptable)){
  name <- names(poptable)[i]
  cells <- binded_df[which(binded_df$Tehsil == name),]
  pop<- sum(cells$Population,na.rm=TRUE)
  name <- solve_name(name)
  pop_df[which(pop_df$Tehsil == name),]$population <- pop
}
pop_df <- pop_df[which(pop_df$Tehsil %in% tehsils$TEHSIL),]
tehsils$child_population <- NA
for(i in 1:NROW(pop_df)){
  cell <- pop_df[i,]
  name <- solve_name(cell$Tehsil)
  tehsils[which(tehsils$TEHSIL == name),]$child_population <- cell$population
}


## UC ----

proj4string(child_population) <- proj4string(uc_shp)

pts <- over(child_population, uc_shp)

binded <- cbind(pts, child_population$population)

binded_df <- data.frame("UC" = binded[,4], "Population" = binded[,20])
binded_df<- binded_df %>% 
  mutate(UC = toupper(UC))

poptable <- table(binded_df$UC)
pop_df <- data.frame("UC" = names(poptable))
pop_df$population <- 0

for(i in 1:length(poptable)){
  name <- names(poptable)[i]
  cells <- binded_df[which(binded_df$UC == name),]
  pop <- sum(cells$Population,na.rm=TRUE)
  pop_df[which(pop_df$UC == name),]$population <- pop
}
pop_df <- pop_df[which(pop_df$UC %in% ucs$UC),]
ucs$child_population <- NA
for(i in 1:NROW(pop_df)){
  cell <- pop_df[i,]
  name <- cell$UC
  ucs[which(ucs$UC == name),]$child_population <- cell$population
}



# Population Density ----

tehsils$population_density <- 0
districts$population_density <- 0
ucs$population_density <- 0


tehsils$population_density <- tehsils$Population / tehsils$Shape_Area
districts$population_density <- districts$Population / districts$Shape_Area
ucs$population_density <- ucs$Population / ucs$Shape_Area


