## Purpose: To create figures discussed in the manuscript


source(file='PreRun.r')
library(corrplot)
library("sf")


# Correlation plots ----

## Tehsil level ----

tehsils <- read.csv("results/tehsils_complete_9.15.csv")

tehsils.plot <- tehsils[-c(24,25,31,61,113), c(28,27,26,3:17,19,21,23,22)] %>%
  rename("Outreach Proportion" = OutreachProportion,
         "Clinic Vacc Covereage"= TotalClinicsCoverage, "Outreach Vacc Coverage" =TotalOutreachCoverage, "Fertility" = fertility, 
         "Child Population"=child_population, "Population Density"=population_density, 
         "Distance to Cities"=distance_to_cities, "Urban Vs Rural" =urban_to_rural, "Poverty"=poverty, "Distance to Lakes/Rivers" =distance_to_lakes_rivers, 
         "Elevation"=elevation, "Antenatal Care"=antenatal_care, "Vaccination Card"=card, "Electricity"=electricity, "Television"=television, "Maternal Education"=edu_mode,
         "Mobile Phone"=mobile_phone, "Radio"=radio, "Mother Age"=mothers_age, "# of Clinics"=fac_number, "Night Lights"=night_lights)

tehsils.plot <- tehsils.plot[complete.cases(tehsils.plot),]

tehsils.cor <- cor(tehsils.plot[complete.cases(tehsils.plot),], method = c("pearson"))

corrplot(tehsils.cor, tl.col = "black", tl.cex = 1.8, tl.srt = 45, cl.cex = 1.8)



## UC level ----

ucs <- read.csv("results/uc_complete_clean.csv")

ucs.plot <- ucs[,-c(1:5)] %>%   # 7 features 
  as.data.frame() %>%
  na.omit() %>%
  dplyr::select(12:10,7:1) %>%
  rename("Outreach Proportion" = OutreachProportion,"Clinic Vacc Covereage"= TotalClinicsCoverage, "Outreach Vacc Coverage" =TotalOutreachCoverage, 
         "Fertility" = fertility, "Child Population"=child_population, "Distance to Cities"=distance_to_cities, "Poverty"=poverty,"Elevation"=elevation,
         "Population density" = population_density)

ucs.cor <- cor(ucs.plot, method = c("pearson"))

corrplot(ucs.cor, tl.col = "black", tl.cex = 2.2, tl.srt = 45, cl.cex =2.2)



# Figure 2 ----

## prep ----

tehsils.map <- tehsils %>%
  mutate(clinic_per_child = fac_number / child_population)

theme_set(theme_void())

my_theme <- theme(legend.position = c(0.9, 0.2),
                  legend.title = element_text(colour="black", size=48, face="bold"),
                  legend.text=element_text(size=40),
                  legend.key.size = unit(3, 'cm'))

## read in punjab province shape data

punjab.polygon <- st_read("D:/Xiaoting/Vaccination_Project/VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp") %>%
  filter(PROVINCE == "PUNJAB") %>%
  mutate(TEHSIL = sapply(TEHSIL,solve_name)) 

punjab.polygon[which(punjab.polygon$TEHSIL == "SAHIWAL" & punjab.polygon$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL"

punjab.map <- merge(punjab.polygon, tehsils.map[,c(2,22:29)], by = "TEHSIL", all.x = T)


## single plots ----

## Fig 2A
fac_num <- ggplot(punjab.map) + 
  geom_sf(aes(fill=fac_number)) +
  scale_fill_gradient(name = "Number of\nClinics", low="lightgreen", high="darkgreen") +
  my_theme 

## Fig 2B
in_clinic <- ggplot(punjab.map) + 
  geom_sf(aes(fill=TotalClinicsCoverage)) +
  scale_fill_gradient(name = "In-clinic vacc per\nchild capita", low="lightgreen", high="darkgreen", breaks = seq(0,0.15,0.025)) +
  my_theme 

## Fig 2C
outreach <- ggplot(punjab.map) + 
  geom_sf(aes(fill=TotalOutreachCoverage)) +
  scale_fill_gradient(name = "Outreach vacc per\nchild capita", low="lightgreen", high="darkgreen", breaks = seq(0.0,0.7,0.1)) +
  my_theme 

## Fig 2D
proportion <- ggplot(punjab.map) + 
  geom_sf(aes(fill=OutreachProportion)) +
  scale_fill_gradient(name = "Outreach\nProportion", low="lightgreen", high="darkgreen") +
  my_theme

## Fig 2E
tehsils.scatter <- tehsils[order(tehsils$OutreachProportion, decreasing = T),] %>%
  mutate(Tehsil = as.numeric(1:137))
theme_set(theme_classic())
e <- ggplot(tehsils.scatter, aes(x=Tehsil, y= OutreachProportion)) +
  geom_point(size=7, shape=1) +
  scale_x_continuous(breaks = seq(from = 0, to = 140, by = 20)) +
  scale_y_continuous(breaks = seq(from = 0.5, to = 1, by = 0.1)) +
  xlab("Tehsil (index)") + 
  ylab("Outreach/all vaccination ratio") +
  theme(axis.title = element_text(size = 45,color = "black", face="bold"),
        axis.text = element_text(size =40))

## map on the upper left
pak <- getData("GADM", country="PK", level=1)
pak.province <- fortify(pak, region = "NAME_1") %>%
  mutate(punjab = 0)
pak.province[which(pak.province$id == "Punjab"),]$punjab <- 1
theme_set(theme_void())
pak <- ggplot(pak.province, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(color = as.factor(punjab), fill = as.factor(punjab)), size = 1.5) +
  scale_color_manual(values = c('1' = 'red', '0' = "Black")) +
  scale_fill_manual(values =  c('1' = 'white', '0' = "white")) +
  theme(legend.position = "none")

