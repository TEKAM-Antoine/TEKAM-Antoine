#################### chargement des packages

library(raster) # Manipulating rasters
library(rgdal) # GIS data
library(tidyverse) # has dplyr
library(colorRamps)
library(stringr)
library(readxl)
library(writexl)
library(data.table)
library(lubridate)
library(patchwork)

r2 <- matlab.like2(200)
require(sf) # GIS data



setwd("C:/Users/Etudiants/Documents/Antoine_Tekam/Base de données/")
getwd()

# charger OU LIRE UN fichier excel (dat, dataset, etc sONT DES OBJETS)

dat <- read_excel("~/Antoine_Tekam/Base de données/Pheno_Bouamir_Crowns.xlsx")
dat

### quelle est la class ou les classes de l'object dataset
class(dat)

#### pour filtrer les VALEURS
sss <- c(1, 2, 3, NA)

is.na(sss)  ######## fonction pour filtrer (sss = objet créer)

#### pour avoir les noms des colonnes
colnames(dat) 

## filtrer les NA de la colonne Top_of_canopy
dat %>% 
  filter(is.na(Top_of_canopy))

filter(dat, is.na(Top_of_canopy))

### filtrer condition 1 OU condition 2 (| signifie ou et , signifie égal à)
dat %>% 
  filter(PH_ID == "PH128" | PH_ID == "PH129")

vec_selec <- c("PH128", "PH129", "PH130")

dat %>% 
  filter(PH_ID %in% vec_selec)

######## 
dat %>% 
  filter(!is.na(tax_gen)) %>%         ###(filtrer les données manquantes "NA" dans tax_gen)
  distinct(tax_gen) %>%           ##### pour les valeurs uniques
  arrange(tax_gen)               #### pour arranger par ordre alphabetique ou croisant ou décroissant
                                 ####  arrange(tax_gen) signifie arrange les genres par ordre croissant

 ### identification des familles
dat %>% 
  filter(!is.na(tax_fam)) %>% 
  distinct(tax_fam) %>% 
  arrange(tax_fam)


### nombre d'individu par famille
res_fam <- dat %>%             ####### res_fam est l'objet créer avec lequel le resultat serait enregistrer
  distinct(tax_fam, PH_ID) %>%
  filter(!is.na(tax_fam)) %>%   ### filter les NA dans la colonne tax_fam
  group_by(tax_fam) %>%         ##### pour grouper les informations d'une colonne ou pluisieurs colonnes
  count() %>%                   ###   
  arrange(desc(n))              ### arrange (desc(n) signifie arrange le nbre d'ind par famille en ordre decroissant

write_xlsx(res_fam, path = "Base de données/Resultat/Resultat Pheno/fam_ind.xlsx")    ### permet d'enregistrer les resultats en fichier excel (ne pa oublier l'instation .xlsx)

### pour avoir le nombre ind par genre
res_gen <- dat %>% 
  distinct(tax_gen, PH_ID) %>%
  filter(!is.na(tax_gen)) %>% 
  group_by(tax_gen) %>% 
  count() %>% 
  arrange(desc(n))

write_xlsx(res_gen, path = "Base de données/Resultat/Resultat Pheno/gen_ind.xlsx")

### nombre ind par espece
res_sp <- dat %>% 
  distinct(Gns_spc, PH_ID) %>%
  filter(!is.na(Gns_spc)) %>% 
  group_by(Gns_spc) %>% 
  count() %>% 
  arrange(desc(n))

write_xlsx(res_sp, path = "Base de données/Resultat/Resultat Pheno/sp_ind.xlsx")


fam_sp <- dat %>% 
  filter(!is.na(tax_fam)) %>% 
  distinct(Gns_spc, tax_fam)

res_sp <- res_sp %>% 
  left_join(fam_sp, by = c("Gns_spc" = "Gns_spc")) %>% 
  relocate(tax_fam, .before = Gns_spc)

write_xlsx(res_sp, path = "Base de données/Resultat/Resultat Pheno/sp_ind.xlsx")



#### graphe nbre ind par espece
p <- 
  ggplot(data = res_sp, aes(x = reorder(Gns_spc, -n), y = n)) +
  geom_bar(stat="identity") +
  scale_x_discrete(guide = guide_axis(angle = (60))) +             ##### on peut charger la valeur de l'angle d'inclinaison 60 
  xlab(label = "species") +
  ylab(label = "Number of individual") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5)
p

##### classe de diametre
dbh_sp <- dat %>% 
  distinct(PH_ID, dbh_cm_Sep21, Gns_spc) %>% 
  filter(!is.na(Gns_spc))

### Calcul les diametres moyennes

mean_dbh <- dbh_sp %>% 
  group_by(Gns_spc) %>% 
  summarise(mean_dbh = mean(dbh_cm_Sep21, na.rm = T))
view(mean_dbh)

write_xlsx(mean_dbh, path = "Base de données/Resultat/Resultat Pheno/mean_dbh.xlsx")

#### joindre les valeurs des diametres moyennes des especes

dbh_sp <- dbh_sp %>% 
  left_join(mean_dbh, by = c("Gns_spc" = "Gns_spc"))

write_xlsx(dbh_sp, path = "Base de données/Resultat/Resultat Pheno/dbh_sp.xlsx")


### 
p <- 
  ggplot(dbh_sp, aes(x = reorder(Gns_spc, -mean_dbh), y = dbh_cm_Sep21)) +
  geom_violin() +
  scale_x_discrete(guide = guide_axis(angle = 60)) +
  xlab(label = "Species") +
  ylab(label = "DBH (cm)")
p


############################################################################################################################

##### Phenologie graphes #####

min(dat$date_census, na.rm = T) ## première date d'observation
max(dat$date_census, na.rm = T) ## dernière date d'observation

### voir la première et dernière observation pour chaque individu

dat %>% 
  select(date_census, PH_ID)%>% 
  group_by(PH_ID) %>% 
  summarise(min = min(date_census),
            max = max(date_census))



### ajouter une colonne avec le mois d'observation
dat <- dat %>% 
  mutate(month = month(date_census))
str(dat)

### ajouter une colonne avec l'année d'observation
dat <- dat %>% 
  mutate(year = year(date_census))

### ajouter une colonne qui contient une concatenation du mois et de l'année
dat <- dat %>% 
  mutate(year_month = paste(month, year, sep = "_"))

sub_data <- 
  dat %>%
  select(PH_ID, Gns_spc, year_month, starts_with("fruit_"),starts_with("flower_"),starts_with("leaves_"), date_census,dbh_cm_May21,dbh_cm_Sep21) %>% #### augmenter les starts_with("leaves_") pour les graphes des feuilles
  filter(!is.na(date_census)) %>% 
  mutate(date_census = as.Date(date_census))

sub_data %>% 
  distinct(Gns_spc) %>% 
  view()


species_selected <- "Xylopia rubescens"    #### on remplace le nom de l'espéce pour trouver les graphes des autres par exemple"Heisteria zimmereri"

sub_data_species <- sub_data %>% 
  filter(Gns_spc == species_selected)

sub_data_species %>% 
  distinct(PH_ID)

########

########### pour les feuilles ##################################################################

### jeunes feuille

p_leaves_young <-
  ggplot(
    sub_data_species %>% filter(!is.na(leaves_young)),
    aes(
      x = date_census,
      y = as.factor(leaves_young),
      colour = PH_ID,
      group = PH_ID,
      size=dbh_cm_Sep21
    )
  ) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", guide = guide_axis(angle = 60)) +
  xlab("Date of observation") +
  ylab(label = "Index of abundance of leaves_young") +
  ggtitle(label = paste("leaves_young for", species_selected))
p_leaves_young


#################################################################################################
####### Feuilles matures

p_leaves_old <-
  ggplot(
    sub_data_species %>% filter(!is.na(leaves_old)),
    aes(size= dbh_cm_Sep21,
      x = date_census,
      y = as.factor(leaves_old),
      colour = PH_ID,
      group = PH_ID
    )
  ) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", guide = guide_axis(angle = 60)) +
  xlab("Date of observation") +
  ylab(label = "Index of abundance of leaves_old") +
  ggtitle(label = paste("leaves_old for", species_selected))
p_leaves_old

###########################################################################################################
#####  veilles feuilles
p_leaves_old <-
  ggplot(
    sub_data_species %>% filter(!is.na(leaves_old)),
    aes(size= dbh_cm_Sep21,
      x = date_census,
      y = as.factor(leaves_old),
      colour = PH_ID,
      group = PH_ID
    )
  ) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", guide = guide_axis(angle = 60)) +
  xlab("Date of observation") +
  ylab(label = "Index of abundance of leaves_old") +
  ggtitle(label = paste("leaves_old for", species_selected))

p_leaves_old


##### pour les fleurs

###### fleur ouverte
p_open_flower <-
  ggplot(
    data = sub_data_species %>% filter(!is.na(flower_open)), 
    mapping = aes(size= dbh_cm_Sep21,                         #### on ajoute size= dbh_cm_May21, dans mapping=aes( ie mappingmapping=aes(size= dbh_cm_May21,
      x = date_census,
      y = as.factor(flower_open),
      colour = PH_ID,
      group = PH_ID
    )
  ) +                                                   ########## refaire la meme proceduire pour les feuilles en remplecant les flower_open en leaves_young, etc

  geom_point() +
  scale_x_date(date_breaks = "1 month", guide = guide_axis(angle = 60)) +     ######### remplacer geom_line en geom_point pour avoir les plots en point en double cliquant sur les differents elements p_open_flower,p_fruit_ripe, etc  
  xlab(label = "Date of observation") +
  ylab(label = "Index of abundance of open flowers") +
  ggtitle(label = paste("Flowering for", species_selected))
p_open_flower

###################################################################################################
######## bourgeon fleur
p_flower_bud <-
  ggplot(
    sub_data_species %>% filter(!is.na(flower_bud)),
    aes(size= dbh_cm_Sep21,
      x = date_census,
      y = as.factor(flower_bud),
      colour = PH_ID,
      group = PH_ID
    )
  ) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", guide = guide_axis(angle = 60)) +
  xlab("Date of observation") +
  ylab(label = "Index of abundance of bud flowers") +
  ggtitle(label = paste("Flower bud for", species_selected))

p_flower_bud

########################################################################################################
######## pour les fruits

#### fruit mur

p_fruit_ripe <-
  ggplot(
    sub_data_species %>% filter(!is.na(fruit_ripe)),
    aes(size= dbh_cm_Sep21,
      x = date_census,
      y = as.factor(fruit_ripe),
      colour = PH_ID,
      group = PH_ID
    )
  ) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", guide = guide_axis(angle = 60)) +
  xlab("Date of observation") +
  ylab(label = "Index of abundance of riped fruits") +
  ggtitle(label = paste("Fruit ripe for", species_selected))

p_fruit_ripe


####### fruit non mur
p_fruit_unripe <- ggplot(sub_data_species %>% filter(!is.na(fruit_unripe)), 
                         aes(size= dbh_cm_Sep21,
                           x = date_census, 
                             y = as.factor(fruit_unripe), 
                             colour = PH_ID,
                             group = PH_ID)) +
  geom_point() + 
  scale_x_date(date_breaks = "1 month", guide = guide_axis(angle = 60)) +
  xlab("Date of observation") +
  ylab(label = "Index of abundance of unripe fruit") +
  ggtitle(label = paste("Unripe fruit for", species_selected))

p_fruit_unripe

######### grouper les graphes ####################################################################
 
all_plot <-  
p_leaves_young + p_leaves_old + p_leaves_old + p_fruit_unripe + p_flower_bud  + p_fruit_ripe + p_open_flower + plot_layout(ncol = 2) ##########  ajouter leaves_young,	leaves_old,	leaves_old, pour avoir les graphes avec different elements et double clic sur all_plot

all_plot

########## Enregistrer dans "graphes"
ggsave(
  filename = paste0("graphes/", gsub(" ", "_", species_selected), ".png"),
  plot = all_plot,
  width = 10,
  height = 14
)

##############################################################################################################
### proportion d'individu avec fruits toutes espèces comprises

sub_data %>% 
  mutate(fruiting = ifelse(fruit_ripe > 0, 1, 0)) %>% 
  group_by(year_month) %>% 
  summarise(nbe_fruiting = sum(fruiting),
            n = n()) %>% 
  mutate(nbe_fruiting = replace(nbe_fruiting, is.na(nbe_fruiting), 0)) %>% 
  mutate(prop = nbe_fruiting/n*100)

write_xlsx(sub_data, path = "C:/Users/Etudiants/Documents/Antoine_Tekam/Base de données/Resultat/Resultat Pheno/Proportion_fruit.xlsx")


###########################################################################################################################

###### ajouter les données de précipitation

############## importer Bouamir weather summary

dat <- read_excel("~/Antoine_Tekam/Base de données/Pheno_Bouamir_Crowns.xlsx")


###################################################################################
######## import total species data

table(dat$Gns_spc)
Xylopia_rubescens <- subset(dat, Gns_spc=="Xylopia rubescens")                   ########### remplacer le nom d'autres espèces pour trouver leur resultat
dim(dat)
dim(Xylopia_rubescens)

head(Xylopia_rubescens)

Xylopia_rubescens$Year <- str_sub(Xylopia_rubescens$date_census,1,4)
Xylopia_rubescens$Month <- str_sub(Xylopia_rubescens$date_census,6,7)

# import total weather data
weather_total <- read_excel("~/Antoine_Tekam/Base de données/Bouamir_weather_summary_Antoine.xls")

head(weather_total)

# weather_total <- weather_total[,c(1:6)]
head(weather_total)

# create Year column
# weather_total$Year <- str_sub(weather_total$date,1,4)

# head(weather_total)

# create Month column
# weather_total$Month <- str_sub(weather_total$date,6,7)

# head(weather_total)

# subset to 2020 and 2021

subset_dat <- subset(weather_total, Year %in% c(2020,2021))

table(subset_dat$Year)


# find mean Monthly rainfall for 2020 and 2021 and total
mean_dat <- weather_total %>% group_by(Month, Year) %>% 
  summarize(mean_Monthly = mean(Rainfall_sum, na.rm=T),
            sd_Monthly = sd(Rainfall_sum, na.rm=T),
            n_dat = n())

head(mean_dat)

subset_dat <- subset(mean_dat, Year %in% c(2020,2021))

table(subset_dat$Year)

head(mean_dat)

# mean_Monthly_dat <- mean_dat %>% group_by(Month) %>% 
#  summarize(mean_annual = mean(mean_Monthly, na.rm=T))

mean_Monthly_dat_v2 <- weather_total %>% group_by(Month) %>% 
  summarize(mean_Monthly = mean(Rainfall_sum, na.rm=T),
            sd_Monthly = sd(Rainfall_sum, na.rm=T),
            n_dat = n())


head(mean_Monthly_dat_v2)
mean_Monthly_dat_v2$Year <- rep("2017-2022", length(mean_Monthly_dat_v2$Month))
head(mean_Monthly_dat_v2)

mean_Monthly_dat_v2

# calculate confidence interval for X? (precip data)
# 95th confidence interval = the mean +- 1.960 * (standard deviation / square root of the sample size) 
mean_Monthly_dat_v2$ci_lower <- mean_Monthly_dat_v2$mean_Monthly - 
  (1.960 * (mean_Monthly_dat_v2$sd_Monthly) / sqrt(mean_Monthly_dat_v2$n_dat))
mean_Monthly_dat_v2$ci_upper <- mean_Monthly_dat_v2$mean_Monthly + 
  (1.960 * (mean_Monthly_dat_v2$sd_Monthly) / sqrt(mean_Monthly_dat_v2$n_dat))
head(mean_Monthly_dat_v2)

# plot pheno change with weather data

##################################################################################################################
# young leaves (find mean and sd)
yl_Xylopia_rubescens_sum_dat <- Xylopia_rubescens[!is.na(Xylopia_rubescens$Year),] %>% group_by(Month, Year) %>%    ########### [!is.na(Xylopia_rubescens$Year),] a été ajouté pour retirer les NA dans les données de Xylopia_rubescens 
  summarize(leaves_young_mean = mean(leaves_young, na.rm=T), leaves_young_sd = sd(leaves_young, na.rm=T))
yl_Xylopia_rubescens_sum_dat$leaves_young_mean_V2=yl_Xylopia_rubescens_sum_dat$leaves_young_mean*120

yl_Xylopia_rubescens_sum_dat=
  yl_Xylopia_rubescens_sum_dat %>% 
  mutate(per_t=leaves_young_mean/sum(leaves_young_mean)*100)
  
Xylopia_rubescens_new_leaf=
  ggplot() +
  geom_bar(data = yl_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=leaves_young_mean_V2, x=Month),
           position="dodge", stat="identity") +
  geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = factor(Year))) + 
  geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = factor(Year)), color = "#1f78b4") +
  # geom_smooth(data = mean_Monthly_dat, aes(x=Month, y=mean_annual, group = Year), linetype = 0, fill = "green")+
  geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % young leaves", breaks=c(0,60,120,180,240,300,360), labels=c("0","2","4","6","8","10","12"), ######### les valeurs de breaks ont été changés
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  # scale_y_continuous("mean % young leaves", breaks=c(0,100,200,300,400), labels=c("0","0-25","25-50","50-75","75-100"), ######### les valeurs de breaks ont été changés
  #                    sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("Month", labels=c("Mar","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +                            ###### les mois ont été remplacer de maras à descembre
  labs(title="Young Leaf Change over Time in Xylopia rubescens Tree Species", fill="Year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Xylopia_rubescens_new_leaf

ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_leaves_young.pdf")
ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_new_leaf.png")     ########## dossier d'enregistrement


###################################################################################################################
# Xylopia_rubescens_new_leaf <- ggplot() +
#   geom_bar(data = yl_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=leaves_young_mean_V2, x=Month),
#            position="dodge", stat="identity") +
#   geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = factor(Year))) +
#   geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = factor(Year)), color = "#1f78b4") +
#   # geom_smooth(data = mean_Monthly_dat, aes(x=Month, y=mean_annual, group = Year), linetype = 0, fill = "green")+
#   geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year),
#               fill="#1f78b4",alpha = 0.2) +
#   scale_y_continuous("mean % young leaves", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"),
#                      sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
#   scale_x_discrete("Month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
#   labs(title="Young Leaf Change over Time in Xylopia_rubescens Tree Species", fill="Year") +
#   theme_classic() + theme(plot.title = element_text(hjust = 0.5))
# 
# Xylopia_rubescens_new_leaf

###################################### les changement qui ont été fait pour trouver le graphe d'annickia


##################################################################################################################
# mature leaves
head(Xylopia_rubescens)

ml_Xylopia_rubescens_sum_dat <- Xylopia_rubescens[!is.na(Xylopia_rubescens$Year),] %>% group_by(Month, Year) %>% 
  summarize(leaves_mature_mean = mean(leaves_mature, na.rm=T), leaves_mature_sd = sd(leaves_mature, na.rm=T))
ml_Xylopia_rubescens_sum_dat

ml_Xylopia_rubescens_sum_dat$leaves_mature_mean_V2=ml_Xylopia_rubescens_sum_dat$leaves_mature_mean*120

ml_Xylopia_rubescens_sum_dat=
  ml_Xylopia_rubescens_sum_dat %>% 
  mutate(per_t=leaves_mature_mean/sum(leaves_mature_mean)*100)


Xylopia_rubescens_mature_leaf=
  ggplot() +
  geom_bar(data = ml_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=leaves_mature_mean_V2, x=Month),
           position="dodge", stat="identity") +
  geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = factor(Year))) + 
  geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = factor(Year)), color = "#1f78b4") +
  # geom_smooth(data = mean_Monthly_dat, aes(x=Month, y=mean_annual, group = Year), linetype = 0, fill = "green")+
  geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % leaves_mature", breaks=c(0,60,120,180,240,300,360), labels=c("0","2","4","6","8","10","12"), ######### les valeurs de breaks ont été changés
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  # scale_y_continuous("mean % leaves_mature", breaks=c(0,100,200,300,400), labels=c("0","0-25","25-50","50-75","75-100"), ######### les valeurs de breaks ont été changés
  #                    sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("Month", labels=c("Mar","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +                            ###### les mois ont été remplacer de maras à descembre
  labs(title="Mature Leaf Change over Time in Xylopia rubescens Tree Species", fill="Year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Xylopia_rubescens_mature_leaf

ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_mature_leaf.png")
ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_mature_leaf.pdf")


# Xylopia_rubescens_mature_leaf <- ggplot() +
#   geom_bar(data = ol_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=leaves_old_mean * 3, x=Month), 
#            position="dodge", stat="identity") + 
#   geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = Year)) + 
#   geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = Year), color = "#1f78b4") +
#   geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
#               fill="#1f78b4",alpha = 0.2) + 
#   scale_y_continuous("mean % mature leaves", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
#                      sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
#   scale_x_discrete("Month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
#   labs(title="Mature Leaf Change over Time in Xylopia_rubescens Tree Species", fill="Year") + 
#   theme_classic() + theme(plot.title = element_text(hjust = 0.5))
# 
# Xylopia_rubescens_mature_leaf
# 
# ggsave("analysis_plots/Xylopia_rubescens/Xylopia_rubescens_mature_leaf.pdf")
# ggsave("Xylopia_rubescens_mature_leaf.png")

##################################################################################################################
# old leaves
ol_Xylopia_rubescens_sum_dat <- Xylopia_rubescens[!is.na(Xylopia_rubescens$Year),] %>% group_by(Month, Year) %>% 
  summarize(leaves_old_mean = mean(leaves_old, na.rm=T), leaves_old_sd = sd(leaves_old, na.rm=T))
ol_Xylopia_rubescens_sum_dat

ol_Xylopia_rubescens_sum_dat$leaves_old_mean_V2=ol_Xylopia_rubescens_sum_dat$leaves_old_mean*120

ol_Xylopia_rubescens_sum_dat=
  ol_Xylopia_rubescens_sum_dat %>% 
  mutate(per_t=leaves_old_mean/sum(leaves_old_mean)*100)


Xylopia_rubescens_ol_leaf=
  ggplot() +
  geom_bar(data = ol_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=leaves_old_mean_V2, x=Month),
           position="dodge", stat="identity") +
  geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = factor(Year))) + 
  geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = factor(Year)), color = "#1f78b4") +
  # geom_smooth(data = mean_Monthly_dat, aes(x=Month, y=mean_annual, group = Year), linetype = 0, fill = "green")+
  geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % leaves_old", breaks=c(0,60,120,180,240,300,360), labels=c("0","2","4","6","8","10","12"), ######### les valeurs de breaks ont été changés
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  # scale_y_continuous("mean % leaves_old", breaks=c(0,100,200,300,400), labels=c("0","0-25","25-50","50-75","75-100"), ######### les valeurs de breaks ont été changés
  #                    sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("Month", labels=c("Mar","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +                            ###### les mois ont été remplacer de maras à descembre
  labs(title="Old Leaf Change over Time in Xylopia rubescens Tree Species", fill="Year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Xylopia_rubescens_ol_leaf

ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_ol_leaf.png")
ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_ol_leaf.pdf")



# Xylopia_rubescens_old_leaf <- ggplot() +
#   geom_bar(data = ol_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=leaves_old_mean * 3, x=Month), 
#            position="dodge", stat="identity") + 
#   geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = Year)) + 
#   geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = Year), color = "#1f78b4") +
#   geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
#               fill="#1f78b4",alpha = 0.2) + 
#   scale_y_continuous("mean % old leaves", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
#                      sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
#   scale_x_discrete("Month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
#   labs(title="Old Leaf Change over Time in Xylopia_rubescens Tree Species", fill="Year") + 
#   theme_classic() + theme(plot.title = element_text(hjust = 0.5))
# 
# Xylopia_rubescens_old_leaf
# ggsave("analysis_plots/Xylopia_rubescens/Xylopia_rubescens_old_leaf.pdf")



##################################################################################################################

# flower bud
fb_Xylopia_rubescens_sum_dat <- Xylopia_rubescens[!is.na(Xylopia_rubescens$Year),] %>% group_by(Month, Year) %>% 
  summarize(flower_bud_mean = mean(flower_bud, na.rm=T), flower_bud_sd = sd(flower_bud, na.rm=T))

fb_Xylopia_rubescens_sum_dat$flower_bud_mean_V2=fb_Xylopia_rubescens_sum_dat$flower_bud_mean*120


fb_Xylopia_rubescens_sum_dat=
  fb_Xylopia_rubescens_sum_dat %>% 
  mutate(per_t=flower_bud_mean/sum(flower_bud_mean)*100)

Xylopia_rubescens_flower_bud_leaf=
  ggplot() +
  geom_bar(data = fb_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=flower_bud_mean_V2, x=Month),
           position="dodge", stat="identity") +
  geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = factor(Year))) + 
  geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = factor(Year)), color = "#1f78b4") +
  # geom_smooth(data = mean_Monthly_dat, aes(x=Month, y=mean_annual, group = Year), linetype = 0, fill = "green")+
  geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % leaves_old", breaks=c(0,60,120,180,240,300,360), labels=c("0","2","4","6","8","10","12"), ######### les valeurs de breaks ont été changés
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  # scale_y_continuous("mean % leaves_old", breaks=c(0,100,200,300,400), labels=c("0","0-25","25-50","50-75","75-100"), ######### les valeurs de breaks ont été changés
  #                    sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("Month", labels=c("Mar","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +                            ###### les mois ont été remplacer de maras à descembre
  labs(title="flower_bud Change over Time in Xylopia rubescens Tree Species", fill="Year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Xylopia_rubescens_flower_bud_leaf

ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_flower_bud_leaf.png")
ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_flower_bud_leaf.pdf")


# fb_Xylopia_rubescens_sum_dat
# Xylopia_rubescens_flower_bud <- ggplot() +
#   geom_bar(data = fb_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=flower_bud_mean * 3, x=Month), 
#            position="dodge", stat="identity") + 
#   geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = Year)) + 
#   geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = Year), color = "#1f78b4") +
#   geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
#               fill="#1f78b4",alpha = 0.2) + 
#   scale_y_continuous("mean % flower buds", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
#                      sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
#   scale_x_discrete("Month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
#   labs(title="Flower Bud Change over Time in Xylopia_rubescens Tree Species", fill="Year") + 
#   theme_classic() + theme(plot.title = element_text(hjust = 0.5))
# 
# Xylopia_rubescens_flower_bud
# ggsave("analysis_plots/Xylopia_rubescens/Xylopia_rubescens_flower_bud.pdf")

##################################################################################################################

# open flower
fo_Xylopia_rubescens_sum_dat <- Xylopia_rubescens[!is.na(Xylopia_rubescens$Year),] %>% group_by(Month, Year) %>% 
  summarize(flower_open_mean = mean(flower_open, na.rm=T), flower_open_sd = sd(flower_open, na.rm=T))

fo_Xylopia_rubescens_sum_dat <- Xylopia_rubescens %>% group_by(Month, Year) %>% 
  summarize(flower_open_mean = mean(flower_bud, na.rm=T), flower_open_sd = sd(flower_open, na.rm=T))


fo_Xylopia_rubescens_sum_dat$flower_open_mean_V2=fo_Xylopia_rubescens_sum_dat$flower_open_mean*120

fo_Xylopia_rubescens_sum_dat=
  fo_Xylopia_rubescens_sum_dat %>% 
  mutate(per_t=flower_open_mean/sum(flower_open_mean)*100)

Xylopia_rubescens_flower_open_leaf=
  ggplot() +
  geom_bar(data = fo_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=flower_open_mean_V2, x=Month),
           position="dodge", stat="identity") +
  geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = factor(Year))) + 
  geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = factor(Year)), color = "#1f78b4") +
  # geom_smooth(data = mean_Monthly_dat, aes(x=Month, y=mean_annual, group = Year), linetype = 0, fill = "green")+
  geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % flower_open", breaks=c(0,60,120,180,240,300,360), labels=c("0","2","4","6","8","10","12"), ######### les valeurs de breaks ont été changés
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  # scale_y_continuous("mean % flower_open", breaks=c(0,100,200,300,400), labels=c("0","0-25","25-50","50-75","75-100"), ######### les valeurs de breaks ont été changés
  #                    sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("Month", labels=c("Mar","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +                            ###### les mois ont été remplacer de maras à descembre
  labs(title="Open Flower Change over Time in Xylopia rubescens Tree Species", fill="Year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Xylopia_rubescens_flower_open_leaf

ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_flower_open_leaf.png")
ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_flower_open_leaf.pdf")


# fo_Xylopia_rubescens_sum_dat
# 
# Xylopia_rubescens_flower_open <- ggplot() +
#   geom_bar(data = fo_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=flower_open_mean * 3, x=Month), 
#            position="dodge", stat="identity") + 
#   geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = Year)) + 
#   geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = Year), color = "#1f78b4") +
#   geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
#               fill="#1f78b4",alpha = 0.2) + 
#   scale_y_continuous("mean % open flower", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
#                      sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
#   scale_x_discrete("Month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
#   labs(title="Open Flower Change over Time in Xylopia_rubescens Tree Species", fill="Year") + 
#   theme_classic() + theme(plot.title = element_text(hjust = 0.5))
# 
# Xylopia_rubescens_flower_open
# ggsave("analysis_plots/Xylopia_rubescens/Xylopia_rubescens_flower_open.pdf")

##################################################################################################################

# unripe fruit
fu_Xylopia_rubescens_sum_dat <- Xylopia_rubescens[!is.na(Xylopia_rubescens$Year),] %>% group_by(Month, Year) %>% 
  summarize(fruit_unripe_mean = mean(fruit_unripe, na.rm=T), fruit_unripe_sd = sd(fruit_unripe, na.rm=T))

fu_Xylopia_rubescens_sum_dat$fruit_unripe_mean_V2=fu_Xylopia_rubescens_sum_dat$fruit_unripe_mean*120

fu_Xylopia_rubescens_sum_dat=
  fu_Xylopia_rubescens_sum_dat %>% 
  mutate(per_t=fruit_unripe_mean/sum(fruit_unripe_mean)*100)

Xylopia_rubescens_fruit_unripe_leaf=
  ggplot() +
  geom_bar(data = fu_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=fruit_unripe_mean_V2, x=Month),
           position="dodge", stat="identity") +
  geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = factor(Year))) + 
  geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = factor(Year)), color = "#1f78b4") +
  # geom_smooth(data = mean_Monthly_dat, aes(x=Month, y=mean_annual, group = Year), linetype = 0, fill = "green")+
  geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % fruit_unripe", breaks=c(0,60,120,180,240,300,360), labels=c("0","2","4","6","8","10","12"), ######### les valeurs de breaks ont été changés
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  # scale_y_continuous("mean % fruit_unripe", breaks=c(0,100,200,300,400), labels=c("0","0-25","25-50","50-75","75-100"), ######### les valeurs de breaks ont été changés
  #                    sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("Month", labels=c("Mar","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +                            ###### les mois ont été remplacer de maras à descembre
  labs(title="Unripe Fruit Change over Time in Xylopia rubescens Tree Species", fill="Year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Xylopia_rubescens_fruit_unripe_leaf

ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_fruit_unripe.pdf")
ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_fruit_unripe.png")


# fu_Xylopia_rubescens_sum_dat
# 
# Xylopia_rubescens_fruit_unripe <- ggplot() +
#   geom_bar(data = fu_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=fruit_unripe_mean * 3, x=Month), 
#            position="dodge", stat="identity") + 
#   geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = Year)) + 
#   geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = Year), color = "#1f78b4") +
#   geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
#               fill="#1f78b4",alpha = 0.2) + 
#   scale_y_continuous("mean % unripe fruit", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
#                      sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
#   scale_x_discrete("Month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
#   labs(title="Unripe Fruit Change over Time in Xylopia_rubescens Tree Species", fill="Year") + 
#   theme_classic() + theme(plot.title = element_text(hjust = 0.5))
# 
# Xylopia_rubescens_fruit_unripe
# ggsave("analysis_plots/Xylopia_rubescens/Xylopia_rubescens_fruit_unripe.pdf")

##################################################################################################################

# ripe fruit
fr_Xylopia_rubescens_sum_dat <- Xylopia_rubescens[!is.na(Xylopia_rubescens$Year),] %>% group_by(Month, Year) %>% 
  summarize(fruit_ripe_mean = mean(fruit_ripe, na.rm=T), fruit_ripe_sd = sd(fruit_ripe, na.rm=T))

fr_Xylopia_rubescens_sum_dat$fruit_ripe_mean_V2=fr_Xylopia_rubescens_sum_dat$fruit_ripe_mean*120

fr_Xylopia_rubescens_sum_dat=
  fr_Xylopia_rubescens_sum_dat %>% 
  mutate(per_t=fruit_ripe_mean/sum(fruit_ripe_mean)*100)

Xylopia_rubescens_fruit_ripe_leaf=
  ggplot() +
  geom_bar(data = fr_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=fruit_ripe_mean_V2, x=Month),
           position="dodge", stat="identity") +
  geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = factor(Year))) + 
  geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = factor(Year)), color = "#1f78b4") +
  # geom_smooth(data = mean_Monthly_dat, aes(x=Month, y=mean_annual, group = Year), linetype = 0, fill = "green")+
  geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % fruit_ripe", breaks=c(0,60,120,180,240,300,360), labels=c("0","2","4","6","8","10","12"), ######### les valeurs de breaks ont été changés
                     sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  # scale_y_continuous("mean % fruit_ripe", breaks=c(0,100,200,300,400), labels=c("0","0-25","25-50","50-75","75-100"), ######### les valeurs de breaks ont été changés
  #                    sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
  scale_x_discrete("Month", labels=c("Mar","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +                            ###### les mois ont été remplacer de maras à descembre
  labs(title="Ripe Fruit Change over Time in Xylopia rubescens Tree Species", fill="Year") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

Xylopia_rubescens_fruit_ripe_leaf

ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_fruit_ripe.pdf")
ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_fruit_ripe.png")



# fr_Xylopia_rubescens_sum_dat
# 
# Xylopia_rubescens_fruit_ripe <- ggplot() +
#   geom_bar(data = fr_Xylopia_rubescens_sum_dat, aes(fill=as.factor(Year), y=fruit_ripe_mean * 3, x=Month), 
#            position="dodge", stat="identity") + 
#   geom_line(data=subset_dat, aes(x=Month, y=mean_Monthly, group = Year, linetype = Year)) + 
#   geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = Year), color = "#1f78b4") +
#   geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
#               fill="#1f78b4",alpha = 0.2) + 
#   scale_y_continuous("mean % ripe fruit", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"), 
#                      sec.axis = sec_axis(~ . *1, name = "rainfall_mm")) +
#   scale_x_discrete("Month", labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
#   labs(title="Ripe Fruit Change over Time in Xylopia_rubescens Tree Species", fill="Year") + 
#   theme_classic() + theme(plot.title = element_text(hjust = 0.5))
# 
# Xylopia_rubescens_fruit_ripe
# ggsave("analysis_plots/Xylopia_rubescens/Xylopia_rubescens_fruit_ripe.pdf")

#################################################################################################
all_plot <-  
  Xylopia_rubescens_new_leaf + Xylopia_rubescens_mature_leaf + Xylopia_rubescens_ol_leaf + Xylopia_rubescens_fruit_unripe_leaf + Xylopia_rubescens_flower_bud_leaf  + Xylopia_rubescens_fruit_ripe_leaf + Xylopia_rubescens_flower_open_leaf + plot_layout(ncol = 2) ##########  ajouter leaves_young,	leaves_old,	leaves_old, pour avoir les graphes avec different elements et double clic sur all_plot

all_plot

########## Enregistrement

ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_combi_Pheno_precipitation.pdf")
ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_combi_Pheno_precipitation.png")

# ggsave(
#   filename = paste0("graphes/", gsub(" ", "_", species_selected), ".pdf"),
#   plot = all_plot,
#   width = 10,
#   height = 14
# )


##################################################################################################################
# graphique combiné avec les élements phéno au cours de l'année

#find avg of pheno metrics between both Years (2020 and 2021)
#young leaves
yl_Xylopia_rubescens_avg <- yl_Xylopia_rubescens_sum_dat %>% group_by(Month) %>% 
  summarize(total_avg = mean(leaves_young_mean, na.rm=T), 
            sd_avg = mean(leaves_young_sd),
            n_dat = n(),
            CI_lower =  total_avg - (1.960 * (sd_avg/sqrt(n_dat))),
            CI_upper =  total_avg + (1.960 * (sd_avg/sqrt(n_dat))))

yl_Xylopia_rubescens_avg

#open flowers
fo_Xylopia_rubescens_avg <- fo_Xylopia_rubescens_sum_dat %>% group_by(Month) %>% 
  summarize(total_avg = mean(flower_open_mean, na.rm=T), 
            sd_avg = mean(flower_open_sd),
            n_dat = n(),
            CI_lower =  total_avg - (1.960 * (sd_avg/sqrt(n_dat))),
            CI_upper =  total_avg + (1.960 * (sd_avg/sqrt(n_dat)))) 

fo_Xylopia_rubescens_avg

#unripe fruit
fu_Xylopia_rubescens_avg <- fu_Xylopia_rubescens_sum_dat %>% group_by(Month) %>% 
  summarize(total_avg = mean(fruit_unripe_mean, na.rm=T), 
            sd_avg = mean(fruit_unripe_sd),
            n_dat = n(),
            CI_lower =  total_avg - (1.960 * (sd_avg/sqrt(n_dat))),
            CI_upper =  total_avg + (1.960 * (sd_avg/sqrt(n_dat)))) 

fu_Xylopia_rubescens_avg

#merge data sets by appending

yl_Xylopia_rubescens_avg$pheno_metric = "young_leaf"
fo_Xylopia_rubescens_avg$pheno_metric = "open_flower"
fu_Xylopia_rubescens_avg$pheno_metric = "unripe_fruit"

Xylopia_rubescens_combined = rbind(yl_Xylopia_rubescens_avg, fo_Xylopia_rubescens_avg, fu_Xylopia_rubescens_avg)  

#################################################################################################################
#plot (still need error bars)

# Xylopia_rubescens_combined_plot <- ggplot() +
#   geom_bar(data=Xylopia_rubescens_combined, aes(fill=as.factor(pheno_metric), y=total_avg * 3, x=Month),
#            position='dodge', stat='identity') +
#   geom_errorbar(data = Xylopia_rubescens_combined, aes(x = Month, ymin = total_avg-CI_lower, ymax = total_avg+CI_upper, group=as.factor(pheno_metric)),
#                 width = .2 , position = "dodge", stat="identity")+
#   geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = Year), color = "#1f78b4") +
#   geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year),
#               fill="#1f78b4",alpha = 0.2) +
#   scale_y_continuous("mean % pheno metric intensity", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"),
#                      sec.axis = sec_axis(~ . *1, name = "rainfall (mm)")) +
#   scale_x_discrete("Month", labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) +
#   labs(title="Combined Phenological Change over Time in Xylopia_rubescens Tree Species", fill="Year") +
#   theme_classic() + theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_manual('Phenology Metric', values=c('#ffff99','#bebada','#7fc97f'))
# 
# Xylopia_rubescens_combined_plot

##################################################################################################################
Xylopia_rubescens_combined_plot <- ggplot() +
  geom_bar(data=Xylopia_rubescens_combined, aes(fill=as.factor(pheno_metric), y=total_avg * 35, x=Month), 
           position='dodge', stat='identity') +
  geom_errorbar(data = Xylopia_rubescens_combined, aes(x = Month, ymin = total_avg-CI_lower, ymax = total_avg+CI_upper, group=as.factor(pheno_metric)), 
                width = .2 , position = "dodge", stat="identity")+
  geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = Year), color = "#1f78b4") +
  geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
              fill="#1f78b4",alpha = 0.2) + 
  scale_y_continuous("mean % pheno metric intensity", breaks=c(0,60,120,180,240,300,360), labels=c("0","2","4","6","8","10","12"),
                     sec.axis = sec_axis(~ . *1, name = "rainfall (mm)")) +
  scale_x_discrete("Month", labels=c("Mar","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Combined Phenological Change over Time in Xylopia rubescens Tree Species", fill="Year") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual('Phenology Metric', values=c('#ffff99','#bebada','#7fc97f'))

Xylopia_rubescens_combined_plot

ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_combined_plot.pdf")


###############################################################################################################
ggplot() +
  # geom_bar(data=Xylopia_rubescens_combined, aes(x=Month, y=total_avg * 35, fill=as.factor(pheno_metric)),        ##### les barres ont été masquées
  #          stat='identity', position=position_dodge()) +
  geom_errorbar(data=Xylopia_rubescens_combined, aes(x=Month, group=as.factor(pheno_metric), 
                                                    ymin = total_avg*35-abs(sd_avg*35), ymax = total_avg*35+abs(sd_avg*35)),      ##### la valeur 3 a été remplacer par 35
                width = 0.2, position=position_dodge(0.3))+
  geom_point(data=Xylopia_rubescens_combined, aes(x=Month, y=total_avg * 35, fill=as.factor(pheno_metric)), 
             pch=21, size=4, position=position_dodge(0.3)) +
  geom_line(data=Xylopia_rubescens_combined, aes(x=Month, y=total_avg * 35, group=as.factor(pheno_metric)), linetype="dashed",
            position=position_dodge(0.3)) +
  geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = Year), color = "#1f78b4") +
  geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year), 
              fill="#1f78b4",alpha = 0.2) + 
  geom_hline(yintercept = 0) + 
  scale_y_continuous("mean % pheno metric intensity", breaks=c(0,60,120,180,240,300,360), labels=c("0","2","4","6","8","10","12"), 
                     sec.axis = sec_axis(~ . *1, name = "rainfall (mm)")) +
  scale_x_discrete("Month", labels=c("Mar","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title="Combined Phenological Change over Time in Xylopia rubescens Tree Species", fill="Year") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_fill_manual('Phenology Metric', values=c('blue','orange','green'))     ######'#ffff99','#bebada','#7fc97f'


ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_changement_pheno.pdf")

ggsave("C:/Users/Etudiants/Documents/Antoine_Tekam/graphes/Xylopia_rubescens_changement_pheno.png")



# ggplot() +
#   # geom_bar(data=Xylopia_rubescens_combined, aes(x=Month, y=total_avg * 3, fill=as.factor(pheno_metric)),
#   #          stat='identity', position=position_dodge()) +
#   geom_errorbar(data=Xylopia_rubescens_combined, aes(x=Month, group=as.factor(pheno_metric),
#                                                     ymin = total_avg*3-abs(sd_avg*3), ymax = total_avg*3+abs(sd_avg*3)),
#                 width = 0.2, position=position_dodge(0.3))+
#   geom_point(data=Xylopia_rubescens_combined, aes(x=Month, y=total_avg * 3, fill=as.factor(pheno_metric)),
#              pch=21, size=4, position=position_dodge(0.3)) +
#   geom_line(data=Xylopia_rubescens_combined, aes(x=Month, y=total_avg * 3, group=as.factor(pheno_metric)), linetype="dashed",
#             position=position_dodge(0.3)) +
#   geom_line(data=mean_Monthly_dat_v2, aes(x=Month, y=mean_Monthly, group = Year), color = "#1f78b4") +
#   geom_ribbon(data = mean_Monthly_dat_v2, aes(x=Month, ymin = ci_lower, ymax = ci_upper, group=Year),
#               fill="#1f78b4",alpha = 0.2) +
#   geom_hline(yintercept = 0) +
#   scale_y_continuous("mean % pheno metric intensity", breaks=c(0,3,6,9,12), labels=c("0","0-25","25-50","50-75","75-100"),
#                      sec.axis = sec_axis(~ . *1, name = "rainfall (mm)")) +
#   scale_x_discrete("Month", labels=c("J","F","M","A","M","J","J","A","S","O","N","D")) +
#   labs(title="Combined Phenological Change over Time in Xylopia_rubescens Tree Species", fill="Year") +
#   theme_classic() + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
#   scale_fill_manual('Phenology Metric', values=c('#ffff99','#bebada','#7fc97f'))
# ggplot

##############################################################################################################################

#### les espèces avec Y= couronne bien visible, YP= couronne partiellement visible, N= couronne non visible




### graphique individuel au cours de l'année (2020, 2021)
### feuillaison


###floraison


### fructification


### graphe combiné avec les stapes phéno (feuillaison, floraison, fructification)



### graphe de floraison et fructification par classe de diametre 


#########################################################################################################################
#chargement shapefile

crowns <- readOGR(dsn="C:/Users/Etudiants/Documents/Antoine_Tekam/Base de données/data_crowns", layer="PhenologyTreeList_crowns")

dim (crowns)
plot(crowns)

head(crowns)

# vérification de deux colonnes  identiques
identical(crowns$Gns_spc,dat$Gns_spc) # false

###chercher les lignes qui ne sont pas identiques )
ifelse(crowns$Gns_spc==dat$Gns_spc,"Yes","No") 
head(crowns)

#### calcul des EVI


#
