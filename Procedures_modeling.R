#########################################################################################################################################################
# For data cleaner procedures, the code shown below is adapted from: https://modtools.wordpress.com/2021/03/22/import-and-clean-gbif-data/              #
# Therefore, credit for this methodology goes to A.M. Barbosa (github: https://github.com/AMBarbosa).                                                   #
#########################################################################################################################################################
# Packages
library(countrycode)
library(devtools)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sf)

# Obtain GBIF data via rgbif
dat <- occ_search(scientificName = "Favolus brasiliensis", 
                  limit = 10000, 
                  hasCoordinate = TRUE)

# View the data
dat <- dat$data

# Select the columns of interest
dat <- dat %>%
  dplyr::select(species, decimalLongitude, 
                decimalLatitude, countryCode, individualCount,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters,
                year, basisOfRecord, institutionCode, datasetName)

# Remove records without coordinates
dat <- dat %>%
  filter(!is.na(decimalLongitude )) %>%
  filter(!is.na(decimalLatitude))

# View the records on maps
wm <- borders("world", colour = "darkgray", fill = "darkgray")
ggplot()+
  coord_fixed() +
  wm +
  geom_point(data = dat,
             aes(x = decimalLongitude, y = decimalLatitude),
             colour = "black",
             size = 0.5) +
  theme_bw()

# Convert data ISO2 to ISO3
dat$countryCode <- countrycode(dat$countryCode,
                               origin = 'iso2c',
                               destination = 'iso3c')


# Remove the problems of the Flags
dat <- data.frame(dat)
flags <- clean_coordinates(x = dat,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids",
                                     "equals", "zeros", "countries"))

# View the flags
summary(flags)
plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

# Remove problematics records
dat <- dat[flags$.summary,]

# Improving data quality
dat %>%
  mutate(Uncertainty = coordinateUncertaintyInMeters/1000) %>%
  ggplot(aes(x = Uncertainty)) +
  geom_histogram() +
  xlab("Coordinate uncertainty in meters")+
  theme_bw()

dat <- dat %>%
  filter(coordinateUncertaintyInMeters / 100 <= 100 | is.na(coordinateUncertaintyInMeters))

# Remove data from human observation 
## Be careful! This is only necessary when your organism has similar morphologies to other species or is a complex species.
table(dat$basisOfRecord)
dat <- filter(dat, basisOfRecord != "HUMAN_OBSERVATION" |
                basisOfRecord == "MATERIAL_SAMPLE" |
                basisOfRecord == "PRESERVED_SPECIMEN")

# Revome individual counts
table(dat$individualCount)
dat <- dat %>%
  filter(individualCount > 0 | is.na(individualCount)) %>%
  filter(individualCount < 99 | is.na(individualCount))

# Now you will select only Species, Longitute and Latitude
dat <- dat %>%
  dplyr::select(species, decimalLongitude, 
                decimalLatitude)

# Run ENMTML
ENMTML(pred_dir ="Path to your predict variabless",
       occ_file= dat, 
       result_dir="Path to your result folder",
       proj_dir= "Path to your projection variables",
       sp="species", x="decimalLongitude", y="decimalLatitude", 
       min_occ=50,
       colin_var=c(method="PCA"),
       imp_var = TRUE,
       eval_occ = NULL,
       sp_accessible_area = NULL,
       pres_abs_ratio = 1, 
       pseudoabs_method = c(method='GEO_ENV_CONST', width='50'),
       part=c(method= 'BLOCK'),
       save_part=FALSE, save_final=TRUE,
       algorithm = c("MXS", "RDF", "SVM"), 
       cores=8,
       thr=c(type='JACCARD'), 
       msdm=NULL,
       ensemble=c(method='SUP', metric='Jaccard'),
       extrapolation=TRUE)




