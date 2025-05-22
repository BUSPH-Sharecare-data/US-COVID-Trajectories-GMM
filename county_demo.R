###########################################################################################
# PROJECT         : Sharecare
# SPONSOR/PI      : Kim Dukes
# PROGRAM NAME    : County_demo.R
# DESCRIPTION     : demographic from counties
#                 
# PROGRAMMER      : Pengsheng Ni
# DATE WRITTEN    : 3/7/25
# NOTE            : 
###########################################################################################


library(tidycensus)
library(tidyverse)
library(ggplot2)
library(tigris)
library(openxlsx)
library(readxl)
library(psych)

# \\Ad.bu.edu\\bumcfiles\\SPH\\Projects\\SC\\08Data\\07Codebooks\\02Analytic data\\BU-SC SDOH Detail.xlsx
# pct_black:      ((B03002_004E + B03002_014E)/B03002_001E)*100
# pct_hisp:       (B03002_012E/B03002_001E)*100
# med_home_inc:   med_home_inc_e = B19013_001E

# \\Ad.bu.edu\bumcfiles\SPH\Projects\SC\06Methods_and_Specifications\02Programming\03Derived_Variables\01ReferenceSets\08ACS\ACS_demographics_2019.R
# med_age:        B01002_001E    

var <- c("B03002_004E", "B03002_014E", "B03002_001E", 
         "B03002_012E", "B19082_001E", "B19082_002E",
         "B19082_003E", "B19082_004E", "B19082_005E",
         "B19013_001E", "B01002_001E")


# get above data at county level in 2020 ACS 2016-2020 5-year ACS
acs <- get_acs(
  geography = "county",
  variables =  var,
  year = 2020,
  output = "wide"
)


dim(acs)  # 3221*24

# county 02261 not in data-set, and it was split into two counties
# '02063' and '02066', both of those two counties are in the data-set
# we need to combine both counties together and assign the combined value
# as "02261"

acs[acs$GEOID=="02261",]   # none
acs[acs$GEOID %in% c("02063","02066"),]  # two counties


# for "02063","02066" 
# for count variables
acs_2counties_1 <- acs[acs$GEOID %in% c("02063","02066"),
                     c("B03002_004E","B03002_014E","B03002_012E","B03002_001E")]  # 2*4

acs_2counties_1 <- as.data.frame(t(apply(acs_2counties_1,2,sum)))
# acs_2counties_1$FIPS_CODE <- c("02261")

acs_2counties_1 <- acs_2counties_1 %>%
  mutate(pct_black = ((B03002_004E + B03002_014E)/B03002_001E)*100)
acs_2counties_1 <- acs_2counties_1 %>%
  mutate(pct_hisp = (B03002_012E/B03002_001E)*100)

# for continuous variables
acs_2counties_2 <- acs[acs$GEOID %in% c("02063","02066"),
                       c("GEOID","B19013_001E","B01002_001E","B03002_001E")]  # 2*3

tot_n <-sum(acs_2counties_2$B03002_001E)

# acs_2counties_2$w <- acs_2counties_2$B03002_001E/tot_n
# calculate from the ACS
#  02063  0.692
#  02066  0.308

# 4/10/25  
# it seems we need to use weights: 
#  02063  0.7307
#  02066  0.2693

acs_2counties_2$w <- c(0.7307, 0.2693)

median_household_income_ <- weighted.mean(acs_2counties_2$B19013_001E,
                                          acs_2counties_2$w)

median_age_ <- weighted.mean(acs_2counties_2$B01002_001E,
                                          acs_2counties_2$w)

acs_2counties_1$FIPS_CODE <- "02261"
acs_2counties_1$median_household_income <- median_household_income_
acs_2counties_1$median_age <- median_age_

acs_2counties_1 <- acs_2counties_1[,colnames(acs_2counties_1) %in% c("FIPS_CODE",
                                                  "median_household_income",
                                                  "median_age",
                                                  "pct_black",
                                                  "pct_hisp")]

# for non "02063","02066" do the regular calculation
acs_others <- acs[!(acs$GEOID %in% c("02063","02066")),]  # 3219*24

acs_others$median_household_income <- acs_others$B19013_001E
acs_others$median_age <- acs_others$B01002_001E
acs_others <- acs_others %>%
  mutate(pct_black = ((B03002_004E + B03002_014E)/B03002_001E)*100)
acs_others <- acs_others %>%
  mutate(pct_hisp = (B03002_012E/B03002_001E)*100)
acs_others <- acs_others %>%
  dplyr::rename(FIPS_CODE = GEOID)
 
acs_others_demographic_2020 <- acs_others[,c(1,25:28)]

county_demographic_2020 <- rbind(acs_2counties_1, 
                                 acs_others_demographic_2020)


# dim(county_demographic_2020)   # 3220*5
# keep 3142

urban <- read_excel("//ad.bu.edu/bumcfiles/SPH/Projects/SC/08Data/01Raw_Data/01ReferenceSets/31Relational_Files/crosswalk_files/Urban_Rural/Urban_Rural_County_2018v3.xlsx")
names(urban)[1] <- "FIPS_CODE"

county_demographic_2020_ <- merge(county_demographic_2020, urban, by="FIPS_CODE",all.y=TRUE)
county_demographic_2020_ <- county_demographic_2020_[,c(1:5)]

describe(county_demographic_2020_[,c(2:5)])  
# 1 counties missing in median_household_income                 

county_demographic_2020_[which(is.na(county_demographic_2020_$median_household_income)),]

t <- acs[acs$GEOID %in% c("48243"),]  # median household income missing

# write out xlsx file

write.xlsx(county_demographic_2020_,file="//ad.bu.edu/bumcfiles/SPH/Projects/SC/08Data/03Analytic_Data/01ReferenceData/01Demographic/county_demographic_2020_updatedcovid.xlsx")

