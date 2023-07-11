#Caleb Hightower
#Advisor: Lance Waller
#02/08/2021
#R 4.0.4 "Lost Library Book"

#########
### Social Vulnerability Index and Opioid Overdoses in Georgia
### Assessing County Level Vulnerability to Opioid Overdoses
########

#import libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)
library(RColorBrewer)
#library(rgdal)
#library(broom)
library(sf)
library(tidycensus)
library(stringr)
library(table1)
library(rqdatatable)

#library(RJSONIO)
library(tidygeocoder)
library(ggmap)

library(sp)
library(maps)
library(maptools)
library(spData)
#useful operator
'%!in%' <- function(x,y)!('%in%'(x,y))
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}


##########################
#Import SVI data
##########################

#Read in county social vulnerability data for Georgia
setwd("H:/chight3/SVI")

GA.county <- read.csv(file = 'Georgia_COUNTY.csv')
head(GA.county)
str(GA.county)

#Read in census tract data for Georgia
GA.tracts <- read.csv(file = 'Georgia_Tracts.csv')
head(GA.tracts)
str(GA.tracts)

##spatial data
#read in the shape file with sf package
GA.shp <- st_read(dsn = "Georgia_COUNTY", layer = "SVI2018_GEORGIA_county")

pal <- brewer.pal(7, "OrRd") # we select 7 colors from the palette, color brewer package
class(pal)

#create choropleth maps for county vulnerabilty for overall and each theme
plot(GA.shp["RPL_THEMES"], 
     main = "Overall SVI Score by County in GA", 
     breaks = "quantile", nbreaks = 7,
     pal = pal)

plot(GA.shp["RPL_THEME1"], 
     main = "Socioeconomic Summary Theme by County in GA", 
     breaks = "quantile", nbreaks = 7,
     pal = pal)

plot(GA.shp["RPL_THEME2"], 
     main = "Household Composition Summary Theme by County in GA", 
     breaks = "quantile", nbreaks = 7,
     pal = pal)

plot(GA.shp["RPL_THEME3"], 
     main = "Race/Ethnicity/Language Summary Theme by County in GA", 
     breaks = "quantile", nbreaks = 7,
     pal = pal)

plot(GA.shp["RPL_THEME4"], 
     main = "Housing/Transportation Summary Theme by County in GA", 
     breaks = "quantile", nbreaks = 7,
     pal = pal)

###########################################
#plotting tract versus county vulnerability
###########################################

#add code for tract and a code for census, and concatenate the two data sets
#rbind requires the data frames have the same columns
GA.county2 <- GA.county
GA.county2$STCNTY <- GA.county$FIPS
GA.county2$county_tract <- "County"

GA.tracts2 <- GA.tracts
GA.tracts2$county_tract <- "Tract"
GA.SVI <- rbind(GA.tracts2, GA.county2)
#999 represents missing values
GA.SVI <- na_if(GA.SVI, -999)

#ideally, I want to make a four panel graph with 159 ticks
#each signifying the counties vulnerability rating for each of the four SVI categories: 
#SES, Household Composition, Race/Ethnicity/Language, Housing/Transport
#this might be a little hard to look at unless the resolution is really large, so I'm going to make the four graphs separately

#Overall SVI score
ggplot(GA.SVI, aes(x = COUNTY, y = RPL_THEMES, color = county_tract)) +
  geom_point() +
  ggtitle("Overall SVI Score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(color = "Region", x = "County", y = "SVI score")

#SES
ggplot(GA.SVI, aes(x = COUNTY, y = RPL_THEME1, color = county_tract)) +
  geom_point() +
  ggtitle("Socioeconomic Summary Theme Ranking") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(color = "Region", x = "County", y = "SVI score")

#Household Composition
ggplot(GA.SVI, aes(x = COUNTY, y = RPL_THEME2, color = county_tract)) +
  geom_point() +
  ggtitle("Household Composition Summary Theme Ranking") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(color = "Region", x = "County", y = "SVI score")

#Race/Ethnicity/Language
ggplot(GA.SVI, aes(x = COUNTY, y = RPL_THEME3, color = county_tract)) +
  geom_point() +
  ggtitle("Race/Ethnicity/Language Summary Theme Ranking") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(color = "Region", x = "County", y = "SVI score")

#Housing/Transportation
ggplot(GA.SVI, aes(x = COUNTY, y = RPL_THEME4, color = county_tract)) +
  geom_point() +
  ggtitle("Housing/Transportation Summary Theme Ranking") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(color = "Region", x = "County", y = "SVI score")



##########################
#Import and clean possible predictors
########################

#tidycensus package will be used
#IE American Community Survey

#fetch my census key
census_api_key("e0553a6d356d95c3dde186008e1f849883310299")

#select 27 variables, some will be derived rates
#many overlap with SVI variables
#Percentage uninsured
#Percentage with vehicle access, calculate
#Percentage with no high school diploma
#Population per square mile (log), calculate
#Population per square mile, calculate
#Per capita income
#Per capita income (log), calculate
#Percentage living in poverty
#Percentage of population that is non-Hispanic White
#Percentage of the population that is unemployed (alternative S2702_C01_049)
#Teen birth rate
#Percentage of population with a disability
#population estimates
#Population decline, calculate
#Total housing units
#Occupied housing units
#Vacant housing units
#Crowded housing units, calculate
#Percentage of Female-headed households
#Number of mobile homes
#Percentage of mobile homes
#Homes with no phone service
#Population aged 18-29, calculate
#Percentage of population age 18-29, calculate
#Gini coefficient
#Residents who were never married
#Percentage of population over 15 never married

#get most recent acs data for select variables
ACS <- get_acs(geography = "county",
               variables = c(pctuninsured = "S2701_C05_001", totvehicle = "B08141_001", novehicle = "B08141_002",
                             pctnodiploma = "S1501_C02_002", population = "B01003_001",
                             incomepercap = "B19301_001", pctpoverty = "S1701_C03_001",
                             pctnonhiswhite = "S2702_C01_026", pctunemployed = "S2301_C04_001", teenbirth = "S1301_C04_002",
                             pctdisability = "S1810_C03_001", pop18to24 = "S0101_C01_022", pop25to29 = "S0101_C01_007",
                             gini = "B19083_001", pctfemalehouse = "S2501_C01_019", pctnevermarried = "S1201_C06_001"),
               state = "GA",
               output = "wide")

head(ACS)

#calcualte percent with a vehicle
#pctvehicle = 1-(novehicle / totvehicle)

#calculate population 18 to 29
#pop18to29 = pop18to24 + pop25to29

#calulate population per square mile and log pop
#population per square mile = population/land area
#log population per square mile = log(population/land area)

#calculate log per capita income
#log per capita income = log(per capita income)

ACS <- ACS %>% mutate(pctvehicle = 1 - ((novehicleE / totvehicleE)), novehicleE = NULL, totvehicleE=NULL,
                      pop18to29 = pop18to24E + pop25to29E, pop18to24E = NULL, pop25to29E = NULL,
                      pctpop18to29 = pop18to29 / populationE * 100,
                      logincomepercap = log(incomepercapE)) %>%
               rename(County = NAME)

head(ACS)

#read in land area by county
ga.land.area <- read.csv(file = 'GALandArea.csv', header = T)
ga.land.area <- ga.land.area %>% rename(landarea = Value)
ga.land.area$County <- paste0(ga.land.area$County, " County, Georgia")

head(ga.land.area)

#merge with ACS data
#calculate population density
ACS2 <- merge(ACS, ga.land.area, by="County") %>%
        mutate(popdensity = populationE / landarea, landarea = NULL,
               logpopdensity = log(popdensity))

head(ACS2)

#get housing ACS data
housing <- get_acs(geography = "county",
                   variables = c(tothousing = "DP04_0001", totoccupied = "DP04_0002", totvacant = "DP04_0003",
                                 occperroom1 = "DP04_0078", occperroom2 = "DP04_0079",
                                 mobilehome = "DP04_0014", pctmobilehome = "DP04_0014P",
                                 phone = "DP04_0075", pctphone = "DP04_0075P"),
                   state = "GA",
                   output = "wide")

head(housing)

#calculate crowded housing score
housing <- housing %>% mutate(crowded = (occperroom1E + occperroom2E) / tothousingE * 100, occperroom1E = NULL, occperroom2E = NULL) %>%
                       rename(County = NAME)

head(housing)

#crowded housing we will have to sum all the houses with >1 person per bedroom
#(occperroom1 + occperroom2) / tothousing
#will have to compare 2 tables for population decline between 2011 and 2019


#get population from 2011
ACSpop2011 <- get_acs(geography = "county",
                      year = 2011,
                      variables = c(pop2011 = "B01003_001"),
                      state = "GA",
                      output = "wide") %>%
              rename(County = NAME)

head(ACSpop2011)

#create an indicator variable = 1 if population declined since 2011, 0 if not
ACS3 <- merge(ACS2, ACSpop2011, by = c("County", "GEOID")) %>%
        mutate(popdecline = ifelse((populationE - pop2011E) < 0, 1, 0), pop2011E = NULL,
               popdecline = factor(popdecline, levels = c(1,0), labels = c("Population Declined", "Population Increased")))

head(ACS3)

#merge housing and earlier ACS data by county and FIPS
#delete margin of error variables
#delete the E off of variable names
ACS4 <- merge(ACS3, housing, by = c("County", "GEOID")) %>%
        rename(FIPS = GEOID) %>%
        select(-ends_with("M")) %>%
        rename_with(~gsub('.{1}$', '', .x), ends_with("E", ignore.case=F))

head(ACS4)

#Import County Health Rankings
#select the 8 variables we need
#Premature deaths, Years of potential life lost, Adults reporting poor/fair health, Poor physical health days,
#Poor mental health days, Injury-related deaths, Rate of injury-related death, Percentage of adults who smoke
#remove total counts for Georgia which is the first row
County.Health <- read.csv(file = 'CountyHealthRankings.csv', header = T)

head(County.Health)

County.Health <- County.Health %>%
                 select("FIPS", "County", "Deaths", "Years.of.Potential.Life.Lost.Rate", "X..Fair.or.Poor.Health",
                        "Average.Number.of.Physically.Unhealthy.Days", "Average.Number.of.Mentally.Unhealthy.Days",
                        "X..Injury.Deaths", "Injury.Death.Rate", "X..Smokers") %>%
                 rename(prematuredeaths = Deaths,
                        potlifelost = Years.of.Potential.Life.Lost.Rate,
                        poorhealth = X..Fair.or.Poor.Health,
                        poorhealthdays = Average.Number.of.Physically.Unhealthy.Days,
                        mentaldays = Average.Number.of.Mentally.Unhealthy.Days,
                        injurydeaths = X..Injury.Deaths,
                        injurydeathrate = Injury.Death.Rate,
                        smokers = X..Smokers) %>%
                mutate(County = paste0(County, " County, Georgia")) %>%
                slice(-1)

head(County.Health)

#merge with ACS data
possible.pred <- merge(ACS4, County.Health, by = c("County", "FIPS"), all = T)

head(possible.pred)

#Religion Census 2010
#Total Adherants? Rate per 1000?
#Percentage of membership? I.e. adherents / county population
#Right now we will take total adherents and rate
Religion <- read.csv(file = 'ReligionCensus.csv', header = T) 

head(Religion)

Religion <- Religion %>%
            filter(STNAME == "Georgia") %>%
            select("FIPS", "CNTYNAME", "TOTADH", "TOTRATE") %>%
            rename(County = CNTYNAME,
                   reladherance = TOTADH,
                   reladherancerate = TOTRATE) %>%
            mutate(County = paste0(County, ", Georgia"))

head(Religion)

#merge
possible.pred2 <- merge(possible.pred, Religion, by = c("County", "FIPS"), all = T)

head(possible.pred2)

#Urban Rural
#How to code this?
#As is: Metro, Micro, and Neither
#NOTE there isn't an OMB designation for every county, only 127
Urban.Rural <- read.csv(file = 'UrbanRural.csv', header = T) 

head(Urban.Rural)

#select GA and rename variables
Urban.Rural <- Urban.Rural %>%
               filter(STATE == "GA") %>%
               select("coname", "CBSA2020.Update") %>%
               distinct() %>%
               rename(County = coname,
                      urbanrural = CBSA2020.Update) %>%
               mutate(County = paste0(County, ", Georgia"),
                      urbanrural = factor(urbanrural))

head(Urban.Rural)

#merge
possible.pred3 <- merge(possible.pred2, Urban.Rural, by = "County", all = T)

head(possible.pred3)

#SAMHSA
#need to make 2 estimates
#number of providers per county
#providers per capita
Buprenorphine <- read.csv(file = 'Buprenorphine.csv', header = T) 

head(Buprenorphine)

#select GA and count number of providers
Buprenorphine <- Buprenorphine %>%
                 filter(state == "GA") %>%
                 add_count(county) %>%
                 select(county, n) %>%
                 distinct() %>%
                 rename(County = county, numproviders = n) %>%
                 mutate(County = str_to_title(County)) %>%     #this uses stringr function
                 filter(County %!in% c("Aiken", "Contra Costa", "Cuyahoga", "Dallas", "Duval",
                                       "New Haven", "Norfolk", "Oakland", "Richland"))

#note there are a handful of counties/mistakes that were added        
#Aiken, Contra Costa, Cuyahoga, Dallas (could be the city), Duval, New Haven, Norfolk, Oakland, Richland (maybe city)

#two counties have weird capitalization: DeKalb and McIntosh; needed corrected
#fix DeKalb and McIntosh
Buprenorphine$County[Buprenorphine$County == "Dekalb"] <- "DeKalb"
Buprenorphine$County[Buprenorphine$County == "Mcintosh"] <- "McIntosh"
Buprenorphine <- Buprenorphine %>% mutate(County = paste0(County, " County, Georgia"))

head(Buprenorphine)

#merge
#if counties had missing numproviders, then that means there were simply no providers. set to 0.
possible.pred4 <- merge(possible.pred3, Buprenorphine, by = "County", all = T)

head(possible.pred4)

possible.pred4$numproviders[is.na(possible.pred4$numproviders)] <- 0

#create rate
possible.pred4 <- possible.pred4 %>% mutate(providerspercap = numproviders / population)

head(possible.pred4)

#High intensity drug trafficking area
#no csv available for download but the counties are listed on the site
hidta.counties <- c("Floyd", "Bartow", "Cherokee", "Forsyth",
                    "Cobb", "Barrow", "Gwinnett", "Douglas",
                    "Fulton", "DeKalb", "Fayette", "Clayton", 
                    "Henry", "Chatham")

hidta <- data.frame(County = hidta.counties, HIDTA = c(rep(1, length(hidta.counties)))) %>%
         mutate(County = paste0(County, " County, Georgia"))

head(hidta)

#merge
#if NA then not a HIDTA
possible.pred5 <- merge(possible.pred4, hidta, by = "County", all = T) 
possible.pred5$HIDTA[is.na(possible.pred5$HIDTA)] <- 0
possible.pred5 <- possible.pred5 %>% mutate(HIDTA = factor(HIDTA, levels = c(1,0), labels = c("HIDTA County", "Not HIDTA County")))

head(possible.pred5)

#Fatal Overdose Data
#asterisks indicated that the number occurring was less than 5 and so they didn't create a rate
#is it better to change these to NA or 0 for the sake of modeling?
#or should I recalculate the rate using census population and note this in the methods?
#for now I will replace with NA for simplicity

#all drugs
setwd("H:/chight3/SVI/OASIS")
alldrug.overdoses <- read.csv(file = 'AllDrugOverdoses.csv', header = T) %>%
                     rename(deathalldrug = Deaths, deathalldrugR = Death.Rate) %>%
                     na_if("*") %>%
                     mutate(deathalldrugR = as.numeric(deathalldrugR))

head(alldrug.overdoses)

#all opioids
allopioid.overdoses <- read.csv(file = 'AllOpioidS.csv', header = T) %>%
                       rename(deathallopioid = Deaths, deathallopioidR = Death.Rate) %>%
                       na_if("*") %>%
                       mutate(deathallopioidR = as.numeric(deathallopioidR))

head(allopioid.overdoses)

#heroin
heroin.overdoses <- read.csv(file = 'HeroinOverdoses.csv', header = T) %>%
                    rename(deathheroin = Deaths, deathheroinR = Death.Rate) %>%
                    na_if("*") %>%
                    mutate(deathheroinR = as.numeric(deathheroinR))

head(heroin.overdoses)

#synthetic opioids that don't include methadone
synthetic.overdoses <- read.csv(file = 'SyntheticOverdoses.csv', header = T) %>%
                       rename(deathsynthetic = Deaths, deathsyntheticR = Death.Rate) %>%
                       na_if("*") %>%
                       mutate(deathsyntheticR = as.numeric(deathsyntheticR))
 
head(synthetic.overdoses)

#natural, semisynthetic, and synthetic
natural.overdoses <- read.csv(file = 'NaturalSemisyntheticOverdoses.csv', header = T) %>%
                     rename(deathnatural = Deaths, deathnaturalR = Death.Rate) %>%
                     na_if("*") %>%
                     mutate(deathnaturalR = as.numeric(deathnaturalR))

head(natural.overdoses)

#methadone
methadone.overdoses <- read.csv(file = 'MethadoneOverdoses.csv', header = T) %>%
                       rename(deathmethadone = Deaths, deathmethadoneR = Death.Rate) %>%
                       na_if("*") %>%
                       mutate(deathmethadoneR = as.numeric(deathmethadoneR))

head(methadone.overdoses)

#ER patients for drugs, i.e. nonfatal overdoses
#All drugs
alldrug.nonfatal <- read.csv(file = 'ERDrugOverdoses.csv', header = T) %>%
                    rename(eralldrug = ER.Inpatient.Visits, eralldrugR = ER.Inpatient.Visit.Rate) %>%
                    na_if("*") %>%
                    mutate(eralldrug = as.numeric(eralldrug), eralldrugR = as.numeric(eralldrugR))

head(alldrug.nonfatal)

#all opioids
allopioid.nonfatal <- read.csv(file = 'ERopioidOverdoses.csv', header = T) %>%
                      rename(erallopioid = ER.Inpatient.Visits, erallopioidR = ER.Inpatient.Visit.Rate) %>%
                      na_if("*") %>%
                      mutate(erallopioidR = as.numeric(erallopioidR))

head(allopioid.nonfatal)

#heroin
heroin.nonfatal <- read.csv(file = 'ERopioidOverdoses.csv', header = T) %>%
                   rename(erheroin = ER.Inpatient.Visits, erheroinR = ER.Inpatient.Visit.Rate) %>%
                   na_if("*") %>%
                   mutate(erheroinR = as.numeric(erheroinR))

head(heroin.nonfatal)

#STDs
#Chlamydia and gonorrhea 
chlamydia.gonorrhea <- read.csv(file = 'ChlamydiaGonorrhea.csv', header = T) %>%
                       rename(std = STD.Cases, stdR = STD.Rate) %>%
                       na_if("*") %>%
                       mutate(std = as.numeric(std), stdR = as.numeric(stdR))

head(chlamydia.gonorrhea)

#all forms of syphilis
syphilis <- read.csv(file = 'Syphilis.csv', header = T) %>%
            rename(syphilis = STD.Cases, syphilisR = STD.Rate) %>%
            na_if("*") %>% na_if("N/A5") %>%
            mutate(syphilis = as.numeric(syphilis), syphilisR = as.numeric(syphilisR))

head(syphilis)

#merge
OASIS <- alldrug.overdoses %>% full_join(allopioid.overdoses, by = "County") %>%
                               full_join(heroin.overdoses, by = "County") %>%
                               full_join(synthetic.overdoses, by = "County") %>%
                               full_join(natural.overdoses, by = "County") %>%
                               full_join(methadone.overdoses, by = "County") %>%
                               full_join(alldrug.nonfatal, by = "County") %>%
                               full_join(allopioid.nonfatal, by = "County") %>%
                               full_join(heroin.nonfatal, by = "County") %>%
                               full_join(chlamydia.gonorrhea, by = "County") %>%
                               full_join(syphilis, by = "County") %>%
                               mutate(County = paste0(County, " County, Georgia"))
                               
head(OASIS)

#merge with possible predictors
possible.pred6 <- merge(possible.pred5, OASIS, by = "County", all = T) 

head(possible.pred6)

#Georgia Crime data from 2017
setwd("H:/chight3/SVI/")
crime <- read.csv(file = 'GACrime2017.csv', header = T)

crime <- crime %>% group_by(County) %>%
                   summarize_all(sum) 
crime$County[crime$County == "Dekalb"] <- "DeKalb"
crime <- crime %>% mutate(County = paste0(County, " County, Georgia"))

head(crime)

#merge
possible.pred7 <- merge(possible.pred6, crime, by = "County", all = T)

head(possible.pred7)


#import HIV data
#new diagnoses by county
#negative numbers are censored

# Suppressed and/or missing data are represented as -1, -2, -4, or -9 in the dataset and indicate the following:
#   
# -1: Data are not shown to protect privacy because a small number of cases and/or a small population size for reason listed below.
# -2: Data were not released to AIDSVu because the state health department, per its HIV data re-release agreement with CDC, requested not to release data to AIDSVu below a certain population threshold. 
#     The data re-release agreement was updated last year, which is why the data for this year may look different than last year.
# -4: Data are not available at county-level for these counties as this time.
# -9: Data are missing.
# 
# The downloadable datasets include a rate stability variable for each indicator. 
# As is standard in the display of health statistics, rates generated from a numerator less than 12 are considered unreliable and should be interpreted with caution.
# 
# Y: Reliable rates (i.e. those generated with a numerator of 12 or greater)
# 
# N: Unreliable rates (i.e. those generated with a numerator less than 12)
# 
# Rate stability for suppressed rates is listed as -9 for HIV data and -1 for PrEP data.

HIV.case <- read.csv("AIDSVu_County_NewDX_2018.csv", header = T) 

#filter to GA and get new diagnoses rate
#the rates are per 100000
#do we want to deal with unstable estimates? similar to drug rates problem

#first, there is a big issue with this data set
#the counties are right but the state is a little off so we use FIPS to merge
HIV.case2 <- HIV.case %>% filter(substr(GEO.ID,1,2) == "13") %>%
                          select(GEO.ID, County.Name, New.Diagnoses.Rate) %>%
                          rename(County = County.Name, FIPS = GEO.ID, HIVincidence = New.Diagnoses.Rate) %>%
                          mutate(HIVincidence = replace(HIVincidence, which(HIVincidence<0), NA)) #impute negatives

#problems with county names, Dekalb, Mcintosh, Mcduffie
HIV.case2$County[HIV.case2$County == "Dekalb County"] <- "DeKalb County"
HIV.case2$County[HIV.case2$County == "Mcintosh County"] <- "McIntosh County"
HIV.case2$County[HIV.case2$County == "Mcduffie County"] <- "McDuffie County"

head(HIV.case2)

#import HIV prevalence and HIV cases related to IDU
#should we calculate the rate? or is that stepping too far
#I tried recalculating the rate of prevalence and it was a little different
#probably because the HIV data is from 2018 and the pop data is from 2019
HIV.prev <- read.csv("AIDSVu_County_Prev_2018.csv", header = T)

HIV.prev2 <- HIV.prev %>% filter(State.Abbreviation == "GA") %>%
                          select(GEO.ID, County.Name, County.Rate, IDU.Cases) %>%
                          rename(County = County.Name, FIPS = GEO.ID, HIVprevalenceR = County.Rate, HIV_IDU = IDU.Cases) %>%
                          mutate(HIVprevalenceR = replace(HIVprevalenceR, which(HIVprevalenceR<0), NA), 
                                 HIV_IDU = replace(HIV_IDU, which(HIV_IDU<0), NA)) #impute negatives

#merge HIV 
HIV <- merge(HIV.case2, HIV.prev2, by = c("FIPS", "County"), all = T)

#prep users, number and cases
Prep <- read.csv("AIDSVu_County_PrEP_2018-1.csv", header = T)

Prep2 <- Prep %>% filter(State.Abbreviation == "GA") %>%
                  select(GEO.ID, County, County.PrEP.Users, County.PrEP.Rate) %>%
                  rename(FIPS = GEO.ID, prep = County.PrEP.Users, prepR = County.PrEP.Rate) %>%
                  mutate(prep = replace(prep, which(prep<0), NA), 
                         prepR = replace(prepR, which(prepR<0), NA)) #impute negatives

#merge with HIV
HIV.prep <- merge(HIV, Prep2, by = c("FIPS", "County"), all = T) %>% mutate(County = paste0(County, ", Georgia"))


#merge with bigger file       
possible.pred8 <- merge(possible.pred7, HIV.prep, by = c("County", "FIPS"), all = T)


#opioid dispensing rate per 100
Dispensing <- read.csv("OpioidDispensing.csv", header = T)

Dispensing2 <- Dispensing %>% filter(State == "GA") %>%
                              select(-State) %>%
                              rename(FIPS = County.FIPS.Code) %>%
                              mutate(County = paste0(County, ", Georgia")) 
#missing Echols and Glascock counties

#merge
possible.pred9 <- merge(possible.pred8, Dispensing2, by = c("County", "FIPS"), all = T)
                
              
#opioid dispensing %days with overlapping opioid prescription
#                  %days with overlapping benzodiazepine prescription

#file was copy pasted from a pdf from GA DPH
#messy, I want last two numerics
OpioidOverlap <- read.csv("PDMP.csv", header = F)

#remove every 3rd row starting from 1st
#get second number of strings with two numbers
#sapply(mylist,`[`,2)
OpioidOverlap2 <- OpioidOverlap %>% filter(row_number() %% 3 != 1) %>%
                                    mutate(V1 = ifelse(grepl(" ", V1), sapply(strsplit(V1, " "),`[`,2), V1)) %>%
                                    mutate(ind = rep(c(1, 2),length.out = n())) %>%
                                    group_by(ind) %>%
                                    mutate(id = row_number()) %>%
                                    spread(ind, V1) %>%
                                    select(-id) %>%
                                    rename(overlap.opioid = "1", overlap.benzo = "2")

#bind it to Dispensing2
possible.pred10 <- cbind(possible.pred9, OpioidOverlap2)


#CMS, National Provider Information
#this file eats up all my RAM and crashes R
#need to look into alternative ways of reading it or use cluster computing

#subset to practice on it
setwd("H:/chight3/SVI/NPI")
# NPI <- read.csv(file = 'npidata_pfile_20050523-20210207.csv') %>% 
#        filter(Provider.Business.Practice.Location.Address.State.Name == "GA")
# NPI.names <- read.csv(file = 'npidata_pfile_20050523-20210207_FileHeader.csv')

#sqldf package
# e.g. df <- read.csv.sql("sample.csv", "select id, name from file where age=23")
# NPI <- read.csv.sql('npidata_pfile_20050523-20210207.csv', 
#                      sql = "select * from file where 'Provider.Business.Mailing.Address.State.Name' = 'GA' ")

#READ in subset of only Georgia data I ran on virtual machine
npi.ga <- read.csv(file = 'npiga.csv')
head(npi.ga)

#first we have to figure out how to convert addresses to counties
#zip codes make this difficult
#Idea: geocode these addressesses to long lat and then work from there to counties

###ArcGIS data
setwd("H:/chight3/SVI/")
#these are zips that are contained completely within a given county
contained.zips <- read.table("Contained_Zips.txt", sep = ",", header = T)

contained.zips <- contained.zips %>% select(NAME, ZipCode) %>%
                                     rename(County = NAME)

#subset npi.ga and see if we can reduce the amount of geocoding that needs done
npi.subset <- NPI.geocode %>% filter(!substr(Provider.Business.Mailing.Address.Postal.Code,1,5) %in% contained.zips$ZipCode) %>%
                              select(NPI,
                                     Provider.First.Line.Business.Practice.Location.Address,
                                     Provider.Business.Practice.Location.Address.City.Name,
                                     Provider.Business.Practice.Location.Address.State.Name,
                                     Provider.Business.Practice.Location.Address.Postal.Code) %>%
                              rename(Street = Provider.First.Line.Business.Practice.Location.Address,
                                     City = Provider.Business.Practice.Location.Address.City.Name,
                                     State = Provider.Business.Practice.Location.Address.State.Name,
                                     Zip_Code = Provider.Business.Practice.Location.Address.Postal.Code) %>%
                              mutate(Zip_Code = substr(Zip_Code,1,5))

#write.csv(npi.subset, "npi_geocode.csv", row.names = F)

#get long lat using the address
#there are a couple ways to do this, but I will take advantage of two methods
#first, package tinygeocoder,  I think it uses census information
#then, for the addresses that still aren't geocoded, I will use google maps through ggmap package
#the reason I am doing it in this order is because google has limits on the API

#create long lat variables for NPI
#use practice location instead of mailing address

npi.ga.short <- read.csv("npi_addresses.csv", header = T)

NPI.geocode <- npi.ga.short %>% mutate(address = paste(paste(Street, City, sep = ", "), State, sep = ", "))

#geocode using tidygeocode
#these have to be broken up depending on my internet and because sometimes the queries can be overwhelmed

coordinates1 <- slice(NPI.geocode, 1:1000) %>% tidygeocoder::geocode(address)
coordinates2 <- slice(NPI.geocode, 1001:2000) %>% tidygeocoder::geocode(address)
coordinates3 <- slice(NPI.geocode, 2001:5000) %>% tidygeocoder::geocode(address)
coordinates4 <- slice(NPI.geocode, 5001:10000) %>% tidygeocoder::geocode(address)
coordinates5 <- slice(NPI.geocode, 10001:14000) %>% tidygeocoder::geocode(address)
coordinates6 <- slice(NPI.geocode, 14001:15000) %>% tidygeocoder::geocode(address)
coordinates7 <- slice(NPI.geocode, 15001:16000) %>% tidygeocoder::geocode(address)
coordinates8 <- slice(NPI.geocode, 16001:18000) %>% tidygeocoder::geocode(address)
coordinates9 <- slice(NPI.geocode, 18001:20000) %>% tidygeocoder::geocode(address)
beep()

test1 <- rbind(coordinates1, coordinates2, coordinates3, coordinates4, coordinates5, coordinates6, coordinates7, coordinates8, coordinates9)
write.csv(test1, "geocodes20000.csv", row.names = F)

sum(is.na(test1$lat))

coord1 <- slice(NPI.geocode, 20001:23000) %>% tidygeocoder::geocode(address)
coord2 <- slice(NPI.geocode, 23001:28000) %>% tidygeocoder::geocode(address)
coord3 <- slice(NPI.geocode, 28001:30000) %>% tidygeocoder::geocode(address)
coord4 <- slice(NPI.geocode, 30001:35000) %>% tidygeocoder::geocode(address)
coord5 <- slice(NPI.geocode, 35001:40000) %>% tidygeocoder::geocode(address)
coord6 <- slice(NPI.geocode, 40001:45000) %>% tidygeocoder::geocode(address)
coord7 <- slice(NPI.geocode, 45001:50000) %>% tidygeocoder::geocode(address)
coord8 <- slice(NPI.geocode, 50001:55000) %>% tidygeocoder::geocode(address)
coord9 <- slice(NPI.geocode, 55001:60000) %>% tidygeocoder::geocode(address)
beep()

test2 <- rbind(coord1, coord2, coord3, coord4, coord5, coord6, coord7, coord8, coord9)
write.csv(test2, "geocodes20000_60000.csv", row.names = F)

sum(is.na(test2$lat))

coor1 <- slice(NPI.geocode, 60001:65000) %>% tidygeocoder::geocode(address)
coor2 <- slice(NPI.geocode, 65001:70000) %>% tidygeocoder::geocode(address)
coor3 <- slice(NPI.geocode, 70001:75000) %>% tidygeocoder::geocode(address)
coor4 <- slice(NPI.geocode, 75001:80000) %>% tidygeocoder::geocode(address)
coor5 <- slice(NPI.geocode, 80001:85000) %>% tidygeocoder::geocode(address)
coor6 <- slice(NPI.geocode, 85001:90000) %>% tidygeocoder::geocode(address)
coor7 <- slice(NPI.geocode, 90001:95000) %>% tidygeocoder::geocode(address)
coor8 <- slice(NPI.geocode, 95001:100000) %>% tidygeocoder::geocode(address)
coor9 <- slice(NPI.geocode, 100001:104000) %>% tidygeocoder::geocode(address)
coor91 <- slice(NPI.geocode, 104001:107000) %>% tidygeocoder::geocode(address)
coor92 <- slice(NPI.geocode, 107001:110000) %>% tidygeocoder::geocode(address)
coor93 <- slice(NPI.geocode, 110001:112653) %>% tidygeocoder::geocode(address)
beep()

test3 <- rbind(coor1, coor2, coor3, coor4, coor5, coor6, coor7, coor8, coor9, coor91, coor92, coor93)
write.csv(test3, "gecodes60001_112653.csv", row.names = F)

coordinates <- rbind(test1, test2, test3)
write.csv(coordinates, "tidygeocoded_addresses.csv", row.names = F)

#count number of NAs
(nummissing <- sum(is.na(coordinates$lat))) #37561

#coverage percentage
100 - (nummissing/length(coordinates$NPI)*100) #66.66%

#cost to run the rest in google to raise the coverage: $187.81
0.005*nummissing

#Now if there was an NA, let's check the address with Google using ggmap package
#call in my API

#googlekey <- "AIzaSyCUh8NAiTkDsXZmbtOb14vKjp5oHKRE-qE"
googlekey <- "AIzaSyBqh0LgFQfNMVVMUhlyOKdSN0jKAAp7DnU"
register_google(googlekey)

google <- coordinates %>% filter(is.na(lat)) %>% select(-long, -lat)
google.addresses <- ggmap::geocode(google$address) 
write.csv(google.addresses, "google_geocoded_addresses.csv", row.names = F)
beep()

#combine and merge back into the old set
google2 <- cbind(google, google.addresses)

#uses package rqdatatable to coalesce long and lat
NPI.geocode2 <- coordinates %>% rename(lon = long) %>%
                                natural_join(google2, by = "NPI", jointype = "FULL")

write.csv(NPI.geocode2, "NPI_geocoded.csv", row.names = F)

#count number of NAs
(nummissing <- sum(is.na(NPI.geocode2$lat))) #294

#coverage percentage
100 - (nummissing/length(NPI.geocode2$NPI)*100) #99.74, amazing

#NOW we have long lat for each provider
#use these to determine county
#uses sp, maps, maptools packages
#code adapted from https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r/8751965#8751965
#https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  
  #returns county and state, change to county only
  x <- countyNames[indices]
  return(str_to_title(sapply(strsplit(x, ","), function(X) X[2])))
}

#now mutate NPI.geocode and add county variable
#need to subset it so there are not any NAs
#can deal with this later when we put everything back together
NPI.geocodenarm <- NPI.geocode2 %>% filter(!is.na(lat))

NPI.geocode3 <- NPI.geocodenarm %>% mutate(County = latlong2county(data.frame(lon, lat))) %>%
                                    select(NPI, County)

#Assign counties based on ArcMap file
NPI.contained <- npi.ga %>% mutate(ZipCode = as.integer(substr(Provider.Business.Practice.Location.Address.Postal.Code,1,5))) %>%
                            filter(ZipCode %in% contained.zips$ZipCode) %>%
                            select(NPI, ZipCode) %>% left_join(contained.zips, by = "ZipCode")

#geocoded the right ones, accidentally subset initially using the mailing address instead of the practice location
#makes for a tricky merge now
#subset NPI.geocode3 to those not in NPI.contained because we prefer contained over geocoded
NPI.merge <- merge(NPI.contained, filter(NPI.geocode3, NPI.geocode3$NPI %!in% NPI.contained$NPI), by = c("NPI", "County"), all = T)

#Query remaining that I messed up by mixing up mailing address and provider location on the initial subset 
#should be about 7000
NPI.remaining <- npi.ga %>% filter(NPI %!in% NPI.merge$NPI) %>% 
                            select(NPI,
                                   Provider.First.Line.Business.Practice.Location.Address,
                                   Provider.Business.Practice.Location.Address.City.Name,
                                   Provider.Business.Practice.Location.Address.State.Name,
                                   Provider.Business.Practice.Location.Address.Postal.Code) %>%
                            rename(Street = Provider.First.Line.Business.Practice.Location.Address,
                                   City = Provider.Business.Practice.Location.Address.City.Name,
                                   State = Provider.Business.Practice.Location.Address.State.Name,
                                   Zip_Code = Provider.Business.Practice.Location.Address.Postal.Code) %>%
                            mutate(Zip_Code = substr(Zip_Code,1,5)) %>%
                            mutate(address = paste(paste(Street, City, sep = ", "), State, sep = ", ")) 

google.remaining <- ggmap::geocode(NPI.remaining$address) 

#combine and merge back into the old set
NPI.remaining2 <- cbind(NPI.remaining, google.remaining) 

NPI.remainingnarm <- NPI.remaining2 %>% filter(!is.na(lat)) %>% mutate(County = latlong2county(data.frame(lon, lat))) %>%
                                                                select(NPI, County)

NPI.remaining3 <- merge(NPI.remaining2, NPI.remainingnarm, by = "NPI", all = T) #MERGE GOOD

NPI.remaining4 <- NPI.remaining3 %>% select(NPI, County)

#merge with bigger file
NPI.finalgeocode <- merge(NPI.merge, NPI.remaining3, by = c("NPI", "County"), all = T) %>%
                    select(NPI, County)

#count number of NAs
(nummissing <- sum(is.na(NPI.finalgeocode$County))) #633

#coverage percentage
100 - (nummissing/length(NPI.finalgeocode$NPI)*100) #99.60, nice

#now we want to combine contained zips (NPI, ZIP, County), google+tidygeocoded addresses (NPI, County), and NPI Georgia data (NPI)
#merge with bigger NPI file
NPI <- merge(npi.ga, NPI.finalgeocode, by = "NPI", all = T) #merge good

write.csv(NPI, "NPI_finalgeocode.csv", row.names = F)

#could rewrite the last 5 actions or so^ to save space and time using dplyr, but right now I want to make sure everything runs cleanly

#Now combine all these parsed NPI addresses back together and add the rest of the NPI information

#now, reduce taxonomy codes to figure out the information we need from providers
#reduce to wide format

#mental health codes
mentalhealth.tax <- read.csv("MentalHealthTaxonomy.csv", header = F)

#get only the codes
mh.tax <- mentalhealth.tax %>% mutate(mh = sapply(strsplit(V1, " "),`[`,1), V1 = NULL)

#create count of mental health providers for each county
# Buprenorphine <- Buprenorphine %>%
#   filter(state == "GA") %>%
#   add_count(county) %>%
#   select(county, n) %>%
#   distinct()

test <- npi.ga %>% filter(Healthcare.Provider.Taxonomy.Code_1 %in% mh.tax$mh)


#########################################
#Descriptive table and maps
#########################################

final <- possible.pred6

#table1 package
#labels
label(final$population) <- "Population size"
label(final$incomepercap)   <- "Per capita income"
label(final$logincomepercap)    <- "Per capita income (log)"
label(final$gini) <- "Gini coefficient"
label(final$pctuninsured) <- "Percentage uninsured"
label(final$pctnodiploma) <- "Percentage with no high school diploma"
label(final$pctpoverty) <- "Percentage living in poverty"
label(final$pctnonhiswhite) <- "Percentage of population non-Hispanic White"
label(final$pctunemployed) <- "Percentage of population unemployed"
label(final$pctdisability) <- "Percentage of population with a disability"
label(final$pctfemalehouse) <- "Percentage of female-headed households"
label(final$pctnevermarried) <- "Percentage of population never married"
label(final$pctvehicle) <- "Percentage with vehicle access"
label(final$tothousing) <- "Housing units"
label(final$totoccupied) <- "Occupied housing units"
label(final$totvacant) <- "Vacant housing units"
label(final$mobilehome) <- "Mobile homes"
label(final$mobilehomeP) <- "Percentage of mobile homes"
label(final$phone) <- "Home without phone service"
label(final$phoneP) <- "Percentage of homes without phone service"
label(final$crowded) <- "Crowded housing units"
label(final$prematuredeaths) <- "Premature deaths"
label(final$potlifelost) <- "Years of potential life lost"
label(final$poorhealth) <- "Adults reporting poor/fair health"
label(final$poorhealthdays) <- "Poor physical health days"
label(final$mentaldays) <- "Poor mental health days"
label(final$injurydeaths) <- "Injury-related deaths"
label(final$injurydeathrate) <- "Rate of injury-related deaths"
label(final$smokers) <- "Percentage of adults who smoke"
label(final$teenbirth) <- "Teen birth rate per thousand"
label(final$reladherance) <- "Total church adherents"
label(final$reladherancerate) <- "Rate of church adherence"
label(final$urbanrural) <- "NCHS Urban-Rural status"
label(final$numproviders) <- "Buprenorphine providers"
label(final$providerspercap) <- "Per capita buprenorphine providers"
label(final$HIDTA) <- "High Intensity Drug Trafficking Area"


#create table one
#predictor, mean (SD), median, range, missing
# table1(~population + incomepercap + logincomepercap + gini + pctuninsured +
#         pctnodiploma + pctpoverty + pctnonhiswhite + pctunemployed + pctdisability +
#         pctfemalehouse + pctnevermarried + pctvehicle +
#         tothousing + totoccupied + totvacant + mobilehome + mobilehomeP +
#         phone + phoneP + crowded + prematuredeaths + potlifelost +
#         poorhealth + poorhealthdays + mentaldays + injurydeaths + injurydeathrate +
#         smokers + teenbirth + reladherance + reladherancerate + urbanrural +
#         numproviders + providerspercap + HIDTA, data = final)

table1(~., data = select(final, -County, -FIPS))
