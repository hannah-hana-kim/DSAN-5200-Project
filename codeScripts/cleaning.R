library(tidyverse)

#initial read in
europeNA <- read.csv('RawData/Europe+NA/Food_Security_Data_E_Northern_America_and_Europe_NOFLAG.csv')
africa <- read.csv('RawData/Africa/Food_Security_Data_E_Africa_NOFLAG.csv')

#add region code for ease of comparison later
europeNA <- europeNA %>% 
  mutate('Region' = 'Europe')
africa <- africa %>%
  mutate('Region' = 'Africa' )

#grab europe country list to filter out North American countries
#print(unique(europeNA$Area))
noThanks <- c("Canada", "Bermuda", "Greenland", "United States of America")
europe <- europeNA %>%
  filter(!(Area %in% noThanks))
#print(unique(europe$Area))

#merge em for pulling out metrics
bigPapa <- rbind(europe, africa)

print(unique(bigPapa$Item))

#lists that make life easier
#for dropping unneccessary cols
excludeInThreeYearAvg <- c("Area.Code", "Area.Code..M49.", "Item.Code", "Item", "Element.Code", 
                  "Element", "Unit", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005",
                  "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014",
                  "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020", "Y2021", "Y2022")
excludeInYearly <- c("Area.Code", "Area.Code..M49.", "Item.Code", "Item", "Element.Code", 
                    "Element", "Unit", "Y20002002", "Y20012003", 'Y20022004', "Y20032005", "Y20042006", "Y20052007", "Y20062008",
                    "Y20072009", "Y20082010", "Y20092011", "Y20102012", "Y20112013", "Y20122014", "Y20132015", 
                    "Y20142016", "Y20152017", "Y20162018", "Y20172019", "Y20182020", "Y20192021", "Y20202022")

# ------------------------------
# DIETARY ENERGY ADEQUACY
# ------------------------------

#initial filter
energyAdequacy <- bigPapa %>%
  filter(Item == "Average dietary energy supply adequacy (percent) (3-year average)")  

#get rid of unnecessary columns
energyAdequacy <- energyAdequacy %>%
  select (-all_of(excludeInThreeYearAvg))

head(energyAdequacy)

#pivot
energyPapa <- energyAdequacy %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "Percent"
  )

#export
write.csv(energyPapa, "CleanedData/energyadequacy.csv")

# ------------------------------
# RAIL DENSITY
# ------------------------------

#intial filter
railLine <- bigPapa %>%
  filter(Item == "Rail lines density (total route in km per 100 square km of land area)")  

#drop extra cols
railLine <- railLine %>%
  select (-all_of(excludeInYearly))

#pivot
railPapa <- railLine %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'Year', 
    names_prefix = 'Y',
    values_to = "RailDensity"
  )

#export
write.csv(railPapa, "CleanedData/raildensity.csv")

# ------------------------------
# GDP PER CAPITA
# ------------------------------

#intial filter
gdp <- bigPapa %>%
  filter(Item == "Gross domestic product per capita, PPP, (constant 2017 international $)")  

#drop extra cols
gdp <- gdp %>%
  select (-all_of(excludeInYearly))

#pivot
gdPapa <- gdp %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'Year', 
    names_prefix = 'Y',
    values_to = "GDP_PerCapita"
  )

#export
write.csv(gdPapa, "CleanedData/gdp.csv")


# ------------------------------
# UNDERNOURISHMENT
# ------------------------------

#initial filter
undernourish <- bigPapa %>%
  filter(Item == "Prevalence of undernourishment (percent) (3-year average)")  

#get rid of unnecessary columns
undernourish <- undernourish %>%
  select (-all_of(excludeInThreeYearAvg))

#pivot
underPapa <- undernourish %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "Percent"
  )


#export
write.csv(underPapa, "CleanedData/undernourishment.csv")

# ------------------------------
# CEREAL IMPORT RATIO
# ------------------------------

#initial filter
cerealImport <- bigPapa %>%
  filter(Item == "Cereal import dependency ratio (percent) (3-year average)")  

head(cerealImport)

#get rid of unnecessary columns
cerealImport <- cerealImport %>%
  select (-all_of(excludeInThreeYearAvg))

#pivot
cerealPapa <- cerealImport %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "Percent"
  )

head(cerealPapa)

#export
write.csv(cerealPapa, "CleanedData/cerealimportratio.csv")



