library(plyr)
library(data.table)
library(lattice)
library(ggplot2)
library(ggmap)
library(lubridate)
df <- fread("../data/chicago_01_present.csv", data.table = FALSE)
names(df) <- sub(" ", "", names(df))

"""
TODO
Add:
- demographic data: https://www.cityofchicago.org/city/en/depts/doit/supp_info/census_maps.html
- microeconomic data: median household income by district, ward, or location some other way 
- macroeconomic data: annual GDP, monthly unemployment, CPI maybe
    - city-specific maybe? Not sure if this is possible
- population density data  
"""