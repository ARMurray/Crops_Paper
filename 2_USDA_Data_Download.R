# Install usdarnass package if needed
#install.packages('devtools')
devtools::install_github("rdinter/usdarnass")

# Load libraries
library(usdarnass)
library(dplyr)

# Input your api key (can be obtained here: https://quickstats.nass.usda.gov/api )
nass_set_key("C30FDA05-58D5-3632-8543-83A4C25A2145")

# Get Corn Data
nass_data(year = seq(1981,2018),
          state_name = c("NORTH CAROLINA","SOUTH CAROLINA","GEORGIA"),
          )