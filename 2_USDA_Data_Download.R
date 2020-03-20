### STILL NEED TO FINISH THE FILTERING



# Install usdarnass package if needed
#install.packages('devtools')
devtools::install_github("rdinter/usdarnass")

# Load libraries
library(usdarnass)
library(dplyr)

# Input your api key (can be obtained here: https://quickstats.nass.usda.gov/api )
nass_set_key("C30FDA05-58D5-3632-8543-83A4C25A2145")

# Download data by state, The download function only accepts one argument
# so we have to loop through the years and I just use a seperate call for each state

# North Carolina
NC <- data.frame()
for(n in 1981:2018){
  data <- nass_data(year = n,
                    state_name = "NORTH CAROLINA",
                    statisticcat_desc = "YIELD")%>%
    filter(commodity_desc %in% c("CORN","COTTON","PEANUTS","SOYBEANS","SWEET POTATOES"),
           short_desc %in% c("CORN, GRAIN - YIELD, MEASURED IN BU / ACRE",
                             "COTTON, UPLAND - YIELD, MEASURED IN LB / ACRE",
                             "PEANUTS - YIELD, MEASURED IN LB / ACRE",
                             "SOYBEANS - YIELD, MEASURED IN BU / ACRE",
                             "SWEET POTATOES - YIELD, MEASURED IN CWT / ACRE"))
  NC <- rbind(NC,data)
}


# South Carolina
SC <- data.frame()
for(n in 1981:2018){
  data <- nass_data(year = n,
                    state_name = "SOUTH CAROLINA",
                    statisticcat_desc = "YIELD")%>%
    filter(commodity_desc %in% c("CORN","COTTON","PEANUTS","SOYBEANS"),
           short_desc %in% c("CORN, GRAIN - YIELD, MEASURED IN BU / ACRE",
                             "COTTON, UPLAND - YIELD, MEASURED IN LB / ACRE",
                             "PEANUTS - YIELD, MEASURED IN LB / ACRE",
                             "SOYBEANS - YIELD, MEASURED IN BU / ACRE"))
  SC <- rbind(SC,data)
}

# Georgia
GA <- data.frame()
for(n in 1981:2018){
  data <- nass_data(year = n,
                    state_name = "GEORGIA",
                    statisticcat_desc = "YIELD")%>%
    filter(commodity_desc %in% c("CORN","COTTON","PEANUTS","SOYBEANS"),
           short_desc %in% c("CORN, GRAIN - YIELD, MEASURED IN BU / ACRE",
                             "COTTON, UPLAND - YIELD, MEASURED IN LB / ACRE",
                             "PEANUTS - YIELD, MEASURED IN LB / ACRE",
                             "SOYBEANS - YIELD, MEASURED IN BU / ACRE"))
  GA <- rbind(GA,data)
}



# Plot Corn in NC
NC%>%
  filter(commodity_desc == "CORN")%>%
  ggplot()+
  geom_line(aes(x=year, y=Value, group = county_code))
