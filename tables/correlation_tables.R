library(here)
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)

ppt <- read.csv(here("data/Correlations/ppt_Correlations.csv"))%>%
  replace(.,is.na(.),"--")

# Georgia ppt
gaPpt <- ppt%>%
  filter(State == "Georgia")%>%
  select(-"Sweet.Potatoes", -State)
gaPptTbl <- formattable(gaPpt,align =c("l","c","c","c","c"), 
            list(`Month` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")) 
            ))
gaPptTbl
# North Carolina ppt
ncPpt <- ppt%>%
  filter(State == "N_Carolina")%>%
  select(-State)
colnames(ncPpt)[6] <- "Sweet Potatoes"
ncPptTbl <- formattable(ncPpt,align =c("l","c","c","c","c","c"), 
                        list(`Month` = formatter(
                          "span", style = ~ style(color = "grey",font.weight = "bold")) 
                        ))
ncPptTbl

# South Carolina ppt
scPpt <- ppt%>%
  filter(State == "S_Carolina")%>%
  select(-State,-Sweet.Potatoes)
scPptTbl <- formattable(scPpt,align =c("l","c","c","c","c","c"), 
                        list(`Month` = formatter(
                          "span", style = ~ style(color = "grey",font.weight = "bold")) 
                        ))
scPptTbl
