library(gt)
library(tidyverse)
library(scales)
library(here)
library(htmlwidgets)
library(webshot)

df <- read.csv(here("data/Crops_Final_Figs/Crops_Final_Figs/Tables1and2/StateCropsPPtCorrelation.csv"))%>%
  mutate("Month" = month.abb[month],
         "stateNo" = ifelse(State == "NORTH CAROLINA",1,
                            ifelse(State == "SOUTH CAROLINA",2,3)))%>%
  select(State,stateNo,Month,month,Commodity,corr,pval)%>%
  arrange(month,stateNo)

# create color scale to match the corelation plots
scale <- col_bin(cols <- c("#800505","#bf1717","#d95d5d","#e0ce75","#43c463","#1d7d35","#025216"),
                 domain = c(-0.71,0.67),
                 bins = c(-0.71,-0.5,-0.25,-0.05,0.05,0.25,0.5,0.67),
                 na.color = "#c4c4c0")

# Pivot the table to wide format
dfWide <-df%>%
  pivot_wider(id_cols = c("State","stateNo","Month","month"), names_from = Commodity, values_from = c(corr,pval))
colnames(dfWide) <- c("State","stateNo","Month","month","CORN","COTTON","PEANUTS","SOYBEANS","SWEET POTATOES",
                      "pCORN","pCOTTON","pPEANUTS","pSOYBEANS","pSWEETPOTATOES")

# Make Table
ppt <- dfWide%>%
  gt(groupname_col = "State")%>%
  tab_header("Precipitation")%>%
  tab_source_note(md("Statistically significant correlations (p-value < 0.05) denoted by **bold white** font"))%>%
  fmt_number(columns = vars(CORN,COTTON,PEANUTS,SOYBEANS,'SWEET POTATOES'), decimals = 3)%>%
  data_color(columns = vars(CORN,COTTON,PEANUTS,SOYBEANS,'SWEET POTATOES'),
             colors = scale)%>%
  cols_hide(columns = vars(pCORN,pCOTTON,pPEANUTS,pSOYBEANS,pSWEETPOTATOES,month,stateNo))%>%
  tab_style(style = cell_text(weight = "bold", color = "#ffffff"),
    locations = cells_body(
      columns = vars(COTTON),
      rows = pCOTTON <= 0.05))%>%
  tab_style(style = cell_text(weight = "bold", color = "#ffffff"),
    locations = cells_body(
      columns = vars(CORN),
      rows = pCORN <= 0.05))%>%
  tab_style(style = cell_text(weight = "bold", color = "#ffffff"),
            locations = cells_body(
              columns = vars(PEANUTS),
              rows = pPEANUTS <= 0.05))%>%
  tab_style(style = cell_text(weight = "bold", color = "#ffffff"),
            locations = cells_body(
              columns = vars(SOYBEANS),
              rows = pSOYBEANS <= 0.05))%>%
  tab_style(style = cell_text(weight = "bold", color = "#ffffff"),
            locations = cells_body(
              columns = vars('SWEET POTATOES'),
              rows = pSWEETPOTATOES <= 0.05))%>%
  cols_align(align = "center", columns = TRUE)%>%
  tab_options(
    summary_row.background.color = "#ACEACE80",
    table.font.color = "#323232",
    table_body.hlines.color = "#000000",
    table_body.vlines.color = "#000000",
    table_body.vlines.style = "solid",
    heading.border.bottom.color = "#000000",
    row_group.border.top.color = "#000000",
    row_group.border.bottom.style = "none",
    stub.border.style = "dashed",
    stub.border.color = "#000000",
    stub.border.width = "1px",
    summary_row.border.color = "#000000"
  )

#gtsave(ppt,here("figures/tables/ppt_correlations.pdf"))
