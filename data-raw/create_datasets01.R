# specification of some handy constants
# Ewan McHenry

library(tidyverse)
library(sf) # for gis

# Define commonly used working directories
gis.wd =  "D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\GIS"
main.wd = "D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\GIS\\Data"
cars.factfile.direct =  paste0(gis.wd, "\\CARs\\CARs factfile\\")


# Generate datasets ----

# Treescapes names
ts.names = st_read("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\GIS\\Data\\Treescape boundaries\\treescapes_230707.shp")$name %>% sort()
ts.abbrv = c("Cumb.", "Devon", "D. 2 D.", "E.Clay.", "Faugh.", "NE Eng.", "N.For.", "S.S.R.", "Sher.", "W & Ds" )
ts.lcm.names = c("cumbria" , "Devon" , "D2D" , "EClay" , "Faughan" , "NEEng" , "NF" , "SSR" , "Sherwood" , "WealdandDowns" )# for folders

ts.nice.names = c("Cumbria", "Devon", "D2D", "E.Claylands.", "Faughan", "NE Eng.", "N.Forest", "ASR.", "Sherwood", "Weald & Ds" )
this.andAll.tss = c(ts.lcm.names,"AllTreescapes")
ts.andAll.nice.names = c(ts.nice.names,"All Treescapes")
ts.andAll.lcm.names = c(ts.lcm.names,"AllTreescapes")

# CARs names
england.car.names = c("Central", "North", "South East", "South West")
countries.names = c("England",  "Northern Ireland", "Scotland", "Wales")


# lcm ----
ceh.col.pallette = c("#FF0000" # broadleaf
                     , "#006600" # conifer
                     , "#732600" # arable
                     , "#00FF00" # improved grass
                     , "#7FE57F" # neutral grass
                     , "#70A800" # calcareous grass
                     , "#998100" # acid grass
                     , "#FFFF00" # fan, marsh, swamp
                     , "#801A80" # heather
                     , "#E68CA6" # heather grass
                     , "#008073" # bog
                     , "#D2D2FF" # inland rock
                     , "#000080" # saltwater
                     , "#0000FF" # freshwater
                     , "#CCB300", "#CCB300" # Supralittoral rock/sediment
                     , "#FFFF80", "#FFFF80" # Littoral rock/sediment
                     , "#8080FF" # saltmarsh
                     , "#000000" # urban
                     , "#808080") # suburban

ceh.full.habtype = c("Broadleaf", "Conifer", "Arable", "Improved grass",
                     "Neutral grass", "Calcareous grass", "Acid grass",
                     "Fen, marsh, swamp", "Heather", "Heather grass",
                     "Bog", "Inland rock", "Saltwater", "Freshwater", "Supralittoral rock", "Supralittoral sediment",
                     "Littoral rock", "Littoral sediment", "Saltmarsh",
                     "Urban", "Suburban")
ceh.full.habtype = factor(ceh.full.habtype, levels = ceh.full.habtype)

ceh.concatted1.habtype = c( "Arable" , "Improved grassland", "Semi-natural grassland", "Mountain, heath and bog",
                            "Coastal", "Built up and gardens")

E.cols = list(
  connectiv.high = "#FFE100",
  connectiv.low = "#0051FF"
)


# Save datasets
usethis::use_data(
  gis.wd,
  main.wd,
  cars.factfile.direct,
  ts.names,
  ts.abbrv,
  ts.lcm.names,
  ts.nice.names,
  this.andAll.tss,
  ts.andAll.nice.names,
  ts.andAll.lcm.names,
  england.car.names,
  countries.names,
  ceh.col.pallette,
  ceh.full.habtype,
  ceh.concatted1.habtype,
  E.cols,
  overwrite = TRUE
)

# projection shortcuts ----
projections <- list(
  ukgrid = "+init=epsg:27700",
  wgs4326 = "+init=epsg:4326"
)
