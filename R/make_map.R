library(sf)
library(dplyr)
library(lubridate)
library(tidyr)

download.file("https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv", "data/MSOAs_latest.csv",
              mode = "wb")

covid <- read.csv("data/MSOAs_latest.csv")
covid$date <- ymd(covid$date)
covid$week <- as.numeric(covid$date - min(covid$date)) / 7 + 11
covid$week <- paste0("wk_",covid$week)

covid <- covid[,c("areaCode","week","newCasesBySpecimenDateRollingSum")]
covid$newCasesBySpecimenDateRollingSum[is.na(covid$newCasesBySpecimenDateRollingSum)] <- 0
covid <- pivot_wider(covid, names_from = "week", values_from = "newCasesBySpecimenDateRollingSum")
covid <- as.data.frame(covid)

dir.create("tmp")
unzip("data/Middle_Layer_Super_Output_Areas__December_2011__Boundaries-shp.zip", exdir = "tmp")
msoa <- read_sf("tmp/Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp")
unlink("tmp", recursive = TRUE)

covid$total <- rowSums(covid[2:ncol(covid)])
covid$rolling <- rowSums(covid[seq(ncol(covid)-4,ncol(covid)-1)])
covid$change_lastwk <- round((covid[,(ncol(covid) - 2)] - covid[,(ncol(covid) - 3)])/ (covid[,(ncol(covid) - 3)]) * 100)
covid$change_lastwk[covid$change_lastwk == Inf] <- NA
covid$change_lastwk[is.nan(covid$change_lastwk)] <- NA

covid$change_lastmt <- round((covid[,(ncol(covid) - 2)] - covid[,(ncol(covid) - 7)])/ (covid[,(ncol(covid) - 7)]) * 100)
covid$change_lastmt[covid$change_lastmt == Inf] <- NA
covid$change_lastmt[is.nan(covid$change_lastmt)] <- NA

msoa <- msoa[,c("msoa11cd")]
msoa <- msoa[substr(msoa$msoa11cd,1,1) == "E",]
msoa <- left_join(msoa, covid, by = c("msoa11cd" = "areaCode"))
msoa <- st_transform(msoa, 4326)

mapview::mapview(msoa[1:10,])
summary(msoa$total)
summary(msoa$wk_41)
summary(msoa$rolling)


write_sf(msoa,"data/covid.geojson", delete_dsn = TRUE)



if(FALSE){

  library(tmap)
  msoa <- st_buffer(msoa, 0)

  unzip("data/Local_Authority_Districts__December_2019__Boundaries_UK_BGC-shp.zip", exdir = "tmp")
  la <- read_sf("tmp/Local_Authority_Districts__December_2019__Boundaries_UK_BGC.shp")
  la <- la[,"lad19cd"]
  la <- la[substr(la$lad19cd,1,1) == "E",]
  la <- st_transform(la, 4326)
  unlink("tmp", recursive = TRUE)

  n <- seq(10, 40, by=1)
  m1 <- tm_shape(msoa) +
  tm_fill(col = paste0("wk_", n),
          legend.show = TRUE,
          breaks = c(0,2,5,10,20,30,40,50,100,500,800),
          title = paste0("Week ", n)) +
  tm_shape(la) +
  tm_borders(lwd = 0.3) +
  tm_facets(free.scales.symbol.size = FALSE, nrow=1,ncol=1, ) +
  tm_layout(frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA)


  Sys.time()
  tmap_animation(
    m1,
    filename = "animation3.gif",
    width = 800,
    height = 1000,
    delay = 40,
    loop = TRUE,
    restart.delay = 1000
  )
  Sys.time()

  data(NLD_prov)

  m1 <- tm_shape(NLD_prov) +
    tm_polygons("yellow") +
    tm_facets(along = "name")

  tmap_animation(m1, filename="Dutch_provinces.gif", width=800, delay=40)

}

