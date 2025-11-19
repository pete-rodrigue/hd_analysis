library(dplyr)
library(leaflet)
library(sf)
library(ggplot2)
library(plotly)
library(stringr)

setwd("C:/Users/ERODRI01/OneDrive - Environmental Protection Agency (EPA)/Downloads/hd_analysis")
getwd()
solar_files<-list.files("building_permits")
solar_files<-solar_files[solar_files!="solar_all_years.csv"]

#for loop smushing 2009-2025 data together in one dataframe
i<-1
for(cur_file in solar_files){
  temp_data<-readr::read_csv(file = file.path("building_permits",cur_file),
                             show_col_types = FALSE )
  if(i==1){
    solar_df<-temp_data
  }else{
    solar_df<-dplyr::bind_rows(solar_df, temp_data)
  }
  i<-i+1
}

#converting issue dates into numeric years
solar_df$year<-as.numeric(substr(x = solar_df$ISSUE_DATE,start = 1,stop = 4))

#getting all rows with solar in the indices for description of work and for subtype name
solar_rows<-unique(
  grep(pattern = "solar|photovoltaic",
                 x = solar_df$DESC_OF_WORK,
                 ignore.case = TRUE),
grep(pattern = "solar",
                 x = solar_df$PERMIT_SUBTYPE_NAME,
                 ignore.case = TRUE)
)
solar_df<-solar_df[solar_rows,]%>%
  distinct(.)

write.csv(x = solar_df,file = "solar_df.csv",row.names = FALSE)

solar_df<-read.csv("solar_addresses_geocoded.csv")%>%
  filter(Geocodio.Accuracy.Score>0)
leaflet()%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addCircleMarkers(data = solar_df,
                   lng = ~Geocodio.Longitude,
                   lat = ~Geocodio.Latitude,
                   radius = 2,
                   stroke = FALSE,
                   label = ~FULL_ADDRESS
                     )
