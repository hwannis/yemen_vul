# Load required packages
library(sf)
library(ggplot2)
library(raster)
library(dplyr)
library(rgdal)
library(viridis)
library(rmapshaper)
library(leaflet)


# # Get the path for yemen admin boundaries shapefile
# admin_level_3_shapefile_path <- "C:\\R\\yemen\\shp\\Yemen_adm3_shp.shp"
# 
# # get the pop local path to your WorldPop raster data
# worldpop_raster_path <- "C:\\R\\yemen\\pop_yem_2020adj.tif"
# 
# # Read the admin boundaries for Yemen
# yemen_admin3 <- st_read(admin_level_3_shapefile_path)
# 
# # Read in WorldPop raster data
# yemen_pop_raster <- raster(worldpop_raster_path)
# 
# # Crop raster data to extent of Yemen admin boundaries
# pop_raster_cropped <- crop(yemen_pop_raster, extent(yemen_admin3))
# 
# # Sum raster cells within each polygon and add it to the yemen_admin3 sf object
# yemen_admin3$population <- extract(pop_raster_cropped, yemen_admin3, fun = sum, na.rm = TRUE)
# 
# yemen_admin3$population_per <- yemen_admin3$population /sum(yemen_admin3$population)
# 
# st_write(yemen_admin3,"C:\\R\\yemen\\shp\\Yemen_adm3wpop_shp1.shp", encoding = "UTF-8", append = FALSE)

## get shapefile at admin2 with grid population these consist of 335 admin2 ploygons
yemen_adm2_wpop<- st_read("C:\\R\\yemen\\shp\\Yemen_adm2_pop.shp")
## get pop at admin3 based on grid population these consist of 2149 ploygons
yemen_pop_adm3<- read.xlsx("c:\\R\\yemen\\yemen_pop_adm3.xlsx")

# Plot
p<-ggplot(data = yemen_adm2_wpop) +
  geom_sf(aes(fill = yemen_adm2_wpop$pop_prop)) +
  scale_fill_viridis_c(option = "D") +
  ggtitle("Grid Population Map of Yemen within Admin Boundaries Level 3") +
  theme_minimal()

############ read UNOCHA population data 
ychildpop<-read.xlsx("C:\\R\\yemen\\yemochapop.xlsx", check.names = FALSE) # these consist of 333 ploygons (2 less than the shapefile and 2 have negative pop data)
####### read vulnerability estimates at admin1 level
y_vuln_est<- read.xlsx("c:\\R\\yemen\\yemen_dep.xlsx")

###pop data and vuln data at adm2 level merged at adm1 level
ychildpop %>% left_join(y_vuln_est,by ="ADM1_PC") -> y_pop_vul


# yemen_vul<- merge(yemen_adm2_wpop,ychildpop,by.x="ADM2_PC",by.y="ADM2_PC", all.x=TRUE, suffixes=c("","_df2"))
# # Drop repeated variables from the second data frame (those with suffix "_df2")
# cols_to_drop <- grep("_df2$", names(yemen_vul), value = TRUE)
# yemen_vul <- yemen_vul[, !names(yemen_vul) %in% cols_to_drop]
# 
# 
# 
# # merge with the shapefile admin1
# yemen_vul<-merge(yemen_vul,y_vuln_est,by="ADM1_PC")


############### index calculations
#calculate index for admin2 based on U5 and MODA dep variable and replace NA with 0 as the difference is miniscule without inlcuding the NAs
y_pop_vul$dep_u5<- ifelse(!is.na(y_pop_vul$U5_t), y_pop_vul$moda_u5_2_6_dep * y_pop_vul$U5_t /100,0)
#standardize the index on the range
y_pop_vul$index_u5<- (y_pop_vul$dep_u5 - min(y_pop_vul$dep_u5,na.rm = TRUE))/ (max(y_pop_vul$dep_u5,na.rm = TRUE)-min(y_pop_vul$dep_u5,na.rm = TRUE))

#quintile U5 class of the admin2 boundaries
y_pop_vul$index_u5_quintile <- cut(y_pop_vul$index_u5, 
                                breaks = quantile(y_pop_vul$index_u5, probs = seq(0, 1, by = 0.2),na.rm=TRUE), 
                                labels = 5:1, 
                                include.lowest = TRUE, 
                                ordered_result = TRUE)


#calculate index for admin2 based on c5-17 and MODA dep variable and replace NA with 0 as the difference is miniscule without inlcuding the NAs
y_pop_vul$dep_c517<- ifelse(!is.na(y_pop_vul$c5_17t), y_pop_vul$moda_517_2_6 * y_pop_vul$c5_17t /100,0)
#standardize the index on the range
y_pop_vul$index_c517<- (y_pop_vul$dep_c517 - min(y_pop_vul$dep_c517,na.rm = TRUE))/ (max(y_pop_vul$dep_c517,na.rm = TRUE)-min(y_pop_vul$dep_c517,na.rm = TRUE))

#quintile c5-17 class of the admin2 boundaries
y_pop_vul$index_c517_quintile <- cut(y_pop_vul$index_c517, 
                                   breaks = quantile(y_pop_vul$index_c517, probs = seq(0, 1, by = 0.2),na.rm=TRUE), 
                                   labels = 5:1, 
                                   include.lowest = TRUE, 
                                   ordered_result = TRUE)

#calculate index for admin2 based on children and MODA dep variable and replace NA with 0 as the difference is miniscule without inlcuding the NAs
y_pop_vul$dep_tot<- ifelse(!is.na(y_pop_vul$child_t), y_pop_vul$moda_tot_2_6 * y_pop_vul$child_t /100,0)
#standardize the index on the range
y_pop_vul$index_tot<- (y_pop_vul$dep_tot - min(y_pop_vul$dep_tot,na.rm = TRUE))/ (max(y_pop_vul$dep_tot,na.rm = TRUE)-min(y_pop_vul$dep_tot,na.rm = TRUE))

#quintile tot class of the admin2 boundaries
y_pop_vul$index_tot_quintile <- cut(y_pop_vul$index_tot, 
                                     breaks = quantile(y_pop_vul$index_tot, probs = seq(0, 1, by = 0.2),na.rm=TRUE), 
                                     labels = 5:1, 
                                     include.lowest = TRUE, 
                                     ordered_result = TRUE)


### calculate weighted and then combined deprivation of children u5 and c517 separatly
y_pop_vul$dep_tot_calc<- y_pop_vul$dep_u5 + y_pop_vul$dep_c517
#standardize the index on the range
y_pop_vul$index_tot_calc<- (y_pop_vul$dep_tot_calc - min(y_pop_vul$dep_tot_calc,na.rm = TRUE))/ (max(y_pop_vul$dep_tot_calc,na.rm = TRUE)-min(y_pop_vul$dep_tot_calc,na.rm = TRUE))

#quintile tot class of the admin2 boundaries
y_pop_vul$index_tot_calc_quintile <- cut(y_pop_vul$index_tot_calc, 
                                    breaks = quantile(y_pop_vul$index_tot_calc, probs = seq(0, 1, by = 0.2),na.rm=TRUE), 
                                    labels = 5:1, 
                                    include.lowest = TRUE, 
                                    ordered_result = TRUE)

### calculate MPI based deprivation index
y_pop_vul$mpi_2013 <- as.numeric(y_pop_vul$mpi_2013)
y_pop_vul$dep_mpi<- ifelse(!is.na(y_pop_vul$mpi_2013), y_pop_vul$mpi_2013 * y_pop_vul$child_t /100,0)
#standardize the index on the range
y_pop_vul$index_mpi<- (y_pop_vul$dep_mpi - min(y_pop_vul$dep_mpi,na.rm = TRUE))/ (max(y_pop_vul$dep_mpi,na.rm = TRUE)-min(y_pop_vul$dep_mpi,na.rm = TRUE))

#quintile tot class of the admin2 boundaries
y_pop_vul$index_mpi_quintile <- cut(y_pop_vul$index_mpi, 
                                    breaks = quantile(y_pop_vul$index_mpi, probs = seq(0, 1, by = 0.2),na.rm=TRUE), 
                                    labels = 5:1, 
                                    include.lowest = TRUE, 
                                    ordered_result = TRUE)

write.xlsx(as.data.frame(y_pop_vul),"data/vul_data.xlsx")

# vul_short_df<- data.frame(ad2=yemen_vul$ADM2_PC, vul_index5=yemen_vul$index_u5,
#                           vul_quint5=yemen_vul$index_u5_quintile,
#                           vul_index517=yemen_vul$index_c517,
#                           vul_quint517=yemen_vul$index_c517_quintile,
#                           vul_index_tot=yemen_vul$index_tot,
#                           vul_quint_tot=yemen_vul$index_tot_quintile,
#                           vul_index_tot_calc=yemen_vul$index_tot_calc,
#                           vul_quint_tot_calc=yemen_vul$index_tot_calc_quintile)

# yemen_vul <- yemen_vul[, !names(yemen_vul) %in% c("ADM0_EN", "ADM0_PC", "date_","validOn","ADM1_AR","ADM2_AR")]
yemen_vul_simp <- yemen_adm2_wpop[, names(yemen_adm2_wpop) %in% c("ADM1_EN", "ADM1_PC", "ADM2_EN", "ADM2_PC","geometry")]

st_write(yemen_vul_simp,"shp\\Yemen_vul_simp.shp", encoding = "UTF-8", append = FALSE)

# test_shp<-merge(yemen_vul_simp,y_pop_vul,by=c("ADM2_PC"))
# st_write(test_shp,"shp\\Yemen_vul_test.shp", encoding = "UTF-8", append = FALSE)
 
 # vuln_data<- st_set_geometry(yemen_vul, NULL)
 

# saveRDS(yemen_vul, "data/Yemen_vul.rds")
 
# testt_simple<- ms_simplify(vul_shp)
#map
# Plot
p5<-ggplot(data = yemen_vul) +
  geom_sf(aes(fill = index_u5_quintile)) +
  scale_fill_manual(values = c("1" = "black", "2" = "red", "3" = "orange", "4" = "yellow", "5" = "green")) +
  ggtitle("Grid Population Map of Yemen within Admin Boundaries Level 3") +
  theme_minimal()

p517<-ggplot(data = yemen_vul) +
  geom_sf(aes(fill = index_c517_quintile)) +
  scale_fill_manual(values = c("1" = "black", "2" = "red", "3" = "orange", "4" = "yellow", "5" = "green")) +
  ggtitle("Grid Population Map of Yemen within Admin Boundaries Level 3") +
  theme_minimal()

ptot<-ggplot(data = yemen_vul) +
  geom_sf(aes(fill = index_tot_quintile)) +
  scale_fill_manual(values = c("1" = "black", "2" = "red", "3" = "orange", "4" = "yellow", "5" = "green")) +
  ggtitle("Grid Population Map of Yemen within Admin Boundaries Level 3") +
  theme_minimal()


ggplot(data = testt_simple) +
  geom_sf(aes(fill = as.ordered(testt_simple$indx___))) +
  scale_fill_manual(values = c("1" = "black", "2" = "red", "3" = "orange", "4" = "yellow", "5" = "green")) +
  ggtitle("Grid Population Map of Yemen within Admin Boundaries Level 3") +
  theme_minimal()

pal <- colorQuantile("viridis", NULL, n = 5)

leaflet(testt_simple) %>% 
  addProviderTiles(providers$Stamen.Toner) %>% 
  addPolygons(
    fillColor = ~pal(as.numeric(testt_simple$indx___)),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7
  ) %>% 
  addLegend(pal = pal, values = ~(as.numeric(testt_simple$indx___)), opacity = 0.7, title = "Population")


# Make sure the data is in a projected CRS that is suitable for your region (for accurate area calculations)
# For example, if your data is initially in WGS 84 (EPSG:4326), you might want to transform it to a suitable projected CRS
# Here is a hypothetical CRS transformation; you should choose an appropriate CRS for Yemen
yemen_admin3_projected <- st_transform(yemen_admin3, crs = 4326)  # Replace 32638 with an appropriate EPSG code

# Calculate area of each polygon
areas <- st_area(yemen_admin3_projected)

# Add areas to the data frame
yemen_admin3_projected$area <- as.numeric(areas)

# View the first few rows to check the 'area' column
head(yemen_admin3_projected)
sum(yemen_admin3_projected$area)


library(jsonlite)
library(httr)
library(jsonlite)
library(httr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(data.table)
library(openxlsx)
library(tidyverse)

callAPI <- function(relative_path, topics_list=FALSE){
  base_url <- "https://population.un.org/dataportalapi/api/v1"
  target <- paste0(base_url, relative_path)
  response <- fromJSON(target)
  # Checks if response was a flat file or a list (indicating pagination)
  # If response is a list, we may need to loop through the pages to get all of the data
  if (class(response)=="list"){
    # Create a dataframe from the first page of the response using the `data` attribute
    df <- response$data
    while (!is.null(response$nextPage)){
      response <- fromJSON(response$nextPage)
      df_temp <- response$data
      df <- rbind(df, df_temp)
    }
    return(df)}
  # Otherwise, we will simply load the data directly from the API into a dataframe
  else{
    if (topics_list==TRUE){
      df <- fromJSON(target, flatten = TRUE)
      return(df[[5]][[1]])
    }
    else{
      df <- fromJSON(target)        
      return(df)
    }
  }
}

# Declares the base url for calling the API
base_url <- "https://population.un.org/dataportalapi/api/v1"

# Creates the target URL, indicators, in this instance
target <- paste0(base_url, "/indicators/")

# Get the response, which includes data as well as information on pagination and number of records
response <- fromJSON(target)

# Get the first page of data
df_indicators <- response$data

# Loop until there are new pages with data
while (!is.null(response$nextPage)){
  
  #call the API for the next page
  response <- fromJSON(response$nextPage)
  
  #add the data of the new page to the data.frame with the data of the precious pages
  df_indicators <- rbind(df_indicators, response$data)
  
}



target <- paste0(base_url, "/locations/")

# Call the API
response <- fromJSON(target)

# Get the first page of data
df_countries <- response$data

# Get the other pages with data
while (!is.null(response$nextPage)){
  
  response <- fromJSON(response$nextPage)
  df_countries <- rbind(df_countries, response$data)
  
}

Yemen_countries<- df_countries[df_countries$name %in% c("Yemen"),]

 # ESA_countries<- df_countries[df_countries$name %in% c("Angola"
 #                                                      ,"Botswana"
 #                                                      ,"Burundi"
 #                                                      ,"Comoros"
 #                                                      ,"Eritrea"
 #                                                      ,"Eswatini"
 #                                                      ,"Ethiopia"
 #                                                      ,"Kenya"
 #                                                      ,"Lesotho"
 #                                                      ,"Madagascar"
 #                                                      ,"Malawi"
 #                                                      ,"Mozambique"
 #                                                      ,"Namibia"
 #                                                      ,"Rwanda"
 #                                                      ,"Somalia"
 #                                                      ,"South Africa"
 #                                                      ,"South Sudan"
 #                                                      ,"United Republic of Tanzania"
 #                                                      ,"Uganda"
 #                                                      ,"Zambia"
 #                                                      ,"Zimbabwe"
 #                                                      ,"Africa"
 #                                                      ,"Sub-Saharan Africa"
 #                                                      ,"World"),]
#identify countries
yemen_countries_id <- as.character(Yemen_countries[,"id"])
yemen_countries_id<- paste(yemen_countries_id, collapse = ",")

#south africa only
#RSA<-df_countries[df_countries$name %in% c("South Africa"),]
#RSA_id <- as.character(RSA[,"id"])


#identify indicators
#df_pop_topics <- df_indicators[df_indicators$topicShortName=="Pop", "id"]
df_pop_topics<- c('70')

df_pop_indicatorCodes<- as.character(df_pop_topics)
df_pop_indicatorCodes <- paste(df_pop_indicatorCodes, collapse = ",")

#choose indicators one by one and time lines -> total pop by sex indicator code=49

#Test ESAR populattion
target <- paste0("/data/indicators/",df_pop_topics,"/locations/",887,"?startYear=2022&endYear=2022&startAge=0&endAge=17&variants=4&sexes=3&format=json")
yemen_tot_pop <- callAPI(target)

# loop countries for downloading and filtering
yemen_population<- data.frame()
for (x in yemen_countries_id){
  target <- paste0("/data/indicators/",df_pop_topics,"/locations/",x,"?startYear=2010&endYear=2050&startAge=0&endAge=17&variants=4&sexes=3&format=json")
  temp<- callAPI(target)
  # temp<- temp %>% filter(ageId==46 | ageId==188)
  
  yemen_population<-rbind(yemen_population,temp)
  
}

esar_population %>% select(location,ageLabel,value,timeLabel)%>% rename(popsize=value) ->pop

pop<- pop %>% left_join(centroids, by = c("location" = "region"))

write.csv(yemen_population,"yemen_pop.csv",row.names = FALSE)




#ESAR populattion change
target <- paste0("/data/indicators/","51","/locations/",ESA_countries_id,"/start/2022/end/2022")
esaTot_pop_change <- callAPI(target)
esaTot_pop_change %>% filter(variantId==4) %>% select(locationId,value) %>% rename(popchange=value)->popchange

#ESAR populattion median
target <- paste0("/data/indicators/","67","/locations/",ESA_countries_id,"/start/2022/end/2022")
esaTot_pop_median <- callAPI(target)
esaTot_pop_median %>% filter(variantId==4) %>% select(locationId,value)%>% rename(medianage=value) ->popmedian


#ESAR populattion by age
target <- paste0("/data/indicators/","70","/locations/",ESA_countries_id,"/start/2022/end/2022")
esaTot_pop_age <- callAPI(target)
esaTot_pop_age %>% filter(variantId==4) %>% filter(ageId==46) %>% filter(sexId==3) %>% select(locationId,value) %>% rename(children=value) ->children

pop_list<-list(pop,popchange,popmedian,children)
pop_list %>% reduce(full_join, by='locationId') ->printpoptable
write.xlsx(printpoptable,paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\5. Data\\14. UNPD\\ESAR Country profile - ",Sys.Date(),".xlsx", sep=""))

write.xlsx(ESAR_pop,paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\5. Data\\14. UNPD\\ESAR population - ",Sys.Date(),".xlsx", sep=""))
write.xlsx(SA2_pop,paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\5. Data\\14. UNPD\\SA2050 population - ",Sys.Date(),".xlsx", sep=""))



