library(tidyverse)
library(sf)

#this file will have all the absolutely old correct variables and matches xanthes original numbers
df <- read_csv("/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_Data_051019_DOB_SMP.csv") 
df<- df %>% filter(treatment != 'Control')

#this file has the updates CMD vaues
df2 <- read_csv('/mnt/data1/boreal/spotter/combustion/intro_files/text_files/Combustion_SynthesisData_07032019_XJW_CMDUpdate.csv')
df2 <- df2 %>% filter(treatment != 'Control')

#for df2 select only the id, above.carbon.combusted and below.ground.carbon.combusted.values
df2 <- df2 %>% select(id, above.carbon.combusted, below.ground.carbon.combusted)

#remove the C values from df
df <- df %>% dplyr::select(-above.carbon.combusted, -below.ground.carbon.combusted)

#replace the new values into the original df
df <- left_join(df, df2, by = 'id')

#select variables of interest

#remove and dplyr::rename some columns
df <- df %>% dplyr::select (-longitude.y, -slope.x, -aspect.x, -elevation.x) %>% dplyr::rename(slope = slope.y) %>%
  dplyr::rename(longitude = longitude.x) %>% dplyr::rename(aspect = aspect.y) %>% dplyr::rename(elevation = elevation.y)

df <- df %>% dplyr::rename(DMC = DMC.y) %>% dplyr::rename(BUI = BUI.y) %>% dplyr::rename(ISI = ISI.y) %>%
  dplyr::rename(DC = DC.y) %>% dplyr::rename(FFMC = FFMC.y) %>% dplyr::rename(FWI = FWI.y)



#master column list which are all the spatial variables I want to use which Brendan or Sander aquired
#see Readme_BR.docx, attached in git hub
master_cols <- c('id','above.carbon.combusted', 'below.ground.carbon.combusted', 'burn_year', 
                 'project.name', 'latitude', 'longitude',
                 'dNBR', 'Tree.cover', 'elevation', 'slope', 'aspect', 'TWI', 
                 'PFI', 'Ruggedness', 'pH_30', 'GRSH', 'DEC', 'BS',
                 'Sand_30', 'Silt_30', 'Clay_30', 'SOC_30', 'NV', 'WS', 'JP',
                 'OCON', 'DOB_lst', 'CNA_Tave_5_8', 'CNA_Tmax_5_8', 'CNA_Tmin_5_8', 'CNA_PPT_5_8',
                 'CNA_Rad_5_8', 'CNA_RH_5_8', 'CNA_CMD_5_8', 'CNA_MAT', 'CNA_MAP', 'CNA_MWMT', 
                 'CNA_TD', 'CNA_DD5', 'CNA_DD18', 'CNA_NFFD', 'CNA_FFP', 'CNA_PAS', 'CNA_CMD', 'CNA_MAR',
                 'CNA_RH', 'CNA_SHM', 'VPD') 

df <- df %>% select(master_cols)

#create the outpath
out <- "/mnt/data1/boreal/spotter/combustion/final_files/raw/"
dir.create(out, recursive = T)

#save the csv to the outpath
write_csv(df, file.path(out, 'for_extraction.csv'))

#now also create a shapefile of the same data

df <- df %>% dplyr::select('id', 'latitude', 'longitude', 'burn_year', 'DOB_lst')
df <-  st_as_sf(df, coords = c("longitude", "latitude"),
                crs = 4326, agr = "constant")

write_sf(df, file.path(out, 'for_extraction.shp'))






