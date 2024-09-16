### SCRIPT FOR CREATING IRRIGATION WATER ALLOCATION (WRO) TABLES AND FOR ANALYZE OUTPUTS ###
library(sf)
library(tidyverse)
library(patchwork)

#### Creating source-demands tables for Water allocation file #### 


# Input data --> Shapefile of the hrus and channels

hrus_sf <- read_sf("Data/GIS/model/hrus2.shp") # HRUs shapefile

subbasins <- read_sf("Data/GIS/model/subs1.shp") # Subbasins shapefile (not really needed)

chandeg <- read_table("Data/GIS/model/chandeg.con", skip = 1) %>% 
  select(id, gis_id) # File with the gis_id and id of the channels (created from chandeg file)

chandeg_shp <- read_sf("Data/GIS/model/channels_model_chandeg.shp") # Shapefile with the gis_id and id of the channels (created from chandeg file)


# Filtering irrigated HRUs

agrr_hrus <- hrus_sf %>% filter(., Landuse == "agrr") %>% # AGRR are irrigated crops in this model
  select(Subbasin, Channel, Landuse, HRUS) %>% 
  mutate(HRUS = as.numeric(HRUS)) %>% 
  rename("gis_id" = "Channel") %>% 
  left_join(., chandeg, "gis_id") %>% 
  rename("channel_id" = "id") %>% 
  select( Subbasin, Landuse, HRUS, channel_id)


# Creating a table with HRUs and channels related (demand and source objects)

irr_wateralloc_input <- agrr_hrus %>% 
  st_drop_geometry() %>%
  separate_rows(., HRUS, sep = ",") %>% # In this case there were not more than 1 hru per row, but just in case
  mutate(HRUS = as.numeric(HRUS)) %>% 
  arrange(., channel_id)

# Filtering the sources from the channels shapefile and saving a shapefile

chandeg_shp <- chandeg_shp %>% filter(chandeg_da %in% unique(irr_wateralloc_input$channel_id)) %>% select(chandeg_da, geometry)

#sf::st_write(obj = chandeg_shp, dsn = "Data/Management_data/Irrigation/irrigation_shps/sources_channels_edited.shp", driver="ESRI Shapefile")


# Visualizing source and demand objects

channels_used <- read_sf("Data/Management_data/Irrigation/irrigation_shps/sources_channels_edited.shp")

#ggplotly(
ggplot() + geom_sf(data = subbasins, fill = "grey60", color = "darkred")+
  geom_sf(data = agrr_hrus, fill = "green", color = "darkgreen")+
  geom_sf(data = channels_used, color = "blue", linewidth = 1)+
  theme_bw()+
  theme(text = element_text(size = 15))
#)


# With the object irr_wateralloc_input it would be possible to create the water allocation tables, since the sources and demand objects are already defined.
# However, these object was adjuted using QGIS, Reasons: A) Reservoirs were introduced as polygons, which eliminated some of the channels. The closest channel to the eliminated
# ones were defined as sources for the HRUs of the eliminated channel- B) Some of the sources (channels) were too small and did not have enougth water, so closest bigger channels were used.

# HRUs remains the same, but less channels are being used.  

#write.csv(irr_wateralloc_input, 
file = "Data/Management_data/Water_allocation_TAGUS/wro_sources-demands.csv",
quote = F, row.names = F)





##### OPTION A: Creating one water allocation table for each source  #####

# Input data (adapated in QGIS)

irr_wateralloc_input <- read.csv("Data/Management_data/Irrigation/wro_sources-demands.csv")

sources <- unique(irr_wateralloc_input$channel_id) # Sources identifiers
tables_number <- length(sources) # How many sources are? To indicate how many tables will be used


# Creating the wro table, indicating the number of wro tables that will contain. This is the final table
wro_file <- wro_file <- file("Data/Management_data/Irrigation/wro_table_const/water_allocation.wro", "w")
cat(file = wro_file, paste("Created with R on", Sys.time(), "using ASG script"), fill = T)
cat(file = wro_file, tables_number, fill = T)
close(wro_file)


# Loop for creating one table for each source
for(i in 1:length(sources)){
  
  # DATA OF EACH TABLE
  # General data of each wro table
  NAME <-  paste("channel_", sources[i], "_irrig", sep = "") # one name for each
  RULE_TYP <- "high_right_first_serve" # Rule type
  SRC_OBS <- 1 # Number of sources in the table (1 in this case)
  DMD_OBS <- irr_wateralloc_input %>% filter(channel_id == sources[i]) %>% .[["HRUS"]] %>% length(.) # Number of demand objects
  CHA_DB <- "y" # There are channels as sources?
  
  # Fist line of the table and header
  header_table <- "     NAME               RULE_TYP    SRC_OBS    DMD_OBS CHA_DB"
  first_table_line <- paste(NAME, RULE_TYP, SRC_OBS, DMD_OBS, CHA_DB, sep = "    ")
  
  # Filtering the input data with each source
  src_data <- irr_wateralloc_input %>% filter(channel_id == sources[i])
  
  # SOURCE OBJECTS TABLE
  header_sources <- "    SRC_NUM OB_TYP     OB_NUM JAN_MIN FEB_MIN MAR_MIN APR_MIN MAY_MIN JUN_MIN JUL_MIN AUG_MIN SEP_MIN OCT_MIN NOV_MIN DEC_MIN                DESCRIPTION"
  
  SRC_NUM <- 1 # Ordinal number for sources (re-start in each table, so in this case is 1)
  OB_TYP <- "cha"
  OB_NUM <- unique(src_data$channel_id) # channel is (no GIS id, but channel id)
  month_min <- 0.01 # Minimum flow/level in the source. m3s for channels
  
  dmnd_dat <- src_data$HRUS  # Hrus irrigated with the filtered channel
  
  source_line <- paste(SRC_NUM, OB_TYP, OB_NUM, paste(rep(month_min, 12), collapse = "     "), sep = "    ")
  
  
  # DEMAND OBJECTS TABLE
  header_demands <- "  NUMB OB_TYP     OB_NUM  WITHDR       AMOUNT  W_RT TR_TYP TREAT RCV_OB    RCV_NUM RCV_DTL  SRCS      SCRC1      FRAC1 COMP1      SCRC2      FRAC2 COMP2      SCRC3      FRAC3 COMP3      SCRC4      FRAC4 COMP4      SCRC5      FRAC5 COMP5             DESCRIPTION"
  
  irr_tab <- "irr_crt"
  irr_amount <- 3.0 # mm per application
  water_rigths <- "sr"
  number_of_sources <- length(unique(src_data$channel_id))
  sources_number <- SRC_NUM
  
  demands_table <- tibble(NUMB = seq(1, length(dmnd_dat), 1),
                          OB_TYP = "hru",
                          OB_NUM = dmnd_dat, 
                          WITHDR = irr_tab,
                          AMOUNT = irr_amount, 
                          W_RT = water_rigths, 
                          TR_TYP = "null", 
                          TREAT = "null", 
                          RCV_OB = "null", 
                          RCV_NUM = 0,
                          RCV_DTL = "null",
                          SRCS = number_of_sources,
                          SRCR1 = sources_number,
                          FRAC1 = 1, 
                          COMP1 = "n")
  
  
  # Creating a temporal file for each table
  
  tmp_file <- file("Data/Management_data/Irrigation/wro_table_const/wro_temp.txt", "w")
  
  cat(header_table, file = tmp_file, fill = T)
  cat(first_table_line, file = tmp_file, fill = T)
  cat(header_sources, file = tmp_file, fill = T)
  cat(source_line, file = tmp_file, fill = T)
  cat(header_demands, file = tmp_file, fill = T)
  
  
  write.table(demands_table, tmp_file, sep = "\t", row.names = FALSE, quote = F, col.names = F)
  cat("", file = tmp_file, fill = T)
  close(tmp_file)
  
  # Copying the content to the entire WRO file
  tmp_file_cont <- readLines("Data/Management_data/Irrigation/wro_table_const/wro_temp.txt")
  wro_file <- file("Data/Management_data/Irrigation/wro_table_const/water_allocation.wro", "a")
  writeLines(tmp_file_cont, wro_file)
  close(wro_file)
  
  # Delete content of temporal file
  tmp_file <- file("Data/Management_data/Irrigation/wro_table_const/wro_temp.txt", "w")
  cat("", file = tmp_file)
  close(tmp_file)
  
}



##### OPTION B: Creating a water allocation table with all the sources 
# SHOULD NOT BE USED FOR CHANNEL OBJECTS AS SOURCES!!!

# Listing sources and including WRO files columns
wro_sources <- sort(unique(irr_wateralloc_input$channel_id)) %>% 
  tibble(channel_id = ., source_id = c(1:length(unique(.)))) %>% # Number of the source in the water allocation file
  mutate(OB_TYP = "cha", # Type of object
         OB_NUM = channel_id, # Object ID of the source (channel id)
         m1 = 0.01,  # minimum flow in the channel that month (m³/s)  
         m2 = 0.01,  # minimum flow in the channel that month (m³/s) 
         m3 = 0.01,  # minimum flow in the channel that month (m³/s) 
         m4 = 0.01,  # minimum flow in the channel that month (m³/s) 
         m5 = 0.01,  # minimum flow in the channel that month (m³/s) 
         m6 = 0.01,  # minimum flow in the channel that month (m³/s) 
         m7 = 0.01,  # minimum flow in the channel that month (m³/s) 
         m8 = 0.01,  # minimum flow in the channel that month (m³/s) 
         m9 = 0.01,  # minimum flow in the channel that month (m³/s) 
         m10 = 0.01, # minimum flow in the channel that month (m³/s)  
         m11 = 0.01, # minimum flow in the channel that month (m³/s)  
         m12 = 0.01) %>% select(., -channel_id)

sources_lookup <- wro_sources[,c(1,3)]


# Listing demand objects and including WRO file columns

wro_demands <- irr_wateralloc_input %>% select(HRUS, channel_id) %>% 
  left_join(., sources_lookup, c("channel_id" = "OB_NUM")) %>% 
  arrange(., HRUS) %>% 
  cbind(OB_NUM_demand = c(1:length(.$HRUS))) %>% # Number of the demand in the water allocation file
  mutate(OB_TYPE = "hru",     # Type of demand object              
         OB_NUMBER = HRUS,    # Number of the demand object (HRU id)       
         WITHDR = "irr_crt", # Irrigation schedule applied          
         AMOUNT = 5,          # Amount of water (mm/d) to apply   
         W_RT = "sr",         # Priority of that action (sr > jr)   
         TR_TYP = "null",     # Related to WWTTP, not used       
         TREAT = "null",      # Related to WWTTP, not used      
         RCV_OB = "null",     # Related to WWTTP, not used       
         RCV_NUM = 0,         # Related to WWTTP, not used   
         RCV_DTL = "null",    # Related to WWTTP, not used        
         SRCS = 1,            # Number of sources that supply 
         SRC1 = source_id,    # OB_NUM of the source in the WRO file    
         FRAC1 = 1.0,         # Fraction of the demand supplied from this source   
         COMP1 = "n") %>% 
  select(OB_NUM_demand, OB_TYPE, OB_NUMBER, WITHDR, AMOUNT,
         W_RT, TR_TYP, TREAT, RCV_OB, RCV_NUM, RCV_DTL, SRCS, SRC1, FRAC1, COMP1)


#  Exporting tables to import them manually to the Water allocation format template

# write.csv(wro_sources, 
file = "Data/Management_data/Water_allocation_TAGUS/wro_sources_format.csv",
quote = F, row.names = F)

#write.csv(wro_demands, 
file = "Data/Management_data/Water_allocation_TAGUS/wro_demands_format.csv",
quote = F, row.names = F)
