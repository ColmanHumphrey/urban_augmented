##------------------------------------
## this file pulls in raw street data and creates intersections

source('../load_packages.R')

data_folder = '../../data/'

load(file = paste0(data_folder, 'prior_data/block_and_group.rdata'))

##------------------------------------##
## this script is messy, because the data are. You'll potentially have to
## adjust the fixes here for your own beautifully messy data.
## No steps should take too long however.

property_file = paste0(data_folder, 'raw/property_data/Properties.csv')
prop_data <- read_csv(file = property_file)

## remove spaces in names:
names(prop_data) = gsub(' ', '_', names(prop_data))

## we don't need all the columns, so we'll reduce the data_frame now
## we won't use all this either, but that's OK
prop_data %<>% select(
                   Location, Census_Tract, Zip_Code, Geographic_Ward,
                   Street_Code, Street_Name, Street_Direction, Street_Designation,
                   House_Number, Unit, Sale_Date, Sale_Price, Unfinished,
                   Market_Value_Date, Market_Value, Category_Code_Description,
                   Building_Code_Description, Zoning, Frontage, Depth, Shape, Total_Area,
                   Topography, Garage_Type, Garage_Spaces, Off_Street_Open,
                   View, Number_Stories, Exterior_Condition, Quality_Grade,
                   Year_Built, Year_Built_Estimate, Number_of_Rooms,
                   Number_of_Bedrooms, Number_of_Bathrooms, Basements,
                   Fireplaces, Central_Air, Interior_Condition, Total_Livable_Area,
                   Coordinates)

##------------------------------------ converting and fixing:
## convert dates
date_strs <- substr(prop_data$Sale_Date, 1, 10)

## fix small error:
date_strs <- gsub('0002', '2002', date_strs)

prop_data$Sale_Date <- as.Date(date_strs, format = "%m/%d/%Y")

## and this date:
date_strs <- substr(prop_data$Market_Value_Date, 1, 10)
date_strs <- gsub('0002', '2002', date_strs)

## note: most of these are the 31 of Dec, 2012 etc
## something to consider...
prop_data$Market_Value_Date <- as.Date(date_strs, format = "%m/%d/%Y")

## convert prices (drop '$')
prop_data$Sale_Price <- as.numeric(substr(prop_data$Sale_Price, 2, 200))
prop_data$Market_Value <- as.numeric(substr(prop_data$Market_Value, 2, 200))

## this is messy. try ignoring all huge and expensive things.
ignore_ind <- pmax(prop_data$Total_Area, prop_data$Total_Livable_Area) > 100000 |
    pmax(prop_data$Sale_Price, prop_data$Market_Value) > 5000000

prop_data <- prop_data[!ignore_ind,]

## now need to convert coords
prop_coords <- prop_data$Coordinates

prop_coords_split <- strsplit(prop_coords, split = ', ')
prop_coords_lon_pre <- unlist(lapply(prop_coords_split, function(x){x[2]}))
prop_coords_lat_pre <- unlist(lapply(prop_coords_split, function(x){x[1]}))

lon_char <- nchar(prop_coords_lon_pre)
prop_coords_lon <- as.numeric(substr(prop_coords_lon_pre, 1, lon_char-1))

prop_coords_lat <- as.numeric(substr(prop_coords_lat_pre, 2, 50))

prop_data$lon <- prop_coords_lon
prop_data$lat <- prop_coords_lat

## gotta drop NA values right away
## this drops a lot
keep_ind <- !is.na(prop_data$lon) & !is.na(prop_data$lat)
prop_data <- prop_data[keep_ind,]

##------------------------------------ getting spatial, checking if in philly

prop_sf <- st_as_sf(prop_data %>% select(lon, lat),
                    coords = c('lon', 'lat'),
                    crs = st_crs(block_geom))

prop_in_philly <- c(st_contains(County_geom, prop_sf, sparse = FALSE))

## so cut out these... 6 points
prop_data <- prop_data[prop_in_philly,]
prop_sf <- prop_sf[prop_in_philly,]

## add id:
prop_data$id = 1:nrow(prop_data)
prop_sf$id = 1:nrow(prop_sf)

save(prop_data,
     prop_sf,
     file = paste0(data_folder, 'prop_data.rdata'))

##------------------------------------
