## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )

## ----setup--------------------------------------------------------------------
library(FjordLight)
library(raster)

## ----eval=FALSE---------------------------------------------------------------
#  fl_ListFjords()

## ----eval=FALSE---------------------------------------------------------------
#  # Choose a fjord from the list above
#  fjord <- "kong"
#  
#  # Note: this will require that a folder named 'data' exists in your current working directory
#  # One may see one's working directory written in small text next to the version of R in the console pane
#  # If the file has already been downloaded a message will be shown
#  fl_DownloadFjord(fjord, dirdata = "data")

## -----------------------------------------------------------------------------
# Chose to load all of the monthly bottom PAR values or not
WANT_TIME_SERIES <- TRUE

# Load the data
fjord <- "test"
fjorddata <- fl_LoadFjord(fjord, dirdata = system.file("extdata", package = "FjordLight"), TS = WANT_TIME_SERIES)
str(fjorddata, list.len = 15)

## -----------------------------------------------------------------------------
flget_geoparameters(fjorddata)

## ----fig.width = 6, fig.height=6----------------------------------------------
# All depths (what = "o" ; o for Ocean), as raster
all_bathy <- flget_bathymetry(fjorddata, what = "o", mode = "raster", PLOT = TRUE)

# Coastal zone [0-200m] (what = "c" ; c for Coastal), as raster
coastal_bathy <- flget_bathymetry(fjorddata, what = "c", mode = "raster", PLOT = TRUE)

# Shallow zone [0-50m] (what = "sl" ; s for Shallow, l to add land), as raster
shallow_land <- flget_bathymetry(fjorddata, what = "sl", mode = "raster", PLOT = TRUE)

# Just land; note the difference in colour palette
just_land <- flget_bathymetry(fjorddata, what = "l", mode = "raster", PLOT = TRUE)

# As a data.frame (mode = "df" : longitude, latitude, depth)
sea <- flget_bathymetry(fjorddata, what = "s", mode = "df", PLOT = FALSE)
cz <- flget_bathymetry(fjorddata, what = "c", mode = "df", PLOT = FALSE)

# you may add letter "l" if you want land elevation
sealand <- flget_bathymetry(fjorddata, what = "sl", mode = "df")

# Example of structure
str(sealand)

## ----fig.width = 6, fig.height=6----------------------------------------------
# PAR0m and PARbottom for July
P07 <- flget_climatology(fjorddata, optics = "PAR0m", period = "Clim", month = 7, PLOT = TRUE)
print(P07)
Pk7 <- flget_climatology(fjorddata, optics = "Kpar", period = "Clim", month = 7, PLOT = TRUE)
print(Pk7)
Pb7 <- flget_climatology(fjorddata, optics = "PARbottom", period = "Clim", month = 7, PLOT = TRUE)
print(Pb7)

# PARbottom Global
PbG <- flget_climatology(fjorddata, optics = "PARbottom", period = "Global", PLOT = TRUE)
print(PbG)

# PAR0m, Kpar, and PARbottom for year 2012 as 3 columns data.frame
P02012 <- flget_climatology(fjorddata, optics = "PAR0m", period = "Yearly", year = 2012, mode = "df")
k2012 <- flget_climatology(fjorddata, optics = "Kpar", period = "Yearly", year = 2012, mode = "df")
Pb2012 <- flget_climatology(fjorddata, optics = "PARbottom", period = "Yearly", year = 2012, mode = "df")
head(Pb2012)

## -----------------------------------------------------------------------------
# first get pixels area
area <- flget_area(fjorddata, mode = "df")

# Then bind the data frames and remove rows with missing values
PAR_area <- cbind(sea, area[3], P02012[3], k2012[3], Pb2012[3])
PAR_area <- PAR_area[complete.cases(PAR_area),]
head(PAR_area)

## ----fig.width = 6, fig.height=6----------------------------------------------
# Years 2003 to 2004 - months July to August
mts <- flget_PARbottomMonthlyTS(fjorddata, month = 7:8, year = 2003:2004, PLOT = TRUE)
print(mts)

# Or as a data.frame
mts_2003 <- flget_PARbottomMonthlyTS(fjorddata, year = 2003, PLOT = FALSE, mode = "df")
head(mts_2003)

## ----eval=FALSE---------------------------------------------------------------
#  # All months - all years - as data.frame:
#    # columns = months (8, March to October) * years (20, 2003 to 2022) + 2 (lon lat) = 162
#  # NB: This may be too large for some laptops, proceed with caution
#  mts_full <- flget_PARbottomMonthlyTS(fjorddata, mode = "df", PLOT = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  # Choose a fjord from the possible options
#  fjord <- "kong"
#  
#  # Note: this will require that a folder named 'data' exists in your current working directory
#  # One may see one's working directory written in small text next to the version of R in the console pane
#  # If the file has already been downloaded a message will be shown
#  fl_DownloadFjord(fjord, layer = "K_PAR" dirdata = "data")

## -----------------------------------------------------------------------------
# Chose to load all of the monthly bottom PAR values or not
WANT_TIME_SERIES <- TRUE

# Load the data
fjord <- "test"
fjorddata_KPAR <- fl_LoadFjord(fjord, layer = "K_PAR", dirdata = system.file("extdata", package = "FjordLight"), TS = WANT_TIME_SERIES)
str(fjorddata_KPAR, list.len = 15)

## -----------------------------------------------------------------------------
# Years 2003 to 2004 - months July to August
mts_KPAR <- flget_KPARMonthlyTS(fjorddata_KPAR, month = 7:8, year = 2003:2004, PLOT = TRUE)
print(mts)

# Or as a data.frame
mts_KPAR_2003 <- flget_KPARMonthlyTS(fjorddata_KPAR, year = 2003, PLOT = FALSE, mode = "df")
head(mts_2003)

## ----eval=FALSE---------------------------------------------------------------
#  # Choose a fjord from the possible options
#  fjord <- "kong"
#  
#  # Note: this will require that a folder named 'data' exists in your current working directory
#  # One may see one's working directory written in small text next to the version of R in the console pane
#  # If the file has already been downloaded a message will be shown
#  fl_DownloadFjord(fjord, layer = "ClimSD", dirdata = "data")
#  fl_DownloadFjord(fjord, layer = "YearlySD", dirdata = "data")

## -----------------------------------------------------------------------------
# Monthly climatology SD
fjorddata_ClimSD <- fl_LoadFjord(fjord, layer = "ClimSD", dirdata = system.file("extdata", package = "FjordLight"))
str(fjorddata_ClimSD, list.len = 15)
fjorddata_YearlySD <- fl_LoadFjord(fjord, layer = "YearlySD", dirdata = system.file("extdata", package = "FjordLight"))
str(fjorddata_YearlySD, list.len = 15)

## -----------------------------------------------------------------------------
# Determine coordinates
lon <- fjorddata$longitude
lat <- fjorddata$latitude
str(lon); str(lat)

# Select a month
month <- 8 # August
im <- which(fjorddata$Months == month)
print(im)

# PAR0m Standard Deviation
PAR0mSD <- raster::raster(list(x = lon, y = lat, z = fjorddata_ClimSD$ClimPAR0mSD[, , im]))
plot(PAR0mSD, main = paste("PAR0m StDev", month.abb[month]))

# kdpar Standard Deviation
KparSD <- raster::raster(list(x = lon, y = lat, z = fjorddata_ClimSD$ClimKparSD[, , im]))
plot(KparSD, main = paste("Kpar StDev", month.abb[month]))

# PARbottom Standard Deviation
PARbottomSD <- raster::raster(list(x = lon, y = lat, z = fjorddata_ClimSD$ClimPARbottomSD[, , im]))
plot(PARbottomSD, main = paste("PARbottom StDev", month.abb[month]))

# PARbottom
PAR0m <- flget_climatology(fjorddata, optics = "PAR0m", period = "Clim", month = month, PLOT = TRUE)

VarCoef <- PAR0mSD / PAR0m
plot(VarCoef, main = "PAR0m Coefficient of Variation")

## -----------------------------------------------------------------------------
# Determine coordinates
lon <- fjorddata$longitude
lat <- fjorddata$latitude
str(lon); str(lat)

# Choose a year
year <- 2007
iy <- which(fjorddata$Years == year)
print(iy)

# PAR0m Standard Deviation
PAR0mSD <- raster::raster(list(x = lon, y = lat, z = fjorddata_YearlySD$YearlyPAR0mSD[, , iy]))
plot(PAR0mSD, main = paste("PAR0m StDev", year))

# kdpar Standard Deviation
KparSD <- raster::raster(list(x = lon, y = lat, z = fjorddata_YearlySD$YearlyKparSD[, , iy]))
plot(KparSD, main = paste("Kpar StDev", year))

# PARbottom Standard Deviation
PARbottomSD <- raster::raster(list(x = lon, y = lat, z = fjorddata_YearlySD$YearlyPARbottomSD[, , iy]))
plot(PARbottomSD, main = paste("PARbottom StDev", year))

# PARbottom
PAR0m <- flget_climatology(fjorddata, optics = "PAR0m", period = "Yearly", year = year, PLOT = TRUE)

VarCoef <- PAR0mSD / PAR0m
plot(VarCoef, main = "PAR0m Coefficient of Variation")

## ----fig.width = 6, fig.height=6----------------------------------------------
# One may create their own functions
fG <- flget_Pfunction(fjorddata, type = "coastal", period = "Global", plot = FALSE)
# Then you can use it; for instance :
irradiance_levels <- c(0.1, 1, 10)
fG(irradiance_levels)

# Or load the pre-calculated values as a 2 column data.frame
f2012 <- flget_Pfunction(fjorddata, type = "coastal", period = "Yearly", year = 2012, mode = "df")
str(f2012)

# Plot P-functions
fGlob <- flget_Pfunction(fjorddata, type = "coastal", period = "Global", PLOT = TRUE, lty = 1, col = 1, lwd = 2, 
                         Main = paste(fjord, "coastal P-functions"), ylim = c(0, 50))

