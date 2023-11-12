## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )

## ----setup--------------------------------------------------------------------
library(FjordLight)

## -----------------------------------------------------------------------------
fl_ListFjords()

## -----------------------------------------------------------------------------
# Choose a fjord from the list above
fjord <- "kong"

# If the file has already been downloaded a message will be shown
# NB: For this example we use 'dirdata = tempdir()',
# but this will not save the data on our computer.
fl_DownloadFjord(fjord, dirdata = tempdir())
WANT_TIME_SERIES <- TRUE
fjorddata <- fl_LoadFjord(fjord, dirdata = tempdir(), TS = WANT_TIME_SERIES)
str(fjorddata, list.len = 15)

## ----eval=FALSE---------------------------------------------------------------
#  # Note: this will require that a folder named 'data' exists in your current workig directory
#  # One may see one's working directory written in small text next to the version of R in the console pane
#  fl_DownloadFjord(fjord, dirdata = "data")

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

