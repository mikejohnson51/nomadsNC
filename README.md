
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nomadsNC

The goal of nomadsNC is to download the most current forecast from the
NWC National Water Model, and pivot the data for timeseries access. You
must have [NCO](http://nco.sourceforge.net/) install for this package to
work.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("mikejohnson51/nomadsNC")
```

## Intro

These are some basic examples which show how to solve some common NWM
forecast problems.

Lets download the most current short range files (18 hours out so 18
files) and consolidate them into a single timeseries optimized NetCDF
file:

``` r
library(nomadsNC)
system.time({
  create_nomads_nc(type = "short_range", dstfile = "data/example.nc")
})

# user     system elapsed 
# 22.425  11.667  72.216 
```

Lets look at the new streamflow 2D variable using RNetCDF:

``` r
library(RNetCDF)
nc   <-  open.nc("data/example.nc")
allQ <-  var.get.nc(nc, "streamflow")
dim(allQ)
#> [1] 2729077      18
```

Great! We have the \~2.7 million reaches on the X dimension and the 18
hour time slices on the Y. Say instead you wanted an 18-hour timeseries
for the 102991 COMID in the file (why that ID? Who knows, more on this
below).

``` r
# Ask for data staring on the feature_id index 102991 and time index 1.
# Read across the time index 1, and across all time indeces (NA)
Q <-  var.get.nc(nc, "streamflow", start = c(102991,1), count =c(1, NA), unpack = TRUE)

plot(Q, main = "NHD Catchement with Index 102991", 
     xlab = "Hours from now...", ylab = "Streamflow (cms)")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

# Specific ID requests

Lets take it one step further, lets say you know your location
(e.g. Colorado Springs), and what to find the streamflow record related
to your catchment. To do this we can use
[dataRetrieval](https://usgs-r.github.io/dataRetrieval/):

``` r
library(dataRetrieval)

# use dataRetrieval::findNLDI to identify your catchment COMID
comid <- dataRetrieval::findNLDI(location = c(-104.8253, 38.83396))$origin$comid

# Find that COMID position in the complete NetCDF file
comid_index <- match(comid, var.get.nc(nc, "feature_id", unpack = TRUE))

# Use that position to extract streamflow records
Q <- var.get.nc(nc, "streamflow", start = c(comid_index,1), count =c(1, NA), unpack = TRUE)

#plot
plot(Q, main = "NHD Catchement at c(-104.8253, 38.83396)", 
     xlab = "Hours from now...", ylab = "Streamflow (cms)")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

# Area requests

Lets say you wanted a map of the maximum streamflow over the current
forcast for the HUC8 experiencing the single largest flow record in this
forecast… wow this is a little trickier but totally doable using
[nhdplusTools](https://usgs-r.github.io/nhdplusTools/)

``` r
library(nhdplusTools)

row_id <- arrayInd(which.max(allQ), dim(allQ))[1]
max_flow_comid <- var.get.nc(nc, "feature_id", unpack = TRUE)[row_id]

feature_of_interest <- nhdplusTools::get_vaa("reachcode") %>% 
  filter(comid == max_flow_comid) %>% 
  mutate(huc8 = substr(reachcode, 1, 8))

huc8 <-  nhdplusTools::get_huc8(id = feature_of_interest$huc8)
nhd  <-  nhdplusTools::get_nhdplus(AOI = huc8)

comid_index <- match(nhd$comid, var.get.nc(nc, "feature_id", unpack = TRUE))

records <- do.call(rbind, lapply(1:length(comid_index), FUN = function(x){ 
  var.get.nc(nc, "streamflow", start = c(comid_index[x],1), count =c(1, NA), unpack = TRUE) 
}))

nhd$maxFlow  <- apply(records, 1, max)

ggplot() + 
  geom_sf(data = huc8) + 
  geom_sf(data = nhd, aes(color = maxFlow)) + 
  scale_color_viridis_c(direction = -1, trans = "log", option = "D") + 
  theme_void() + 
  theme(legend.position = "bottom") + 
  labs(title = "Max flow over the next 18 hours: NWM Short-range forecast",
          subtitle = paste0("HUC08-", huc8$huc8))
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
