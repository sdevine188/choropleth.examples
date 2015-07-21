library(devtools)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
library(ggmap)

## using choroplethr function
## cannot change color
## region and value columns must be numeric
# see ?county_choropleth or ?state_choropleth
library(dplyr)
data(county.regions)
head(county.regions)
data(county.map)
head(county.map)

test <- read.csv("unemployment.csv")
test <- select(test, X.1, X.7)
names(test) <- c("region", "value")
county_choropleth(test, title = "2011 county unemployment rates", legend = "Unemployment rate")
county_choropleth(test, title = "2011 county unemployment rates", legend = "Unemployment rate", state_zoom = "texas")
county_choropleth(test, title = "2011 county unemployment rates", legend = "Unemployment rate", county_zoom = c(1001, 1003, 1005, 1007, 1009))


test2 <- read.csv("choropleth_test.csv")
# not complete
state_choropleth(test, title = "2011 county unemployment rates", legend = "Unemployment rate")


# choropleth with ggplot2 - this works!
# https://trinkerrstuff.wordpress.com/2013/07/05/ggplot2-chloropleth-of-supreme-court-decisions-an-tutorial/
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

states_map <- map_data("state")

ggplot(crimes, aes(map_id = state)) +
        geom_map(aes(fill = Murder), map = states_map) +
        expand_limits(x = states_map$long, y = states_map$lat)

county_map <- map_data("county")

ggplot(county_data, aes(map_id = group)) +
        geom_map(aes(fill = unemp), map = county_map) +
        expand_limits(x = county_map$long, y = county_map$lat)

# another attempt
# http://www.unomaha.edu/mahbubulmajumder/data-science/fall-2014/lectures/06-display-spatial-data/06-display-spatial-data.html#/6
library(stringr)
library(ggplot2)
library(scales)

data(unemp)
data(county.fips)
county_map <- map_data("county")
county_map$polyname <- str_c(county_map$region, county_map$subregion, sep = ",")
county_map <- merge(county_map, county.fips, by = "polyname")
county_map <- merge(county_map, unemp, by = "fips")

ggplot(county_data, aes(x = long, y = lat, group = group)) + 
        geom_polygon() + coord_equal() 


ggplot(choropleth, aes(long, lat, group = group)) +
        geom_polygon(aes(fill = rate_d), colour = alpha("white", 1/2), size = 0.2) + 
        geom_polygon(data = state_df, colour = "white", fill = NA) +
        scale_fill_brewer(palette = "PuRd")







# using ggplot2
# https://gist.github.com/hadley/233134library(ggplot2)
library(maps)

# First (and most annoying) task - get matching state and county variables 
# for both datasets.  And unfortauntely it's not quite right, as you can
# see from the finish product - some counties are missing.
library(RCurl)
unemp_data <- getURL("http://datasets.flowingdata.com/unemployment09.csv")
unemp <- read.csv(text = unemp_data)
names(unemp) <- c("id", "state_fips", "county_fips", "name", "year", 
                  "?", "?", "?", "rate")
unemp$county <- tolower(gsub(" County, [A-Z]{2}", "", unemp$name))
unemp$state <- gsub("^.*([A-Z]{2}).*$", "\\1", unemp$name)

county_df <- map_data("county")
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
county_df$state_name <- NULL

state_df <- map_data("state")

# Combine together 
choropleth <- merge(county_df, unemp, by = c("state", "county"))
choropleth <- choropleth[order(choropleth$order), ]
# Discretise rate to use with Brewer colour scheme - many options here
# choropleth$rate_d <- cut_number(choropleth$rate, 5)
# choropleth$rate_d <- cut_interval(choropleth$rate, 5)
# Nathan's choice is a little odd:
choropleth$rate_d <- cut(choropleth$rate, breaks = c(seq(0, 10, by = 2), 35))

# Once you have the data in the right format, recreating the plot is straight
# forward.

ggplot(choropleth, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = rate_d), colour = alpha("white", 1/2), size = 0.2) + 
        geom_polygon(data = state_df, colour = "white", fill = NA) +
        scale_fill_brewer(palette = "Blues")

# Takes a while to draw because ggplot2 not very efficient with large numbers
# of polygons :(

state_df2 <- filter(state_df, region == "alabama")

ggplot(filter(choropleth, state == "AL"), aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = rate_d), colour = alpha("white", 1/2), size = 0.2) + 
        geom_polygon(data = filter(state_df, region == "alabama"), colour = "white", fill = NA) +
        scale_fill_brewer(palette = "Blues") + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title=element_text(size=20,face="bold")) + 
        labs(x="", y="", title="Unemployment rate in Alabama")
        

# to see all colorbrewer options
library(RColorBrewer)
display.brewer.all()
# http://is-r.tumblr.com/post/34821021257/ggtutorial-day-5-gradient-colors-and-brewer





# county choropleth - this works
# http://bcb.dfci.harvard.edu/~aedin/courses/R/CDC/maps.html

data(unemp)
data(county.fips)

# Plot unemployment by country
library(mapproj)
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", 
           "#980043")
unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 
                                                    10, 100)))
colorsmatched <- unemp$colorBuckets[match(county.fips$fips, unemp$fips)]

map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0, 
    lty = 0)



# example of maps() package
map('county', fill = FALSE, col = palette())

# can click on map to identify region
identify(map("county", fill = TRUE, col = 0))

if(require(mapproj))
        identify(map("world", proj = "lagrange", fill = TRUE, col = 0))

# another attempt - this works! but is not as useful as ggmap, which can take addresses as input instead of lat/long
# http://pakillo.github.io/R-GIS-tutorial/#googlevis
library(googleVis)
library(dismo)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(RgoogleMaps)


# googleVis - plot interactive map to internet - can embed html in websites (or maybe shiny apps?)
library(googleVis)
data(Exports)    # a simple data frame
Geo <- gvisGeoMap(Exports, locationvar="Country", numvar="Profit", 
                  options=list(height=400, dataMode='regions'))
plot(Geo)

# print(Geo) will give html to embed interactive map in web page
print(Geo)

# another gvisGeoMap
G2 <- gvisGeoMap(CityPopularity, locationvar='City', numvar='Popularity',
                 options=list(region='US', height=350,
                              dataMode='markers',
                              colors='[0xFF8747, 0xFFB581, 0xc06000]'))

plot(G2)

# more googleVis plotting to web browser w interactive map
# this has all tutorials!! http://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html
require(datasets)
states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(states, locationvar = "state.name", colorvar = "Illiteracy",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)

# another gvisGeoChart for counties - not very good, seems to only do marker bubbles, not county-fill choropleth style
# http://dataillumination.blogspot.com/2013/03/visualizing-farmers-markets-geo-data_19.html
data <- read.csv("choropleth_test.csv")
head(data)
county_unemp <- gvisGeoChart(data, locationvar = "county", colorvar = "labor_force", 
                             options = list(region = "US", displayMode = "markers", 
                                            resolution = "metros", 
                                            width = 600, height = 400, colorAxis = "{colors:['lightgreen', 'green']}", 
                                            backgroundColor = "yellowgreen"))
plot(county_unemp)

library(plyr)
farmers <- read.csv("farmers.csv")
head(farmers)
vegbycounty <- ddply(farmers, .(County), summarize, NumNo = table(Vegetables)[2], 
                     PercentVeg = table(Vegetables)[3])
head(vegbycounty)
# another googleVis plotting to scrollable google maps online, interactive
AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip", 
                     options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))
plot(AndrewMap)
print(AndrewMap)


# plotting interactive map to internet using google earth (no need to install)
data(Andrew)
M1 <- gvisMap(Andrew, "LatLong", "Tip", 
              options=list(showTip=TRUE, showLine=F, enableScrollWheel=TRUE, 
                           mapType='satellite', useMapTypeControl=TRUE, width=800,height=400))
plot(M1)


# try to read in kml file from census - it works
library(rgdal)
library(plotKML)
library(raster)
library(sp)
# first argument to readOGR is file name, layer argument is the "name" inside the code, to find: view kml w text editor and search for <name> name here <name>
newmap <- readOGR("cb_2014_us_county_500k.kml", layer = "cb_2014_us_county_500k")
plot(newmap)
head(newmap)


# choropleth using census2010
# http://rpackages.ianhowson.com/cran/UScensus2010/man/choropleth.html
## Not run: 
data(oregon.county10)

###Using plot
choropleth(oregon.county10,"P0010001",color = list(fun = "rainbow", attr = list(4)),main="2010 Counties \n Oregon",type="plot",border="transparent")



###Using spplot
choropleth(oregon.county10,"P0010001",main="2010 Counties \n Oregon",border="transparent",type="spplot")
data(countyfips)
countyfips[countyfips$statename=="oregon",]





