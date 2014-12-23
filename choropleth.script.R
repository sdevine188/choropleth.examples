library(devtools)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)

## using choroplethr function
## cannot change color
## region and value columns must be numeric

data(county.regions)

## head(county.regions)
## merge county.regions to dataset of interest by region, which is fips.state.county with no leading zero for state
df$value <- as.numeric(as.character(df$value))
df$region <- as.numeric(as.character(df$region))

county_choropleth(df, title = "2011 county unemployment rates", buckets = 1, legend = "Unemployment rate",
                  zoom = c("california", "nevada", "oregon", "washington", "idaho", "arizona"))

county_choropleth(df, title = "2011 county unemployment rates", buckets = 1, legend = "Unemployment rate")

## choropleth using ggplot2

counties_map <- map_data("county")

ggplot(states, aes(map_id = state)) +
        geom_map(aes(fill = value), map = counties_map) +
        expand_limits(x = counties_map$long, y = counties_map$lat) +
        guides(fill = guide_colorbar(colours = topo.colors(10))) +
        theme(legend.position = "bottom",
              axis.ticks = element_blank(), 
              axis.title = element_blank(), 
              axis.text =  element_blank()) + 
        scale_fill_gradient(low="white", high="blue") + theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_blank()) +
        guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) + 
        ggtitle("2011 state unemployment rates")





