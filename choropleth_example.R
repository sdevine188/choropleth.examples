library(ggplot2)
library(dplyr)
library(datasets)
library(maps)
library(stringr)
library(scales)

# load lat/long map data for county and state boundaries
county_map <- map_data("county")
state_map <- map_data("state")

# attach unemployment data and county fips code data
data(unemp)
data(county.fips)

# create states dataframe with state names and abbreviations
states <- data.frame(state.abb, state.name)
states$state.name <- tolower(state.name)

# merge states with county_map to create county_df
county_df <- merge(county_map, states, by.x = "region", by.y = "state.name")

# create a polyname variable in county_df to use for merge with county.fips
county_df$polyname <- str_c(county_df$region, county_df$subregion, sep = ",")

# merge county_df with county.fips based on polyname
county_df <- merge(county_df, county.fips, by = "polyname")

# merge county_df with unemp based on fips
choropleth <- merge(county_df, unemp, by = "fips")

# create discretized unemployment rate to use with color brewer
choropleth <- choropleth[order(choropleth$order), ]
choropleth$rate_d <- cut(choropleth$unemp, c(seq(from = 0, to = 12, by = 2), 35))

# clean up discrete unemployment rate buckets for use in legend
choropleth$rate_d2 <- str_replace(choropleth$rate_d, "\\(", "")
choropleth$rate_d2 <- str_replace(choropleth$rate_d2, "\\]", "")
choropleth$rate_d2 <- str_replace(choropleth$rate_d2, ",", "% - ")
choropleth$rate_d2 <- str_c(choropleth$rate_d2, "%", sep = "")
levels <- c("0% - 2%", "2% - 4%", "4% - 6%", "6% - 8%", "8% - 10%", "10% - 12%", "12% - 35%")
choropleth$rate_d2 <- factor(choropleth$rate_d2, levels = levels, ordered = TRUE)

# create choropleth of all counties in US
# will take a minute to load 
choropleth_input <- choropleth
state_map_input <- state_map

ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = rate_d), colour = alpha("white", 1/2), size = 0.2) + 
        geom_polygon(data = state_map_input, colour = "black", fill = NA) +
        scale_fill_brewer(palette = "Blues") + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
         axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title=element_text(size=20,face="bold")) + 
        labs(x="", y="", title="Unemployment rate in U.S") + coord_fixed() + coord_map(project="conic", lat0 = 30)

# create choropleth of just counties in alabama
# filter choropleth and state_map down to just alabama rows
choropleth_input <- filter(choropleth, region == "alabama")
state_map_input <- filter(state_map, region == "alabama")

ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = rate_d2), colour = alpha("white", 1/2), size = 0.2) + 
        geom_polygon(data = state_map_input, colour = "black", fill = NA) +
        scale_fill_brewer(palette = "Blues") + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title=element_text(size=20,face="bold")) + 
        labs(x="", y="", title="Unemployment rate in Alabama") + coord_fixed() + coord_map(project="conic", lat0 = 30)

# create choropleth of just EDA Denver Regional Office states
choropleth_input <- filter(choropleth, region == "montana" | region == "north dakota" | region == "south dakota" | region == "wyoming" | region == "colorado" | 
                           region == "nebraska" | region == "kansas" | region == "iowa" | region == "missouri")
state_map_input <- filter(state_map, region == "montana" | region == "north dakota" | region == "south dakota" | region == "wyoming" | region == "colorado" | 
                                  region == "nebraska" | region == "kansas" | region == "iowa" | region == "missouri")

ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = rate_d2), colour = alpha("white", 1/2), size = 0.2) + 
        geom_polygon(data = state_map_input, colour = "black", fill = NA) +
        scale_fill_brewer(palette = "Blues") + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title=element_text(size=20,face="bold")) + 
        labs(x="", y="", title="Unemployment rate in Denver Region Office") + coord_fixed() + coord_map(project="conic", lat0 = 30)


