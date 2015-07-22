# select color palette
display.brewer.all()

colors <- brewer.pal(9, "Blues")
pal <- colorRampPalette(colors)
pal(length(unique(choropleth$rate_d1)))

colors <- brewer.pal(9, "YlOrRd")
pal <- colorRampPalette(colors)
