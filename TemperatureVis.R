library(raster)
require(rasterVis)
library(rgdal)
library(viridis)


r.temp = raster('/Users/matthewxi/Downloads/air.sfc.2015 (1).nc', 80)

stack.temp =  raster::stack('/Users/matthewxi/Downloads/air.sfc.2015 (1).nc')

#animate(stack.temp[[1:50]], pause=0.1,n=2)

zone = shapefile('../GIS/ApproxBasin/ApproxBasinPolyFull.shp')
template = raster('../GIS/air.2m.2015.nc', 1)

zone.narr = spTransform(zone, crs(template))
plot(zone.narr)
plot(extent(zone.narr))

zone.extent = extent(zone.narr)
plot(crop(template, zone.extent))

#air.2m = brick('../GIS/air.2m.2015.nc')
#air.2m.columbia = crop(air.2m, zone.extent)
#writeRaster(air.2m.columbia, '../GIS/air.2m.columbia')
air.2m.columbia = brick('../GIS/air.2m.columbia.grd')


rivers = shapefile('../GIS/Hydrography/MajorRivers.shp')
plot(crop(template, zone.extent))
rivers.narr = spTransform(rivers, crs(template))
plot(rivers.narr, add=TRUE)

demo = crop(template, zone.extent) 
levelplot(demo, col.regions = rev(terrain.colors(255)), cuts=254, margin=FALSE) +
  layer(sp.points(rivers.narr, col = "blue"))

p <- as(zone.extent, 'SpatialPolygons')
crs(p) = crs(zone.narr)
shapefile(p, filename='../GIS/zone.extent')

# animate(air.2m.columbia[[1:50]], pause=0.1,n=2)
nam = names(air.2m.columbia)
palette = colorRampPalette(c("blue", "red"))( 255 )
for(i in (205*8):(212*8)){
  col.reaches = rep('blue',length(rivers.narr$HYDRO_))
  col.reaches[rivers.narr$HYDRO_ == 8445] = 'red'
  my.at <- seq(270, 320)
  pl = levelplot(air.2m.columbia[[i]], 
                 col.regions =  palette,
                 #cuts=254, 
                 main=nam[i],
                 margin=FALSE, at = my.at) +
    layer(sp.lines(rivers.narr, col = col.reaches))  
  print(pl)
  Sys.sleep(.1)
}


#pdf("name.pdf")
#print(levelplot(my_data))
#dev.off()

water.temp = ts.data$Temperature..F.;
nam = names(air.2m.columbia)
palette = colorRampPalette(c("blue", "red"))( 255 )
palette.heat = rev(heat.colors(255))
palette = viridis(255)
palettet = adjustcolor(palette, alpha.f = 0.5) 
for(i in (180*8):(210*8)){
  col.reaches = rep('gray',length(rivers.narr$HYDRO_))
  
  day = floor(i / 8) # the day
  hour = (i %% 8) * 3
  wt.index = 24*day + hour
  
  wt = as.numeric(water.temp$Temperature..F.[wt.index])
  print(water.temp$Temperature..F.[wt.index])
  wt = 5 * (wt - 32) / 9
  wt = floor(((wt-15)/10) * 255)
  print(wt)
  # 8445 Bonneville Area
  # 8291 Ice Harbor Area
  col.reaches[rivers.narr$HYDRO_ == 8445] = palette.heat[wt]
  
  my.at <- seq(273, 323)-273
  pl = levelplot(air.2m.columbia[[i]]-273, 
                 col.regions =  palette,
                 #cuts=254, 
                 main=nam[i],
                 margin=FALSE, at = my.at) +
    layer(sp.lines(rivers.narr, col = col.reaches, lwd=6))  
  print(pl)
  Sys.sleep(.1)
}


