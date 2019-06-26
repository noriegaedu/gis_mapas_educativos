# temperatura
# library(sf)
library(raster)
library(tidyverse)
library(mapview)
# library(smoothr)
library(ggspatial)
library(stars)
library(rasterVis)
library(rgdal)
library(graticule)

# temper <- raster("H:/mmaya/sig/EMPAQUETADOS BHSB/temperatura/commondata/raster_data/T_mean.tif")
# pr_temper <- projectRaster(temper, crs = '+proj=longlat +datum=WGS84 +no_defs')
# writeRaster(pr_temper, "H:/mmaya/sig/EMPAQUETADOS BHSB/temperatura/commondata/raster_data/T_mean_wgs.tif")

temper_wgs <- raster("H:/mmaya/sig/EMPAQUETADOS BHSB/temperatura/commondata/raster_data/T_mean_wgs.tif")
# temper_wgs <- read_stars("H:/mmaya/sig/EMPAQUETADOS BHSB/temperatura/commondata/raster_data/T_mean_wgs.tif")

#### levelplot ####
sudamerica <- readOGR("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/limites/limite_internacional.shp")
# bolivia_dptos <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/limites/Limite_departamental_final.shp") 
bolivia_dptos_exp <- readOGR("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/exportado/limite_dptal.shp") 
lago_lag <- readOGR("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/Lagunas.shp")
titi <- readOGR("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/Lago_Titicaca.shp")
pop <- readOGR("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/Lago_Poopo_Uru_Uru.shp")
salar <- readOGR("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/Salares_Uyuni_Coipasa.shp")
rios <- readOGR("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei1/Rios_Final.shp") %>% 
    spTransform(., crs(salar))
ciudades <- readOGR("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/capital_departamental.shp")

source('H:/@Documentos/Tesis/mi_proy_gmet/eval_estad/scripts/func_cortes_levelplot.R')

level_att <- r_cortes(cortes = seq(0,30,5),
                       titulo = 'Temperatura \n[°C]',
                       tema = 'RdYlBu',
                       sep = ' - ')


## grilla
crs_longlat <- CRS("+init=epsg:4326")
prj <- CRS(projection(temper_wgs))
extLL <- projectExtent(temper_wgs, crs_longlat)
#lons <- pretty(c(xmin(extLL), xmax(extLL)))
#lats <- pretty(c(ymin(extLL), ymax(extLL)))
# se reemplazan los paralelos generados automaticamente para poder lograr un plot mas bonito
lons <- seq(-69, -67, 0.25) 
lats <- seq(-18, -14, 0.2)
# here we push them out a little on each side
xl <-  range(lons) + c(-1, 1)
yl <- range(lats) + c(-1, 1)
# build the lines with our precise locations and ranges
grat <- graticule(lons, lats, proj = prj, xlim = xl, ylim = yl)

## argumentos para levelplot 
# breaks <- nrow(mat)
# my.at <- (0:breaks)
# myColorkey <- list(title = 'msnm', at = my.at, height = 0.95, width = 1, 
#                    labels = list(at = my.at + 0.5, labels = paste0('(', apply(mat[,-3], 1, paste, collapse = '-'), ']') , cex = 0.8))
# #cols <- colorRampPalette(c('palegreen4', 'palegreen3' , 'khaki', 'burlywood4', 'brown'))(length(my.at) - 1) # colores en caso general
# #cols <- c('#5a7a48', '#709959', '#BBCC83', '#F2EAA0', '#F2D88F', '#E3B684', '#C7927D', '#897044', '#000000') # colores del terreno para dem grande
# cols <- colorRampPalette(c('palegreen3', 'palegreen4' , 'khaki', 'burlywood4', 'brown'))(13)[c(1:7,9,10)] # 13 colores originlaes para escoger los que me gustan luego c(1:7,9:10)
# cols <- c(cols, 'gray70') # a los colores se anhade un gris para los nevados
# #titulo_plot <- 'jojojojo' # cambiado a NULL. si sequiere usar tutlo reemplazar NULL con list(titulo_plot)
cex <- 0.8 # tamanho de puntos 

levelplot(temper_wgs,
         at = level_att[[6]],
         par.settings = rasterTheme(region = brewer.pal(level_att[[5]], level_att[[7]]),
                                    panel.background = list(col = 'aliceblue')), 
         colorkey = level_att[[1]],
         #par.settings = list(layout.heights = list(top.padding = pad +1.5, bottom.padding = pad, left.padding = pad, right.padding = pad)),
         #par.settings = themes[[i]],
         #scales = sc,
         margin = FALSE, 
         xlab = '',
         ylab = '') +
    layer(sp.polygons(sudamerica, fill = 'gray80', lwd = 1)) +
    levelplot(temper_wgs,
              at = level_att[[6]],
              par.settings = rasterTheme(region = brewer.pal(level_att[[5]], level_att[[7]])),
              # panel.background = list(col = 'gray90')), 
              colorkey = level_att[[1]],
              #par.settings = list(layout.heights = list(top.padding = pad +1.5, bottom.padding = pad, left.padding = pad, right.padding = pad)),
              #par.settings = themes[[i]],
              #scales = sc,
              margin = FALSE, 
              xlab = '',
              ylab = '') +
    layer(sp.polygons(sudamerica, col = 'black', lwd = 1)) +
    layer(sp.polygons(lago_lag, fill = 'blue3', col = NA, lwd = 1)) +
    layer(sp.polygons(titi, fill = 'blue3', col = NA, lwd = 1)) +
    layer(sp.polygons(pop, fill = 'blue3', col = NA, lwd = 1)) +
    layer(sp.polygons(salar, fill = 'gray95', col = NA, lwd = 1)) +
    layer(sp.polygons(rios, col = 'blue3', lwd = 1)) +
    layer(sp.polygons(bolivia_dptos_exp, col = 'black', lwd = 1)) +
    layer(sp.polygons(ciudades, col = 'red', pch = 16))


#### ggplot ####
sudamerica <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/exportado/limite_internacional_exportado.shp")
# bolivia_dptos <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/limites/Limite_departamental_final.shp") 
bolivia_dptos_exp <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/exportado/limite_dptal.shp") 
lago_lag <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/Lagunas.shp")
titi <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/Lago_Titicaca.shp")
pop <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/Lago_Poopo_Uru_Uru.shp")
salar <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/Salares_Uyuni_Coipasa.shp")
rios <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei1/Rios_Final.shp")
ciudades <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/capital_departamental.shp")

sud <- ggplot(data = st_geometry(sudamerica)) + 
    geom_sf() +
    coord_sf(crs = st_crs(4326), xlim = c(-83,-34), ylim = c(-55, 12), expand = FALSE) +
    geom_rect(xmin = -70.5, xmax = -57, ymin = -23.5, ymax = -9.5, 
              fill = NA, colour = "red") +
    # scale_x_continuous(breaks = c(-40, -60, -80)) +
    theme_bw() +
    theme(axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = 'gray90'),
          panel.background = element_rect(fill = 'aliceblue'))




level_att <- r_cortes(cortes = seq(0,30,5),
                      titulo = 'Temperatura \n[°C]',
                      tema = 'RdYlBu',
                      sep = ' - ')

temper_wgs <- raster("H:/mmaya/sig/EMPAQUETADOS BHSB/temperatura/commondata/raster_data/T_mean_wgs.tif")
temper_wgs_rec <- reclassify(temper_wgs, level_att[[4]])
te <- data.frame(rasterToPoints(temper_wgs)) %>% 
    mutate(categ = cut(.$T_mean_wgs, c(-5,seq(5,30,5)), 
                       include.lowest = TRUE, dig.lab = 10, 
                       labels = c('< 5', '5 - 10', '10 - 15', '15 - 20', '20 - 25', '> 25')))

temperatura <- ggplot() + 
    geom_sf(data = sudamerica, fill = 'gray90') +
    geom_raster(data = te, aes_string(x = 'x', y = 'y', fill = 'categ')) +
    geom_sf(data = sudamerica, fill = NA) +
    geom_sf(data = salar, fill = 'gray95', color = 'gray90') +
    geom_sf(data = bolivia_dptos_exp, fill = NA) +
    geom_sf(data = lago_lag, fill = 'blue3', color = NA) +
    geom_sf(data = titi, fill = 'blue3', color = NA) +
    geom_sf(data = pop, fill = 'blue3', color = NA) +
    geom_sf(data = rios, color = 'blue3') +
    geom_sf(data = ciudades, aes(col = 'purple'), show.legend = 'point') +
    # geom_sf(data = ciudades, col = 'red', show.legend = 'point') +
    coord_sf(xlim = c(-72.7,-56), 
             ylim = c(-24.5, -9), expand = FALSE) +
    # modif para leyenda
    annotate("text", label = c("POTOSI", 'ORURO', 'LA PAZ', 'SANTA CRUZ',
                               'COCHABAMBA','PANDO', 'BENI','CHIQUISACA', 'TARIJA'), 
             x = c(-67,-68,-68.4,-59.5,
                   -65.7,-66.1,-66,-63.2,-63.5), 
             y = c(-21,-18.5,-15,-18.5,
                   -17.7,-10.2,-14.2,-20.6,-21.7), 
             size = 3, colour = "black") +
    scale_fill_manual(values = c('Sin Resultados' = 'lightpink',
                                 '< 5' = RColorBrewer::brewer.pal(6,'RdYlGn')[6],
                                 '5 - 10' = RColorBrewer::brewer.pal(6,'RdYlGn')[5],
                                 '10 - 15' = RColorBrewer::brewer.pal(6,'RdYlGn')[4],
                                 '15 - 20' = RColorBrewer::brewer.pal(6,'RdYlGn')[3],
                                 '20 - 25' = RColorBrewer::brewer.pal(6,'RdYlGn')[2],
                                 '> 25' = RColorBrewer::brewer.pal(6,'RdYlGn')[1]),
                      name = 'Temperatura [°C]',
                      guide = guide_legend(override.aes = list(linetype = "blank",
                                                               shape = NA))) +
    scale_colour_manual(values = c("purple"), labels = c('Capital'),
                        name = 'Símbolos',
                        guide = guide_legend(override.aes = list(linetype = c("blank"),
                                                                 shape = c(16)))) +
    theme_bw() +
    theme(panel.background = element_rect(fill = 'aliceblue'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = c(.17, .44),
          legend.justification = c("right", "top"),
          legend.box.just = "left",
          # legend.margin = margin(4, 4, 4, 4),
          legend.box.margin = margin(4, 4, 4, 1),
          legend.box.background = element_rect(color = "gray"),
          legend.spacing.y = unit(1,'mm'),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 7)) +
    # legend.key.size = unit(5,"mm"))
    guides(shape = guide_legend(override.aes = list(size = 4)))


tempe_ed <- temperatura +
    annotation_custom(
        grob = ggplotGrob(sud),
        xmin = -60.1, # -2750000
        xmax = -60.1 + (-57 - (-70.5))/3, # -2750000 + (1600000 - (-2400000))/2.5
        ymin = -13.7, # -2450000
        ymax = -13.7 + (-9.5 - (-23.5))/3 # -2450000 + (2500000 - 200000)/2.5
    )

ggsave('H:/mmaya/sig/EMPAQUETADOS BHSB/sencillos/temp.jpg', tempe_ed, units = c('cm'),
       height = 15.2, width = 17.5)


