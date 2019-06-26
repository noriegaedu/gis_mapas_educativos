# etp
library(sf)
library(tidyverse)
library(mapview)
library(smoothr)
library(ggspatial)

etp <- st_read("E:/EMPAQUETADOS BHSB/etp/commondata/downloads/WB_Historico_CC_CCL.shp") %>% 
    st_transform(4326) %>% 
    mutate(categ = cut(etp$Balance__2, c(0,1050,1100,1200,1400,1500,2000), 
                       include.lowest = TRUE, dig.lab = 10, 
                       labels = c('< 1050', '1050 - 1100', '1100 - 1200', '1200 - 1400', '1400 - 1500', '> 1500'))) 
levels(etp$categ) <- c(levels(etp$categ),'Sin Resultados')
etp$categ[etp$Balance__2 == 0] <- as.factor('Sin Resultados')

sudamerica <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/exportado/limite_internacional_exportado.shp")
# bolivia_dptos <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/limites/Limite_departamental_final.shp") 
bolivia_dptos_exp <- st_read("E:/EMPAQUETADOS BHSB/Nueva carpeta/exportado/limite_dptal.shp") 
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



etpg <- ggplot() +
    geom_sf(data = sudamerica, fill = 'gray90') +
    geom_sf(data = etp, aes(fill = categ), color = NA) +
    geom_sf(data = sudamerica, fill = NA) +
    geom_sf(data = salar, fill = 'gray95', color = 'gray90') +
    geom_sf(data = bolivia_dptos_exp, fill = NA) +
    geom_sf(data = lago_lag, fill = 'blue3', color = NA) +
    geom_sf(data = titi, fill = 'blue3', color = NA) +
    geom_sf(data = pop, fill = 'blue3', color = NA) +
    geom_sf(data = rios, color = 'blue3') +
    geom_sf(data = ciudades, aes(col = 'red'), show.legend = 'point') +
    # geom_sf(data = ciudades, col = 'red', show.legend = 'point') +
    coord_sf(xlim = c(-73.5,-56), 
             ylim = c(-24.5, -9), expand = FALSE) + # modif para leyenda
    annotate("text", label = c("POTOSI", 'ORURO', 'LA PAZ', 'SANTA CRUZ',
                               'COCHABAMBA','PANDO', 'BENI','CHIQUISACA', 'TARIJA'), 
             x = c(-67,-68,-68.4,-59.5,
                   -65.7,-66.1,-66,-63.2,-63.5), 
             y = c(-21,-18.5,-15,-18.5,
                   -17.7,-10.2,-14.2,-20.6,-21.7), 
             size = 3, colour = "black") +
    scale_fill_manual(values = c('Sin Resultados' = 'lightpink',
                                 '< 1050' = RColorBrewer::brewer.pal(9,'YlOrBr')[2],
                                 '1050 - 1100' = RColorBrewer::brewer.pal(9,'YlOrBr')[3],
                                 '1100 - 1200' = RColorBrewer::brewer.pal(9,'YlOrBr')[4],
                                 '1200 - 1400' = RColorBrewer::brewer.pal(9,'YlOrBr')[5],
                                 '1400 - 1500' = RColorBrewer::brewer.pal(9,'YlOrBr')[7],
                                 '> 1500' = RColorBrewer::brewer.pal(9,'YlOrBr')[9]),
                      name = 'Evapotranspiración \nPotencial [mm/año]',
                      guide = guide_legend(override.aes = list(linetype = "blank", 
                                                               shape = NA))) +
    scale_colour_manual(values = c("red"), labels = c('Capital'),
                        name = 'Símbolos',
                        guide = guide_legend(override.aes = list(linetype = c("blank"),
                                                                 shape = c(16)))) +
    # scale_color_manual(name = 'Simbolos',
    #                    guide = guide_legend(override.aes = list(shape = 16)),
    #                    values = c('red'),
    #                    labels = c('Capital')) +
    theme_bw() +
    theme(panel.background = element_rect(fill = 'aliceblue'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = c(.19, .5),
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


etp_ed <- etpg +
    annotation_custom(
        grob = ggplotGrob(sud),
        xmin = -60.1, # -2750000
        xmax = -60.1 + (-57 - (-70.5))/3, # -2750000 + (1600000 - (-2400000))/2.5
        ymin = -13.7, # -2450000
        ymax = -13.7 + (-9.5 - (-23.5))/3 # -2450000 + (2500000 - 200000)/2.5
    )

ggsave('H:/mmaya/sig/EMPAQUETADOS BHSB/sencillos/etp.jpg', etp_ed, units = c('cm'),
       height = 15.2, width = 17.5)
