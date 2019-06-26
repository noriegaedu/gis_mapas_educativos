# escorrentia
library(sf)
library(tidyverse)
library(mapview)
library(smoothr)
library(ggspatial)
library(colorspace)

escorrentia <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/escorrentia/commondata/downloads/WB_Historico_CC_CCL.shp") %>% 
    st_transform(4326) %>% 
    mutate(categ = cut(escorrentia$Balance__5, c(0,100,200,400,800,1000,25000), 
                       include.lowest = TRUE, dig.lab = 10, 
                       labels = c('< 100', '100 - 200', '200 - 400', '400 - 800', '800 - 1000','> 1000'))) 
levels(escorrentia$categ) <- c(levels(escorrentia$categ),'Sin Resultados')
escorrentia$categ[escorrentia$Balance__5 == 0] <- as.factor('Sin Resultados')

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



escg <- ggplot() +
    geom_sf(data = sudamerica, fill = 'gray90') +
    geom_sf(data = escorrentia, aes(fill = categ), color = NA) +
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
    scale_fill_manual(values = c('Sin Resultados' = 'green',
                                 '< 100' = RColorBrewer::brewer.pal(9,'RdPu')[2],
                                 '100 - 200' = RColorBrewer::brewer.pal(9,'RdPu')[3],
                                 '200 - 400' = RColorBrewer::brewer.pal(9,'RdPu')[4],
                                 '400 - 800' = RColorBrewer::brewer.pal(9,'RdPu')[5],
                                 '800 - 1000' = RColorBrewer::brewer.pal(9,'RdPu')[7],
                                 '> 1000' = RColorBrewer::brewer.pal(9,'RdPu')[9]),
                      name = 'Escorrentía [mm/año]',
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
          legend.position = c(.2, .49),
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


esc_ed <- escg +
    annotation_custom(
        grob = ggplotGrob(sud),
        xmin = -60.1, # -2750000
        xmax = -60.1 + (-57 - (-70.5))/3, # -2750000 + (1600000 - (-2400000))/2.5
        ymin = -13.7, # -2450000
        ymax = -13.7 + (-9.5 - (-23.5))/3 # -2450000 + (2500000 - 200000)/2.5
    )

ggsave('H:/mmaya/sig/EMPAQUETADOS BHSB/sencillos/esc.jpg', esc_ed, units = c('cm'),
       height = 15.2, width = 17.5)
