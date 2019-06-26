# unidades hidrograficas
library(sf)
library(tidyverse)
library(mapview)
library(smoothr)
library(ggspatial)

# uh <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/downloads/WB_Historico_CC_CCL.shp")
uh_1 <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/nivel_1/Unidades_Hidrográficas.shp") %>% 
    st_transform(4326)
uh_1_macro <- uh_1$Macrocuenc %>% unique() %>% as.character()

## ya no necesario, uh_1 ya fue escrito
# lapply(seq_along(uh_1_macro), 
#        function(x) uh_1 %>% 
#            filter(Macrocuenc %in% uh_1_macro[x]) %>% 
#            st_union() %>% 
#            st_sf(Cuenca = uh_1_macro[x])) %>%
#     do.call(rbind, .) %>% 
#     fill_holes(threshold = 100000) %>% 
#     st_write('H:/mmaya/proyectos_R/gis_mapas_educativos/salidas/salidas_sig_educ/uh.shp')
    
# mapview(cuencas)
cuencas <- st_read('H:/mmaya/proyectos_R/gis_mapas_educativos/salidas/salidas_sig_educ/uh.shp')
sudamerica <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/exportado/limite_internacional_exportado.shp")
# bolivia_dptos <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/limites/Limite_departamental_final.shp") 
bolivia_dptos_exp <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/exportado/limite_dptal.shp") 
lago_lag <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/Lagunas.shp")
titi <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/Lago_Titicaca.shp")
pop <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/Lago_Poopo_Uru_Uru.shp")
salar <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/Salares_Uyuni_Coipasa.shp")
rios <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei1/Rios_Final.shp")
ciudades <- st_read("H:/mmaya/sig/EMPAQUETADOS BHSB/Nueva carpeta/commondata/mapas_finales_sei/capital_departamental.shp")

#### ggplot ####

# cortar sudamerica con bordes de mapa bol_c
labels_sud <- st_crop(sudamerica, st_polygon(list(cbind(c(-72.7,-57,-57,-72.7,-72.7),
                                                        c(-24,-24,-9,-9,-24))))) %>% 
    st_centroid() #%>% 
    # st_set_geometry(list(c(-64.396, -64.6706, -58.5, -69.5, -59.80634, -71.5), c(-23.5, -16.71497, -11.5, -21.54047, -21.80938, -13.89131)))
#

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

bol_c <- ggplot() +
    geom_sf(data = sudamerica, fill = 'gray90') +
    geom_sf(data = cuencas, aes(fill = Cuenca), color = NA) +
    geom_sf(data = sudamerica, fill = NA) +
    geom_sf(data = salar, fill = 'gray95', color = NA) +
    # geom_sf_label(data = labels_sud %>% filter(!PAÍS %in% 'Bolivia'), aes(label = PAÍS)) +
    # geom_text(data = sudamerica, aes(x=X, y=Y, label=name),
    #           color = "darkblue", fontface = "bold", check_overlap = FALSE) +
    geom_sf(data = bolivia_dptos_exp, fill = NA) +
    geom_sf(data = lago_lag, fill = 'blue3', color = NA) +
    geom_sf(data = titi, fill = 'blue3', color = NA) +
    geom_sf(data = pop, fill = 'blue3', color = NA) +
    geom_sf(data = rios, color = 'blue3') +
    geom_sf(data = ciudades, aes(col = 'red'), show.legend = 'point') +
    coord_sf(xlim = c(-72.7,-56), 
             ylim = c(-24, -9), expand = FALSE) +
    # annotation_scale(location = "br", width_hint = 0.2) +
    # geom_sf_text(data = bolivia_dptos_exp, aes(label = DEPARTAMEN), size = 3) +
    annotate("text", label = c("POTOSI", 'ORURO', 'LA PAZ', 'SANTA CRUZ',
                               'COCHABAMBA','PANDO', 'BENI','CHIQUISACA', 'TARIJA'), 
             x = c(-67,-68,-68.4,-59.5,
                   -65.5,-66.1,-66,-63.2,-63.5), 
             y = c(-21,-18.5,-15,-18.5,
                   -17.2,-10.2,-14.2,-20.6,-21.7), 
             size = 3, colour = "black") +
    # scale_fill_brewer(palette = "Accent") +
    scale_fill_manual(values = c('Altiplano' = RColorBrewer::brewer.pal(3,'Accent')[1],
                                 'Amazonas' = RColorBrewer::brewer.pal(3,'Accent')[2],
                                 'La Plata' = RColorBrewer::brewer.pal(3,'Accent')[3]), 
                      name = 'Cuencas',
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
          legend.position = c(.19, .33),
          legend.justification = c("right", "top"),
          legend.box.just = "left",
          # legend.margin = margin(4, 4, 4, 4),
          legend.box.margin = margin(4, 4, 4, 4),
          legend.box.background = element_rect(color = "gray"),
          legend.spacing.y = unit(1,'mm'),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 7))

# ggplot() +
#     geom_sf(data = ciudades, 
#             aes(col = 'red'), pch = 16, show.legend = 'point') + 
#     scale_color_manual(name = 'Simbolos', 
#                        guide = guide_legend(override.aes = list(shape = 16)), 
#                        values = c('red'), 
#                        labels = c('Capital'))

uh_ed <- bol_c +
    annotation_custom(
        grob = ggplotGrob(sud),
        xmin = -60.1, # -2750000
        xmax = -60.1 + (-57 - (-70.5))/3, # -2750000 + (1600000 - (-2400000))/2.5
        ymin = -13.7, # -2450000
        ymax = -13.7 + (-9.5 - (-23.5))/3 # -2450000 + (2500000 - 200000)/2.5
    )

ggsave('H:/mmaya/sig/EMPAQUETADOS BHSB/sencillos/uh.jpg', uh_ed, units = c('cm'),
       height = 15.2, width = 17.5)

#### base ####
plot(st_geometry(cuencas), axes = TRUE, graticule = TRUE, reset = FALSE,
     xlim = c(-72.7,-56), ylim = c(-24, -9), bg = 'aliceblue')
plot(sudamerica, add = TRUE, col = 'gray90')
plot(cuencas, axes = TRUE, col = RColorBrewer::brewer.pal(3, 'Accent'), add = TRUE, border = 'gray')
plot(st_geometry(sudamerica), add = TRUE)
plot(st_geometry(bolivia_dptos), lwd = 2, add = TRUE)
plot(lago_lag, col = 'aliceblue', add = TRUE)
plot(titi, col = 'aliceblue', add = TRUE)
plot(pop, col = 'aliceblue', add = TRUE)
plot(salar, col = 'gray99', add = TRUE)
box(lwd = 1)
legend('bottomright', legend = c('Amazonas', 'La Plata', 'Altiplano'),
       fill = RColorBrewer::brewer.pal(3, 'Accent'),
       # pch = rep(16,3),
       title = 'Cuencas')
prettymapr::addscalebar(padin = c(0.15, 0.2))

#
par(new = TRUE) # overlay existing plot
par(mar = c(0,0,0,0)) # strip out the margins for the inset plot
par(fig = c(0.75,0.95,0.6,0.9)) # fig shrinks and places relative to figure region

plot(sudamerica, col = 'gray90', axes = FALSE, graticule = TRUE, main = '',
     xlim = c(-83,-34), ylim = c(-55, 12), bg = 'aliceblue', reset = FALSE)
rect(xleft = -70.5, 
     xright = -57,
     ybottom = -23.5,
     ytop = -9.5, border = 'red')
