
library(hydroTSM)

p_58 <- read.csv("H:/mmaya/BHSB/Precipitacion/modificados/P_bol_chi_SW.csv") %>% 
    mutate(fecha = as.Date(fecha))

p_58_ts <- xts(p_58[-1], order.by = p_58$fecha)
ep <- endpoints(p_58_ts, on = 'months')


# p_58[is.na(p_58)] <- -999.9
ts_m <- period.apply(p_58_ts, ep, FUN = function(x) apply(x, 2, FUN = sum))

ts_m_df <- as.data.frame(ts_m) %>% 
    mutate(mes = lubridate::month(as.Date(rownames(.))),
           anio = lubridate::year(as.Date(rownames(.)))) 

wide_ts <- lapply(1:58, function(x) reshape(ts_m_df[, c(x,59,60)], 
                                               idvar = 'anio', 
                                               timevar = 'mes', 
                                               direction = 'wide')) %>% 
    lapply(function(x) replace(x, is.na(x), -999.9)) %>% # forma de reemplazar NA en lappl
    setNames(ts_m %>% names())


for (x in 1:58) {
    file(paste0('H:/mmaya/BHSB/Precipitacion/exploracion/homog/BOLCH',
                ifelse(x<10,paste0('0',x),x),
                '.txt'), 'w')
    writeLines(names(ts_m)[x], 
               con = paste0('H:/mmaya/BHSB/Precipitacion/exploracion/homog/BOLCH',
                            ifelse(x<10,paste0('0',x),x),'.txt'), 
               sep = '\n')
}

# lapply(1:58, function(x) 
#     file(paste0('BOLCHI',x,'.txt'), 'w')
#     writeLines(names(ts_m)[x], con = paste0('BOLCH',x,'.txt'), sep = '\n'))


mapply(function(df, nombre) write.table(df, nombre, 
                                        col.names = FALSE, 
                                        row.names = FALSE,
                                        append = TRUE,
                                        sep = '\t'), 
       df = wide_ts,
       nombre = paste0('H:/mmaya/BHSB/Precipitacion/exploracion/homog/BOLCH',
                       c(paste0('0', 1:9), seq(10,58)),
                       '.txt'))

gc()


#### input para info del bhsb no rellenada ####
p_58_nr <- read.csv("H:/mmaya/BHSB/Precipitacion/modificados/P_bol_chi_SW_sin_relleno.csv") %>% 
    mutate(fecha = as.Date(fecha))

p_58_ts_nr <- xts(p_58_nr[-1], order.by = p_58$fecha)
ep <- endpoints(p_58_ts_nr, on = 'months')


# p_58[is.na(p_58)] <- -999.9
ts_m_nr <- period.apply(p_58_ts_nr, ep, FUN = function(x) apply(x, 2, FUN = sum))

ts_m_df_nr <- as.data.frame(ts_m_nr) %>% 
    mutate(mes = lubridate::month(as.Date(rownames(.))),
           anio = lubridate::year(as.Date(rownames(.)))) 

wide_ts_nr <- lapply(1:58, function(x) reshape(ts_m_df_nr[, c(x,59,60)], 
                                            idvar = 'anio', 
                                            timevar = 'mes', 
                                            direction = 'wide')) %>% 
    lapply(function(x) replace(x, is.na(x), -999.9)) %>% # forma de reemplazar NA en lappl
    setNames(ts_m_nr %>% names())


for (x in 1:58) {
    file(paste0('H:/mmaya/BHSB/Precipitacion/exploracion/homog_nr/BOLCH',
                ifelse(x<10,paste0('0',x),x),
                '.txt'), 'w')
    writeLines(names(ts_m_nr)[x], 
               con = paste0('H:/mmaya/BHSB/Precipitacion/exploracion/homog_nr/BOLCH',
                            ifelse(x<10,paste0('0',x),x),'.txt'), 
               sep = '\n')
}

# lapply(1:58, function(x) 
#     file(paste0('BOLCHI',x,'.txt'), 'w')
#     writeLines(names(ts_m)[x], con = paste0('BOLCH',x,'.txt'), sep = '\n'))


mapply(function(df, nombre) write.table(df, nombre, 
                                        col.names = FALSE, 
                                        row.names = FALSE,
                                        append = TRUE,
                                        sep = '\t'), 
       df = wide_ts_nr,
       nombre = paste0('H:/mmaya/BHSB/Precipitacion/exploracion/homog_nr/BOLCH',
                       c(paste0('0', 1:9), seq(10,58)),
                       '.txt'))

gc()



