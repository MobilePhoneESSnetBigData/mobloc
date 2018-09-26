create_prototypes <- function(cp) {
  
  cp_ovals <- cp %>% filter(!is.na(direction))
  ids <- which(!is.na(cp$direction))
  
  km <- kmeans(cp_ovals[, c("tilt", "beam_h", "beam_v", "height")] %>% st_set_geometry(NULL), centers = 10)
  
  cp$prototype <- 0L
  cp$prototype[ids] <- km$cluster
  
  plot(cp %>% st_set_geometry(NULL) %>% filter(prototype == 57) %>% select(tilt, height), pch = 21)
  
  as.data.frame(km$centers)
  
  xs <- seq(-4000, 4000, by = 20)
  ys <- seq(-4000, 8000, by = 20)
  
  co <- as.data.frame(expand.grid(x=xs, y=ys, z=0))
  
  # 47
  prototypes <- lapply(1:nrow(km$centers), function(i) {
    input <- as.list(km$centers[i, ])
    
    co2 <- cbind(co, signal_strength(0, 0, input$height, direction = 0, tilt = input$tilt, beam_h = input$beam_h, beam_v =  input$beam_v, small = FALSE, co = co, param = param))
    co2$db[co2$db > -60] <- -60
    co2$db[co2$db < -130] <- -130
    
    dbs <- matrix(co2$db, nrow = length(ys))
    
    cl <- contourLines(xs, ys, dbs, levels = -110)
    
    cl <- lapply(cl, function(cli) list(cbind(cli$x, cli$y)))
    
    st_multipolygon(cl)
  })
  
  
  ptypes <- st_sfc(prototypes, crs = st_crs(cp))
  
  
  ptypes <- st_sf(ptypes)
  ptypes$class <- 1:nrow(ptypes)
  
  ttm()
  
  qtm(ptypes, by = "class", free.coords = FALSE, drop.units = TRUE)
  
  
  st_as_sf(prototypes[1:2])
  
  
  prototypes[[1]][[1]] * rot(pi)
  
  
  
  do.call()
  
  
  
  qtm(prototypes[[47]])
  
  
  
  for (i in ) {
    
  }
  
 
