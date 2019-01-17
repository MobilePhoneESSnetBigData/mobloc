library(tmap)
library(tmaptools)
library(sf)
library(dplyr)
library(lwgeom)
ZL_bbox <- st_bbox(c(xmin = 172700, ymin = 306800, xmax = 204800, ymax = 342700), crs = st_crs(28992))



### gemeente en buurt shape
is_num <- function(x) suppressWarnings(!any(is.na(as.numeric(x))))
tmp <- tempfile(fileext=".zip")
download.file("https://www.cbs.nl/-/media/cbs/dossiers/nederland%20regionaal/wijk-en-buurtstatistieken/2018/shape%202018%20versie%2010.zip", destfile = tmp)

tmpdir <- tempdir()
unzip(tmp, exdir = tmpdir)
#buurt <- st_read(file.path(tmpdir, "buurt_2015.shp"))  ## GIVES OFFSET


wijk <- st_read(file.path(tmpdir, "Uitvoer_shape/wijk_2018.shp"))
wijk <- st_transform(wijk, crs = 28992)

wijk <- wijk[wijk$WATER=="NEE", ]

isf <- sapply(wijk, is.factor)
for (nm in names(isf)[isf]) {
    col <- wijk[[nm]]

    wijk[[nm]] <- local({
        if (is_num(levels(col))) {
            col2 <-  as.numeric(as.character(col))
            col2[col2 < -99999998] <- NA
            col2int <- as.integer(col2)
            if (all.equal(col2, col2int)) col2int else col2
        } else {
            col
        }
    })
}
wijk <- st_make_valid(wijk)

gem <- st_read(file.path(tmpdir, "Uitvoer_shape/gem_2018.shp"))
gem <- st_transform(gem, crs = 28992)
ids <- which(st_coordinates(st_centroid(gem))[,2] < 340000)
gem_ZL <- gem[ids, ]
zl <- st_union(gem_ZL)


wijk_ZL <- wijk %>%
    filter(GM_CODE %in% gem_ZL$GM_CODE)


buurt <- st_read(file.path(tmpdir, "Uitvoer_shape/buurt2018.shp"))
buurt <- st_transform(buurt, crs = 28992)

buurt <- buurt[buurt$WATER=="NEE", ]

isf <- sapply(buurt, is.factor)
for (nm in names(isf)[isf]) {
    col <- buurt[[nm]]

    buurt[[nm]] <- local({
        if (is_num(levels(col))) {
            col2 <-  as.numeric(as.character(col))
            col2[col2 < -99999998] <- NA
            col2int <- as.integer(col2)
            if (all.equal(col2, col2int)) col2int else col2
        } else {
            col
        }
    })
}
buurt <- st_make_valid(buurt)

buurt_ZL <- buurt %>%
    filter(GM_CODE %in% gem_ZL$GM_CODE)




####### ZL_land: wijk_ZL minus water
library(osmdata)
bbL <- bb(matrix(ZL_bbox, ncol=2), current.projection = st_crs(zl)$proj4string, projection = "longlat")
q1 <- opq(bbL)
q2 <- add_osm_feature(q1, key="natural", value="water")
q3 <- add_osm_feature(q1, key="waterway", value="canal")
osm2 <- osmdata_sf(q2)
osm3 <- osmdata_sf(q3)
osm <- c(osm3$osm_polygons$geometry, osm2$osm_polygons$geometry, osm2$osm_multipolygons$geometry)
osm <- osm[as.numeric(st_area(osm))>(200^2)]

st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
zl2 <- st_erase(zl, st_transform(osm, crs= st_crs(zl)))
ZL_land <- zl2


save(ZL_land, file = "../mobloc/data/ZL_land.rda", compress = "xz")



set.seed(1234)
wijk_ZL$pop <- round(wijk_ZL$AANT_INW / 6000)
ZL_cellplan_normal <- st_sample(wijk_ZL[wijk_ZL$pop!=0,], wijk_ZL$pop[wijk_ZL$pop!=0], type = "regular")



set.seed(1242)
buurt_ZL$pop <- round(buurt_ZL$AANT_INW / 6000)
buurt_ZL$small_cell <- buurt_ZL$STED==1 & sample(0:1, size = nrow(buurt_ZL), replace = TRUE, prob = c(.5, .5))

ZL_cellplan_small <- st_sample(buurt_ZL[buurt_ZL$small_cell,], rep(1, sum(buurt_ZL$small_cell)), type = "regular")
qtm(ZL_cellplan_small)




ZL_cellplan_normal %>%
      mutate(direction = round(runif(min = 0, max = 360)),
             height = )


wijk_ZL$dens <- wijk_ZL$AANT_INW / (as.numeric(st_area(wijk_ZL)) / 1e6)


qtm(wijk_ZL, fill = "dens")

wijk_ZL$small_cell <- round((wijk_ZL$dens / max(wijk_ZL$dens)) * (wijk_ZL$AANT_INW / max(wijk_ZL$AANT_INW)) * 5)

ZL_cellplan3 <- st_sample(wijk_ZL[wijk_ZL$small_cell > 0, ], wijk_ZL$small_cell[wijk_ZL$small_cell > 0], type = "regular")

ZL_c <- st_sf(small = c(rep(FALSE, length(ZL_cellplan2)), rep(TRUE, length(ZL_cellplan3))), geometry = c(ZL_cellplan2, ZL_cellplan3), crs = 28992)


ZL_c$site <- paste(toupper(substr(gem_ZL$GM_NAAM[unlist(st_intersects(ZL_c, gem_ZL))], 1, 3)), round(runif(nrow(ZL_c), min = 100, max = 999)), sep = "_")




cpl2$site <- as.integer(factor(paste(cpl2$x, cpl2$y, sep="_")))

## take 90% of sites
set.seed(1)
site_sample <- sample(unique(cpl2$site), round(max(cpl2$site) * .9))
cpl3 <- cpl2[cpl2$site %in% site_sample, ]

## create cell-number (per site, so typically 1-3)
cpl3$cell_nr <- 1L
for (s in unique(cpl3$site)) {
    k <- sum(cpl3$site == s)
    cpl3$cell_nr[cpl3$site == s] <- 1L:k
}

N <- nrow(cpl3) # number of cells
n <- max(cpl3$site) # number of sites

## first digit: s (small cell) or d (directional, large), digit 2-5 site, last digit cell-id
cpl3$Cell_name <- as.factor(paste0(ifelse(cpl3$indoor, "s", "d"), as.hexmode(sample(4096:65535, size = n))[cpl3$site], letters[cpl3$cell_nr]))

## add offset noise
cpl3$offx <- pmax(-300, pmin(300, rnorm(n, mean = 0, sd = 50)[cpl3$site]))
cpl3$offy <- pmax(-300, pmin(300, rnorm(n, mean = 0, sd = 50)[cpl3$site]))
cpl3$offz <- pmax(-10, pmin(10, rnorm(n, mean = 0, sd = 5)[cpl3$site]))
cpl3$offdir <- round(runif(n, min = -30, 30)[cpl3$site])

cpl3$x <- cpl3$x + cpl3$offx
cpl3$y <- cpl3$y + cpl3$offy
cpl3$z <- cpl3$z + cpl3$offz
cpl3$direction <- cpl3$direction + cpl3$offdir



qtm(x)




qtm(NLD_muni, fill = "pop_est")



region_ZL <- region %>%
    filter(region$GM_CODE %in% paste0("GM", NLD_zl$code))

qtm(region_ZL)

