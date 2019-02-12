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
#zl <- st_union(gem_ZL)


ZL_muni <- gem_ZL %>% select(GM_CODE, GM_NAAM, AANT_INW) %>%
    rename(muni_code = GM_CODE, muni_name = GM_NAAM, population = AANT_INW)


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
# library(osmdata)
# bbL <- bb(matrix(ZL_bbox, ncol=2), current.projection = st_crs(zl)$proj4string, projection = "longlat")
# q1 <- opq(bbL)
# q2 <- add_osm_feature(q1, key="natural", value="water")
# q3 <- add_osm_feature(q1, key="waterway", value="canal")
# osm2 <- osmdata_sf(q2)
# osm3 <- osmdata_sf(q3)
# osm <- c(osm3$osm_polygons$geometry, osm2$osm_polygons$geometry, osm2$osm_multipolygons$geometry)
# osm <- osm[as.numeric(st_area(osm))>(200^2)]
#
# st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
# zl2 <- st_erase(zl, st_transform(osm, crs= st_crs(zl)))


save(ZL_muni, file = "../mobloc/data/ZL_muni.rda", compress = "xz")


####### Generate normal antenna locations

set.seed(1234)
wijk_ZL$pop <- round(wijk_ZL$AANT_INW / 6000)
ZL_cellplan_normal <- st_sample(wijk_ZL[wijk_ZL$pop!=0,], wijk_ZL$pop[wijk_ZL$pop!=0], type = "regular")


####### Generate small cell locations

set.seed(1242)
buurt_ZL$pop <- round(buurt_ZL$AANT_INW / 6000)
buurt_ZL$small_cell <- buurt_ZL$STED==1 & sample(0:1, size = nrow(buurt_ZL), replace = TRUE, prob = c(.5, .5))

ZL_cellplan_small <- st_sample(buurt_ZL[buurt_ZL$small_cell,], rep(1, sum(buurt_ZL$small_cell)), type = "regular")
qtm(ZL_cellplan_small)



#######  Create antenna data

get_gamma_sample <- function(n, shape, rate, min, max, digits = 0) {
    set.seed(round(rnorm(n + shape * rate))) # to make sure the same sample is generated
    x <- rgamma(n, shape, rate)
    x <- (x - min(x)) / diff(range(x))
    round(x * (max - min) + min, digits = digits)
}

#######  Create height data

if (FALSE) {
    cp <- readRDS("cp.rds") # data distribution taken from a cellplan file cp.rds
    summary(cp)
    plot(sort(cp$height[!cp$small]))
    quantile(cp$height[!cp$small], probs = seq(0, 1, by = 0.01))

    plot(sort(cp$height[cp$small]))
    quantile(cp$height[cp$small], probs = seq(0, 1, by = 0.01))
}

nn <- length(ZL_cellplan_normal)
ns <- length(ZL_cellplan_small)

sample_heights_n <- get_gamma_sample(nn, 5, .5, 10, 100, 2)
quantile(sample_heights_n, probs = seq(0, 1, by = 0.01))
plot(sort(sample_heights_n))

sample_heights_s <- get_gamma_sample(ns, 2, .5, 1, 30, 2)
quantile(sample_heights_s, probs = seq(0, 1, by = 0.01))
plot(sort(sample_heights_s))

#######  Create tilt data

if (FALSE) {
    plot(sort(cp$tilt[!cp$small]))
    plot(cp$tilt, cp$height)
}

sample_tilt <- get_gamma_sample(nn, 2, .5, 1, 15, 0)
quantile(sample_tilt, probs = seq(0, 1, by = 0.01))
plot(sort(sample_tilt))




######### create cp data, and set directions

if (FALSE) {
    temp1 <- cp %>%
        st_set_geometry(NULL) %>%
        mutate(site_id = substr(CELL.NE_ID, 1, nchar(CELL.NE_ID) - 2)) %>%
        group_by(site_id) %>%
        summarize(direction1 = sort(direction - min(direction))[2] - sort(direction - min(direction))[1],
                  direction2 = sort(direction - min(direction))[3] - sort(direction - min(direction))[2]) %>%
        select(site_id, direction1, direction2)

    temp2 <- temp1 %>%
        mutate(dir1 = pmin(direction1, direction2),
               dir2 = pmax(direction1, direction2),
               dir2 = pmin(dir2, 360 - dir2)) %>%
        select(dir1, dir2)

    table(temp2$dir1, temp2$dir2)
}

set.seed(1234)
dir_not120 <- sample(c(T,F), nn, prob = c(.4, .6), replace = TRUE)
dir_diff1 <- (rnorm(nn, mean = 100, sd = 10) %/% 5) * 5
dir_diff2 <- (rnorm(nn, mean = 120, sd = 10) %/% 5) * 5

dir_diff1[!dir_not120] <- 120
dir_diff2[!dir_not120] <- 120

ZL_cellplan_normal_sf <- st_sf(geometry = ZL_cellplan_normal,
                            direction = (round(runif(nn, min = 0, max = 360)) %/% 5) * 5,
                            height = sample_heights_n,
                            tilt = sample_tilt,
                            #beam_h = fixed_beam_h,
                            #beam_v = sample_beam_v,
                            site = paste(toupper(substr(gem_ZL$GM_NAAM[unlist(st_intersects(ZL_cellplan_normal, gem_ZL))], 1, 3)), round(runif(nn, min = 100, max = 999)), "N", sep = "_"),
                            small = FALSE) %>%
    mutate(antenna = paste0(site, 1))

ZL_cellplan_normal_sf2 <- ZL_cellplan_normal_sf %>%
    mutate(direction = direction + dir_diff1,
           antenna = paste0(site, 2))

ZL_cellplan_normal_sf3 <- ZL_cellplan_normal_sf %>%
    mutate(direction = direction + dir_diff1 + dir_diff2,
           antenna = paste0(site, 3))

ZL_cellplan_normal_sf_v2 <- rbind(ZL_cellplan_normal_sf, ZL_cellplan_normal_sf2, ZL_cellplan_normal_sf3) %>%
    mutate(direction = direction %% 360,
           site = NULL)


#######  Create beam data

if (FALSE) {
    plot(sort(cp$beam_h[!cp$small]))
    plot(sort(cp$beam_v[!cp$small]))

    plot(sort(cp$beam_v[!cp$small]), cp$height[!cp$small])

}
fixed_beam_h <- 65
set.seed(1234)
sample_beam_v <- sample(c(4, 7.5, 9, 14), nn * 3, prob = c(.1, .3, .5, .1), replace = TRUE)
plot(sort(sample_beam_v))

ZL_cellplan_normal_sf_v2 <- ZL_cellplan_normal_sf_v2 %>%
    mutate(beam_h = fixed_beam_h,
           beam_v = sample_beam_v)


ZL_cellplan_small_sf <- st_sf(geometry = ZL_cellplan_small,
                              direction = NA,
                              height = sample_heights_s,
                              tilt = NA,
                              small = TRUE,
                              antenna = paste(toupper(substr(gem_ZL$GM_NAAM[unlist(st_intersects(ZL_cellplan_small, gem_ZL))], 1, 3)), round(runif(ns, min = 100, max = 999)), "S1", sep = "_"),
                              beam_h = NA,
                              beam_v = NA)
    mutate(

    )


ZL_cellplan <- rbind(ZL_cellplan_normal_sf_v2, ZL_cellplan_small_sf) %>%
    select(antenna, small, height, direction, tilt, beam_h, beam_v) %>%
    arrange(antenna)

# filter antennas that are inside land
it <- sapply(st_intersects(ZL_cellplan, ZL_land), length)
ZL_cellplan <- ZL_cellplan[it==1, ]

save(ZL_cellplan, file = "../mobloc/data/ZL_cellplan.rda", compress = "xz")
