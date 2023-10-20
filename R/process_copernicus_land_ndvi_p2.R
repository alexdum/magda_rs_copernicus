library(terra)
#library(sf)
library(dplyr)
setwd("~/magda_rs_copernicus")
#source("R/fun/write_netcdf.R")
#tab <- read.table("https://land.copernicus.vgt.vito.be/manifest/swi_v1_1km/manifest_cgls_swi_v1_1km_20230504.txt")
# write without auxiliary file

fr <- ext(4.611391,4.943384,46.76524,47.10746)
fr_utm <- vect("shp/fr_L1_roi.shp")
fr_geo <- project(fr_utm, "EPSG:4326")
ro <- ext(27.66815,28.09678, 45.00853,45.34393) 
ro_utm <- vect("shp/ro_L1_roi.shp")
ro_geo <- project(ro_utm, "EPSG:4326")
it <- ext(7.3786,7.806193, 44.30032,44.62541)
it_utm <- vect("shp/it_L1_roi.shp")
it_geo <- project(it_utm, "EPSG:4326")


# zone de interes si proiectii
exts <- list(fr = fr, ro = ro, it = it)
crss <- list(fr = crs(fr_utm), ro = crs(ro_utm), it = crs(it_utm))

unlink(list.files("tmp", full.names = T))

# functie pentru extragere nume tara sis citere fisiere
ctry_longname <- function(ctry) {
  if (ctry == "fr") {
    ctry_long <- "France"
  } else if (ctry == "it") {
    ctry_long <- "Italy"
  } else {
    ctry_long <- "Romania"
  }
  return(ctry_long)
}




files2 <- list.files("/data/MTDA/BIOPAR/BioPar_NDVI300_V2_Global/", recursive = T, full.names = T, pattern = ".nc$")
dats2_all <- strsplit(files2, "300_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")

files <- c(files2)
dats_all <- strsplit(files, "300_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")
dats_all[duplicated(dats1_all)] 
# selecteaza doar pe cele din ziua 1, care reprezinta ndvi pentru ultimele 30 de zile

# elimina duplicatele si alege ultima versiunea superioara a fisierelor
versions <- 
  strsplit(files, "_V|/c_") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(4) |>
  unlist() %>% gsub("\\.", "",.) |> as.numeric() |> bind_cols(dats_all) |> 
  rename(version = 1, date = 2) |>
  mutate(cod = paste(date, version))

versions_max <- 
  versions |>
  group_by(date) |> summarise(max = max(version)) |>
  mutate(cod = paste(date, max)) |>
  filter(date >= as.Date("2023-01-01")) # selecteaza fisierele dupa anul 2015

files_sub <- files[versions$cod %in% versions_max$cod]
dats_sub <- strsplit(files_sub, "NDVI300_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")
length(unique(dats_sub))
summary(dats_sub)



for (i in 1:length(files_sub)) {
  # fromateaza data
  day <- dats_sub[i]
  print(day)
  # incepe procesarea
  r <- rast(files[i], subds = "NDVI")
  
  # pentru fiecare zona
  for (e in 1:length(exts)) {
    # transformarea de scare se face automat - se pare
    r.sub <- crop(r, exts[[e]]) #* 0.5
    #r.sub[r.sub %in% c(241, 242, 251, 252, 253, 254)] <- NA
    
    # ctry_utm <-  vect(paste0("shp/",names(exts[e]),"_L1_roi.shp"))
    # print(show( ctry_utm ))
    # ctry_geo <- project(ctry_utm, "EPSG:4326")
    # #plot(mean(r.sub, na.rm = T))
    # #plot(ctry_geo, add = T)
    
    r_utm <- project(r.sub, crss[[e]], res = 300) #|>
    #crop(vect(ctry))
    # pentru evitare scriere fisiere auxiliare
    time(r_utm) <- NULL
    
    
    
    ctry <- ctry_longname(names(exts[e]))
    
    file_name <- paste0("NDVI_", format(day, "%Y%m%dXX"))
    file_path <- paste0("grids/NDVI/",names(exts[e]),"/", format(day, "%Y"))
    if (!dir.exists(file_path)) dir.create(file_path, recursive = T)
    writeRaster(r_utm, paste0(file_path,"/", file_name, ".tif"), overwrite = T)
    
    
    # RCurl::ftpUpload(
    #   paste0("),
    #   paste0("ftp://213.127.133.58/MAGDA/input/",ctry,"/Dynamic/RS/SWI/", file_name, "-XX-XX.tif"),
    #   userpwd = "MAGDA:FW@MAGDA_RS"
    # )
    # system(paste0("curl --user 'MAGDA:FW@MAGDA_RS' -T ncs/SWI/",names(exts[e]),"/", file_name, "-XX-XX.tif ftp://MAGDA_RS@213.127.133.58/MAGDA/input/",ctry,"/Dynamic/RS/SWI/", file_name, "-XX-XX.tif"))
  }
}





r <- rast(list.files("grids/NDVI/fr/", full.names = T, pattern = ".tif$", recursive = T))
summary(as.vector(values(r)))
