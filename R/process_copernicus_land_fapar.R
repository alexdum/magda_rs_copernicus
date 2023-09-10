library(terra)
#library(sf)
library(dplyr)
setwd("~/magda_rs_copernicus/")

source("R/fun/write_netcdf.R")
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
# variabile de interes
names_swi <- c("SWI_002",  "SWI_005", "SWI_010", "SWI_015",  "SWI_020", "SWI_040", "SWI_060", "SWI_100")

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

# # reluare de unde s-a oprit
# dats_all <- strsplit(files_final, "1km_|_CEURO") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
#   unlist() |> substr(1,8) |> as.Date("%Y%m%d")
# tifs <- list.files("grids/SWI/ro", pattern = "tif$", recursive = T) |> substr(13, 20) |> as.Date("%Y%m%d")
# 
# files_final <- files_final[!dats_all %in% tifs]


files1 <- 
  list.files("/data/MTDA/BIOPAR/BioPar_FAPAR300_V1_Global/", recursive = T, full.names = T, pattern = ".nc$") %>%
  grep("GLOBE_PROBAV_",., value = T) %>% grep("RT",., value = T, invert = T)
dats1_all <- strsplit(files1, "300_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")
files1 <- files1[dats1_all >= as.Date("2015-01-01") & dats1_all < as.Date("2016-09-10")]
dats1_all <- strsplit(files1, "300_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")

files2 <- 
  list.files("/data/MTDA/BIOPAR/BioPar_FAPAR300_V1_Global/", recursive = T, full.names = T, pattern = ".nc$") %>%
  grep("GLOBE_PROBAV_",., value = T) %>% grep("RT0",., value = T)
dats2_all <- strsplit(files2, "RT0_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(4) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")
files2 <- files2[dats2_all <  as.Date("2020-10-20")]
dats2_all <- strsplit(files2, "RT0_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(4) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")

files3 <- 
  list.files("/data/MTDA/BIOPAR/BioPar_FAPAR300_V1_Global/", recursive = T, full.names = T, pattern = ".nc$") %>%
  grep("GLOBE_OLCI_",., value = T)  %>% grep("RT0",., value = T) 
dats3_all <- strsplit(files3, "RT0_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(4) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")




files <- c(files1, files2,files3)
dats_all <- c(dats1_all, dats2_all, dats3_all)

files <- files[!duplicated(dats_all)]
dats_all <- strsplit(files, "/") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(8) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")
# selecteaza doar pe cele din ziua 1, care reprezinta ndvi pentru ultimele 30 de zile

for (i in s:length(files)) {
  # fromateaza data
  day <- dats_all[i]
  print(day)
  # incepe procesarea
  r <- rast(files[i], subds = "FAPAR")
  
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
    
    file_name <- paste0("FAPAR_", format(day, "%Y%m%dXX"))
    file_path <- paste0("grids/FAPAR/",names(exts[e]),"/", format(day, "%Y"))
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





r <- rast(list.files("grids/FAPAR/fr/", full.names = T, pattern = ".tif$", recursive = T))
summary(as.vector(values(r)))





# # upload to ftp
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/fr/  /mnt/Z//MAGDA/input/France/Dynamic/RS/SWI')  
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/ro/  /home/rstudio/fw_ftp/MAGDA/input/Romania/Dynamic/RS/SWI')
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/it/  /home/rstudio/fw_ftp/MAGDA/input/Italia/Dynamic/RS/SWI')


