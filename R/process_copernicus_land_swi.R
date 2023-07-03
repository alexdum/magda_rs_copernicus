library(terra)
#library(sf)
library(dplyr)
library(RCurl)
setwd("~/magda_rs_copernicus")
#tab <- read.table("https://land.copernicus.vgt.vito.be/manifest/swi_v1_1km/manifest_cgls_swi_v1_1km_20230504.txt")
# write without auxiliary file
setGDALconfig("GDAL_PAM_ENABLED", "FALSE")
files <- list.files("/data/MTDA/BIOPAR/BioPar_SWI1km_V1_Global/", recursive = T, full.names = T, pattern = ".nc$")
dats_all <- strsplit(files, "1km_|_CEURO") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")

# selecteaza versiuns V1.0.2 cand se suprapun
dats_duplicat <- dats_all[duplicated(dats_all)]
files_duplicat <- 
  files[dats_all %in% dats_duplicat] %>%
  grep("V1.0.2", ., value = T)
# aranjeaza fisierele astefel incat sa fie fara duplicate
files_final <- 
  files[!dats_all %in% dats_duplicat] %>%
  c(files_duplicat) %>% sort()

dats_posib <- seq(as.Date("2015-01-01"), as.Date("2023-05-06"), "days")
dats_posib[!dats_posib %in% dats_all]


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

for (i in 1:length(files_final)) {
  # fromateaza data
  day <- strsplit(files_final[i], "1km_|_CEURO")[[1]][5] %>% substr(1,12) %>% as.POSIXct("%Y%m%d%H%M", tz = "UTC")
  print(day)
  # incepe procesarea
  r <- rast(files_final[i])
  
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
    
    r_utm <- project(r.sub, crss[[e]], res = 1000) #|>
    #crop(vect(ctry))
    # pentru evitare scriere fisiere auxiliare
    time(r_utm) <- NULL
    
    for (n in 1:length(names_swi)) {
      
      names_r <- names_swi[n]
      ctry <- ctry_longname(names(exts[e]))
      
      r.nam <- r.sub[[names_r]]
      
      # plot(mean(r_utm, na.rm = T))
      # plot(as_Spatial(ctry_utm), add = T)
      # summary(minmax(r_utm)[2,])
      # summary(minmax(r_utm)[1,])
      file_name <- paste0(names(r_utm[[names_r]]) %>% gsub("_","",.), "_", format(day, "%Y%m%d%H%M"))
      file_path <- paste0("grids/SWI/",names(exts[e]),"/", format(day, "%Y"))
      if (!dir.exists(file_path)) dir.create(file_path, recursive = T)
      writeRaster(r_utm[[names_r]], paste0(file_path,"/", file_name, ".tif"), overwrite = T)
      
      # RCurl::ftpUpload(
      #   paste0("),
      #   paste0("ftp://213.127.133.58/MAGDA/input/",ctry,"/Dynamic/RS/SWI/", file_name, "-XX-XX.tif"),
      #   userpwd = "MAGDA:FW@MAGDA_RS"
      # )
      # system(paste0("curl --user 'MAGDA:FW@MAGDA_RS' -T ncs/SWI/",names(exts[e]),"/", file_name, "-XX-XX.tif ftp://MAGDA_RS@213.127.133.58/MAGDA/input/",ctry,"/Dynamic/RS/SWI/", file_name, "-XX-XX.tif"))
    }
  }
}



ctry <- c("fr","it","ro")

for (i in 1:length(ctry)) {
  

  if (ctry[i] == "fr") {
    ctry_long <- "France"
  } else if (ctry[i] == "it") {
    ctry_long <- "Italy"
  } else {
    ctry_long <- "Romania"
  }
  
  print(c(ctry[i], ctry_long))
  system(paste0("zip -r  grids/SWI/",ctry[i], ".zip grids/SWI/",ctry[i]))
  
  RCurl::ftpUpload(
    paste0("grids/SWI/",ctry[i], ".zip"),
    paste0("ftp://213.127.133.58/MAGDA/input/",ctry_long,"/Dynamic/RS/SWI/",ctry[i], ".zip"),
    userpwd = "MAGDA:FW@MAGDA_RS"
  )
}

# # upload to ftp


# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/fr/  /mnt/Z//MAGDA/input/France/Dynamic/RS/SWI')  
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/ro/  /home/rstudio/fw_ftp/MAGDA/input/Romania/Dynamic/RS/SWI')
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/it/  /home/rstudio/fw_ftp/MAGDA/input/Italia/Dynamic/RS/SWI')


