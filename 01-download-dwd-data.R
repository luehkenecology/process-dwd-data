dwd <- function(dwd_var = "wind",
                x_coordinates = c(9.000461),
                y_coordinates = c(50.13213),
                ids = c("A"),
                from_date = "2017-03-01",
                to_date = "2017-10-31"){

  # download station info----------------------------------------------------------------------
  # temporary directory
  td = tempdir()
  
  # create temporary file
  tf = tempfile(tmpdir=td, fileext=".txt")
  
  # get all URLs of files (different directories for solar)
  if(dwd_var == "solar"){
    station_url <- getURL(paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/",dwd_var, "/", sep = ""),
                          verbose=TRUE,ftp.use.epsv=TRUE,
                          dirlistonly = TRUE) 
  } else {
    station_url <- getURL(paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/",dwd_var,"/recent/", sep = ""),
                          verbose=TRUE,ftp.use.epsv=TRUE,
                          dirlistonly = TRUE) 
  }
  
  # merge all urls into one file
  station_url_2 <- unlist(strsplit(as.character(station_url), "\r\n"))
  
  # get url of the txt-file with station information
  station_url_3 <- station_url_2[(str_sub(unlist(station_url_2), -3, -1) == "txt")==T]
  
  # download txt-file with station information
  if(dwd_var == "solar"){
    download.file(paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/", dwd_var,"/", station_url_3, sep = ""), tf)
  } else {
    download.file(paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/", dwd_var, "/recent/", station_url_3, sep = ""), tf)
  }
  
  # read txt-file with station information
  station_infos <- read.table(tf, sep = "\t")
  
  # extract information from txt-file with station information
  station_infos_2 <- lapply(1:nrow(station_infos), function(x) strsplit(as.character(station_infos[x,]), "\\s+")[[1]])
  station_infos_3 <- do.call(rbind, lapply(station_infos_2, function(x) x[1:6]))
  station_infos_4 <- data.frame(station_infos_3[-c(1,2),])
  dimnames(station_infos_4)[[2]] <- station_infos_3[1,]
  
  station_infos_4[,2] <- as.numeric(unlist(lapply(station_infos_4[,2], as.vector)))
  station_infos_4[,3] <- as.numeric(unlist(lapply(station_infos_4[,3], as.vector)))
  station_infos_4[,4] <- as.numeric(unlist(lapply(station_infos_4[,4], as.vector)))
  station_infos_4[,5] <- as.numeric(unlist(lapply(station_infos_4[,5], as.vector)))
  station_infos_4[,6] <- as.numeric(unlist(lapply(station_infos_4[,6], as.vector)))
  
  if(dwd_var == "solar"){
    recent_urls <- getURL(paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/",dwd_var,"/", sep = ""),
                          verbose=TRUE,ftp.use.epsv=TRUE,
                          dirlistonly = TRUE) 
    recent_urls_2 <- unlist(strsplit(as.character(recent_urls), "\r\n"))
    recent_urls_3 <- recent_urls_2[(str_sub(unlist(recent_urls_2), -3, -1) == "zip")==T]
    
  }else{
    recent_urls <- getURL(paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/",dwd_var,"/recent/", sep = ""),
                          verbose=TRUE,ftp.use.epsv=TRUE,
                          dirlistonly = TRUE) 
    recent_urls_2 <- unlist(strsplit(as.character(recent_urls), "\r\n"))
    recent_urls_3 <- recent_urls_2[(str_sub(unlist(recent_urls_2), -3, -1) == "zip")==T]
    
    historic_urls <- getURL(paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/",dwd_var, "/historical/", sep = ""),
                            verbose=TRUE,ftp.use.epsv=TRUE,
                            dirlistonly = TRUE) 
    historic_urls_2 <- unlist(strsplit(as.character(historic_urls), "\r\n"))
    historic_urls_3 <- historic_urls_2[(str_sub(unlist(historic_urls_2), -3, -1) == "zip")==T]
  }
  
  # get urls of data files
  if(dwd_var == "solar"){
    # extract station id from the download path
    recent_urls_3_data <- data.frame(Stations_id = str_sub(recent_urls_3, -13, -9),
                                     url = recent_urls_3)
    
    station_info_merge_all_3 <- merge(station_infos_4, recent_urls_3_data, by = "Stations_id", all.y = T)
    
  } else{
    
    # extract station id from the download path
    recent_urls_3_data <- data.frame(Stations_id = str_sub(recent_urls_3, -13, -9),
                                     url = recent_urls_3)
    historic_urls_3_data <- data.frame(Stations_id = str_sub(historic_urls_3, -32, -28),
                                       url = historic_urls_3)
    
    station_info_merge_recent <- merge(station_infos_4, recent_urls_3_data, by = "Stations_id", all.y = T)
    station_info_merge_historic <- merge(station_infos_4, historic_urls_3_data, by = "Stations_id", all.y = T)
    station_info_merge_all <- merge(station_info_merge_recent,
                                    station_info_merge_historic,
                                    by = "Stations_id", all = T)
    station_info_merge_all_2 <- station_info_merge_all[!(is.na(station_info_merge_all[,8])),]
    station_info_merge_all_3 <- station_info_merge_all_2[,c(1,8:12,7,13)]
    
    station_info_merge_all_3[,7] <- paste("recent/", station_info_merge_all_3[,7], sep = "")
    station_info_merge_all_3[,8] <- paste("historical/", station_info_merge_all_3[,8], sep = "")}
  
  # convert all start dates
  station_info_merge_all_3$start_date <- as.POSIXlt(as.Date(paste(str_sub(station_info_merge_all_3[,2], 1, 4),
                                                                  str_sub(station_info_merge_all_3[,2], 5, 6), 
                                                                  str_sub(station_info_merge_all_3[,2], 7, 8),
                                                                  sep="-")),
                                                    format = "%Y-%m-%d")
  
  # convert all end dates
  station_info_merge_all_3$end_date <- as.POSIXlt(as.Date(paste(str_sub(station_info_merge_all_3[,3], 1, 4),
                                                                str_sub(station_info_merge_all_3[,3], 5, 6), 
                                                                str_sub(station_info_merge_all_3[,3], 7, 8),
                                                                sep="-")),
                                                  format = "%Y-%m-%d")
  
  # subset available data within time frame
  station_info_merge_all_4 <- station_info_merge_all_3[(station_info_merge_all_3$start_date <= from_date)*
                                                         (station_info_merge_all_3$end_date >= to_date) == T,]
  
  # file with coordinate information of each sampling site
  new.pos <- cbind(y_coordinates,
                   x_coordinates)
  
  # identify stations in minimal distance----------------------------------------------------------------------
  # calculate minimal distance to each station
  station_to_download <- apply(new.pos, 1, function(x) as.numeric(minDist(station_info_merge_all_4[,5:6], x)))
  
  # link the station names to download for subsetting in the loop
  coordinates$station_to_download <- station_to_download
  
  # extract the station ids
  station_to_download_ids <- as.numeric(names(table(station_to_download)))
  
  # extract how often each station has to be downloaded
  station_to_download_val <-as.vector(table(station_to_download))
  
  # extract gps info of station
  gps_info_station <- data.frame(ID = ids,
                                 x = station_info_merge_all_4[station_to_download,6],
                                 y = station_info_merge_all_4[station_to_download,5],
                                 distance =   unlist(lapply(1:nrow(new.pos), function(x) round(spDistsN1(pts = as.matrix(station_info_merge_all_4[,5:6]), new.pos[x,], longlat=T)[station_to_download[x]], 2))),
                                 cx = x_coordinates,
                                 cy = y_coordinates)
  
 
   
  # data.frame for saving from loop to loop
  result_file <- data.frame()
  
  # loop through the coordinates
  for(i in 1:length(station_to_download_ids)){
    # just for script testing i<-1
    
    # temporary directory
    td = tempdir()
    
    # create temporary file
    tf = tempfile(tmpdir = td, fileext=".zip")
    
    # download the file
    download.file(paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/", dwd_var, "/",
                        (ifelse(dwd_var == "solar", as.character(station_info_merge_all_4$url[station_to_download_ids[i]]), 
                                as.character(station_info_merge_all_4$url.x[station_to_download_ids[i]]))), 
                        sep=""), tf)
    
    # unzip the file to extract the file names in the zip
    unzf <- unzip(tf,  exdir = td)
    
    # unzip the data file in the zip
    e <- unzip(tf,
               files= str_sub(unzf[length(unzf)], -45),
               exdir = td, overwrite=T)
    
    # identify the file.path of the unzipped data file
    fpath = file.path(td, str_sub(unzf[length(unzf)], -45))
    
    # read the data file  
    rdata <- read.table(fpath, sep = ";", header = T)
    
    # date to POSIXct-format (solar data have a different format)
    if(dwd_var == "solar"){
      
      # date to POSIXct
      rdata$date <- as.POSIXct(as.character(str_sub(as.character(rdata$MESS_DATUM), 1,10)), 
                               format = "%Y%m%d%H") 
    }else{
      
      # date to POSIXct
      rdata$date <- as.POSIXct(as.character(rdata$MESS_DATUM), 
                               format = "%Y%m%d%H")  
    }
    
    # built subset for sites within this loop with the same weather station
    coordinates_sub <- coordinates[coordinates$station_to_download == station_to_download_ids[i],]
    
    # merge data with site ID for future merging
    for(y in 1:station_to_download_val[i]){
      
      # if/else becasue only for air_temperature with want to extract temperature for the hour before 
      if(dwd_var == "air_temperature"){
        # add site ID
        rdata$ID <- coordinates_sub$ID[y]
        
        # extract temperature for the hour before
        rdata$before <- c(NA, rdata$TT_TU[1:length(rdata$TT_TU)-1])  
        
        # merge data
        result_file <- rbind(result_file, rdata)
        
      }else{
        
        # add site ID
        rdata$ID <- coordinates_sub$ID[y]
        
        # merge data
        result_file <- rbind(result_file, rdata)
      }
    }
    
    # save results
    write.table(result_file, paste("output/", dwd_var, ".csv", sep = ""), sep = ";", row.names = F)
    
    # progress
    print(i)
    
    # return list
    return(list(gps_info_station = gps_info_station,
         result_file = result_file))
  }
}

eg <-    list(gps_info_station = gps_info_station,
       result_file = result_file)

eg <- dwd(dwd_var = "air_temperature")
write.table(eg[[1]], paste("output/", "air_temperature", "_GPS.csv"),sep=";")

# plot map with sampling sites-weather stations connected
png(paste("figs/", dwd_var, "_map.png"),width = 6, height=5, units = 'in', res = 1000)
plot(d_shp, main = paste(dwd_var))
points(gps_info_station[,2:3], pch = 19)
points(gps_info_station[,5:6], pch = 19, col = "red")

for(z in 1:nrow(gps_info_station)){
  cc <- Lines(list(Line(rbind(as.numeric(gps_info_station[z,2:3]),
                              as.numeric(gps_info_station[z,5:6])))), ID ="a")
  m.sl = SpatialLines(list(cc))
  plot(m.sl, add = T)
}
dev.off()


dwd(dwd_var = "solar")
dwd(dwd_var = "wind")
dwd(dwd_var = "precipitation")