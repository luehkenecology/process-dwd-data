down_unzip_dwd <- function(url_var, dwd_var){
  # temporary directory
  td = tempdir()
  
  # create temporary file
  tf = tempfile(tmpdir = td, fileext=".zip")
  
  # download the file
  download.file(paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/", dwd_var, "/",
                      url_var, 
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
  return(read.table(fpath, sep = ";", header = T))
}