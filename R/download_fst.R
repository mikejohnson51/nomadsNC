#' @title Download and format NWM data into a FST flow matrix
#' @param type a NWM configuration
#' @param num the number of files to get
#' @param ensemble the ensemble number (medium and long range)
#' @param dstfile the location of the output file
#' @return a file path
#' @importFrom fst write.fst
#' @importFrom stats na.omit setNames
#' @importFrom RNetCDF open.nc close.nc var.get.nc
#' @export

create_nomads_fst = function(type = "short_range", num = 10000, 
                         ensemble = NULL, dstfile = NULL) {
  
  fileList = get_nomads_filelist(type = type, num  = num, ensemble = ensemble)
  
  if(file.exists(dstfile)){file.remove(dstfile)}
  
  dir      = paste0(dirname(dstfile), "/")
  
  fileList = download_nomads(fileList, dir = dir)
  
  times    = paste0('t_', as.POSIXct(fileList$unix, origin = "1970-01-01"))
  
  extract_var = function(x, var){ 
     nc   = open.nc(x)
     vals = var.get.nc(nc, var, unpack = TRUE)
     close.nc(nc)
     vals
    }

 v = lapply(fileList$local, extract_var, var = "streamflow") %>% 
   dplyr::bind_cols()

 df = cbind(extract_var(fileList$local[1], "feature_id"), v)  %>% 
   na.omit() %>% 
   setNames(c('COMID', times))
 
 file.remove(fileList$local)
 
 fst::write.fst(df, path = dstfile, compress = 100)
 
 return(dstfile)
}

