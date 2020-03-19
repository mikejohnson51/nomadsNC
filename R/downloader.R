#' @title Download and format NWM data into a timesubsetable NetCDF
#' @param type a NWM configuration
#' @param num the number of files to get
#' @param ensemble the ensemble number (medium and long range)
#' @param dstfile the location of the output file
#' @return a file path
#' @export

create_nomads_nc = function(type = "short_range", 
                         num = 10000, 
                         ensemble = NULL, 
                         dstfile = NULL) {
  
  fileList = get_nomads_filelist(type = type,
                                 num  = num,
                                 ensemble = ensemble)
  
  if(file.exists(dstfile)){file.remove(dstfile)}
  
  dir      = paste0(dirname(dstfile), "/")
  
  fileList = download_nomads(fileList, dir = dir)
  
  in_files   = fileList$local
  comid.file = paste0(dir, "comids.nc")
  
  # Extract COMIDs to new file
  new.comid.file = paste('ncks -O -v feature_id', in_files[1], comid.file)
  message("Extracting COMID file...")
    system(new.comid.file)
  
  # Loop over files extracting streamflow, adding time dimininson, making it record dim
    message("Extracting streamflow and adding time...")
    for (i in 1:length(in_files)) {
    extract.streamflow = paste('ncks -O -4 -L 1 --cnk_plc=all --cnk_map=dmn -C -v streamflow',
                               in_files[i],
                               in_files[i])
    
      system(extract.streamflow)
    
    add.time = paste0(
      'ncap2 -O --cnk_plc=uck -s ',
      "'defdim(",
      '"time",1);time[time]=',
      fileList$unix[i],
      ';streamflow[time,feature_id]=streamflow;time@units="Seconds since 1970-01-01 00:00:00 UTC"',
      "' ",
      in_files[i],
      " ",
      in_files[i]
    )
    
      system(add.time, intern = TRUE)
    
    make.time.record = paste('ncks -O --mk_rec_dmn time', in_files[i], in_files[i])
    
      system(make.time.record, intern = TRUE)
  }
  
  # Concate all NC file by time diminsion
    message("Concatinating files...")
  concat.files = paste('ncrcat', paste(in_files, collapse = " "), dstfile)
    system(concat.files, intern = TRUE)
    file.remove(in_files)

  paste("ncap2 -O -s'feature_id[$feature_id]=1234'", dstfile, dstfile) %>% system()
    
  message("Fill COMID values...")
  add.comid.values = paste('ncks -A -v feature_id', comid.file, dstfile)
    system(add.comid.values, intern = TRUE)
  
  ## REMOVE COMIDS
  
    file.remove(comid.file)
  
    message("Rechunk and pack file...")
  rechunk = paste0(
    'ncks -O --cnk_plc=g2d --cnk_dmn feature_id,10000 --cnk_dmn time,',
    length(fileList$local),
    ' --deflate 0 ',
    dstfile,
    " ",
    dstfile
  )
  
    system(rechunk, intern = T)
  
  packQ = paste("ncap2 -O -s 'streamflow=pack(streamflow,0.01,0);'",
                dstfile,
                dstfile)
  
    system(packQ)
  
    message("Compress file...")
  compress = paste('ncks -4 -L 3 -O', dstfile, dstfile)
    system(compress)
  
  return(dstfile)
  
}
