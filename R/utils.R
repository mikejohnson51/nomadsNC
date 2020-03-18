#' @title make NOMADS url from date and configuration
#' @param date a data in form (YYY-MM-DD)
#' @param type a NWM configuration
#' @return a url path
#' @export

make_url = function(date, type){
  base =  "http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/prod/nwm."
  paste0(base, gsub("-", "", date), "/", type, "/")
}

#' @title Error Checking for confirugartion/ensemble pairs
#' @param type a NWM configuration
#' @param ensemble an ensemble member number
#' @return a kosher configuration
#' @export

error_checking = function(type, ensemble){
  
  current_type =  c('analysis_assim', 
                    'analysis_assim_extend',
                    'analysis_assim_long',
                    'analysis_assim_hawaii',
                    'long_range',
                    'medium_range',
                    'short_range',
                    'short_range_hawaii')
  
  if(!type %in% current_type){
    stop("type must be one of:\n\t ", paste(current_type, collapse = ",\n\t "), call. = F)
  }
  
  if(type %in% c('long_range', 'medium_range')){
    if(is.null(ensemble)){
      stop("Ensemble member needed for ", type, ": member 1 selected", call. = F)
      ensemble <- 1
    }
    
    if(type == "medium_range"){
      check = dplyr::between(ensemble, 1,4)
      if(!check){
        stop('Only 4 medium range ensembles available', call. = F)
      }
    }
    
    if(type == "long_range"){
      check = dplyr::between(ensemble, 1,7)
      if(!check){
        stop('Only 7 long range ensembles available', call. = F)
      }
    }
    
    type = paste0(type, "_mem", ensemble)
  }
  
  return(type)
  
}

#' @title Define 'f'
#' @param type a NWM configuration
#' @param files a file list
#' @param domain a domain (conus or hawaii)
#' @return a vector of f values.
#' @export

find_f = function(type, files, domain){
  if(grepl("analysis_assim", type)){
    pattern = paste0('^.*tm\\s*|\\s*.', domain ,'.*$')
  } else {
    pattern = paste0('^.*f\\s*|\\s*.', domain ,'.*$')
  }
  as.numeric(gsub(pattern, '', files))
}

#' @title Build file metadata for most current NWM configuration
#' @param type a NWM configuration
#' @param ensemble an ensemble member number
#' @param num a number of files to download, default = all
#' @return a list of meta.data information
#' @export

get_nomads_filelist = function(type = NULL,
                               ensemble = NULL,
                               num = 6) {
  
  
  type     <-  error_checking(type, ensemble)
  ensemble <-  paste0("_mem", ensemble)
  
  if(grepl("hawaii", type)){
    domain = 'hawaii'
    base.type = gsub("_hawaii", "", type)
  } else {
    domain = "conus"
    base.type = type
  }
  
  tmp = 'https://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/prod/'
  
  avail.days = grep("nwm\\.", readLines(tmp), value = TRUE)
  avail.days = gsub(".*[m.]([^.]+)[<].*", "\\1", avail.days)
  date = max(as.Date(gsub("/", "", avail.days), format = "%Y%m%d"))

  base.url <- make_url(date, type)
  files    <- suppressMessages(readLines(base.url, warn = FALSE))

  filenames = regmatches(files, gregexpr('(\").*?(\")', files, perl = TRUE))
  filenames = filenames[grep(paste0(gsub(ensemble, "", base.type),".channel"), filenames)] 
  fileList = gsub("^\"|\"$", "", filenames)

  if(is.null(num)){num = 100000}
  
  all = data.frame(files = fileList, stringsAsFactors = FALSE) %>% 
    dplyr::arrange(dplyr::desc(files)) %>% 
    dplyr::mutate(t = gsub('^.*m.t\\s*|\\s*z.*$', '', files),
                  f = find_f(type, files, domain)) %>% 
    dplyr::filter(as.numeric(t) == as.numeric(max(t))) %>% 
    dplyr::arrange(f) %>% 
    dplyr::slice(c(1:num)) %>% 
    dplyr::mutate(files = paste0(make_url(date, type), files)) %>% 
    dplyr::mutate(time = as.POSIXct(paste(date, t), "%Y-%m-%d %H", tz = "GMT") + f*3600) %>%  dplyr::mutate(unix = as.numeric(time))
  
  return(
    list(type = type,
         date = date,
         startTime = timeOrigin,
         unix = as.numeric(dd),
         urls = all$files)
    )
}

#' @title Download NOMADs files
#'
#' @param fileList return from \code{get_nomads_filelist}
#' @param dir a directory to write data to
#' @return a list of meta.data information
#' @export

download_nomads = function(fileList = NULL, dir = NULL){
  
  if (!dir.exists(dir)) { dir.create(dir, recursive = T) }
  
  fileList$local = file.path(dir, basename(fileList$urls))
  
  for(i in seq_along(urls)){
    
    if (!file.exists(outfiles[i])) {
      
      message("Downloading ", basename(fileList$urls[i]))
      resp <-  httr::GET(fileList$urls[i],
                         httr::write_disk(fileList$outfiles[i], overwrite = TRUE),
                         httr::progress())
      
      if (resp$status_code != 200) {
        stop("Download unsuccessfull :(")
      }
    } else {
      message(basename(fileList$urls[i]), " already exisits")
    }
  }

  return(fileList)
}