#' @title A function to download all the selected palaeoclimatic variables from CHELSA
#' @description This function download, extract, and delete downloaded tif files so they do not fill up storage space
#' @return A tibble with the meta data, and climatic data for all locations in extract data
get_chelsa_download <- function(md, skip_existing = TRUE, method = "curl", extract.data = NULL){
  
  dest <- tempdir()
  
  for(i in 1:nrow(md)){
    message(paste("File", i, "of", nrow(md), "..."))
    md$status[i] <- "incomplete"
    md$path[i] <- paste0(dest, "/", basename(md$file[i]))
    
    runs <- c("1", "2", "12")
    
    if(skip_existing){
      # previously-failed downloads have small file size
      paths <- sapply(runs, function(x) sub("\\*", x, md$path[i]))
      size <- file.size(paths)
      if(any(!is.na(size) & log(size)>10)){
        md$path[i] <- paths[!is.na(size) & log(size)>10]
        md$status[i] <- "already done"
        next()
      }
    }
    
    # run numbers vary by model. try all options.
    for(run in runs){
      url <- sub("\\*", run, md$url[i])
      path <- sub("\\*", run, md$path[i])
      r <- try(download.file(url, path, method=method, quiet=T))
      size <- file.size(path)
      if(!is.na(size) & log(size)>10){
        md$url[i] <- url
        md$path[i] <- path
        break()
      }
      file.remove(path)
    }
    
    if(class(r)=="try-error"){
      md$status[i] <- as.character(r)
      next()
    }
    if(file.exists(md$path[i])) md$status[i] <- "download completed"
    
    if(!is.null(extract.data) & file.exists(md$path[i])){
      
      r <- terra::rast(md$path[i]) 
      climate_table <- data.frame(extract.data, terra::extract(r, extract.data)) %>%
        rownames_to_column("dataset_id") %>%
        rename(value = starts_with("CHELSA_"))
      md$climate[i] <- list(climate_table)
      md$status[i] <- "data extracted"
    }
    
    unlink(md$path[i])
    
  }
  
  return(md)
} #fix warning messages
