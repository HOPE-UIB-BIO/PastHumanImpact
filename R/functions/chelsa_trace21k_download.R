

# A function to create a set of URLs for downloading CHELSA TraCE21k

get_chelsa_trace21k_urls <- function(variables = c("bio", "tasmin", "tasmax", "pr"),
                                     name = "CHELSA_TraCE21k",
                                     bio.var = c(1:19),
                                     month.var = c(1:12),
                                     time.var = c(20:-220)) {
  
  base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/chelsa_trace/"
  
  expand_grid(model = name, time.id = time.var, variable = variables,  bio = bio.var, month = month.var) %>%
    mutate(
      month = replace(month, variable == "bio", NA),
      bio = replace(bio, variable != "bio", NA),
      histdir = case_when(variable == "bio" ~ "bio/",
                        variable == "tasmin" ~ "tasmin/",
                        variable == "tasmax" ~ "tasmax/",
                        variable == "pr"~ "pr/"),
      file = case_when(variable == "bio" ~ 
                     paste0(histdir, model, "_", variable, str_pad(bio, 2, "left", "0"), "_", time.id,"_V1.0.tif"),
                   variable != "bio" ~
                     paste0(histdir, model, "_", variable, "_", month, "_", time.id, "_V1.0.tif")),
  url = paste0(base_url, file)) %>%
    dplyr::select(file, url) %>%
    distinct()
}


ch_dl <- function(md, dest=NULL, skip_existing=TRUE, method="curl", crop=NULL){
  
  if(is.null(dest)) dest <- getwd()
  
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
    
    if(!is.null(crop) & file.exists(md$path[i])){
      require(raster)
      r <- raster(md$path[i]) %>%
        crop(crop) %>%
        writeRaster(md$path[i], overwrite=T)
      md$status[i] <- "raster cropped"
    }
  }
  return(md)
}

################### TEST
test <- get_chelsa_trace21k_urls(variables = "tasmin",
                         bio.var = c(1,5),
                         month.var = 10,
                         time.var = c(20:15)) %>%
  ch_dl

#------------- ok



################# MODIFY
# modify code so that for each iteration: import data to R, extract data into lists and remove downloaded file to avoid filling up storage 


ch_dl_version2 <- function(md, dest=NULL, skip_existing=TRUE, method="curl", crop=NULL){
  
  if(is.null(dest)) dest <- getwd()
  
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
    
    if(!is.null(crop) & file.exists(md$path[i])){
      require(raster)
      r <- raster(md$path[i]) %>%
        crop(crop) %>%
        writeRaster(md$path[i], overwrite=T)
      md$status[i] <- "raster cropped"
    }
    
   # IMPORT/SAVE DATA TO R 
   # CREATE TABLE
   # REMOVE/DELETE DOWNLOADED FILE 
    
    
    
  }
  
  # STACK TIME SERIES/GRIDDED DATA AND RETURN TIBBLE 
  return(md)
}




