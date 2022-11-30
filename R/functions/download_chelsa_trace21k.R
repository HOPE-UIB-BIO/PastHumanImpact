#' @title Download CHELSA-TraCE21k – 1km climate timeseries since the LGM
#' @description This function will download selected climate variables for time series to be extracted for HOPE data
#' @param 
#' @param 
#' @param   
#' @return 
## bio6 = Min Temperature of Coldest Month [C], 
## tasmin = Daily Maximum Near-Surface Air Temperature [K/10] 
## bio18 = Precipitation of Warmest Quarter [kg m-2 quarter-1]
## bio19 = Precipitation of Coldest Quarter [kg m-2 quarter-1]
## bio01 = Annual Mean Temperature [C]
## bio12 = Annual Precipitation [kg m-2 year-1 ]
## bio15 = Precipitation Seasonality [Unitless]




#CHELSA_TraCE21k_bio1_-10_V1.tif
# string: [model]_bio[bio.var]_[timeslice]_V1.tif]



#CHELSA_TraCE21k_bio1_-210_V1.tif
#CHELSA_TraCE21k_tasmin_10_-210_V1.tif 
# string: [model]_[parameter]_[month]_[timeslice]_V1.tif]

bio.var.wanted = c(1, 6, 12, 15, 18, 19)
here::here()

devtools:::install_github("gearslaboratory/gdalUtils")
install.packages("https://gitlab.rrz.uni-hamburg.de/helgejentsch/climdatdownloadr/-/archive/master/climdatdownloadr-master.tar.gz", repos = NULL, type = "source")

packages_climate <- c("gdalUtils", "httr", "ncdf4", "qpdf", "raster", "RCurl", "RefManageR", "rgdal", "stringr", "sf", "sp", "svMisc", "utils", "ClimDatDownloadR")


sapply(packages_climate, library, character.only = TRUE)


test <- download_chelsa_trace21k(save.location = here::here(), 
                                 parameter = "bio", 
                                 model =  "CHELSA_TraCE21k", 
                                 bio.var = c(1,5), 
                                 time.var = c(20:-10)
                                 )






download_chelsa_trace21k <- function(save.location = "./",
                                    parameter = c("bio", "tasmax","tasmin", "pr"),
                                    model = "CHELSA_TraCE21k",
                                    bio.var = c(1:19),
                                    month.var = c(1:12),
                                    time.var = c(20:-200), 
                                    clipping = FALSE,
                                    clip.shapefile = NULL,
                                    buffer = 0,
                                    clip.extent = c(-180, 180, -90, 90),
                                    convert.files.to.asc = FALSE,
                                    stacking.data = FALSE,
                                    combine.raw.zip = FALSE,
                                    delete.raw.data = TRUE,
                                    save.bib.file = FALSE
                                    ) {
  
  
# requireNamespace("stringr")
# requireNamespace("RCurl")
# requireNamespace("ncdf4")
gc()
call.time <- str_replace_all(str_replace_all(paste0(Sys.time()), pattern = ":", replacement = "-"), pattern = " ", replacement = "_")

# initial check -----------------------------------------------------------
# normalize Path for easier application later
save.location <- normalizePath(save.location, winslash = "/")
# Check which parameters are put in and if the connected
# month/bio-variables are correctly input
if(is.element("pr", parameter)|is.element("tasmax", parameter)|is.element("tasmin", parameter)){
  month.var <- c(month.var)
  if(!is.numeric(month.var)) stop("month.var needs to be a numeric vector")
}

if(is.element("bio", parameter)){
  bio.var <- c(bio.var)
  if(!is.numeric(bio.var)) stop("bio.var needs to be a numeric vector")
  bio.var <- as.character(bio.var)
  bio.var <- str_pad(bio.var, 2, 'left', pad = "0")
}

# work through all given parameters --------------------------------------------

for(i in parameter){
  # clear up the temporary directory
  unlink(list.files(tempdir(), recursive = T, full.names=T))

  
  # create intermediate strings for later use
  interm <- switch(i,
                   "pr" = "pr/",
                   "tasmax" = "tasmax/",
                   "tasmin" = "tasmin/",
                   "bio"  = "bio/",
                   stop())
  
  
  variable.numbers <- switch(i,
                             "bio" = bio.var,
                             "tasmin" = month.var,
                             "tasmax" = month.var,
                             "pr" = month.var,
                             stop())
  
 
  # create new directory
  if(!dir.exists(paste0(save.location, "/", i))){
    dir.create(paste0(save.location, "/", i))
  }
  temp.save.location <- paste0(save.location, "/", i, "/")
  
    
    temp.temp.save.location <- paste0(temp.save.location,
                                      str_replace_all(interm,
                                                      pattern = "/",
                                                      "_"),"Paleoclimate","/")
    if(!dir.exists(temp.temp.save.location)){
      dir.create(temp.temp.save.location)
    }
    
    # temp.temp.save.location <- normalizePath(temp.temp.save.location,
    #                                          winslash = "/")
    
    
    for (timeslice in 1:length(time.var)) {
      
      # download of the requested datasets -------------------------------------

      
      # Check if bio is not requested
      if(i != "bio"){
        
        # work through every requested month
        for(month in 1:length(month.var)){
      
          # clear up the temporary directory
          unlink(list.files(tempdir(), recursive = T, full.names=T))
          
          dest.temp <- 
                paste0(temp.temp.save.location, model, "_",
                              i, month.var[month], "_", time.var[timeslice], "_V1.0.tif")
          
          if(!file.exists(dest.temp)){
            URL.temp <-
              paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/chelsa_trace/",
                     interm, model, "_", i, month.var[month], "_", time.var[timeslice], "_V1.0.tif")
       
            
            # check if URL is available
            if(url.exists(URL.temp)){
              # download file to save location
              download.file(url = URL.temp,
                            destfile = dest.temp,
                            overwrite = TRUE,
                            mode = 'wb',
                            quiet = FALSE)
              
              if(i != "pr"){
                raster.temp <- raster(dest.temp)
                
                raster.temp <- clamp(raster.temp, lower = -1000, useValues = FALSE)
                gc()
                
                # Conversion Float
                gain(raster.temp) <- 0.1
                # umrechnung Kelvin - Celsius
                gc()
                offs(raster.temp) <- -273.15
                
                writeRaster(raster.temp,
                            dest.temp,
                            overwrite = TRUE)
                rm(raster.temp)
                gc()
              }else{
                # for precipitation 
                raster.temp <- raster(dest.temp)
                raster.temp <- clamp(raster.temp, upper = 30000, useValues= FALSE)
                gc()
                gain(raster.temp) <- 0.1
                writeRaster(raster.temp,
                            dest.temp,
                            overwrite = TRUE)
                rm(raster.temp)
                gc()
              }
            }else{
              # Error message
              warning(paste0("File does not exist. Did not download: \n", URL.temp, "\n\n"),
                      call. = TRUE, immediate. = FALSE)
            }
          }
          if(month.var[month] == month.var[length(month.var)] &
             length(list.files(temp.temp.save.location,
                               pattern = ".tif",
                               include.dirs = FALSE)) != 0){
            if(clipping == TRUE){
              clipping.tif(clip.save.location = temp.temp.save.location,
                           clip.shapefile = clip.shapefile,
                           clip.extent = clip.extent,
                           buffer = buffer,
                           convert.files.to.asc = convert.files.to.asc,
                           time.stamp.var = call.time)
            }
            if(convert.files.to.asc == TRUE){
              convert.to.asc(save.location = temp.temp.save.location,
                             time.stamp.var = call.time)
            }
            if(stacking.data == TRUE){
              if(clipping==TRUE){
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = i,
                                         variable.numbers = variable.numbers,
                                         stack.clipped = TRUE,
                                         time.stamp.var = call.time)
              }else{
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = i,
                                         variable.numbers = variable.numbers,
                                         time.stamp.var = call.time)
              }
            }
            if(combine.raw.zip == TRUE){
              combine.raw.in.zip(save.location = temp.temp.save.location,
                                 zip.name = paste0("CHELSATraCE_", i, ""),
                                 time.stamp.var = call.time)
            }
            if(delete.raw.data == TRUE){
              unlink(list.files(temp.temp.save.location,
                                pattern = ".tif",
                                include.dirs = FALSE, full.names = T), force = TRUE)
            }
          }
        }
      } else{
        # analog to other parameters
        # just that bio.var instead of month.var is used.
        for(bio in bio.var){
        
          dest.temp <- paste0(temp.temp.save.location, model,
                              "_bio", bio, "_", time.var[timeslice], "_V1.0.tif")
          
          if(!file.exists(dest.temp)){
            URL.temp <-
              paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/chelsa_trace/",
                     interm, model,"_bio",bio, "_", time.var[timeslice], "_V1.0.tif")
            
            # check if URL is available
            if(url.exists(URL.temp)){
              # download file to save location
              download.file(url = URL.temp,
                            destfile = dest.temp,
                            overwrite = TRUE,
                            mode = 'wb',
                            quiet = FALSE)
              # Casting into floats and deleting NA values
              raster.temp <- raster(dest.temp)
              raster.temp <- clamp(raster.temp, lower = -1000, useValues= FALSE)
              gc()
              
              if(bio <= 11){
                # values(raster.temp) <- as.numeric(values(raster.temp)/10)
                gain(raster.temp) <- 0.1
              }
              writeRaster(raster.temp,
                          dest.temp,
                          overwrite = TRUE)
              rm(raster.temp)
              gc()
            }else{
              # Error message
              warning(paste0("File does not exist. Did not download: \n", URL.temp),
                      call. = TRUE, immediate. = FALSE)
            }
          }
          if(bio == bio.var[length(bio.var)] &
             length(list.files(temp.temp.save.location,
                               pattern = ".tif",
                               include.dirs = FALSE)) != 0){
            if(clipping == TRUE){
              clipping.tif(clip.save.location = temp.temp.save.location,
                           clip.shapefile = clip.shapefile,
                           clip.extent = clip.extent,
                           buffer = buffer,
                           convert.files.to.asc = convert.files.to.asc,
                           time.stamp.var = call.time)
            }
            if(convert.files.to.asc == TRUE){
              convert.to.asc(save.location = temp.temp.save.location,
                             time.stamp.var = call.time)
            }
            if(stacking.data == TRUE){
              if(clipping==TRUE){
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = i,
                                         variable.numbers = variable.numbers,
                                         stack.clipped = TRUE,
                                         time.stamp.var = call.time)
              }else{
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = i,
                                         variable.numbers = variable.numbers,
                                         time.stamp.var = call.time)
              }
            }
            if(combine.raw.zip == TRUE){
              combine.raw.in.zip(save.location = temp.temp.save.location,
                                 zip.name = paste0("CHELSATraCE_", i, ""),
                                 time.stamp.var = call.time)
            }
            if(delete.raw.data == TRUE){
              unlink(list.files(temp.temp.save.location,
                                pattern = ".tif",
                                include.dirs = FALSE, full.names = T), force = TRUE)
            }
          }
        }
      }
      
      if(length(list.files(temp.temp.save.location,
                           include.dirs = TRUE)) == 0){
        unlink(str_sub(temp.temp.save.location, 1, end = str_length(temp.temp.save.location)-1),
               force = T, recursive = TRUE)
      }
    }
  }

# Saving BIB File
if(save.bib.file == TRUE) save.citation(save.location = save.location, dataSetName = "CHELSA_PALEOCLIMATE")
}

