# get a dataframe of raster for plotting
# found on web
get_rasterdf <- function(x, aggregate = 1) {
  resampleFactor <- aggregate        
  inputRaster <- x    
  inCols <- ncol(inputRaster)
  inRows <- nrow(inputRaster)
  # Compute numbers of columns and rows in the resampled raster
  resampledRaster <- rast(ncol=(inCols / resampleFactor), 
                          nrow=(inRows / resampleFactor),
                          crs = crs(inputRaster))
  # Match to the extent of the original raster
  ext(resampledRaster) <- ext(inputRaster)
  # Resample data on the new raster
  y <- resample(inputRaster,resampledRaster,method='near')
  # Extract cell coordinates into a data frame
  coords <- xyFromCell(y, seq_len(ncell(y)))
  # Extract layer names
  dat <- stack(values(y, dataframe = TRUE))
  # Add names - 'value' for data, 'variable' for different
  # layer names in a multilayer raster
  names(dat) <- c('value', 'variable')
  dat <- cbind(coords, dat)
  dat
}
