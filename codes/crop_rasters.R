

source_rmd = function(file, ...) {
  tmp_file = tempfile(fileext=".R")
  on.exit(unlink(tmp_file), add = TRUE)
  knitr::purl(file, output=tmp_file)
  source(file = tmp_file, ...)
}

#load prevoius files
setwd("D:/AGH/INZ/data")
source("../codes/setup.R")
source_rmd("../codes/shapefiles.Rmd")

#load rasters
wd <- "./rasters/stacks/"
rnames <- c("real_spei1_stack.tif", "real_spei3_stack.tif", "real_spei6_stack.tif", "real_spei12_stack.tif") 
fnames <- c("forecast_spei1_stack.tif", "forecast_spei3_stack.tif", "forecast_spei6_stack.tif", "forecast_spei12_stack.tif")

#put rasters in data.frame
spei_rasters <- c()
for (i in seq(1, length(rnames)))
{
  spei_rasters <- append(spei_rasters, stack(paste0(wd, rnames[i])))
}
for (i in seq(1, length(fnames)))
{
  spei_rasters <- append(spei_rasters, stack(paste0(wd, fnames[i])))
}

spei_rasters <- data.frame(matrix(spei_rasters, nrow=4, ncol=2))
colnames(spei_rasters) <- c("real", "forecast")
rownames(spei_rasters) <- c(1,3,6,12)


#crop and mask rasters
ex <- extent(as_Spatial(both))

crop_mask <- function(a) { #input: rasterstack
  r_list <- list()
  for (i in seq(1, length(names(a))))
  {
    print(i)
    y <- a[[i]]
    y <- crop(y, ex)
    y <- mask(y, as_Spatial(both))
    r_list <- append(r_list, y)
  }
  return(stack(r_list))  
}

spei <- c()
for (i in seq(1,2))
{
  for (j in seq(1,4))
  {
    spei <- append(spei, crop_mask(spei_rasters[j,i][[1]])) 
  }
}

#save in new data.frame
spei_rasters1 <- data.frame(matrix(spei, nrow=4, ncol=2))
colnames(spei_rasters1) <- c("real", "forecast")
rownames(spei_rasters1) <- c(1,3,6,12)

#visualize
pal <- colorRampPalette(c("red","blue"))
cuts<-seq(-3,3,0.5)

save.image("../codes/manual_save_app.RData")
