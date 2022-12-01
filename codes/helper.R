library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
library(zoo)
library(animation)
#library(raster)

setwd("D:/AGH/INZ")


ncin<-nc_open("data/forecast/CHELSAcmip5ts_tasmax_ACCESS1-3_rcp45_2006-2029_V1.1.nc")
ncin<-nc_open("data/cru_ts4.05.1901.2020.tmp.dat.nc")
ncin <- nc_open("data/real/poland/agg_terraclimate_tmax_1958_CurrentYear_GLOBE.nc")
#ncin<-nc_open("data/cru_ts4.05.1901.2020.tmp.dat.nc")

print(ncin)

lon <- ncvar_get(ncin,"lon")
head(lon)

nlon <- dim(lon)
nlon

lat <- ncvar_get(ncin,"lat")
head(lat)

nlat <- dim(lat)
time <- ncvar_get(ncin,"time")
head (time)
nt <-dim(time)
nt

#real data coordinates
real_coor <- c(range(na.omit(as.numeric(lat)))[1], range(na.omit(as.numeric(lat)))[2], range(na.omit(as.numeric(lon)))[1], range(na.omit(as.numeric(lon)))[2])

w1 <- which(lon >= 0 & lon <= 35)
w2 <- which(lat >= 20 & lat <= 80)
ncin_tmp<-ncvar_get(ncin,"air_temperature",start=c(w1[1],w2[1],time[1]),count=c(1,1,192))
cru_all_tmp<-ncvar_get(ncin,"tmax",start=c(1,1,6),count=c(720,360,1))
tmax_tmp <- ncvar_get(ncin, "tmax")


tunits <- ncatt_get(ncin,"time","units")
tunits

ncatt_get(ncin,0,"title")
ncatt_get(ncin,0,"institution")
ncatt_get(ncin,0,"source")# data source
ncatt_get(ncin,0,"references")
ncatt_get(ncin,0,"history")
ncatt_get(ncin,0,"Conventions")

tmp_array <- ncvar_get(ncin,"air_temperature")
head(tmp_array)


dlname <- ncatt_get(ncin,"ppt","long_name")
dlname

dunits <- ncatt_get(ncin,"ppt","units")
dunits

fillvalue <- ncatt_get(ncin,"ppt","_FillValue")
fillvalue

dim(tmp_array)

tustr <- strsplit(tunits$value, " ")

tdstr <- strsplit(unlist(tustr)[3], "-")
tdstr
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
time_ch<-chron(time,origin=c(tmonth, tday,tyear))
head(time_ch)
tail(time_ch)
time_m_y<-as.yearmon(time_ch)
time_m_y
Sys.setlocale("LC_TIME", "C")
time_m_y<-as.yearmon(time_ch)
head(time_m_y)
tmp_slice <- tmp_array[,,7]
range(na.omit(as.numeric(cru_all_tmp)))
range(na.omit(as.numeric(lon)))
range(((na.omit(as.numeric(lat)))))

plot(south_bind)
image(lon,lat,cru_all_tmp, col=rev(brewer.pal(10,"RdBu")), xlim=c(-181,181),ylim=c(-91,90),zlim=c(-15,40))
#TODO COS TU JEST NIE TAK Z LAT EWIDENTNIE
grid <- expand.grid(lon=lon, lat=rev(lat))
head(grid)
tail(grid)


pcts <- data.frame(lon[21:93], rev(lat))
plot(south_bind)
points(fr)

summary(tmp_slice)
rownames(tmp_slice) <- lon
colnames(tmp_slice) <- lat
as.vector(tmp_slice)
tmp_vec <- as.vector(tmp_slice)[1:145]

fr <- data.frame("b"=lon, "c"=lat[20])
extract(ncin, south_WGS84)


time_july2006 <- time[7]
time_july2006
tmax_july2006 <- tmax_array[,,7]
tmax_july2006
rownames(tmax_july2006) <- lon
colnames(tmax_july2006) <- lat
plot(tmp, pch=13, col='green')
plot(south)
plot(north, add=TRUE)


tmp <- data.frame()

for (i in lat){
    for (j in lon){
        tmp <- rbind(tmp, c(i, j))
    }
}

max(st_coordinates(both))

both <- st_union(south, north)
plot(both)
#bb <- st_bbox(xmin = 15, xmax = 27, ymin = 45, ymax = 56)

##########################################################################
##########################################################################

#lon_july <- lon[,1,1]

range(na.omit(as.numeric(tmax_july[])))
range(na.omit(as.numeric(lon)))
range(((na.omit(as.numeric(lat)))))

for(i in 1:1)
{
    image(lon,rev(lat),tmax_real_july[,,1], col=rev(brewer.pal(10,"RdBu")), xlim=c(13,27),ylim=c(46,57),zlim=c(8,33))
}
#image(lon,rev(lat),tmax_july[,,2], col=rev(brewer.pal(10,"RdBu")), xlim=c(15,25),ylim=c(47,55),zlim=c(9,33))
#points(lon, rep(lat[1],length(lon)))



grid <- expand.grid(lon=lon, lat=lat)
head(grid)
cutpts <- seq(15, 19, 21, 23, 25, 27, 29, 31, 33)
cutpts
levelplot(tmax_tmp ~ lon * lat, data=grid, at=cutpts, cuts=10, pretty=T, col.regions=(rev(brewer.pal(10,"RdBu"))),aspect=0.5)

for (i in 1:length(lat))
{
    points(lon, rep(lat[i],length(lon)))
}


##################################################33
##################################################

ncin<-nc_open("data/cru_ts4.05.1901.2020.tmp.dat.nc")
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")
cru_all_tmp<-ncvar_get(ncin,"tmp",start=c(1,1,1),count=c(720,360,1))
#tmax_tmp <- ncvar_get(ncin, "tmp")

grid <- expand.grid(lon=lon, lat=lat)
head(grid)
cutpts <- c(15, 19, 21, 23, 25, 27, 29, 31, 33)
cutpts
levelplot(cru_all_tmp ~ lon * lat, data=grid, at=cutpts, cuts=10, pretty=T, col.regions=(rev(brewer.pal(10,"RdBu"))),aspect=0.5)


ncin_real_tmax<-nc_open("data/real/poland/agg_terraclimate_tmax_1958_CurrentYear_GLOBE.nc")
lon_real <- ncvar_get(ncin_real_tmax, "lon")
lat_real <- ncvar_get(ncin_real_tmax, "lat")
tmax_real_array <- ncvar_get(ncin_real_tmax, "tmax")

cru_all_tmp<-ncvar_get(ncin_real_tmax,"tmax",start=c(1,1,1),count=c(dim(lon_real),dim(lat_real),1))

grid <- expand.grid(lon=lon_real, lat=lat_real)
head(grid)
cutpts <- c(-12, 8, 15, 18, 21, 24, 27, 30, 33)
cutpts

levelplot(cru_all_tmp ~ lon_real * lat_real, data=grid, at=cutpts, cuts=10, pretty=T, col.regions=(rev(brewer.pal(10,"RdBu"))),aspect=0.5)


library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
library(zoo)
library(animation)
#library(raster)

setwd("D:/AGH/INZ")

ncin_real <- nc_open("data/real/poland/agg_terraclimate_tmax_1958_CurrentYear_GLOBE.nc")
lon <- ncvar_get(ncin_real,"lon")
lat <- ncvar_get(ncin_real,"lat")
tmax_real <- ncvar_get(ncin_real,"tmax",start=c(1,1,7),count=c(dim(lon),dim(lat),1))

grid_real <- expand.grid(lon=lon, lat=lat)
head(grid_real)
cutpts <- c(15, 19, 21, 23, 25, 27, 29, 31, 33)
cutpts
levelplot(tmax_real ~ lon * lat, data=grid_real, at=cutpts, cuts=10, pretty=T, col.regions=(rev(brewer.pal(10,"RdBu"))),aspect=0.5)



ncin_2 <- nc_open("data/real/poland/agg_terraclimate_tmax_1958_CurrentYear_GLOBE.nc")
lon_2 <- ncvar_get(ncin_2,"lon")
lat_2 <- ncvar_get(ncin_2,"lat")
cru_all_tmp_2<-ncvar_get(ncin_2,"tmax",start=c(1,1,7),count=c(dim(lon_2),dim(lat_2),1))
#tmax_tmp <- ncvar_get(ncin, "tmp")

grid_2 <- expand.grid(lon=lon_2, lat=lat_2)
head(grid_2)
cutpts_2 <- c(15, 19, 21, 23, 25, 27, 29, 31, 33)
cutpts_2
levelplot(cru_all_tmp_2 ~ lon_2 * lat_2, data=grid_2, at=cutpts_2, cuts=10, pretty=T, col.regions=(rev(brewer.pal(10,"RdBu"))),aspect=0.5)
/*













ncin_real_ppt<-nc_open("real/agg_terraclimate_ppt_1958_CurrentYear_GLOBE.nc")
ncin_real_tmax<-nc_open("real/agg_terraclimate_tmax_1958_CurrentYear_GLOBE.nc")
ncin_real_tmin<-nc_open("real/agg_terraclimate_tmin_1958_CurrentYear_GLOBE.nc")


ncin<-ncin_real_tmax

lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

#to samo dla szerokooci 
lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)
time <- ncvar_get(ncin,"time")
time
tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
time_ch<-chron(time,origin=c(tmonth, tday,tyear))

time_ch
#widaa ?e odczyt jest zawsze przypisany do 16 dnia miesi1ca wiec t1 informacje mo?na pomin1c w wektorze
Sys.setlocale("LC_TIME", "C")
time_m_y<-as.yearmon(time_ch)

#format daty miesiac rok: 
time_m_y

m <- 1
len<-length(time_m_y)
time_m_y[m]
variable_name<-"air_temperature"
tmax <- ncvar_get(ncin_real_tmax, "tmax")
tmin <- ncvar_get(ncin_real_tmin, "tmin")
ppt <- ncvar_get(ncin_real_ppt, "ppt")

d <- dim(tmax)
dim(tmin)
PET_array <- array(rep(NA, prod(d)), dim=d)


for (i in 1:d[1]){
	for (j in 1:d[2]){
		tmp_min<-tmin[i,j,]
		tmp_max<-tmax[i,j,]
		print(i)
		PET_array[i,j,]<-hargreaves(tmp_min,tmp_max,lat=lat[j])
	}
}

grid_pet <- expand.grid(lon=lon, lat=lat)
grid_pet$z<-as.numeric(PET_array[,,7])

#prosta mapa
levelplot(z ~ lon * lat,data=grid_pet)

SPEI<- array(rep(NA, prod(d)), dim=d)

for (i in 1:d[1]){
	for (j in 1:d[2]){
		print(i)
		a<-as.vector(spei( as.numeric(ppt[i,j,])-as.numeric(PET_array[i,j,])  ,6)) # ostatni parametr -> liczba miesi�cy
		SPEI[i,j,]<-as.vector(a$fitted)
		#BAL[i,j,]<-as.numeric(prec[i,j,])-as.numeric(PET[i,j,]) 
		
	}
}

m<-7

grid_spei <- expand.grid(lon=lon, lat=lat)
grid_spei$z<-as.numeric(SPEI[,,m])

#prosta mapa
levelplot(z ~ lon * lat, data=grid_spei)

spei_r_WGS<-rasterFromXYZ(grid_spei,crs=4326)


pal <- colorRampPalette(c("red","blue"))
cuts<-seq(-3,3,0.5)
plot(spei_r_WGS,main=time_m_y[7],col=pal(13),breaks=cuts)
plot(both,add=TRUE,lwd=2)


r <- raster(nrow=5, ncol=5, xmn=0, xmx=10, ymn=0, ymx=10, crs="")
set.seed(1)
values(r) <- sample(1:25)
r[r < 15] <- NA
xyz <- rasterToPoints(r)
rst <- rasterFromXYZ(xyz)
plot(r, col=pal(10), breaks=cuts)
is(r)

is(spei_real_raster)



#####################################################################################################################

south_UTM <- st_transform(south, CRS("+proj=utm +zone=34 +datum=WGS84"))

coord<-as.data.frame(st_coordinates(south_UTM))
summary(coord)

left_down<-c( min(coord$X), min(coord$Y)) #lewy dolny róg
right_up<-c( max(coord$X), max(coord$Y))

size<-c(4000,4000)

points<- (right_up-left_down)/size
num_points<-ceiling(points) # zaokroąglenie w górę
num_points
eps<-10
num_points<-num_points+eps
grid <- GridTopology(left_down, size, num_points)
grid
gridpoints <- SpatialPoints(grid, proj4string = CRS("+proj=utm +zone=34 +datum=WGS84"))


bound<-st_as_sf(south_UTM)
plot(bound)

cropped_gridpoints <- crop(gridpoints,bound )
plot(gridpoints)
plot(cropped_gridpoints,add=TRUE,col="Red")
plot(bound, add=TRUE)
#plot(Window(data15_ppp_e),add=TRUE)

cor<-st_coordinates(bound) #pozyskujemy koordynaty w formie macierzy
summary(cor)

p <- Polygon(cbind(cor[,1],cor[,2]))
ps <- Polygons(list(p),1)
sps <- SpatialPolygons(list(ps))
dev.off()
plot(sps)

cropped_gridpoints <- crop(gridpoints,sps)
plot(cropped_gridpoints,col="Red")
#plot(Window(data15_ppp_e),add=TRUE)


spgrid <- SpatialPixels(cropped_gridpoints)
coordnames(spgrid) <- c("x", "y")
plot(spgrid)




########################################################################################################################################

(as.numeric(ncatt_get(ncin_real_ppt, 0,"geospatial_lat_min")))[2]




p <- raster(nrow=3, ncol=3)
values(p) <- 1:ncell(p)
r <- raster(nrow=10, ncol=10)
values(r) <- 1:ncell(r)
s <- raster(nrow=3, ncol=3)
s <- resample(r, s, method='bilinear')
par(mfrow=c(1,3))
plot(r)
plot(s)
plot(p)


nrow(spei_real_raster)
ncol(spei_real_raster)

nrow(spei_fore_raster)
ncol(spei_fore_raster)
spei_fore_raster[1]
head(grid_real_spei$lon, n=20)
head(grid_fore_spei$lon, n=20)

grid_real_spei$lon[length(grid_real_spei$lon)] - grid_real_spei$lon[1]
grid_fore_spei$lon[length(grid_fore_spei$lon)] - grid_fore_spei$lon[1]

grid_real_spei$lat[length(grid_real_spei$lat)] - grid_real_spei$lat[1]
grid_fore_spei$lat[length(grid_fore_spei$lat)] - grid_fore_spei$lat[1]

#spei_real_gen <- resample(spei_real_raster
                          
spei_fore_bilinear <- resample(spei_fore_raster, spei_real_raster, method='bilinear')


spei_fore_raster$lon[1]                          




##########
projection(spei_fore_bilinear)

# Convert raster to SpatialPointsDataFrame
pts <- rasterToPoints(spei_fore_bilinear, spatial=TRUE)
proj4string(pts)

# reproject sp object
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
pts <- spTransform(pts, CRS(proj4string(pts))) 
proj4string(pts)

# Assign coordinates to @data slot, display first 6 rows of data.frame
pts@data <- data.frame(pts@data, long=coordinates(pts)[,1],
                         lat=coordinates(pts)[,2])                         
head(pts@data)


#calculate MSE
mean((unique(pts@data$long) - lon_real)^2)


###################################################################################
lon_real_2d = t(array(grid_real_spei[,1],dim=c(length(lon_real),length(lat_real))))
lat_real_2d = t(array(grid_real_spei[,2],dim=c(length(lon_real),length(lat_real))))
lon_fore_2d = t(array(grid_real_spei[,1],dim=c(length(lon_real),length(lat_real))))
lat_fore_2d = t(array(grid_real_spei[,2],dim=c(length(lon_real),length(lat_real))))

lon_real_raster = raster(ncol=length(lon_real), nrow=length(lat_real))
lat_real_raster = raster(ncol=length(lon_real), nrow=length(lat_real))
lon_fore_raster = raster(ncol=length(lon_fore), nrow=length(lat_fore))
lat_fore_raster = raster(ncol=length(lon_fore), nrow=length(lat_fore))

values(lon_real_raster) = as.vector(grid_real_spei[,1])
values(lat_real_raster) = as.vector(grid_real_spei[,2])
values(lon_fore_raster) = as.vector(grid_fore_spei[,1])
values(lat_fore_raster) = as.vector(grid_fore_spei[,2])

par(mfrow=c(2,2))
plot(lon_real_raster)                         
plot(lat_real_raster)
plot(lon_fore_raster)
plot(lat_fore_raster)

#5->4
lon_fore_bilinear <- resample(lon_fore_raster, lon_real_raster, method='bilinear')
lat_fore_bilinear <- resample(lat_fore_raster, lat_real_raster, method='bilinear')

ncol(lon_real_raster)

mse1=0
for (i in 1:dim(lon_fore_bilinear)[1])
{
  for (j in 1:dim(lon_fore_bilinear)[2])
  {
    print(i)
    mse1 = mse1 + sqrt((lon_fore_bilinear[i,j] - lon_real_raster[i,j])^2 + (lat_fore_bilinear[i,j] - lat_real_raster[i,j])^2)
  }
}
mse1 = mse1 / (dim(lon_fore_bilinear)[1] * dim(lon_fore_bilinear)[2]) 
mse1 #0.01299204

#4->5
lon_real_bilinear <- resample(lon_real_raster, lon_fore_raster, method='bilinear')
lat_real_bilinear <- resample(lat_real_raster, lat_fore_raster, method='bilinear')

ncol(lon_real_bilinear)

mse2=0
for (i in 1:dim(lon_real_bilinear)[1])
{
  for (j in 1:dim(lon_real_bilinear)[2])
  {
    print(i)
    mse2 = mse2 + sqrt((lon_real_bilinear[i,j] - lon_fore_raster[i,j])^2 + (lat_real_bilinear[i,j] - lat_fore_raster[i,j])^2)
  }
}
mse2 = mse2 / (dim(lon_real_bilinear)[1] * dim(lon_real_bilinear)[2]) 
mse2 #0.01299202



#4,5->20

(lon_real[123] - lon_real[122]) - (lon_fore[123] - lon_fore[122])
lat_fore[123] - lat_fore[122]

#wielkosc oczka siatki, jednostka - stopien:
#real: 0.04166667
#fore: 0.04490319

raster20 <- raster(ncol=, nrow=)

217/4
202/5
145/4
135/5

(lon_fore_max - lon_fore_min) / 202
(lat_fore_max - lat_fore_min) / 135

lon_real_bilinear20 <- resample(lon_real_raster, lon_fore_raster, method='bilinear')
lat_real_bilinear20 <- resample(lat_real_raster, lat_fore_raster, method='bilinear')
lon_fore_bilinear20 <- resample(lon_fore_raster, lon_fore_raster, method='bilinear')
lat_fore_bilinear20 <- resample(lat_fore_raster, lat_fore_raster, method='bilinear')

ncol(lon_real_bilinear)

mse3=0
for (i in 1:dim(lon_real_bilinear)[1])
{
  for (j in 1:dim(lon_real_bilinear)[2])
  {
    print(i)
    mse3 = mse3 + sqrt((lon_real_bilinear[i,j] - lon_fore_raster[i,j])^2 + (lat_real_bilinear[i,j] - lat_fore_raster[i,j])^2)
  }
}
mse3 = mse3 / (dim(lon_real_bilinear)[1] * dim(lon_real_bilinear)[2]) 
mse3 

spei6_real_arr = list()
spei6_fore_arr = list()

for (i in july_real)
{
  
  grid_fore_spei <- expand.grid(lon=lon_fore, lat=lat_fore)
  grid_fore_spei$z<-as.numeric(SPEI_fore[,,i])
  
  spei_fore_raster<-rasterFromXYZ(grid_fore_spei,crs=4326)
  
  spei6_fore_arr <- append(spei6_fore_arr, spei_fore_raster)
  
  
 
  grid_real_spei <- expand.grid(lon=lon_real, lat=lat_real)
  grid_real_spei$z<-as.numeric(SPEI_real[,,i])
  
  spei_real_raster<-rasterFromXYZ(grid_real_spei,crs=4326)
  spei_real_r2 <- resample(spei_real_raster, spei_fore_raster, method='bilinear')
  
  spei6_real_arr <- append(spei6_real_arr, spei_real_r2)
}


real_spei6_stack <- stack(spei6_real_arr)
fore_spei6_stack <- stack(spei6_fore_arr)
plot(real_spei6_stack)
plot(fore_spei6_stack)

x <- corLocal(real_spei6_stack, fore_spei6_stack, method="spearman", test=TRUE)
plot(x)
xm <- mask(x[[1]], x[[2]] < 0.05, maskvalue=FALSE)
plot(xm)



load("data/manual_real_fore_ras_corr_crop.RData")
library(ggplot2)


ggplot() +
  geom_raster(data = spei_rasters_cropped[1,2][[1]][[1]] , aes(x = x, y = y,
                                       fill = z)) + 
  scale_fill_manual(values = pal(12)) + 
  coord_quickmap()

ggplot() +
  geom_raster(spei_rasters_cropped[1,2][[1]][[1]])
