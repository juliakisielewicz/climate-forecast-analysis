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
