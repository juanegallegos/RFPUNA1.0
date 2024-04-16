### Generación del producto grillado RFPUNA ### 
if (!require(lubridate)){install.packages("lubridate"); require(lubridate)}
if(!require(ncdf4)){install.packages("ncdf4"); require(ncdf4)} #lectura de netcdf
if (!require(zoo)){install.packages("zoo"); require(zoo)}
if (!require(sf)){install.packages("sf"); require(sf)}
if (!require(rgdal)){install.packages("rgdal"); require(rgdal)}
if (!require(raster)){install.packages("raster"); require(raster)}
if (!require(hydroTSM)){install.packages("hydroTSM"); require(hydroTSM)}
if (!require(ggplot2)){install.packages("ggplot2"); require(ggplot2)}
if (!require(tidyr)){install.packages("tidyr"); require(tidyr)}
if (!require(openxlsx)){install.packages("openxlsx"); require(openxlsx)}
if (!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if (!require(reshape2)){install.packages("reshape2"); require(reshape2)}
if (!require(gridExtra)){install.packages("gridExtra"); require(gridExtra)}
if (!require(patchwork)){install.packages("patchwork"); require(patchwork)}
if (!require(matrixStats)){install.packages("matrixStats"); require(matrixStats)}
if (!require(data.table)){install.packages("data.table"); require(data.table)}

main_data<-"C:/Users/juang/Documents/Tesis/Inputs"
setwd(main_data)


#Basic configuration----
#Set years of analysis
fecha_0 <- as.Date("1975-01-01") #Initial date of ground data collected
fecha_inicio<- as.Date("2010-01-01") #Initial date to set RFPUNA
fecha_final <- as.Date("2010-12-31") #Final date to set RFPUNA
fecha_2010<- as.Date("2010-12-31") #Important date to set CHELSA data
fecha_chelsa <- as.Date("2001-01-01") #Initial date of CHELSA data

INIT_YEAR <- format(fecha_inicio, "%Y")
END_YEAR  <- format(fecha_final, "%Y")
AGNOS     <- INIT_YEAR:END_YEAR

f1 <- as.numeric(difftime(fecha_inicio, fecha_0, units = "days"))+1
f2 <- as.numeric(difftime(fecha_final, fecha_0, units = "days"))+1
f3 <- as.numeric(difftime(fecha_2010,fecha_chelsa, units = "days"))+1
f4 <- as.numeric(difftime(fecha_inicio, fecha_chelsa, units = "days"))+1
f5 <- as.numeric(difftime(fecha_final, fecha_2010+1, units = "days"))+1
f6 <- as.numeric(difftime(fecha_final, fecha_chelsa, units = "days"))+1

rango<- f1:f2
rango2<- f4:f3
rango3 <- 1:f5
rango4 <- f4:f6

#Set extension limits
xmin <- -69.5
xmax <- -64
ymin <- -27
ymax <- -21



#Inputs------

est<-read.csv("input_series_pub.csv", header=TRUE, sep=",",row.names = 1, na.strings = "NA")[rango,]
fecha<-row.names(est)
sapply(est, class)
est<- lapply(est, as.numeric)
est<- as.data.frame(est)
rownames(est)<- fecha
est<-est[,colSums(is.na(est))/nrow(est) <= 0.99] #To eliminate stations with no data 
ncol(est)

#GIS data
gis<-read.csv("Input_gis_pub.csv", header=TRUE, sep = ",",na.strings = "NA")
gis<- gis[gis$ID %in% colnames(est),]
nrow(gis)

#Physiographic attributes
DEM_area <- raster("Rasters/dem_5km.tif")
Shp_area <- shapefile("Shp/Pol_real.shp")
aspect<- raster("Rasters/Aspect_5km.tif")
slope<- raster("Rasters/slope_5km.tif")


#NETCDF gridded products---------
{
  
  #IMERG
  imerg<-c()
  for (i in AGNOS){
    #Input NetCDF file path
    ruta_netcdf <- paste0("Productos Grillados/IMERG/GPM_3IMERGDF.06_Aggregation_",i,".ncml.ncml.nc4")
    #Read NetCDF file as a RasterBrick object
    brick <- brick(ruta_netcdf)
    #Original spacial resolution (0.1° a 0.05°)
    a<- disaggregate(brick, fact=2) 
    #Cuts according to the input geometry
    a<-crop(a,Shp_area)
    #Second cut according to input extension
    extent(a) <- extent(xmin, xmax,ymin,ymax)
    #All together
    imerg<-c(imerg,a)
    
  }
  #All together
  imerg<-stack(imerg)
  #Standardize names
  names<-names(imerg)
  
  
  #PERSIANN
  pers<-c()
  for (i in AGNOS){
    #Input NetCDF file path
    ruta_netcdf <- paste0("Productos Grillados/Persiann/CDR_2023-07-01042519pm_",i,".nc")
    #Read NetCDF file as a RasterBrick object
    brick <- brick(ruta_netcdf)
    #Original spacial resolution (0.5°) (0.25°)
    a<- disaggregate(brick, fact=5) 
    #Cuts according to the input geometry
    a<-crop(a,Shp_area)
    #Second cut according to input extension
    extent(a) <- extent(xmin, xmax,ymin,ymax)
    #All together
    pers<-c(pers,a)
  }
  #All together
  persiann<-stack(pers)
  #Standardize names
  names(persiann)<-names
  
  
  #CHIRPS
  
  crps<-c()
  for (i in AGNOS){
    #Input NetCDF file path
    ruta_netcdf <- paste0("Productos Grillados/CHIRPS/chirps-v2.0.",i,".days_p05.nc")
    #Read NetCDF file as a RasterBrick object
    brick <- brick(ruta_netcdf)
    #Cuts according to the input geometry
    a<-crop(brick, Shp_area)
    #Second cut according to input extension
    extent(a) <- extent(xmin, xmax,ymin,ymax)
    #All together
    crps<-c(crps,a)
  }
  #All together
  chirps<-stack(crps)
  #Standardize names
  names(chirps)<-names
  
  
  #ERA5
  #nput NetCDF file path
  ruta_netcdf <- "Productos Grillados/era5/era5_total.nc"
  #Read NetCDF file as a RasterBrick object
  era5_pp <- brick(ruta_netcdf, varname= 'tp')
  era5_temp <- brick(ruta_netcdf, varname= 't2m')
  era5_pressure<- brick(ruta_netcdf, varname='sp')
  era5_rad<- brick(ruta_netcdf, varname='ssr')
  era5_pev<- brick(ruta_netcdf, varname='pev')
  
  #Precipitation ERA5
  
  #Cuts according to the input geometry
  era5_pp<-crop(era5_pp, Shp_area)
  #Measurement and time-period adjustment calculations
  era5_pp <- calc(era5_pp[[rango]], function(x) x * 1000)
  #Original spacial resolution (0.25°)
  era5_pp<- disaggregate(era5_pp, fact=5) 
  #Second cut according to input extension
  extent(era5_pp) <- extent(xmin, xmax,ymin,ymax)
  #Standardize names
  names(era5_pp)<-names
  
  
  #Temp ERA5
  #Cuts according to the input geometry
  era5_t<-crop(era5_temp, Shp_area)
  #Measurement and time-period adjustment calculations
  era5_t <- calc(era5_t[[rango]], function(x) x - 273.15)
  #Original spacial resolution (0.25°)
  era5_t<- disaggregate(era5_t, fact=5) 
  #Second cut according to input extension
  extent(era5_t) <- extent(xmin, xmax,ymin,ymax)
  #Standardize names
  names(era5_t)<-names
  
  #Presión [Pa]
  #This parameter is the pressure (force per unit area) of the atmosphere on the surface of land, sea and in-land water.

  #Cuts according to the input geometry
  era5_p<-crop(era5_pressure, Shp_area)
  #Original spacial resolution (0.5°) 
  era5_p<- disaggregate(era5_p[[rango]], fact=5)
  #Second cut according to input extension
  extent(era5_p) <- extent(xmin, xmax,ymin,ymax)
  #Standardize names
  names(era5_p)<-names
  
  #Radiation (W/m2)

  #Cuts according to the input geometry
  era5_rad<-crop(era5_rad, Shp_area)
  #Original spacial resolution (0.25°)
  era5_rad<- disaggregate(era5_rad[[rango]], fact=5)
  #Second cut according to input extension
  extent(era5_rad) <- extent(xmin, xmax,ymin,ymax)
  #Standardize names
  names(era5_rad)<-names
  
  #PET
  #Cuts according to the input geometry
  era5_pev<-crop(era5_pev, Shp_area)
  #Original spacial resolution (0.25°)
  era5_pev<- disaggregate(era5_pev[[rango]], fact=5) 
  #Second cut according to input extension
  extent(era5_pev) <- extent(xmin, xmax,ymin,ymax)
  #Standardize names
  names(era5_pev)<-names
  
  
  #CHELSA
  chelsa<-c()
  #Input NetCDF file path
  ruta_netcdf <-  paste0("Productos Grillados/CHELSA/CHELSA_total_2001_2010.nc")
  #Read NetCDF file as a RasterBrick object
  brick1 <- brick(ruta_netcdf, varname= "pr")
  brick2 <- brick(ruta_netcdf, varname = "tas")
  
  #Precipitation CHELSA
  
  #Cuts according to the input geometry
  brick1<-crop(brick1,Shp_area)
  #Measurement and time-period adjustment calculations
  brick1<- calc(brick1[[rango2]], function(x) x*1000)
  #Original spacial resolution (0.5°)
  a1<- disaggregate(brick1, fact=10) 
  #Second cut according to input extension
  extent(a1) <- extent(xmin, xmax,ymin,ymax)
  #All together
  chelsa_pp<-stack(a1)
  #Standardize names
  names(chelsa_pp)<-names
  
  
  #Temp CHELSA
  
  #Cuts according to the input geometry
  brick2<-crop(brick2,Shp_area)
  #Measurement and time-period adjustment calculations
  brick2<- calc(brick2[[rango2]], function(x) x - 273.15)
  #Original spacial resolution (0.5°) 
  a2<- disaggregate(brick2, fact=10) 
  #Second cut according to input extension
  extent(a2) <- extent(xmin, xmax,ymin,ymax)
  #All together
  chelsa_temp<-stack(a2)
  #Standardize names
  names(chelsa_temp)<-names
  
}

PunaDEM <- brick(DEM_area)
Puna_aspect <-brick(aspect)
Puna_slope <-brick(slope)


fechas <- as.Date(row.names(est))
x.zoo <- zoo(est, order.by = fechas) 

## ----SpatialMetadata----------------------------------------------------------
stations <- gis
( stations <- st_as_sf(stations, coords = c('lon', 'lat'), crs = 4326) )

## ----ReprojectingMetadata-----------------------------------------------------
stations.utm <- sf::st_transform(stations, crs=32719) # for 'sf' objects
#cn.utm <- sf::st_transform(st_as_sf(DEM_cn), crs=32719)
st.coords <- st_coordinates(stations.utm)
lon <- st.coords[, "X"]
lat <- st.coords[, "Y"]
gis.utm <- data.frame(id=stations.utm[["ID"]], lon=lon, lat=lat)


## ----ReprojectingRasters------------------------------------------------------

Shp_area.utm <- sf::st_transform(st_as_sf(Shp_area), crs=32719)
utmz19s.p4s <- sf::st_crs(stations.utm)$proj4string # WGS 84 / UTM zone 19S

#Products
persiann_puna.utm <- projectRaster(from=persiann, crs=utmz19s.p4s)
persiann_puna.utm<- crop(persiann_puna.utm,Shp_area.utm)
chirps.utm<- projectRaster(from= chirps, crs=utmz19s.p4s)
chirps.utm<- crop(chirps.utm,Shp_area.utm)
era5_pp.utm <- projectRaster(from= era5_pp, crs=utmz19s.p4s)
era5_pp.utm<- crop(era5_pp.utm,Shp_area.utm)
imerg.utm <- projectRaster(from= imerg, crs=utmz19s.p4s)
imerg.utm<- crop(imerg.utm,Shp_area.utm)
chelsa_pp.utm <- projectRaster(from= chelsa_pp, crs=utmz19s.p4s)
chelsa_pp.utm<- crop(chelsa_pp.utm,Shp_area.utm)
chelsa_temp.utm <- projectRaster(from= chelsa_temp, crs=utmz19s.p4s)
chelsa_temp.utm<- crop(chelsa_temp.utm,Shp_area.utm)
era5_temp.utm <- projectRaster(from= era5_t, crs=utmz19s.p4s)
era5_temp.utm <- crop(era5_temp.utm,Shp_area.utm)
era5_p.utm <- projectRaster(from= era5_p, crs=utmz19s.p4s)
era5_p.utm <- crop(era5_p.utm,Shp_area.utm)
era5_rad.utm <- projectRaster(from= era5_rad, crs=utmz19s.p4s)
era5_rad.utm <- crop(era5_rad.utm,Shp_area.utm)
era5_pev.utm <- projectRaster(from= era5_pev, crs=utmz19s.p4s)
era5_pev.utm <- crop(era5_pev.utm,Shp_area.utm)

#Physiographic attributes
DEM_area.utm <- projectRaster(from=PunaDEM, crs=utmz19s.p4s)
DEM_area.utm<- crop(DEM_area.utm,Shp_area.utm)
Puna_aspect.utm <- projectRaster(from=Puna_aspect, crs=utmz19s.p4s)
Puna_aspect.utm<- crop(Puna_aspect.utm,Shp_area.utm)
Puna_slope.utm <- projectRaster(from=Puna_slope, crs=utmz19s.p4s)
Puna_slope.utm<- crop(Puna_slope.utm,Shp_area.utm)






## ----CovariatesCreation----------------------------
#It is important to verify that the extension, the number of layers (dates) 
#and the resolution of the covariates are the same

#RFMEP
covariates_rfmep <- list( chirps=chirps.utm, 
                          persiann=persiann_puna.utm,
                          era5_pp=era5_pp.utm,
                          dem=DEM_area.utm)
#RFPUNA
covariates_rfpuna <- list( chirps=chirps.utm, 
                           persiann=persiann_puna.utm,
                           era5_pp=era5_pp.utm, imerg=imerg.utm,
                           era5_temp = era5_temp.utm,
                           era5_p = era5_p.utm, era5_rad= era5_rad.utm,
                           era5_pev= era5_pev.utm,
                           dem=DEM_basin.utm, aspect=Puna_aspect.utm,
                           slope=Puna_slope.utm)

#Final set up and Random Forest----

##Training < 1 (To evaluate with training and evaluation datasets)---- 

set.seed(1)
par.nnodes <- min(parallel::detectCores()-1, 4)
drty.out_rfpuna <- file.path(tempdir(), "Test.1")

rfpuna <- RFmerge(x=x.zoo, metadata=gis.utm, cov=covariates_rfpuna,
                  id="id", lat="lat", lon="lon", mask=Shp_basin.utm ,
                  training=1, write2disk=TRUE, drty.out=drty.out_rfpuna,
                  par.nnodes=par.nnodes, ntree = 2000)

ts.path <- paste0(drty.out_rfpuna, "/Ground_based_data/Evaluation/Evaluation_ts.txt")
metadata.path <- paste0(drty.out_rfpuna, "/Ground_based_data/Evaluation/Evaluation_metadata.txt")
eval.ts <- read.zoo(ts.path, header = TRUE)
eval.gis <- read.csv(metadata.path)
# promoting 'eval.gis' into a spatial object (in order to be plotted)
( eval.gis.utm <- st_as_sf(eval.gis, coords = c('lon', 'lat'), crs = 32719) )

coordinates(eval.gis) <- ~ lon + lat
rfpuna.ts <- t(raster::extract(rfpuna, eval.gis))
chirps.ts <- t(raster::extract(chirps.utm, eval.gis))
persiann.ts <- t(raster::extract(persiann_puna.utm, eval.gis))
era5.ts <- t(raster::extract(era5_pp.utm, eval.gis))

sres <- list(chirps.ts, persiann.ts, era5.ts, rfpuna.ts)
nsres <- length(sres)
nstations <- ncol(eval.ts)
tmp <- rep(NA, nstations)
kge.table <- data.frame(ID=eval.gis[["ID"]], CHIRPS=tmp, PERSIANN_CDR=tmp, 
                        ERA5=tmp, RF_MEP=tmp)

# Computing the KGE between the observed rainfall measured in each one of the raingauges
# of the training dataset and CHIRPSv2, PERSIANN-CDR, the merged product `rfmep`:
for (i in 1:nsres) {
  ldates <- time(eval.ts)
  lsim <- zoo(sres[[i]], ldates)
  kge.table[, (i+1)] = hydroGOF::KGE(sim= lsim, obs= eval.ts, method="2012")
} # FOR end

# Boxplot with a graphical comparison
sres.cols <- rainbow(4)
boxplot(kge.table[,2:5], main = "KGE evaluation for Jan - Aug 1983",
        xlab = "P products", ylab = "KGE'", ylim = c(0, 1), # horizontal=TRUE,
        col=sres.cols)
legend("topleft", legend=c("CHIRPS", "PERSIANN-CDR","ERA5", "RFPUNA"), col=sres.cols,
       pch=15, cex=1.5, bty="n")
grid()


##Training = 1 (To evaluate with Leave One Out Cross Validation method)---- 

set.seed(1)
par.nnodes <- min(parallel::detectCores()-1, 4)
drty.out_rfpuna <- file.path(tempdir(), "Test.2")

rfpuna_stack<-list()
( ncol(x.zoo) == nrow(gis.utm) )
{
  start.time<- Sys.time()
  j <- ncol(est)
  aux <- 1:j
  
  for(k in 1:j){ #estaciones

    rango_rfpuna <- aux[-k]
    est_aux <- est[,rango_rfpuna]
    
    #(ncol(est1) == nrow(gis1) )
    gis_aux<-gis[rango_rfpuna,]
    
    fechas <- as.Date(row.names(est_aux))
    x.zoo <- zoo(est_aux, order.by = fechas) 
    x.zoo <- as.zoo( apply(x.zoo, 2, function(x) ifelse(x == "<NA>", NA, as.numeric(x))))
    
    ## ----SpatialMetadata
    stations <- gis_aux
    
    ( stations <- st_as_sf(stations, coords = c('lon', 'lat'), crs = 4326) )
    
    stations.utm <- sf::st_transform(stations, crs=32719) 
    st.coords <- st_coordinates(stations.utm)
    lon <- st.coords[, "X"]
    lat <- st.coords[, "Y"]
    gis.utm <- data.frame(id=stations.utm[["ID"]], lon=lon, lat=lat)
    rfpuna <- RFmerge(x=x.zoo, metadata=gis.utm, cov=covariates_rfpuna,
                      id="id", lat="lat", lon="lon", mask=Shp_basin.utm ,
                      training=1, write2disk=TRUE, drty.out=drty.out_rfpuna,
                      par.nnodes=par.nnodes, ntree = 2000)
    rfpuna_stack[[k]]<-stack(rfpuna) 
  }
  
  end.time<- Sys.time()
  print(round(end.time-start.time,2))
  
}
