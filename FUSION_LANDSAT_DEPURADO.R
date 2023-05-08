#CODIGO DEPURADO FUSION DE IMAGENES


library(raster)
library(terra)
library(rgdal)

setwd("C:/MASTER_GOFOREST/2023/SENSORES/tarea_isable_2")



LST <- list.files(".", recursive = TRUE, full.names= TRUE, pattern = "B[12345678].TIF")
LST
LST <- lapply(1:length(LST), function (x) {raster(LST[x])})

#cargamos en un stack las bandas multiespectrales y por otro lado 
#la banda Pancromatica

B8<-LST[[8]]
LST<-LST[1:7]
LST_stack <- stack(LST)

# we do a clip centered in our region of interest in this case 
#is the Natural Park "Punta entinas del Sabinar" located in El Ejido, Almería.

PES<-readOGR("./PES.shp")

#we could not do a stack of all of our images because Pancromatic band
#has less scale or size of pixel compared with the others, the multiespectral images.

PES_stack_crop <- terra::crop(LST_stack, PES)
plot(PES_stack_crop)

B8<-terra::crop(B8,PES)
plot(B8)

# 1- DATA REESCALE

PES_15<-resample(PES_stack_crop,B8)

dim(PES_15)
dim(B8)

#Calculate mean and standar desviation of each band of multiespectral images

#extract values of each band of image

sts <- data.frame(B_mean = numeric(),B_sd = numeric())

for (i in 1:ncol(PES_15@data@values)){
  sts[i,1] <-mean(PES_15@data@values[,i])
  sts[i,2] <-sd(PES_15@data@values[,i])
  rownames(sts)[i]<-paste0("B",i, sep="")
}

#añadimos las estadisticas mean y sd de la banda pancromatica a tabla de estadisticas

sts[c("B8"),1]<-c(mean(B8[]))
sts[c("B8"),2]<-c(sd(B8[]))

# Calculate the parameters of the transformation image: Torus's Method 

#we do the same loop to calculate ai and bi from each spectral band

# ai =  sd_Bi/sd_B8_PAN

# bi =  mean_Bi-((sd_Bi/sd_B8_PAN)*mean_B8_PAN)

# And finally:

#PAN_Bi = ai * PAN +bi

# WE CALCULATE

#define a data.frame with a and b like empty comlumns
rm(param)
param<-data.frame(a= numeric(),
                  b= numeric())

#calculate each index for each spectral band
for (i in 1:ncol(PES_15@data@values)){
  param[i,1] <-sts$B_mean[i]/sts$B_mean[8]
  param[i,2] <-sts$B_mean[i]-((sts$B_sd[i]/sts$B_sd[8])*sts$B_mean[8])
  rownames(param)[i]<-paste0("B",i, sep="")
}

#PAN_B1 = a1 * B8 +b1

rm(PAN_stk)
PAN_stk<-list()

for (i in 1:ncol(PES_15@data@values)){
  PAN_stk[i] <-param$a[i]*B8 +param$b[i]
  names(PAN_stk[[i]])<-paste0("B",i, sep="")
}

PAN_stk<-stack(PAN_stk)
plot(PAN_stk)

#Define the filter (kernel1)
# Apply the filter that will smooth each image

kernel1 <- matrix(c(0.0039060000, 0.015625000, 0.023438000, 0.015625000, 0.0039060000,
                    0.015625000, 0.062500000, 0.093750000, 0.062500000, 0.015625000,
                    0.023438000, 0.093750000, 0.14062500, 0.093750000, 0.023438000,
                    0.015625000, 0.062500000, 0.093750000, 0.062500000, 0.015625000,
                    0.0039060000, 0.015625000, 0.023438000, 0.015625000, 0.0039060000), 
                  nrow = 5)

print(kernel1)

#Apply the filter to all multiespectral images transformed
filtered<-list()
for (i in 1:ncol(PES_15@data@values)){
  filtered[i] <-focal(PAN_stk[[i]],kernel1)
  names(filtered[[i]])<-paste0("B",i, sep="")
}

filtered_stack<-stack(filtered)
plot(filtered_stack)

#When we alpply the low filter o the images next step is subtract this images with originals images


Detail<-list()

for (i in 1:ncol(PES_15@data@values)){
  Detail[i] <-PAN_stk[[i]]-filtered[[i]]
  names(Detail[[i]])<-paste0("DetB",i,sep="")
}

Detail_stk<-stack(Detail)
plot(Detail_stk)

#once we applied the "filtro por lo alto" we sum the bands 


fusion<-list()

for (i in 1:ncol(PES_15@data@values)){
  fusion[i] <-PES_15[[i]]+Detail_stk[[i]]
  names(fusion[[i]])<-paste0("FusB",i,sep="")
}

fusion_stk<-stack(fusion)
plot(fusion_stk)

plotRGB(fusion_stk, r=5, g=4, b=3, scale=maxValue(fusion_stk[[5]]), stretch= "hist")

writeRaster(fusion_stk,"./stack_fus_automatic.tiff")

#una vez que esta echa la funsion de las imagenes puedo hacer una clasificaion no supervisada y
#comparar si existen variaciones en la clasificacion gracias a la fusion de la imagen

b<-fusion_stk
b[is.na(b)]<-0
kMeansResult <- kmeans(scale(b[]), centers=6)
kMeansResult$centers

#creamos el raster vacio para luego rellenarlo
result <- raster(b[[1]])

#rellenamos el raster creado con los datos del cluster del k-means
result <- setValues(result, kMeansResult$cluster)
writeRaster(result,"./stack_fus_kmeans.tiff")

#lot(result, col=c("red","black","yellow","green","blue","white"))

##########################################################################
#ahora probamos la clasificacion con las imagenes normales sin modificar
##########################################################################


b<-PES_stack_crop
b[is.na(b)]<-0
kMeansResult <- kmeans(scale(b[]), centers=6)
kMeansResult$centers

#creamos el raster vacio para luego rellenarlo
result <- raster(b[[1]])

#rellenamos el raster creado con los datos del cluster del k-means
result <- setValues(result, kMeansResult$cluster)
writeRaster(result,"./landsat_kmeans.tiff")

#lot(result, col=c("red","black","yellow","green","blue"))
