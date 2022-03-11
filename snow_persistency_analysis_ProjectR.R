
library(sp)
library(raster)
library(ggplot2)
#unlink("C:/Users/giris/Dropbox/PC/Documents/R/win-library/4.1/00LOCK-ggspatial", recursive = TRUE)options("install.lock"=FALSE)
#install.packages("ggplot2")

library(ggspatial)

  # clean R working environment
  rm(list=ls())

  # set working directory 
  setwd('C:/Users/giris/Desktop/R_Project/data_2017')

  # lst all files within the folder (make sure that you only have tiff file inside the folder)
  t_list=list.files(getwd(), pattern='.tif$')

  # print tiff list
  print(t_list)

  # let's make raster stack using all tif image that are listed above
  snow_stack=stack(t_list[1:46])

  # get detail of the stack
  snow_stack

  # get frequency of first layer
  freq(snow_stack[[1]])

  # make plot of first layer
  plot(snow_stack[[1]])

  # no snow is represented by NA in the data, replace NA by zero
  snow_stack[is.na(snow_stack)]=0 # this will take some time

  # check frequency of each class in the data
  freq(snow_stack[[1]]);

  # make plot of some raster within the stack
  #plot(snow_stack)

  # make plot of first layer in the stack
  plot(snow_stack[[1]])

  # import solukhumbu polygon
  solu_poly = shapefile('C:/Users/giris/Desktop/R_Project/Solukhumbu.shp')

  # visualize it
  plot(solu_poly,axes=T)


  # plot snow data and overlay solukhumbu polygon
  plot(snow_stack[[1]])
  plot(solu_poly,add=T)

  # mask snow data by solu polygon
  solu_poly_snow=mask(snow_stack,solu_poly)

  # plot  solu snow, 1st layer
  plot(solu_poly_snow[[1]])
  plot(solu_poly,add=T,lwd=2)

  # now we will reclassify the raster stack into two classes before deriving snow persistency (Snow class is according to MODIS manual)
  solu_poly_snow[solu_poly_snow==200]<-1
  solu_poly_snow[solu_poly_snow==150|solu_poly_snow==100|solu_poly_snow==50|solu_poly_snow==25|solu_poly_snow==37]<-0

  
  # plot and get frequency of a layer
  plot(solu_poly_snow[[1]])
    freq(solu_poly_snow[[1]])

  # now calculate snow cover persistency for a specific year
  SP_year=sum(solu_poly_snow)/nlayers(solu_poly_snow)*100
 
  # finally visualize SP map
  
  plot(SP_year)
  
  # make histogram of SP to better visualize the frequency distribution of SP
  hist(SP_year)

  # let's make some fancy plots using ggplot2 package


  df_SP=as.data.frame(SP_year,xy=T)
  str(df_SP)

  df_SP=na.omit(df_SP)

  #png("Snow_persistency 2017.png")
  ggplot()+ ggtitle("Annual Snow Persistency Map of Solukhumbu (2017)")+
  geom_tile(data=df_SP,aes(x = x, y = y,fill =layer), alpha = 0.8)+
  xlab('Longitude')+ylab("Latitude")+
  theme(plot.title = element_text(size = 8, face = "bold"),
      legend.title=element_text(size=8),legend.text 
      =element_text(size=6),
      axis.text = element_text(size=8,colour = 'black'),
      axis.title =element_text(size=8,colour = 'black'))+ theme_bw()+
  geom_polygon(data = solu_poly, aes(x = long, y = lat, group = group), 
             colour = "black", fill = NA,size=0.5)
  #dev.off()

  # export annual_SP raster
  #writeRaster(SP_year,"C:/Users/giris/Desktop/R_Project/Outputs/SP_solukhumbu_2017.tif",format="GTiff",overwrite=T)


  # classify SP into different snow zones based on Hammond et al., 2017)

  a <- c(-1, 7, 1,  7,30 ,2, 30,90, 3, 90,101,4)
  rclmat <- matrix(a, ncol=3, byrow=TRUE)
  SP_class <- reclassify(SP_year, rclmat)
  plot(SP_class)
  freq(SP_class)

  #make df of sp class to use using ggplot
  df_SP_reclassify=as.data.frame(SP_class,xy=T)
  str(df_SP_reclassify)

  df_SP_reclassify=na.omit(df_SP_reclassify)

  ## Make reclassified map in the using ggplot2
  #uncomment png and dev.off() to save image as png
  #png("reclassified 2017.png")
  ggplot()+ ggtitle("Reclassified Snow Zones of Solukhumbu (2017)")+
    geom_tile(data=df_SP_reclassify,aes(x = x, y = y,fill =layer), alpha = 0.8)+
    xlab('Longitude')+ylab("Latitude")+
    theme(plot.title = element_text(size = 8, face = "bold"),
        legend.title=element_text(size=8),legend.text 
        =element_text(size=6),
        axis.text = element_text(size=8,colour = 'black'),
        axis.title =element_text(size=8,colour = 'black'))+ theme_bw()+
    geom_polygon(data = solu_poly, aes(x = long, y = lat, group = group), 
               colour = "black", fill = NA,size=0.5)
  #dev.off()

  # export SP class
  writeRaster(SP_class,"C:/Users/giris/Desktop/R_Project/Outputs/SP_solukhumbu_2017_reclassified.tif",format="GTiff",overwrite=T)

  #calculating  the percentage of each reclassified map
  f_sp_class=freq(SP_class)
  f_sp_class
  total_frequency=sum(f_sp_class[-5,2])
  percent_snow_zone=f_sp_class[-5,2]/total_frequency*100
  percent_snow_zone
  sum(percent_snow_zone)
  #######################################################################################
  #print(paste0('Little or no snow cover is ',percent_snow_zone[1]," % of land area in 2017"))
  #print(paste0('intermittent snow cover is ',percent_snow_zone[2]," % of land area in 2017"))
  #print(paste0('seasonal snow cover is ',percent_snow_zone[3]," % of land area in 2017"))
  #print(paste0('permanent snow cover is ',percent_snow_zone[4]," % of land area in 2017"))


  # snow cover area calculation -------------------------------------------------------------

  #counting the pixels of snow, no snow, and NA zones 
  pixel_count=t(freq(solu_poly_snow,merge=T))

  # get first 6 row of pixel count
  head(pixel_count)

  # convert to snow cover area as a percent of total pixel
  snow_pixel=as.numeric(pixel_count[-1,2]) # why -1 and 2 here
  sum(snow_pixel)
  total_pixel_count=unique(rowSums(pixel_count[-1,]))
  SCA_percent=snow_pixel/total_pixel_count*100

  #png('SCA_2017.png')
  plot(SCA_percent,type='b',xlab='',ylab='SCA(%)')
  MODIS_data_date=seq(as.Date('2017-01-01'),as.Date('2017-12-31'),by='8 day')
  plot(MODIS_data_date,SCA_percent,type='b',main = "Snow Cover Area (SCA) of Solukhumbu(2017)",xlab='Month',ylab='SCA (%)')
  grid()
  #dev.off()


  ###### Find the Conversion Factor #################
  ###########calculate snow cover area in square kilometer
  #according manual,  spatial extent : 500m*500m
  #changing ko sq.km spatial extent:



  SCA_sq_km=snow_pixel*0.25 # sq.kilometre equivalent 

  png('SCA_2017(sq.km).png')
  # plot sca_sq_km as a function of date
  plot(MODIS_data_date,SCA_sq_km,type='b',main = "Snow Cover Area (SCA) of Solukhumbu(2017)",xlab='Month',ylab='SCA (Sq.km)')
  grid()
    dev.off()

  # store date and sca in data frame and export
  df_output=data.frame(Date=MODIS_data_date,SCA_percent=SCA_percent,SCA_Sq_km=SCA_sq_km)

  # export csv
  write.csv(df_output,'C:/Users/giris/Desktop/R_Project/csv_outputs/SCA(2017).csv',row.names=F)

#----------------------------------THE END ----------------------------------------------------------------------------------------