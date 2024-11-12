#' A function to concatenate SEBASTES data to .csv files
#'
#' This function inputs a directory folder containing multiple stereo camera deployments,
#' reads all the *.db files and extracts the frame data and the target data from all the
#' deployments contained in the directory. It then concatenates these together, assigning
#' a unique identifier (deployment_ID) and outputs two .csv files, one containing target
#' data and one containing the frame data. Any accessory data from sensors will also be
#' included with the frame data.
#' @param project.dir directory containing the multiple deployment files 
#' @keywords stereo camera, SEBASTES, data concatenation
#' @export
#' @examples
#' SEB_data_concatenate("C:/Users/rooperc/Desktop/Rockfish Projects/Longline Survey Gear Comparisons/TrigCamData/Gadus")

SEB_data_concatenate<-function(project.dir){
  require(RSQLite)
  require(lubridate)
  require(tidyr)
  #project.dir<-"D:/SeamountTransectData"
  `%nin%` = Negate(`%in%`)
  deployments<-list.dirs(project.dir,recursive=FALSE,full.names=TRUE)
  target.data<-NULL
  frame.data<-NULL
  
  for(i in 1:length(deployments)){
  
  data_files<-list.files(paste0(deployments[i],"/data"),pattern=".db",recursive=TRUE,full.names=TRUE)
  data_files2<-list.files(paste0(deployments[i],"/data"),pattern=".sql3",recursive=TRUE,full.names=TRUE)
  if(length(data_files2)>0){
    print("It looks like you have .sql3 data files, are you using an older version of SEBASTES? If so use the SEB_data_concatenate_sql function")
    break}
  
  acc_files<-paste0(deployments[i],"/logs/","CamTrawlMetadata.db3")
  if(length(data_files)==0){next}
  
  drop.datac<-dbConnect(RSQLite::SQLite(),dbname=data_files)
  frame.datat<-dbReadTable(drop.datac, "FRAMES")
  substrate.data<-dbReadTable(drop.datac,"FRAME_METADATA")
  substrate.data<-subset(substrate.data,substrate.data$METADATA_VALUE!="")
  substrate.data<-pivot_wider(substrate.data,names_from = METADATA_TYPE,values_from=METADATA_TAG)
  substrate.data<-data.frame(FRAME_NUMBER=substrate.data$FRAME_NUMBER,Primary_habitat=substrate.data$`Primary Habitat`,Secondary_habitat=substrate.data$`Secondary Habitat`)
  frame.datat<-merge(frame.datat,substrate.data,by="FRAME_NUMBER",all.x=TRUE)
  frame.datat<-frame.datat[,-3]
  name1<-unique(frame.datat$DEPLOYMENT_ID)
  target.datat<-dbReadTable(drop.datac, "TARGETS")
  dbDisconnect(drop.datac)
  
  acc.datac<-dbConnect(RSQLite::SQLite(),dbname=acc_files)
  acc.data<-dbReadTable(acc.datac,"sensor_data")
  image.data<-dbReadTable(acc.datac,"images")
  dbDisconnect(acc.datac)
  
  acc.datad<-dbConnect(RSQLite::SQLite(),dbname=acc_files)
  acc.dataad<-dbReadTable(acc.datad,"async_data")
  dbDisconnect(acc.datad)
  
  depth<-acc.data$data[acc.data$sensor_id=="CTControl"]
  depth<-gsub("\\$OHPR,","",depth)
  
  depth<-matrix(unlist(strsplit(depth, ",")),ncol=8,byrow=TRUE)[,1:8]
  depth<-apply(depth, 2, as.numeric)
  depth<-data.frame(acc.data$number[acc.data$sensor_id=="CTControl"],depth)
  colnames(depth)<-c("FRAME_NUMBER","HEADING","PITCH","ROLL","TEMPERATURE","DEPTH","ACCEL_X","ACCEL_Y","ACCEL_Z")
  
  image.data<-unique(data.frame(FRAME_NUMBER=image.data$number,time=image.data$time))

  if("GPS"%in%unique(acc.data$sensor_id)){
  gps<-matrix(unlist(sapply(strsplit(acc.data$data[acc.data$sensor_id=="GPS"], ","),'[',c(3,5))),ncol=2,byrow=TRUE)
  gps<-apply(gps, 2, as.numeric)
  gps<-gps/100
  gps<-data.frame(acc.data$number[acc.data$sensor_id=="GPS"],gps)
  colnames(gps)<-c("FRAME_NUMBER","LATITUDE","LONGITUDE")
  depth<-merge(depth,gps,by="FRAME_NUMBER",all.x=TRUE) 
  }
  
  
  
  if("GPS"%nin%unique(acc.data$sensor_id)&"GPS"%in%unique(acc.dataad$sensor_id)){
  datetime1<-data.frame(FRAME_TIME=as.POSIXct(acc.data$time),FRAME_NUMBER=acc.data$number)
  datetime1$FRAME_TIME<-round_date(datetime1$FRAME_TIME,unit="second")
  gps<-matrix(unlist(sapply(strsplit(acc.dataad$data[acc.dataad$sensor_id=="GPS"], ","),'[',c(3,5))),ncol=2,byrow=TRUE)
  gps<-apply(gps, 2, as.numeric)
  gps<-gps/100
  gps<-data.frame(acc.dataad$time[acc.dataad$sensor_id=="GPS"],gps)
  colnames(gps)<-c("FRAME_TIME","LATITUDE","LONGITUDE")
  gps$FRAME_TIME<-round_date(as.POSIXct(gps$FRAME_TIME),unit="second")
  gps<-merge(gps,datetime1,by="FRAME_TIME")
  gps<-gps[!duplicated(gps$FRAME_TIME),]
  gps<-gps[,-1]
  depth<-merge(depth,gps,by="FRAME_NUMBER",all.x=TRUE) 
  }
  
  if("GPS"%nin%unique(acc.data$sensor_id)&"GPS"%nin%unique(acc.dataad$sensor_id)){
    depth$LATITUDE<-NA
    depth$LONGITUDE<-NA}
  
  depth<-merge(image.data,depth,by="FRAME_NUMBER",all.x=TRUE)
  #depth$IMAGE_NUMBER<-depth$FRAME_NUMBER
  #depth$FRAME_NUMBER<-seq(1,length(depth$FRAME_NUMBER),1)
  
  frame.datat<-merge(frame.datat,depth,by="FRAME_NUMBER",all=TRUE) 
      
    frame.datat$DEPLOYMENT_ID[is.na(frame.datat$DEPLOYMENT_ID)]<-name1
    colnames(frame.datat)[colnames(frame.datat)=="time"]<-"FRAME_TIME"
  
    frame.data<-rbind(frame.data,frame.datat)
    target.data<-rbind(target.data,target.datat)}
  
  write.csv(target.data,file=paste(project.dir,"/targetout_data.csv",sep=""),row.names=FALSE)
  write.csv(frame.data,file=paste(project.dir,"/frameout_data.csv",sep=""),row.names=FALSE)
  return(list(target.data=target.data,frame.data=frame.data))}

#' A function to concatenate SEBASTES data to .csv files
#'
#' This function inputs a directory folder containing multiple stereo camera deployments,
#' reads all the *.db files and extracts the frame data and the target data from all the
#' deployments contained in the directory. It then concatenates these together, assigning
#' a unique identifier (deployment_ID) and outputs two .csv files, one containing target
#' data and one containing the frame data. Any accessory data from sensors will also be
#' included with the frame data. This function works with the new "project" style data.
#' @param project.dir directory containing the multiple deployment files 
#' @keywords stereo camera, SEBASTES, data concatenation
#' @export
#' @examples
#' SEB_data_concatenate_project("C:/Users/rooperc/Desktop/Rockfish Projects/Longline Survey Gear Comparisons/TrigCamData/Gadus")

SEB_data_concatenate_project<-function(project.dir){
  require(RSQLite)
  require(lubridate)
  require(tidyr)
  #project.dir<-"D:/SeamountTransectData"
  `%nin%` = Negate(`%in%`)
  deployments<-list.dirs(project.dir,recursive=FALSE,full.names=TRUE)
  target.data<-NULL
  frame.data<-NULL
  
  for(i in 1:length(deployments)){
    
    data_files<-list.files(paste0(deployments[i],"/data"),pattern=".db",recursive=TRUE,full.names=TRUE)
    data_files2<-list.files(paste0(deployments[i],"/data"),pattern=".sql3",recursive=TRUE,full.names=TRUE)
    if(length(data_files2)>0){
      print("It looks like you have .sql3 data files, are you using an older version of SEBASTES? If so use the SEB_data_concatenate_sql function")
      break}
    
    acc_files<-paste0(deployments[i],"/logs/","CamTrawlMetadata.db3")
    if(length(data_files)==0){next}
    
    drop.datac<-dbConnect(RSQLite::SQLite(),dbname=data_files)
    frame.datat<-dbReadTable(drop.datac, "FRAMES")
    frame.datat<-frame.datat[,-c(1,5:6,8:9)]
    substrate.data<-dbReadTable(drop.datac,"FRAME_METADATA")
    substrate.data<-subset(substrate.data,substrate.data$METADATA_VALUE!=""&substrate.data$METADATA_VALUE!="NULL")
    substrate.data<-pivot_wider(substrate.data[,2:7],names_from = METADATA_TYPE,values_from=METADATA_TAG)
    substrate.data<-data.frame(FRAME_NUMBER=substrate.data$FRAME_NUMBER,Primary_habitat=substrate.data$`Primary Habitat`,Secondary_habitat=substrate.data$`Secondary Habitat`)
    frame.datat<-merge(frame.datat,substrate.data,by="FRAME_NUMBER",all.x=TRUE)
    frame.datat<-frame.datat[,-3]
    name1<-unique(frame.datat$DEPLOYMENT_ID)
    target.datat<-dbReadTable(drop.datac, "TARGETS")
    dbDisconnect(drop.datac)
    
    acc.datac<-dbConnect(RSQLite::SQLite(),dbname=acc_files)
    acc.data<-dbReadTable(acc.datac,"sensor_data")
    image.data<-dbReadTable(acc.datac,"images")
    dbDisconnect(acc.datac)
    
    acc.datad<-dbConnect(RSQLite::SQLite(),dbname=acc_files)
    acc.dataad<-dbReadTable(acc.datad,"async_data")
    dbDisconnect(acc.datad)
    
    depth<-acc.data$data[acc.data$sensor_id=="CTControl"]
    depth<-gsub("\\$OHPR,","",depth)
    
    depth<-matrix(unlist(strsplit(depth, ",")),ncol=8,byrow=TRUE)[,1:8]
    depth<-apply(depth, 2, as.numeric)
    depth<-data.frame(acc.data$number[acc.data$sensor_id=="CTControl"],depth)
    colnames(depth)<-c("FRAME_NUMBER","HEADING","PITCH","ROLL","TEMPERATURE","DEPTH","ACCEL_X","ACCEL_Y","ACCEL_Z")
    
    image.data<-unique(data.frame(FRAME_NUMBER=image.data$number,time=image.data$time))
    
    if("GPS"%in%unique(acc.data$sensor_id)){
      gps<-matrix(unlist(sapply(strsplit(acc.data$data[acc.data$sensor_id=="GPS"], ","),'[',c(3,5))),ncol=2,byrow=TRUE)
      gps<-apply(gps, 2, as.numeric)
      gps<-gps/100
      gps<-data.frame(acc.data$number[acc.data$sensor_id=="GPS"],gps)
      colnames(gps)<-c("FRAME_NUMBER","LATITUDE","LONGITUDE")
      depth<-merge(depth,gps,by="FRAME_NUMBER",all.x=TRUE) 
    }
    
    
    
    if("GPS"%nin%unique(acc.data$sensor_id)&"GPS"%in%unique(acc.dataad$sensor_id)){
      datetime1<-data.frame(FRAME_TIME=as.POSIXct(acc.data$time),FRAME_NUMBER=acc.data$number)
      datetime1$FRAME_TIME<-round_date(datetime1$FRAME_TIME,unit="second")
      gps<-matrix(unlist(sapply(strsplit(acc.dataad$data[acc.dataad$sensor_id=="GPS"], ","),'[',c(3,5))),ncol=2,byrow=TRUE)
      gps<-apply(gps, 2, as.numeric)
      gps<-gps/100
      gps<-data.frame(acc.dataad$time[acc.dataad$sensor_id=="GPS"],gps)
      colnames(gps)<-c("FRAME_TIME","LATITUDE","LONGITUDE")
      gps$FRAME_TIME<-round_date(as.POSIXct(gps$FRAME_TIME),unit="second")
      gps<-merge(gps,datetime1,by="FRAME_TIME")
      gps<-gps[!duplicated(gps$FRAME_TIME),]
      gps<-gps[,-1]
      depth<-merge(depth,gps,by="FRAME_NUMBER",all.x=TRUE) 
    }
    
    if("GPS"%nin%unique(acc.data$sensor_id)&"GPS"%nin%unique(acc.dataad$sensor_id)){
      depth$LATITUDE<-NA
      depth$LONGITUDE<-NA}
    
    depth<-merge(image.data,depth,by="FRAME_NUMBER",all.x=TRUE)
    #depth$IMAGE_NUMBER<-depth$FRAME_NUMBER
    #depth$FRAME_NUMBER<-seq(1,length(depth$FRAME_NUMBER),1)
    
    frame.datat<-merge(frame.datat,depth,by="FRAME_NUMBER",all=TRUE) 
    
    frame.datat$DEPLOYMENT_ID[is.na(frame.datat$DEPLOYMENT_ID)]<-name1
    colnames(frame.datat)[colnames(frame.datat)=="time"]<-"FRAME_TIME"
    
    frame.data<-rbind(frame.data,frame.datat)
    target.data<-rbind(target.data,target.datat)}
  
  write.csv(target.data,file=paste(project.dir,"/targetout_data.csv",sep=""),row.names=FALSE)
  write.csv(frame.data,file=paste(project.dir,"/frameout_data.csv",sep=""),row.names=FALSE)
  return(list(target.data=target.data,frame.data=frame.data))}



#' A function to concatenate SEBASTES data to .csv files
#'
#' This function inputs a directory folder containing multiple stereo camera deployments,
#' reads all the *.sql files and extracts the frame data and the target data from all the
#' deployments contained in the directory. It then concatenates these together, assigning
#' a unique identifier (deployment_ID) and outputs two .csv files, one containing target
#' data and one containing the frame data. Any accessory data from sensors will also be
#' included with the frame data. This function works with pre-2020 SEBASTES output.
#' @param project.dir directory containing the multiple deployment files 
#' @keywords stereo camera, SEBASTES, data concatenation
#' @export
#' @examples
#' SEB_data_concatenate("C:/Users/rooperc/Desktop/Rockfish Projects/Longline Survey Gear Comparisons/TrigCamData/Gadus")

SEB_data_concatenate_sql3<-function(project.dir){
  require(RSQLite)
  # project.dir<-"C:/Users/rooperc/Desktop/test"
  `%nin%` = Negate(`%in%`)
  deployments<-list.dirs(project.dir,recursive=FALSE,full.names=TRUE)
  target.data<-NULL
  frame.data<-NULL
  
  for(i in 1:length(deployments)){
    
    data_files<-list.files(paste0(deployments[i],"/data"),pattern=".sql3",recursive=TRUE,full.names=TRUE)
    acc_files<-paste0(deployments[i],"/logs/","CamTrawlMetadata.db3")
    if(length(data_files)==0){next}
    
    drop.datac<-dbConnect(RSQLite::SQLite(),dbname=data_files)
    frame.datat<-dbReadTable(drop.datac, "FRAMES")
    name1<-unique(frame.datat$DEPLOYMENT_ID)
    target.datat<-dbReadTable(drop.datac, "TARGETS")
    dbDisconnect(drop.datac)
    
    acc.datac<-dbConnect(RSQLite::SQLite(),dbname=acc_files)
    acc.data<-dbReadTable(acc.datac,"sensor_data")
    dbDisconnect(acc.datac)

    depth<-matrix(unlist(strsplit(acc.data$data[acc.data$sensor_id=="CTControl"], ",")),ncol=9,byrow=TRUE)[,2:9]
    depth<-apply(depth, 2, as.numeric)
    depth<-data.frame(acc.data$number[acc.data$sensor_id=="CTControl"],depth)
    colnames(depth)<-c("FRAME_NUMBER","HEADING","PITCH","ROLL","TEMPERATURE","DEPTH","ACCEL_X","ACCEL_Y","ACCEL_Z")
    
    if("GPS"%in%unique(acc.data$sensor_id)){
      gps<-matrix(unlist(sapply(strsplit(acc.data$data[acc.data$sensor_id=="GPS"], ","),'[',c(3,5))),ncol=2,byrow=TRUE)
      gps<-apply(gps, 2, as.numeric)
      gps<-gps/100
      gps<-data.frame(acc.data$number[acc.data$sensor_id=="GPS"],gps)
      colnames(gps)<-c("FRAME_NUMBER","LATITUDE","LONGITUDE")
      depth<-merge(depth,gps,by="FRAME_NUMBER",all.x=TRUE) 
    }
    
    if("GPS"%nin%unique(acc.data$sensor_id)){
      depth$LATITUDE<-NA
      depth$LONGITUDE<-NA}
    
    frame.datat<-merge(frame.datat,depth,by="FRAME_NUMBER",all=TRUE) 
    
    frame.datat$DEPLOYMENT_ID[is.na(frame.datat$DEPLOYMENT_ID)]<-name1
    
    frame.data<-rbind(frame.data,frame.datat)
    target.data<-rbind(target.data,target.datat)
    print(i)}
  
  write.csv(target.data,file=paste(project.dir,"/targetout_data.csv",sep=""),row.names=FALSE)
  write.csv(frame.data,file=paste(project.dir,"/frameout_data.csv",sep=""),row.names=FALSE)
  return(list(target.data=target.data,frame.data=frame.data))}





#' A function to replace SEBASTES deployment data with updated versions
#'
#' This inputs a folder containing *.db files from SEBASTES annotations and uses their 
#' file name to overwrite the existing *.db files in deployment folders. This is a handy 
#' function if you have multiple people working on SEBASTES deployments with multiple
#' copies of the images. Use this function to update the old data with the new data after
#' correcting identifications or remeasuring individuals or processing for different groups
#' of species.
#' @param new.data.dir a directory containing the new data (*.db) files 
#' @param project.dir directory containing original deployment files (images, logs, settings)
#' @param name_start a character defining the start of the deployment designation. In SEBASTES this is
#' typically the default "D" (e.g. D20210903-T223225)
#' @keywords stereo camera, SEBASTES, data concatenation, data wrangling
#' @export
#' @examples
#' SEB_data_replace("C:/Users/rooperc/Desktop/Rockfish Projects/Longline Survey Gear Comparisons/TrigCamData/Gadus data/Gadus data","C:/Users/rooperc/Desktop/Rockfish Projects/Longline Survey Gear Comparisons/TrigCamData/Gadus")

SEB_data_replace<-function(new.data.dir,project.dir,name_start="D"){
  #testing
  #new.data.dir<-"D:/Longline Survey Gear Comparisons/TrigCamData/Raja/Raja data/Raja data"
  #project.dir<-"D:/Longline Survey Gear Comparisons/TrigCamData/Raja"
  #name_start<-"D"
  
  require(RSQLite)
  deployments<-list.dirs(project.dir,recursive=FALSE,full.names=FALSE)
  deployment_paths<-list.dirs(project.dir,recursive=FALSE,full.names=TRUE)
  data_files<-list.files(new.data.dir,pattern =paste0("^[",name_start,"]" ))
  dfiles<-list.files(new.data.dir,pattern =paste0("^[",name_start,"]" ),full.names=TRUE)
  
  dname<-unlist(strsplit(data_files,split=".db"))
  t2<-0
  
  for(i in 1:length(dname)){
    
    t1<-which(deployments==dname[i])   
    if(dir.exists(paste0(deployment_paths[t1],"/data"))==FALSE){dir.create(paste0(deployment_paths[t1],"/data"))}
    
    file.rename(from = dfiles[i],  to = paste0(deployment_paths[t1],"/data/",data_files[i]))
    t2<-t2+1
    print(dname[i])}

  
  return(print(paste0("Successfully moved ",t2," files")))}


#' A function to append GPS data to TrigCam SEBASTES deployment data
#'
#' This function merges a GPS position to frame data from SEBASTES output for point data. The GPS data is a 
#' single pair of Latitude and Longitude for each deployment (e.g. TrigCam deployments). A
#' third column identifies the Deployment_ID. Accessory data, such as depth inlcuded in the 
#' data frame can also be appended to the output. 
#' @param frame_data a standard output from the SEB_data_concatenate function used on images
#' analyzed using the SEBASTES software.
#' @param Deployment_ID a column of deployment IDs corresponding to the unique IDs in the frame data
#' @param Longitude a column of longitudes for each deployment ID
#' @param Latitude a column of latitudes for each deployment ID
#' @param Accessory_data columns of accessory data to be attached to the frame data
#'  
#' @keywords stereo camera, SEBASTES, data concatenation, data wrangling
#' @export
#' @examples
#' SEB_data_replace(frame_data, GPS_data$Deployment_ID, GPS_data$Longitude, GPS_data$Latitude, GPS_data[,4:10])

SEB_GPS_point<-function(frame_data,Deployment_ID,Longitude,Latitude,Accessory_data=NULL){
  #Deployment_ID<-GPS_data$Deployment_ID
  #Longitude<-GPS_data$Longitude
  #Latitude<-GPS_data$Latitude
  #Accessory_data<-data.frame(GPS_data$Camera.Name,GPS_data$Depth,GPS_data$Deployment.event)
  GPS_data<-data.frame(DEPLOYMENT_ID=Deployment_ID,LONGITUDE=Longitude,LATITUDE=Latitude,Accessory_data)
  frame_data<-merge(frame_data,GPS_data,by="DEPLOYMENT_ID",all.x=TRUE)
  return(frame_data)}

#' A function to append GPS data to transect SEBASTES deployment data
#'
#' This function merges a GPS position to frame data from SEBASTES output for transect data. The GPS data is a 
#' set of Latitude and Longitude pairs with an associated time for each deployment (e.g. a trackline for a drop 
#' camera deployments). A third column identifies the Deployment_ID. Accessory data, such as depth included in the 
#' data frame can also be appended to the output. 
#' @param frame_data a standard output from the SEB_data_concatenate function used on images
#' analyzed using the SEBASTES software.
#' @param Time a column of times in as.POSIXct format (e.g. '%Y-%m-%d %H:%M:%OS')
#' @param Longitude a column of longitudes for each deployment ID
#' @param Latitude a column of latitudes for each deployment ID
#' @param Accessory_data columns of accessory data to be attached to the frame data
#' @param offset offset in hours for conversion between GPS timestamp and camera timestamp
#'  
#' @keywords stereo camera, SEBASTES, data concatenation, data wrangling
#' @export
#' @examples
#' SEB_GPS_transect(frame_data, GPS_data$Time, GPS_data$Longitude, GPS_data$Latitude, GPS_data[,4:10],offset=-7)

SEB_GPS_transect<-function(frame_data,Time, Longitude,Latitude,Accessory_data=NULL,offset=0){
  # Time<-EK80data$DateTime
  # Longitude<-EK80data$EK80_Longitude
  # Latitude<-EK80data$EK80_Latitude
  # Accessory_data<-EK80data$EK80_Depth
  
  import1<-data.frame(TIME=Time,LONGITUDE=Longitude,LATITUDE=Latitude,Accessory_data)
  t1<-Time
  t1<-round_date(t1,unit="second")
  import1$TIME_STAMP<-t1+offset*3600
  
  t2<-as.POSIXct(frame_data$FRAME_TIME,format='%Y-%m-%d %H:%M:%OS')
  t2<-round_date(t2,unit="second")
  frame_data$TIME_STAMP<-t2
  frame_data<-merge(frame_data,import1,by="TIME_STAMP",all.x=TRUE)
  return(frame_data)}

#' A function to append Seabird SBE data to SEBASTES deployment data
#'
#' This function merges a Seabird SBE file with time, depth and temperature to frame data from SEBASTES output. 
#' @param frame_data a standard output from the SEB_data_concatenate function used on images
#' analyzed using the SEBASTES software.
#' @param SBE_file a single or multiple SBE files in their native format
#' @param offset offset in hours for conversion between SBE timestamp and camera timestamp
#'  
#' @keywords stereo camera, SEBASTES, data concatenation, data wrangling, SeaBird
#' @export
#' @examples
#' SEB_SeaBird_append(frame_data, SBE_file,offset=-7)

SEB_SeaBird_append<-function(frame_data,SBE_file,offset=0){

 # SBE_file<-files
 # frame_data<-frame.data
 # offset<-0

 # SBE_file<-files
  #frame_data<-frame.data
  #offset<-0

  #i<-1
  require(lubridate)
  SBEdata<-NULL
  for(i in 1:length(SBE_file)){
    
  n1<-grep("start sample number = 1", readLines(SBE_file[i]), value = FALSE)

  import1<-read.table(SBE_file[i],skip=n1,fill=TRUE,sep=",")  
  t1<-as.POSIXct(paste(import1$V3,import1$V4),format='%d %b %Y %H:%M:%OS')
  t1<-round_date(t1,unit="second")
  import1$TIME_STAMP<-t1+offset*3600
  import1<-data.frame(Temperature=import1$V1,Depth=import1$V2,TIME_STAMP=import1$TIME_STAMP)
  SBEdata<-rbind(SBEdata,import1)}
  
  t2<-as.POSIXct(frame_data$FRAME_TIME,format='%Y-%m-%d %H:%M:%OS')
  t2<-round_date(t2,unit="second")
  frame_data$TIME_STAMP<-t2
  frame_data<-merge(frame_data,SBEdata,by="TIME_STAMP",all.x=TRUE)
  return(frame_data)}


#########################################################################################################
##########################################################################################################
###########################################################################################################
#' A function to fix bad naming in profile for Sebastes deployment (happened in 2022 GOA and Seamount surveys)
#'
#' This function fixes 5 digit numbering to 6 digit numbering and appendds new number to existing data
#' @param startDir Directory with the bad deployment data.
#' @param camera the name of one of the cameras in the stereo pari (default is DropCam1 'Blackfly BFLY-PGE-50S5C_16396245'

#'  
#' @keywords stereo camera, SEBASTES, data concatenation, data wrangling
#' @export
#' @examples
#' six_digit_framename_fix("C:/Users/rooperc/Desktop/SeamountSebastes")

six_digit_framename_fix<-function(startDir,camera=c('Blackfly BFLY-PGE-50S5C_16396245', 'Blackfly BFLY-PGE-50S5C_16304398')){
require(RSQLite)

#startDir <- 'C:/Users/rooperc/Desktop/SeamountSebastes'
####################   THIS IS VALID FOR DROPCAM UNIT 1 !!!!! ###################
#camera<-c('Blackfly BFLY-PGE-50S5C_16396245', 'Blackfly S BFS-PGE-50S5M_21282929')
##############################################################

#for dirName, subDirs, files in os.walk(startDir):
subDirs<-list.dirs(startDir,recursive=FALSE,full.names=FALSE)
subDirs<-subset(subDirs,substr(subDirs,0,3)=='D20')
subDirCopy<-subDirs

#  iterate through our list of dirs
for(i in 1:length(subDirCopy)){
  dir1<-subDirs[i]
  #  look for targets of interest
  if (substr(dir1,0,5) == 'D2022'){
  
  print(paste0('starting on ',dir1))}

    # get unit
  # open up local db file
 # db = dbConnection.dbConnection(dirName+'/'+dir+'/data/'+dir+'.db', '', '',label='dataDB', driver="QSQLITE")
  db<-dbConnect(RSQLite::SQLite(),dbname=paste0(startDir,'/',dir1,'/data/',dir1,'.db'))
#db.dbOpen()
# check to see if fix has been applied
query1<-dbSendQuery(db,"SELECT FRAME_TIME FROM FRAMES")
query<-dbFetch(query1)
dbClearResult(query1)

val<-query[1,1]
if(val!=''){
  print(paste0('already fixed ',dir1))
  {next}}

# setup image navigation structure that is the same as in sebastes
working_cam<-ifelse(dir.exists(paste0(startDir,'/',dir1,'/images/',camera[1])),camera[1],camera[2])# this is the dropcam #2 scenario

imageFiles<-NULL
imageFiles<-list.files(paste0(startDir,'/',dir1,'/images/',working_cam),pattern=".jpg",recursive=TRUE,full.names=FALSE)

  #imageFiles.append(file.split('\\')[-1])



#  process the filenames in the directory extracting frame number and time
imageFrames<-NULL
imageFramesOld<-NULL
timestamps<-NULL
pydateformat='D%Y%m%d-T%H%M%OS'
totalFrames<-length(imageFiles)
for(j in 1:totalFrames){
  x<-imageFiles[j]
imageFrames<-c(imageFrames,as.numeric(substr(x,0,6)))
imageFramesOld<-c(imageFramesOld,as.numeric(substr(x,0,5)))
tx<-format(as.POSIXlt(substr(x,8,28),tz="UTC",format=pydateformat),"%Y-%m-%d %H:%M:%OS3")
timestamps<-c(timestamps,tx)}



query1<-dbSendQuery(db,"SELECT FRAME_NUMBER FROM FRAMES")
query<-dbFetch(query1)
dbClearResult(query1)

dbExecute(db,"ALTER TABLE frames ADD frame_number2 int(255)")
dbExecute(db,"ALTER TABLE frame_metadata ADD frame_number2 int(255)")
dbExecute(db,"ALTER TABLE targets ADD frame_number2 int(255)")
dbExecute(db,"ALTER TABLE bounding_boxes ADD frame_number2 int(255)")

#indbframes<-NULL
for(k in 1:length(query$FRAME_NUMBER)){

 oldFrameNum<-query$FRAME_NUMBER[k]
 indexframes<-which(imageFramesOld==oldFrameNum)[1]
 
 # this is the image loop
#  indbframes.append(int(oldFrameNum))
#for oldFrameNum in indbframes:
  #index=imageFramesOld.index(int(oldFrameNum))
newFrameNum<-imageFrames[indexframes]
timestamp=timestamps[indexframes]
dbExecute(db,paste0("UPDATE frames SET frame_number2=",newFrameNum,", frame_time='",timestamp,"' where frame_number=",oldFrameNum))
dbExecute(db,paste0("UPDATE frame_metadata SET frame_number2=",newFrameNum," where frame_number=",oldFrameNum))
dbExecute(db,paste0("UPDATE targets SET frame_number2=",newFrameNum," where frame_number=",oldFrameNum))
dbExecute(db,paste0("UPDATE bounding_boxes SET frame_number2=",newFrameNum," where frame_number=",oldFrameNum))
}

dbExecute(db,"UPDATE frames SET frame_number = frame_number*1000000")
dbExecute(db,"UPDATE frames SET frame_number = frame_number2")
dbExecute(db,"ALTER TABLE frames DROP COLUMN frame_number2")

dbExecute(db,"UPDATE frame_metadata SET frame_number = frame_number*1000000")
dbExecute(db,"UPDATE frame_metadata SET frame_number = frame_number2")
dbExecute(db,"ALTER TABLE frame_metadata DROP COLUMN frame_number2")

dbExecute(db,"UPDATE targets SET frame_number = frame_number*1000000")
dbExecute(db,"UPDATE targets SET frame_number = frame_number2")
dbExecute(db,"ALTER TABLE targets DROP COLUMN frame_number2")

dbExecute(db,"UPDATE bounding_boxes SET frame_number = frame_number*1000000")
dbExecute(db,"UPDATE bounding_boxes SET frame_number = frame_number2")
dbExecute(db,"ALTER TABLE bounding_boxes DROP COLUMN frame_number2")



# write out files
db_df<-dbReadTable(db,"FRAMES")
write.csv(db_df,paste0(startDir,'/',dir1,'/data/',dir1,'_frames.csv'),row.names=FALSE)

db_df<-dbReadTable(db,"FRAME_METADATA")
write.csv(db_df,paste0(startDir,'/',dir1,'/data/',dir1,'_frame_metadata.csv'),row.names=FALSE)

db_df<-dbReadTable(db,"TARGETS")
write.csv(db_df,paste0(startDir,'/',dir1,'/data/',dir1,'_targets.csv'),row.names=FALSE)

db_df<-dbReadTable(db,"BOUNDING_BOXES")
write.csv(db_df,paste0(startDir,'/',dir1,'/data/',dir1,'_bounding_boxes.csv'),row.names=FALSE)

dbDisconnect(db)

print(paste0('ending ',paste0(startDir,'/',dir1)))
}

print('done!!')}


#' A function to calculate the swath area viewed by a stereo camera
#'
#' This function uses a horizonal viewing angle from SEB_Viewing_Angles to estimate the horizontal area
#' viewed by a stareo camera at a set range. Typically this is used to compute the area swepte along a 
#' survey transect at a median range of objects that were observed. This method is based on Rooper et al 
#' 2016.  
#' @param horizonal_AOV a horizontal viewing angle calculated from SEB_Viewing_Angles function or other source
#' @param range_distance the range at which to calculate the horizontal distance viewed
#' 
#' @keywords stereo camera, SEBASTES, data concatenation, data wrangling area swept
#' @export
#' @examples
#' swath<-SEB_Swath_Width(angles$horizontal_AOV, median(target_data$range,na.rm=TRUE))

SEB_Swath_Width<-function(horizontal_AOV, range_distance){
  w1<-2*(tan((horizontal_AOV/2)*pi/180))*range_distance
  return(w1)
}

#' A function to calculate the angle viewed by a stereo camera
#'
#' This function uses a stereo camera calibration *.mat file to estimate the horizontal angle of view
#'  for a camera pair. It is a translation from Matlab to R of functions from Kresimir Williams to
#'  calculate horizontal AOV and uses a translated function from the Bouget et al. 2008 to do the  Rodrigues 
#'  transformation of the calibration. 
#' @param calibration_file a calibration file for a stereo camera from matlab (*.mat)
#' @param image_width the image width from the camera in pixels
#' @param image_height the image height from the camera in pixels
#' @param plot.it do you want to generate a plot of the viewing angle?
#' 
#' @keywords stereo camera, SEBASTES, data concatenation, data wrangling area swept
#' @export
#' @examples
#' angles<-SEB_Viewing_Angles('dropcam_unit2_calibration_switched.mat', 2048,1536)

SEB_Viewing_Angles<-function(calibration_file, image_width,image_height,plot.it=TRUE){
  require(R.matlab)
  require(plotly)
  require(ggplot2)
  
#calibration_file<-"C:/Users/rooperc/Desktop/Stereo_Image_Applications/Camera Calibration Instructions and Tools/SEBASTES Calibrations and Configurations/dropcam_unit2_calibration_switched.mat"
  
  if(grepl("*.mat",calibration_file)==TRUE){
  ag<-readMat(calibration_file,header=TRUE)

  Cal.T=ag$T
  Cal.om=ag$om
  Cal.fc_right=ag$fc.right
  Cal.cc_right=ag$cc.right
  Cal.kc_right=ag$kc.right
  Cal.alpha_c_right=ag$alpha.c.right
  Cal.fc_left=ag$fc.left
  Cal.cc_left=ag$cc.left
  Cal.kc_left=ag$kc.left
  Cal.alpha_c_left=ag$alpha.c.left
  
  
  Cal.im_dim<-c(image_width,image_height)
  #Cal.R<-genR(Cal.om,space="SO3")
  Cal.R=rodrigues(Cal.om)
  }
  
if(grepl("*.npz",calibration_file)==TRUE){
Cal.R<-npz_file(calibration_file)
}
  
  # view cone
  
  IP_left = matrix(rbind(c(1,-Cal.alpha_c_left,0),c(0, 1, 0),c(0, 0, 1)),ncol=3)%*%
  matrix(rbind(c(1/Cal.fc_left[1], 0, 0),c(0, 1/Cal.fc_left[2], 0),c(0, 0, 1)),ncol=3)%*%
  matrix(rbind(c(1, 0, -Cal.cc_left[1]),c(0, 1, -Cal.cc_left[2]),c(0, 0, 1)),ncol=3)%*%
  matrix(rbind(c(0, Cal.im_dim[1]-1, Cal.im_dim[1]-1, 0, 0),c(0, 0, Cal.im_dim[2]-1, Cal.im_dim[2]-1, 0),c(1, 1, 1, 1, 1)),ncol=5)
  horizontal_AOV=(180/pi)*(atan(abs(IP_left[1,1]))+atan(abs(IP_left[1,2])))
  vertical_AOV=(180/pi)*(atan(abs(IP_left[2,1]))+atan(abs(IP_left[2,2])))
  BASE_left = 0.5 *(matrix(rbind(c(0, 1, 0, 0, 0, 0),c(0, 0, 0, 1, 0, 0),c(0, 0, 0, 0, 0, 1)),ncol=6))
  IP_left  = matrix(rbind(IP_left,BASE_left[,1]%*%matrix(1,1,5),IP_left),nrow=3,ncol=15)
  scene<-list(camera=list(eye=list(x=-1.5,y=-1.5,z=1.5)))
  plot_ly(x=IP_left[1,],y=IP_left[3,],z=IP_left[2,],type="scatter3d",mode="lines")%>%
    layout(title='view size at 1 m',scene=scene)
  
    return(list(horizontal_AOV=horizontal_AOV,vertical_AOV=vertical_AOV,Cal.R=Cal.R))}

#' A function to do the Rodrigues transform rotation on a matlab calibration
#' 
#' Translated from Matlab Toolbox (Bouget et al. 2008). Requires a *.mat calibration file.
#' 
rodrigues<-function(om){

#om<-Cal.om

m<-dim(om)[1]
n<-dim(om)[2]
eps<-2.2204*10^-16
bigeps = 10e+20*eps

theta = norm(om,type="F")
if(theta < eps){
R = diag(3)

dRdin = matrix(c(0,0,0,0, 0, 1,0, -1,0,0,0,-1, 0, 0, 0,1, 0, 0,0, 1, 0, -1, 0, 0,0, 0, 0),ncol=3)

}


dm3din = matrix(rbind(diag(3),Conj(t(om))/theta),ncol=3)

omega = matrix(om/theta)


dm2dm3 = matrix(rbind(cbind(diag(3)/theta, -om/theta^2), c(matrix(0,1,3),1)),ncol=4)

alpha = cos(theta)
beta = sin(theta)
gamma = 1-cos(theta)
omegav=matrix(c(0, -omega[3], omega[2],omega[3], 0, -omega[1],-omega[2], omega[1], 0),ncol=3,byrow=TRUE)
        A = omega%*%Conj(t(omega))
          
dm1dm2 = matrix(0,21,4)
dm1dm2[1,4] = -sin(theta)
dm1dm2[2,4] = cos(theta)
dm1dm2[3,4] = sin(theta)
dm1dm2[4:12,1:3] = Conj(t(rbind(c(0, 0, 0, 0, 0, 1, 0, -1, 0),
                              c(0, 0, -1, 0, 0, 0, 1, 0, 0),
                              c(0, 1, 0, -1, 0, 0, 0, 0, 0))))

        w1 = omega[1]
        w2 = omega[2]
        w3 = omega[3]

        dm1dm2[13:21,1] = c(2*w1,w2,w3,w2,0,0,w3,0,0)
        dm1dm2[13:21,2] = c(0,w1,0,w1,2*w2,w3,0,w3,0)
        dm1dm2[13:21,3] = c(0,0,w1,0,0,w2,w1,w2,2*w3)

        R = diag(3)*alpha + omegav*beta + A*gamma;

        dRdm1 = matrix(0,9,21)

        dRdm1[c(1, 5, 9),1] = matrix(1,3,1)
        dRdm1[,2] = omegav
        dRdm1[,4:12] = beta*diag(9)
        dRdm1[,3] = A
        dRdm1[,13:21] = gamma*diag(9)

        dRdin = dRdm1%*%dm1dm2%*%dm2dm3%*%dm3din;

return(R)


#fixed April 6th by Bouguet -- not working in all cases!
# out = theta * (sqrt((diag(R)+1)/2).*[1;2*(R(1,2:3)>=0)'-1]);
#theta * (sqrt((diag(R)+1)/2)*as.vector(c(1,2*Conj(t(R[1,2:3]))-1)))     
}


#' A function to extract the om matrix from an npz calibration
#'  
npz_file<-function(calibration_file){
  require(reticulate)
  #Here I had to install numpy in the correct version of python: 
  #in the command prompt "py -3.10 -m pip install numpy"

  np<-import("numpy")
  mat<-np$load(calibration_file)
#mat$files
Cal.R<-matrix(mat$f[["R"]])
return(Cal.R)

}
  


#' A function to concatenate sensor data from a project to attach to image data
#'
#' This function inputs a directory folder containing multiple stereo camera deployments,
#' reads all the sensor data from the deployments contained in the directory. 
#' It then concatenates these together, assigning #' a unique identifier (deployment_ID) 
#' and can outputs a .csv files.
#' @param project.dir directory containing the multiple deployment files 
#' @param make_csv do you want an output sensor.csv file (default = FALSE)
#' @keywords stereo camera, SEBASTES, data concatenation
#' @export
#' @examples
#' SEB_get_sensor("C:/Users/rooperc/Desktop/Rockfish Projects/Longline Survey Gear Comparisons/TrigCamData/Gadus")

SEB_get_sensor<-function(project.dir,make_csv=FALSE){
  require(RSQLite)
  require(lubridate)
  require(tidyr)
  require(stringr)
  #project.dir<-"D:/DropCam"
  `%nin%` = Negate(`%in%`)
  deployments<-list.dirs(project.dir,recursive=TRUE,full.names=TRUE)
  deployments<-subset(deployments,str_detect(deployments,"logs")==TRUE)

  sensor_data<-NULL
  for(i in 1:length(deployments)){
    
    acc_files<-paste0(deployments[i],"/","CamTrawlMetadata.db3")

    acc.datac<-dbConnect(RSQLite::SQLite(),dbname=acc_files)
    acc.data<-dbReadTable(acc.datac,"sensor_data")
    image.data<-dbReadTable(acc.datac,"images")
    dbDisconnect(acc.datac)
    
    acc.datad<-dbConnect(RSQLite::SQLite(),dbname=acc_files)
    acc.dataad<-dbReadTable(acc.datad,"async_data")
    dbDisconnect(acc.datad)
    
    depth<-acc.data$data[acc.data$sensor_id=="CTControl"]
    depth<-gsub("\\$OHPR,","",depth)
    
    depth<-matrix(unlist(strsplit(depth, ",")),ncol=8,byrow=TRUE)[,1:8]
    depth<-apply(depth, 2, as.numeric)
    depth<-data.frame(acc.data$number[acc.data$sensor_id=="CTControl"],depth)
    colnames(depth)<-c("FRAME_NUMBER","HEADING","PITCH","ROLL","TEMPERATURE","DEPTH","ACCEL_X","ACCEL_Y","ACCEL_Z")
    
    image.data<-unique(data.frame(FRAME_NUMBER=image.data$number,time=image.data$time))
    
    if("GPS"%in%unique(acc.data$sensor_id)){
      gps<-matrix(unlist(sapply(strsplit(acc.data$data[acc.data$sensor_id=="GPS"], ","),'[',c(3,5))),ncol=2,byrow=TRUE)
      gps<-apply(gps, 2, as.numeric)
      gps<-gps/100
      gps<-data.frame(acc.data$number[acc.data$sensor_id=="GPS"],gps)
      colnames(gps)<-c("FRAME_NUMBER","LATITUDE","LONGITUDE")
      depth<-merge(depth,gps,by="FRAME_NUMBER",all.x=TRUE) 
    }
    
    
    
    if("GPS"%nin%unique(acc.data$sensor_id)&"GPS"%in%unique(acc.dataad$sensor_id)){
      datetime1<-data.frame(FRAME_TIME=as.POSIXct(acc.data$time),FRAME_NUMBER=acc.data$number)
      datetime1$FRAME_TIME<-round_date(datetime1$FRAME_TIME,unit="second")
      gps<-matrix(unlist(sapply(strsplit(acc.dataad$data[acc.dataad$sensor_id=="GPS"], ","),'[',c(3,5))),ncol=2,byrow=TRUE)
      gps<-apply(gps, 2, as.numeric)
      gps<-gps/100
      gps<-data.frame(acc.dataad$time[acc.dataad$sensor_id=="GPS"],gps)
      colnames(gps)<-c("FRAME_TIME","LATITUDE","LONGITUDE")
      gps$FRAME_TIME<-round_date(as.POSIXct(gps$FRAME_TIME),unit="second")
      gps<-merge(gps,datetime1,by="FRAME_TIME")
      gps<-gps[!duplicated(gps$FRAME_TIME),]
      gps<-gps[,-1]
      depth<-merge(depth,gps,by="FRAME_NUMBER",all.x=TRUE) 
    }
    
    if("GPS"%nin%unique(acc.data$sensor_id)&"GPS"%nin%unique(acc.dataad$sensor_id)){
      depth$LATITUDE<-NA
      depth$LONGITUDE<-NA}
    
    depth<-merge(image.data,depth,by="FRAME_NUMBER",all.x=TRUE)
    #depth$IMAGE_NUMBER<-depth$FRAME_NUMBER
    #depth$FRAME_NUMBER<-seq(1,length(depth$FRAME_NUMBER),1)
    
    colnames(depth)[colnames(depth)=="time"]<-"FRAME_TIME"
    name1<-unlist(strsplit(acc_files,"/"))
    name1<-subset(name1,str_detect(name1,"-T")==TRUE)
    depth$DEPLOYMENT_ID<-name1   
    sensor_data<-rbind(sensor_data,depth)}
  
  if(make_csv==TRUE){write.csv(depth,file="sensor_data.csv",row.names=FALSE)}

  return(sensor_data)}



#' A function to concatenate a SEBASTES project database to .csv files
#'
#' This function inputs a directory folder containing a single project database from 
#' stereo camera deployments. It reads the single *.db file with the combined deployment 
#' data and extracts the frame data and the target data. It then merges with the 
#' sensor data (if path is provided), from CamTrawlMetadata.db assigning a unique identifier 
#' (deployment_ID) and outputs two .csv files, one containing target data and one containing the frame data. Any accessory 
#' data from sensors will also be included with the frame data.
#' @param project.db database with the combined deployment data
#' @param deployment.dir directory that contains subdirectories with the individual deployments with sensor data (CamTrawlMetadata.db files)
#' @keywords stereo camera, SEBASTES, data concatenation
#' @export
#' @examples
#' SEB_project_concatenate("C:/Users/rooperc/Desktop/Deep Sea Coral Research/DeepSeaCoralSurveyDataBase/GOA MACE Data/MACE_2019/MACE2019_GOA.db","D:/DropCam")

SEB_project_concatenate<-function(project.db,deployment.dir){
  require(RSQLite)
  require(lubridate)
  require(tidyr)
  #project.db<-"C:/Users/rooperc/Desktop/Deep Sea Coral Research/DeepSeaCoralSurveyDataBase/GOA MACE Data/MACE_2019/MACE2019_GOA.db"
  #deployment.dir<-"D:/DropCam"
  `%nin%` = Negate(`%in%`)

  target.data<-NULL
  frame.data<-NULL
  
    drop.datac<-dbConnect(RSQLite::SQLite(),dbname=project.db)
    frame.datat<-dbReadTable(drop.datac, "FRAMES")
    substrate.data<-dbReadTable(drop.datac,"FRAME_METADATA")
    substrate.data<-subset(substrate.data,substrate.data$METADATA_VALUE=="Checked"&substrate.data$METADATA_GROUP=="Habitat")
    substrate.data<-pivot_wider(substrate.data,names_from = METADATA_TYPE,values_from=METADATA_TAG)
    substrate.data<-data.frame(DEPLOYMENT_ID=substrate.data$DEPLOYMENT_ID,FRAME_NUMBER=substrate.data$FRAME_NUMBER,Primary_habitat=substrate.data$`Primary Habitat`,Secondary_habitat=substrate.data$`Secondary Habitat`)
    frame.datat<-merge(frame.datat,substrate.data,by=c("DEPLOYMENT_ID","FRAME_NUMBER"),all.x=TRUE)
    frame.datat<-frame.datat[,-4]
    target.datat<-dbReadTable(drop.datac, "TARGETS")
    dbDisconnect(drop.datac)
    
    depth<-SEB_get_sensor(deployment.dir)
    
    frame.datat<-merge(frame.datat,depth,by=c("DEPLOYMENT_ID","FRAME_NUMBER"),all.x=TRUE) 
    
    frame.data<-frame.datat
    target.data<-target.datat
  
  write.csv(target.data,"targetout_data.csv",row.names=FALSE)
  write.csv(frame.data,"frameout_data.csv",row.names=FALSE)
  return(list(target.data=target.data,frame.data=frame.data))}





    