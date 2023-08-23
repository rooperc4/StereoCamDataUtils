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
  deployments<-list.dirs(project.dir,recursive=FALSE,full.names=TRUE)
  target.data<-NULL
  frame.data<-NULL
  
  for(i in 1:length(deployments)){
  
  data_files<-list.files(paste0(deployments[i],"/data"),pattern=".db",recursive=TRUE,full.names=TRUE)
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
  
  depth<-matrix(unlist(strsplit(acc.data$data, ",")),ncol=9,byrow=TRUE)[,2:9]
  depth<-apply(depth, 2, as.numeric)
  depth<-data.frame(acc.data$number,depth)
  colnames(depth)<-c("FRAME_NUMBER","HEADING","PITCH","ROLL","TEMPERATURE","DEPTH","ACCEL_X","ACCEL_Y","ACCEL_Z")
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
#' @param Deployment_ID a column of deployment IDs corresponding to the unique IDs in the frame data
#' @param Time a column of times in as.POSIXct format (e.g. '%Y-%m-%d %H:%M:%OS')
#' @param Longitude a column of longitudes for each deployment ID
#' @param Latitude a column of latitudes for each deployment ID
#' @param Accessory_data columns of accessory data to be attached to the frame data
#' @param offset offset in hours for conversion between GPS timestamp and camera timestamp
#'  
#' @keywords stereo camera, SEBASTES, data concatenation, data wrangling
#' @export
#' @examples
#' SEB_data_replace(frame_data, GPS_data$Deployment_ID, GPS_data$Time, GPS_data$Longitude, GPS_data$Latitude, GPS_data[,4:10],offset=-7)

SEB_GPS_transect<-function(frame_data,Deployment_ID,Time, Longitude,Latitude,Accessory_data=NULL,offset=0){
  
  import1<-data.frame(DEPLOYMENT_ID=Deployment_ID,TIME=Time,LONGITUDE=Longitude,LATITUDE=Latitude,Accessory_data)
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
  #SBE_file<-"D:/SeamountTransectData/Cobb_23.asc"
  #i<-1
  SBEdata<-NULL
  for(i in 1:length(SBE_file)){
    
  n1<-grep("start sample number = 1", readLines(SBE_file[i]), value = FALSE)

  import1<-read.table(SBE_file[i],skip=n1,fill=TRUE,sep=",")  
  t1<-as.POSIXct(paste(import1$V3,import1$V4),format='%d %b %Y %H:%M:%OS')
  import1$TIME_STAMP<-t1+offset*3600
  import1<-data.frame(Temperature=import1$V1,Depth=import1$V2,TIME_STAMP=import1$TIME_STAMP)
  SBEdata<-rbind(SBEdata,import1)}
  
  
  t2<-as.POSIXct(frame_data$FRAME_TIME,format='%Y-%m-%d %H:%M:%OS')
  t2<-round_date(t2,unit="second")
  frame_data$TIME_STAMP<-t2
  frame_data<-merge(frame_data,SBEdata,by="TIME_STAMP",all.x=TRUE)
  return(frame_data)}
