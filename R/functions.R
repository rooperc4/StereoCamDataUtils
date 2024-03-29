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











