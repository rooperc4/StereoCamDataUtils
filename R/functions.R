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
  acc_files<-list.files(paste0(deployments[i],"/logs"),pattern=".db3",recursive=TRUE,full.names=TRUE)
  if(length(data_files)==0){next}
  
  drop.datac<-dbConnect(RSQLite::SQLite(),dbname=data_files)
  frame.datat<-dbReadTable(drop.datac, "FRAMES")
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
  
    frame.data<-rbind(frame.data,frame.datat)
    target.data<-rbind(target.data,target.datat)}
  
  write.csv(target.data,file=paste(project.dir,"/targetout_data.csv",sep=""),row.names=FALSE)
  write.csv(frame.data,file=paste(project.dir,"/frameout_data.csv",sep=""),row.names=FALSE)
  return(list(target.data,frame.data))}


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
  require(RSQLite)
  deployments<-list.dirs(project.dir,recursive=FALSE,full.names=FALSE)
  deployment_paths<-list.dirs(project.dir,recursive=FALSE,full.names=TRUE)
  data_files<-list.files(new.data.dir,pattern =paste0("^[",name_start,"]" ))
  dfiles<-list.files(new.data.dir,pattern =paste0("^[",name_start,"]" ),full.names=TRUE)
  
  dname<-unlist(strsplit(data_files,split=".db"))
  
  t1<-which(deployments %in% dname)
  for(i in 1:length(t1)){
    if(dir.exists(paste0(deployment_paths[t1[i]],"/data"))==FALSE){dir.create(paste0(deployment_paths[t1[i]],"/data"))}
    
    file.rename(from = dfiles[i],  to = paste0(deployment_paths[t1[i]],"/data/",data_files[i]))}

  
  return(print(paste0("Successfully moved ",length(t1)," files")))}
