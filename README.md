Helper functions for SEBASTES data wrangling
================
Chris Rooper
March 08, 2022

To install the package

``` r
library(devtools)
devtools::install_github("rooperc4/StereoCamDataUtils")
library("StereoCamDataUtils")
```

## Purpose

The functions contained in this package are used to deal with the data
from image analyses conducted using SEBASTES stereo camera software.
Currently there are two functions, but more will be added in the future.

![](StereoCam_Data_Utils_files/figure-gfm/Figure%201-1.png)<!-- -->

## Existing Functions

### Data replace

This function replaces the \*.db data files with new versions by
matching the deployment ID’s and replacing the appropriate data files

``` r
SEB_data_replace("D:/Longline Survey Gear Comparisons/TrigCamData/Raja/Raja data/Raja data","D:/Longline Survey Gear Comparisons/TrigCamData/Raja")
SEB_data_replace("D:/Longline Survey Gear Comparisons/TrigCamData/Gadus/Gadus data","D:/Longline Survey Gear Comparisons/TrigCamData/Gadus")
SEB_data_replace("D:/Longline Survey Gear Comparisons/TrigCamData/Alutus/Alutus data/Alutus data","D:/Longline Survey Gear Comparisons/TrigCamData/Alutus")
SEB_data_replace("D:/Longline Survey Gear Comparisons/TrigCamData/Clupea/Clupea data","D:/Longline Survey Gear Comparisons/TrigCamData/Clupea")
```

### Data concatenate

This function concatenates the \*.db data files from multiple
deployments into a single frame table (which includes accessory data)
and a target table.

``` r
SEB_data_concatenate("D:/Longline Survey Gear Comparisons/TrigCamData/Raja")
SEB_data_concatenate("D:/Longline Survey Gear Comparisons/TrigCamData/Gadus")
SEB_data_concatenate("D:/Longline Survey Gear Comparisons/TrigCamData/Alutus")
SEB_data_concatenate("D:/Longline Survey Gear Comparisons/TrigCamData/Clupea")
```

### Merge SEBASTES data with GPS and other data

There are typically two types of GPS data to merge with the image
analysis data. The first are point data, where each camera deployment is
at a single position (e.g. TrigCam deployments). In this case a simple
function to merge the position data with the deployment data is used;
SEB\_GPS\_point. It inputs the frame data outpub from the data
concatenate function and a table of GPS points identified by a
Deployment\_ID column. Accessory data, such as depth or temperature can
also be added in. The code below outlines an example.

``` r
all_frame_data<-rbind(SEB_data_concatenate("D:/Longline Survey Gear Comparisons/TrigCamData/Raja")$frame.data,SEB_data_concatenate("D:/Longline Survey Gear Comparisons/TrigCamData/Clupea")$frame.data,SEB_data_concatenate("D:/Longline Survey Gear Comparisons/TrigCamData/Gadus")$frame.data)

GPS_data<-read.csv("D:/LonglineTrigCamPositions.csv",header=TRUE)

all_frame_data<-SEB_GPS_point(all_frame_data,Deployment_ID=GPS_data$Deployment_ID,Longitude=GPS_data$Longitude,Latitude=GPS_data$Latitude,Accessory_data=data.frame(GPS_data$Camera.Name,GPS_data$Depth,GPS_data$Deployment.event))
```

The second type of GPS data are transect-type data (e.g. drop camera
deployments). Here we have GPS data with a time stamp and a location.
These data are merged with the frame data using the time stamp of the
image and the time stamp of the GPS feed. It is important that the time
zone be consistent between the two feeds, but the function allows the
user to input an offset (such as -7 hours from GMT to PST). Accessory
data can also be added here as well. The below code outlines an example.

``` r
#Example to come
SEB_GPS_transect()
```

### Example output

Finally, here’s an example of pulling the target data using the
SEB\_data\_concatenate function and produce length frequency histograms
in ggplot.

``` r
library(ggplot2)
library(gridExtra)

target_data<-rbind(SEB_data_concatenate("D:/Longline Survey Gear Comparisons/TrigCamData/Raja")$target.data,SEB_data_concatenate("D:/Longline Survey Gear Comparisons/TrigCamData/Clupea")$target.data,SEB_data_concatenate("D:/Longline Survey Gear Comparisons/TrigCamData/Gadus")$target.data)
```

    ## Loading required package: RSQLite

``` r
p1<-ggplot()+geom_histogram(data=target_data,aes(x=LENGTH,color=SPECIES_GROUP))+theme(legend.position="none")
p1+facet_wrap(~SPECIES_GROUP)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](StereoCam_Data_Utils_files/figure-gfm/example%20targets-1.png)<!-- -->

``` r
d1<-aggregate(RANGE~DEPLOYMENT_ID+SPECIES_GROUP,data=target_data,FUN="length")
p1<-ggplot(d1)+geom_bar(aes(x=DEPLOYMENT_ID,y=RANGE,color=SPECIES_GROUP,fill=SPECIES_GROUP),stat="identity",position="fill")
p1
```

![](StereoCam_Data_Utils_files/figure-gfm/example%20targets-2.png)<!-- -->
