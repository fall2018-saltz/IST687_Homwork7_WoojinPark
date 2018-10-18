
##################################################################
# Course number : IST 687         
# Name : Woojin Park
# Homework 7
# Assignment due : Thursday, October 17, 2018
# Submitted by Woojin Park on October 18, 2018


#Step A: Load and Merge datasets
# 1)	Read in the census dataset and the USArrests and merge them 
# (just like HW6)

# So, Read in the census dataset first


clean_data1 <- raw_data

myfunction <-function(clean_data1)
{
  clean_data1<-clean_data1[-53,]
  clean_data1<-clean_data1[-1,]
  cnames <- colnames(clean_data1)
  cnames[5] <-"stateName"
  cnames[6] <-"population"
  cnames[7] <-"popOver18"
  cnames[8] <-"percentOver18"
  colnames(clean_data1) <-cnames
  clean_data1<-clean_data1[,-1:-4]
  return(clean_data1)
}

cleanCensus<- myfunction(clean_data1)
str(cleanCensus)

# Copy the USArrests dataset into a local variable (similar to HW 2)

USArrests 
arrests <- data.frame(USArrests)


#  Create a merged dataframe -- with the attributes from both dataframes
# Hint: get the state names from the USArests dataframe with the rownames 
# Hint: use the merge command 
#first eliminate the "District of Columbia" in cleanCensus to make match with 
# states name in arrests

cleanCensus <- cleanCensus[-9,]
cleanCensus$stateName
arrests$stateName <-rownames(arrests[])
arrests

# and then, merge the two dataframe
MergedDf<- merge(cleanCensus, arrests, by ="stateName")
MergedDf

# 2) Add the area of each state, and the center of each state,
# to the merged dataframe, using the ‘state.center’, ‘state.center’ and ‘state.name’ vectors
library(ggplot2)
library(ggmap)
library("maps")
library("mapproj")

DF <- data.frame(state.name, state.center, state.area)
DF$stateName <-DF$state.name
DF

MergeSt<-merge(MergedDf, DF, by ="stateName")
str(MergeSt)

MergeSt <-MergeSt[,-9]
str(MergeSt)
MergeSt

# Step B: Generate a color coded map
# 3)	Create a color coded map, based on the area of the state 

us <- map_data("state")
us

# store state names in lowercase
MergeSt$stateName <- tolower(MergeSt$stateName)
# named map mappopColor
mappopColor <- ggplot(MergeSt, aes(map_id = stateName))
# named map mappopColor
mappopColor <- mappopColor +geom_map(map=us, aes(fill=state.area))
# make limits the x and y coordinates (longitude and latitude)
mappopColor <- mappopColor + expand_limits(x= us$long, y = us$lat)
# name the map
mappopColor <- mappopColor +coord_map() + ggtitle("Area of the States")
mappopColor


# Step C: Create a color shaded map of the U.S. based on the Murder rate for each state 
# 4) Repeat step B, but color code the map based on the murder rate of each state.

# store state names in lowercase
MergeSt$stateName <- tolower(MergeSt$stateName)
# named map mapmurColor
mapmurColor <- ggplot(MergeSt, aes(map_id =stateName))
# fill in to state area
mapmurColor <- mapmurColor + geom_map(map = us, aes(fill = Murder))
# make limits the x and y coordinates (longitude and latitude)
mapmurColor <- mapmurColor + expand_limits(x= us$long, y = us$lat)
# name the map
mapmurColor <- mapmurColor + coord_map() + ggtitle ("Murder rate of each States")
mapmurColor


# 5) Show the population as a circle per state (the larger the population, the larger the circle), 
# named map mapmurColor
mappopSize <- ggplot(MergeSt, aes(map_id =stateName))
# fill in to state area
mappopSize <- mappopSize + geom_map(map = us, aes(fill = Murder))
# make limits the x and y coordinates (longitude and latitude)
mappopSize <- mappopSize + geom_point(data=MergeSt, aes(x=x, y=y,size=population),colour="red" )
# name the map
mappopSize <- mappopSize + coord_map() + ggtitle ("Population size and Murder Rate of each States")
mappopSize

#5)	 Show the population as a circle per state (the larger the population, the larger the circle), 
#using the location defined by the center of each state
MurderState<-ggplot(MergeState, aes(map_id=state))
MurderState<-MurderState+geom_map(map=us, aes(fill=Murder))+
  expand_limits(x=us$long, y=us$lat)+
  coord_map()+ggtitle("Murder Rate and Population")+
  theme(plot.title = element_text(face="bold", size=20,color="blue",hjust = 0.5))+
  geom_point(data=MergeState, aes(x=MergeState$lon,y=MergeState$lat,size=population))
MurderState


# Step D: Zoom the map
# 6)	Repeat step C, but only show the states in the north east
#get the lat and lon of new york city
NYlatlon <-geocode("newyork,NY",source="dsk")
NYlatlon
# lon           lat
# -74.01024     40.7029

# set the xlim and ylim to NYC +/- 10
# Murder rate of States in the North East
mapmurNEColor <- MurderState + xlim(-84.01024,-64.01024)+ ylim(30.7029,50.7029)
mapmurNEColor <- mapmurNEColor + coord_map() + ggtitle ("Murder rate and Population size of NorthEast States")
mapmurNEColor


