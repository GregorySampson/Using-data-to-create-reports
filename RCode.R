library(dplyr)
library("tidyverse")
library(ggplot2)
accidents = read.csv("accidents.csv", na.strings = c("","NA"))


missing_1 <- accidents[rowSums(is.na(accidents)) > 0,]
missing_1


# replace ----
accidents$FirstRoadClass <- replace(accidents$FirstRoadClass, accidents$FirstRoadClass == 1, "motorway")
accidents$FirstRoadClass <- replace(accidents$FirstRoadClass, accidents$FirstRoadClass == 2, "A(m)")
accidents$FirstRoadClass <- replace(accidents$FirstRoadClass, accidents$FirstRoadClass == 3, "A")
accidents$FirstRoadClass <- replace(accidents$FirstRoadClass, accidents$FirstRoadClass == 4, "B")
accidents$FirstRoadClass <- replace(accidents$FirstRoadClass, accidents$FirstRoadClass == 5, "C")
accidents$FirstRoadClass <- replace(accidents$FirstRoadClass, accidents$FirstRoadClass == 6, "Unclassified")


accidents$RoadSurface <- replace(accidents$RoadSurface, accidents$RoadSurface == 1, "dry")
accidents$RoadSurface <- replace(accidents$RoadSurface, accidents$RoadSurface == 2, "Wet/Damp")
accidents$RoadSurface <- replace(accidents$RoadSurface, accidents$RoadSurface == "wet", "Wet/Damp")
accidents$RoadSurface <- replace(accidents$RoadSurface, accidents$RoadSurface == 3, "Snow")
accidents$RoadSurface <- replace(accidents$RoadSurface, accidents$RoadSurface == 4, "Frost/Ice")
accidents$RoadSurface <- replace(accidents$RoadSurface, accidents$RoadSurface == 5, "Flood(surface water over 3cm deep)")


accidents$WeatherConditions <- replace(accidents$WeatherConditions, accidents$WeatherConditions == 1, "Fine without high winds")
accidents$WeatherConditions <- replace(accidents$WeatherConditions, accidents$WeatherConditions == 2, "Raining without high winds")
accidents$WeatherConditions <- replace(accidents$WeatherConditions, accidents$WeatherConditions == 3, "Snowing without high winds")
accidents$WeatherConditions <- replace(accidents$WeatherConditions, accidents$WeatherConditions == 4, "Fine with high winds")
accidents$WeatherConditions <- replace(accidents$WeatherConditions, accidents$WeatherConditions == 5, "Raining with high winds")
accidents$WeatherConditions <- replace(accidents$WeatherConditions, accidents$WeatherConditions == 6, "Snowing with high winds")
accidents$WeatherConditions <- replace(accidents$WeatherConditions, accidents$WeatherConditions == 7, "Fog or mist - if hazard")
accidents$WeatherConditions <- replace(accidents$WeatherConditions, accidents$WeatherConditions == 8, "Other")
accidents$WeatherConditions <- replace(accidents$WeatherConditions, accidents$WeatherConditions == 9, "Unknown")
# remove ----

nrow(accidents$FirstRoadClass>0)
sum(accidents$FirstRoadClass==1|accidents$FirstRoadClass==2|accidents$FirstRoadClass==3|accidents$FirstRoadClass==4|accidents$FirstRoadClass==5|accidents$FirstRoadClass==6)
sum(accidents$RoadSurface==1|accidents$RoadSurface==2|accidents$RoadSurface==3|accidents$RoadSurface==4|accidents$RoadSurface==5|accidents$RoadSurface==6)

accidents<-accidents%>%distinct()
nrow(accidents)
accidents<-accidents %>% na.omit()
nrow(accidents)

accidents<-select(accidents, -c(LocalAuthority))
accidents
# outliers ----
boxplot(accidents$AgeOfCasualty)
outliers <- boxplot(accidents$AgeOfCasualty, plot=FALSE)$out
outliers

sd_value <- sd(accidents$AgeOfCasualty, na.rm = TRUE)
mean_Value <- mean(accidents$AgeOfCasualty, na.rm = TRUE)
upper_bound <- mean_Value+3*sd_value
lower_bound <- mean_Value-3*sd_value

outliers_sigma <-accidents %>% filter((AgeOfCasualty > upper_bound)| (AgeOfCasualty < lower_bound))
outliers_sigma

median_value <- median(accidents$AgeOfCasualty, na.rm = TRUE)
MAD_value <- mad (accidents$AgeOfCasualty, na.rm = TRUE)

upper_bound <- median_value+3*MAD_value
lower_bound <- median_value-3*MAD_value
clean_accident <- accidents[ - which(accidents$AgeOfCasualty %in% outliers),]
write.csv(clean_accident,"C:/Users/fredr/OneDrive/HS NOW/S19156605_Assessment2//clean_accident.csv", row.names = FALSE)

# graph1 ----
ggplot(clean_accident, aes(SexOfCasualty, fill=WeatherConditions )) +
  geom_bar(position="dodge")
# summary ----
a <- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Fine without high winds") ))
b <- length(which( (clean_accident$SexOfCasualty==1)& (clean_accident$WeatherConditions == "Fine without high winds") ))
if (a>b){
  output <- (c( "Males have ",(a-b)," more accidents in the class 'Fine without high winds' "))
}else if (b>a){
  output <- (c( "Females have ",(b-a)," more accidents in the class 'Fine without high winds' "))
}else{ output1 <- ""}
print(output1)

c <- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Raining without high winds") ))
d <- length(which( (clean_accident$SexOfCasualty==2) & (clean_accident$WeatherConditions == "Raining without high winds") ))
if (c>d){
  output2 <-(c( "Males have ",(c-d)," more accidents in the class 'Raining without high winds' "))
}else if (b>a){
  output2 <- (c( "Females have ",(d-c)," more accidents in the class 'Raining without high winds' "))
}else{ output2 <- ""}

e <- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Snowing without high winds") ))
f <- length(which( (clean_accident$SexOfCasualty==2) & (clean_accident$WeatherConditions == "Snowing without high winds") ))
if (e>f){
  output3 <-(c( "Males have ",(e-f)," more accidents in the class 'Snowing without high winds' "))
}else if (f>e){
  output3 <- (c( "Females have ",(f-e)," more accidents in the class 'Snowing without high winds' "))
}else{ output3 <- ""}

g <- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Fine with high winds") ))
h <- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Fine with high winds") ))
if (c>d){
  output4 <-(c( "Males have ",(c-d)," more accidents in the class 'Fine with high winds' "))
}else if (b>a){
  output4 <- (c( "Females have ",(d-c)," more accidents in the class 'Fine with high winds' "))
}else{ output4 <- ""}

i <- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Raining with high winds") ))
k <- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Raining with high winds") ))
if (c>d){
  output5 <-(c( "Males have ",(c-d)," more accidents in the class 'Raining with high winds' "))
}else if (b>a){
  output5 <- (c( "Females have ",(d-c), " more accidents in the class 'Raining with high winds' "))
}else{ output5 <- ""}

k <- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Snowing with high winds") ))
l <- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Snowing with high winds") ))
if (c>d){
  output6 <-(c( "Males have ",(c-d)," more accidents in the class 'Snowing with high winds' "))
}else if (b>a){
  output6 <- (c( "Females have ",(d-c)," more accidents in the class 'Snowing with high winds' "))
}else{ output6 <- ""}

m<- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Fog or mist - if hazard") ))
n<- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Fog or mist - if hazard") ))
if (c>d){
  output7 <-(c( "Males have ",(c-d)," more accidents in the class 'Fog or mist - if hazard' "))
}else if (b>a){
  output7 <- (c( "Females have ",(d-c)," more accidents in the class 'Fog or mist - if hazard' "))
}else{ output7 <- ""}

o<- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Other") ))
p<- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Other") ))
if (c>d){
  output8 <-(c( "Males have ",(c-d)," more accidents in the class 'Other' "))
}else if (b>a){
  output8 <- (c( "Females have ",(d-c)," more accidents in the class 'Other' "))
}else{ output8 <- ""}

q<- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Unknown") ))
r<- length(which( (clean_accident$SexOfCasualty==1) & (clean_accident$WeatherConditions == "Unknown") ))
if (c>d){
  output9 <-(c( "Males have ",(c-d)," more accidents in the class 'Unknown' "))
}else if (b>a){
  output9 <- (c( "Females have ",(d-c)," more accidents in the class 'Unknown' "))
}else{ output9 <- ""}

print(output1)
print(output2)
print(output3)
print(output4)
print(output5)
print(output6)
print(output7)
print(output8)
print(output9)
# dates ----

x <-format(as.Date(clean_accident$AccidentDate, format="%d/%m/%Y"),"%Y")
y <- 0
z <- 0
n <- 0
f <- 0
other <- 0
for (i in x ){
  
  if ( i == 2017) {
    y <- y + 1
    
  } else if ( i == 2016) {
    z <- z + 1
    
  }else if ( i == 2015) {
    n <- n + 1
    
  }else if ( i == 2014) {
    f <- f + 1
  }else{
    other <- other + 1
  }}
print(c("2017: ",y))
print(c("2016: ",z))
print(c("2015: ",n))
print(c("2014: ",f))
print(c("other: ",other))

# graph2 ----
clean_accident$LightingConditions<-as.factor
clean_accident$LightingConditions<-factor(clean_accident$LightingConditions,
                                         levels=c(1,2,3,4,5,6,7),
                                         labels=c(
                                           "Daylight: street lights present",
                                           "Daylight: no street lighting",
                                           "Daylight: street lighting unknown",
                                           "Darkness: street lights present and lit",
                                           "Darkness: street lights present but unlit",
                                           "Darkness: no street lighting",
                                           "Darkness: street lighting unknown" ))
clean_accident$CasualtySeverity <-as.factor(clean_accident$CasualtySeverity )

clean_accident$CasualtySeverity<-factor(clean_accident$CasualtySeverity,
                                       levels=c(1,2,3),
                                       labels=c("Fatal",
                                                "Serious",
                                                "Slight" ))


ggplot(clean_accident, aes(LightingConditions, fill=CasualtySeverity )) +
  geom_bar(position="dodge")






# graph 3 ----



weather1 <- clean_accident %>%
  group_by(NumberOfVehicles, WeatherConditions) %>%
  filter(WeatherConditions == "Fine without high winds")

weather2 <- clean_accident %>%
  group_by(NumberOfVehicles, WeatherConditions) %>%
  filter(WeatherConditions == "Raining without high winds")

weather3 <- clean_accident %>%
  group_by(NumberOfVehicles, WeatherConditions) %>%
  filter(WeatherConditions ==  "Snowing without high winds")

weather4 <- clean_accident %>%
  group_by(NumberOfVehicles, WeatherConditions) %>%
  filter(WeatherConditions ==     "Fine with high winds")

weather5 <- clean_accident %>%
  group_by(NumberOfVehicles, WeatherConditions) %>%
  filter(WeatherConditions ==         "Raining with high winds")

weather6 <- clean_accident %>%
  group_by(NumberOfVehicles, WeatherConditions) %>%
  filter(WeatherConditions =="Snowing with high winds")

weather7 <- clean_accident %>%
  group_by(NumberOfVehicles, WeatherConditions) %>%
  filter(WeatherConditions == "Fog or mist - if hazard")

weather8 <- clean_accident %>%
  group_by(NumberOfVehicles, WeatherConditions) %>%
  filter(WeatherConditions == "Other")

weather9 <- clean_accident %>%
  group_by(NumberOfVehicles, WeatherConditions) %>%
  filter(WeatherConditions ==  "Unknown")


weather_1_m <-round((mean(weather1[["NumberOfVehicles"]])), digits = 2)
Weather_2_m <-round(( mean(weather2[["NumberOfVehicles"]])), digits = 2)
weather_3_m <-round(( mean(weather3[["NumberOfVehicles"]])), digits = 2)
weather_4_m <-round(( mean(weather4[["NumberOfVehicles"]])), digits = 2)
weather_5_m <-round(( mean(weather5[["NumberOfVehicles"]])), digits = 2)
weather_6_m <-round(( mean(weather6[["NumberOfVehicles"]])), digits = 2)
weather_7_m <-round(( mean(weather7[["NumberOfVehicles"]])), digits = 2)
weather_8_m <-round(( mean(weather8[["NumberOfVehicles"]])), digits = 2)
weather_9_m <-round(( mean(weather9[["NumberOfVehicles"]])), digits = 2)


condition <-c("Fine without high winds",
              "Raining without high winds",
              "Snowing without high winds",
              "Fine with high winds",
              "Raining with high winds",
              "Snowing with high winds",
              "Fog or mist - if hazard",
              "Other",
              "Unknown")

VehicleMean <- c(weather_1_m,Weather_2_m,weather_3_m,weather_4_m,weather_5_m,weather_6_m,weather_7_m,weather_8_m,weather_9_m)
WBC <- data.frame(condition,VehicleMean)

ggplot(data=WBC, aes(x=condition, y=VehicleMean, fill=condition)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=VehicleMean), position=position_dodge(width=0.9), vjust=-0.25)

# Linear Regression ----

Data_Test <- read.csv("accidents.csv", na.strings = c("","NA"))

# The following function takes a vector as an argument and returns a binary vector
# of 0 corresponding the missing value in the argument vector and 1 if there isn't one.
Data_Test
missDummy <- function(t)
{
  x <- dim(length(t))
  x[which(!is.na(t))] = 1
  x[which(is.na(t))] = 0
  return(x)
}
# Now we will use this function to create a 'dummy' variable that will indicate
# missing values by assigning the value '0', otherwise will take the value '1'.

Data_Test$missing <- missDummy(Data_Test$AgeOfCasualty)
Data_Test
# this will pick the instances with values as training data
TrainData<- Data_Test[Data_Test['missing']==1,]
# this will pick the instances with unknown '(NA)' value as testing data
TestData<- Data_Test[Data_Test['missing']==0,]

# This will then fit a linear model with AgeOfCasualty as dependent variable and CasualtyClass, CasualtySeverity, 
# TypeOfVehicle, WeatherConditions as independent variables.

model<- lm(AgeOfCasualty~CasualtyClass+CasualtySeverity+TypeOfVehicle+WeatherConditions, TrainData)

# This will predict missing values based on the model
pred<- predict(model, TestData)
pred
# Next we insert it back in the original,m the first part will show us Where the NAs are.
#the second will replace the NA with the predicted variables
Data_Test$AgeOfCasualty[is.na(Data_Test$AgeOfCasualty)]
Data_Test$AgeOfCasualty[is.na(Data_Test$AgeOfCasualty)]<- pred

write.csv(Data_Test,"regression.csv", row.names = FALSE)
