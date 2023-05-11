#Preparation
#Lesson
#(Quarto ausprobieren)

library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(tmap)
library(gitcreds)
library(dplyr)


#Daten einlesen
wildschwein<- read_csv("wildschwein_BE_2056.csv")

#welche Tiernamen gibts
wildschwein$TierName |> unique()

#nur 3Tage verwenden -> filter
sabi<-wildschwein |>
  filter(TierName== "Sabi") |>
  filter(DatetimeUTC >= "2015-07-01", DatetimeUTC < "2015-07-03")


#plotten
ggplot(sabi, aes(E, N, color=DatetimeUTC)) +
  geom_point() +
  geom_path() +
  coord_equal()

sabi |>
  head(50) |>
  ggplot(aes(DatetimeUTC, 1)) +
  geom_point()

#zusätzliche colums für steplength hinzufügen
sabi |>
  mutate(
    n_plus1 = sqrt((lead(E)-E)^2 + (lead(N)-N)^2),
    n_plus2 = sqrt((lead(E,2)-E)^2 + (lead(N,2)-N)^2)
  )
#steplength n plus 1



sabi<-sabi |>
  mutate(
    n_plus1 = sqrt((lead(E)-E)^2 + (lead(N)-N)^2),
    n_plus2 = sqrt((lead(E,2)-E)^2 + (lead(N,2)-N)^2),
    n_minus1 = sqrt((lag(E)-E)^2 + (lag(N)-N)^2),
    n_minus2 = sqrt((lag(E,2)-E)^2 + (lag(N,2)-N)^2),
    
  )

#we want the mean value per Row, we have to explicitly specify this before mutate() with the function rowwise()
#ungroup damit nicht mehr rowwise gruppiert
sabi<-sabi|>
  rowwise() |>
  mutate(
    stepMean = mean(c(n_minus1, n_minus2, n_plus1, n_plus2), na.rm = TRUE)
  ) |>
  ungroup()


ggplot(sabi, aes(stepMean)) +
geom_histogram(binwidth = 10) +
  geom_vline(xintercept = mean(sabi$stepMean, na.rm = TRUE))

#Remove static points
sabi |>
  mutate(static = stepMean < mean(stepMean, na.rm =TRUE)) |>
  ggplot(aes(E, N)) +
  geom_path() +
  geom_point(aes(color = static)) +
  coord_fixed()




################
#Task with own posmo movement data

#Preparation

library("readr")
library("sf")

posmo <- read_delim("posmo_2023-04-01T00_00_00+02_00-2023-05-04T23_59_59+02_00.csv")
# Keep only the necessary columns
posmo <- select(posmo, datetime, lon_x, lat_y)


#transform data to EPSG 2056
posmo <- st_as_sf(posmo, coords = c("lon_x","lat_y"), crs = 4326) |>
  st_transform(2056)


head(posmo)

#st_coordinates extracts the coordinates from our sf object
#bind these coordinates back to our sf object using cbind
posmo_coordinates <- st_coordinates(posmo)

posmo <- cbind(posmo, posmo_coordinates)

#extract one day for further analysis
posmo_filter <- posmo |>
  filter(as.Date(datetime) == "2023-05-03")

#commit (github), ignore location data 
#???doesnt work. Why?

#Task 1, Segmentation
#plotten
ggplot(posmo_filter, aes(X, Y, color=datetime)) +
  geom_point() +
  geom_path() +
  coord_equal()

posmo_filter |>
  head(50) |>
  ggplot(aes(datetime, 1)) +
  geom_point()

#zusätzliche colums für steplength hinzufügen
posmo_filter |>
  mutate(
    n_plus1 = sqrt((lead(X)-X)^2 + (lead(Y)-Y)^2),
    n_plus2 = sqrt((lead(X,2)-X)^2 + (lead(Y,2)-Y)^2)
  )
#steplength n plus 1



posmo_filter<-posmo_filter |>
  mutate(
    n_plus1 = sqrt((lead(X)-X)^2 + (lead(Y)-Y)^2),
    n_plus2 = sqrt((lead(X,2)-X)^2 + (lead(Y,2)-Y)^2),
    n_minus1 = sqrt((lag(X)-X)^2 + (lag(Y)-Y)^2),
    n_minus2 = sqrt((lag(X,2)-X)^2 + (lag(Y,2)-Y)^2),
    
  )

#we want the mean value per Row, we have to explicitly specify this before mutate() with the function rowwise()
#ungroup damit nicht mehr rowwise gruppiert
posmo_filter<-posmo_filter|>
  rowwise() |>
  mutate(
    stepMean = mean(c(n_minus1, n_minus2, n_plus1, n_plus2), na.rm = TRUE)
  ) |>
  ungroup()


ggplot(posmo_filter, aes(stepMean)) +
  geom_histogram(binwidth = 10) +
  geom_vline(xintercept = mean(posmo_filter$stepMean, na.rm = TRUE))


#Remove static points
posmo_filter |>
  mutate(static = stepMean < mean(stepMean, na.rm =TRUE)) |>
  ggplot(aes(X, Y)) +
  geom_path() +
  geom_point(aes(color = static)) +
  coord_fixed()


#Task 2: Specify and apply threshold d
#Task 3: Visualize segmented trajectories

#explore values of stepMean
boxplot(posmo_filter$stepMean)
hist(posmo_filter$stepMean)
summary(posmo_filter$stepMean)

#Store the new information (boolean to differentiate between stops (TRUE) and moves (FALSE)) in a new column named static.
posmo_filter<-posmo_filter |>
  mutate(static = stepMean < mean(stepMean, na.rm =TRUE))

#Visualize
posmo_filter |>
  ggplot(aes(X, Y)) +
  geom_path() +
  geom_point(aes(color = static)) +
  coord_equal()

#commit


#Task 4: Segment-based analysis

#we need a unique ID for each segment that we can use as a grouping variable.
rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}


posmo_filter <- posmo_filter |>
  mutate(segment_id = rle_id(static))

head(posmo_filter)

#Visualize the moving segments by colourizing them by segment_ID. 
posmo_filter |>
  ggplot(aes(X, Y)) +
  geom_path(aes(color = segment_id)) +
  geom_point() +
  coord_equal()

#Then use segment_ID as a grouping variable to determine the segments duration 
#and remove short segments (e.g. segments with a duration < 5 Minutes)
#??????????? didnt work


#Task 5: Similarity measures
# Import data
pedestrian <- read_delim("pedestrian.csv")

#Visualize different TrailID
p <- pedestrian |>
  ggplot(aes(
    x = E,
    y = N,
    group = TrajID
  ))

p + geom_line(linewidth = 1, color = "pink") +
  geom_point(size = 1, color = "steelblue3") +
  # Stufen von 'TrajID' in die Zeilen
  facet_grid(TrajID ~ .)


#Task 6: Calculate similarity
install.packages("SimilarityMeasures")
library(SimilarityMeasures)

help(package = "SimilarityMeasures")

#compare trajectory 1 to trajectories 2-6 
#using DTW, EditDist, Frechet and LCSS.

#convert data frame to matrix, da DTW Matrix braucht
mat <- as.matrix(pedestrian)

DTW(pedestrian$TrajID, pedestrian$TrajID, pointSpacing=-1)
DTW(traj1, traj2, pointSpacing=-1)


EditDist(traj1, traj2, pointDistance=20)

Frechet(traj1, traj2, testLeash=-1)

LCSS(traj1, traj2, pointSpacing=-1, pointDistance=20, 
     errorMarg=2, returnTrans=FALSE)

#task 6 not completed, how????

