#' Movement data of a White Stork population located nearby Lake Constance, Germany
#'
#' Dataset containing longitude/latitude point cooridnates and acquisition times of several white stork individuals
#' of a population located nearby Lake Constance.
#'
#' \itemize{
#'   \item lon. Longitude
#'   \item lat. Latitude
#'   \item individual. Name of the individual
#'   \item population. Name of the popualtion
#'   \item dt. Date and timestamps (to be used e. g. as POSIXct)
#'   ...
#' }
#' @details The example data have been pre-processed as explained in the examples. Please note that a very basic moving-/non-moving segmentation algorithm has been used in order to keep the code simple.
#' The provided code and data cannot be applied for actual data analysis, only adequate for movement data visualization! 
#' 
#' @format Data frame containing 2408 rows and 5 variables
#' @source Movebank (2013): URL \url{http://www.movebank.org/}
#' 
#' @examples
#' #DISCLAIMER: The provided code and data cannot be applied for actual data analysis
#' #and should only show the derivation of the example data provided with moveVis.
#' #This code is only adequate for movement data visualization.
#' 
#' \dontrun{
#' #Calculate start/stop times, FUNCTION
#' st_times <- function(data){
#'   for(i in 1:length(data)){
#'     if(i == 1){
#'       start_dt <- data[[i]]$dt[1]
#'       stop_dt <- data[[i]]$dt[length(data[[i]]$dt)]
#'     }else{
#'       start_dt <- c(start_dt, data[[i]]$dt[1])
#'       stop_dt <- c(stop_dt, data[[i]]$dt[length(data[[i]]$dt)])
#'     }
#'   }
#'   return(list(start_dt,stop_dt))
#' }

#' #Distance recalculation, FUNCTION
#' calc_dist <- function(data){
#'   p1 <- data[1:(length(data$lon)-1),1:2]
#'   p2 <- data[2:length(data$lon),1:2]
#'   data$dist_m <- NA
#'   data$dist_m[2:length(data$dist_m)] <- distGeo(p1,p2)
#'   return(data)
#' }
#' 
#' #Moving/non-moving segmentator, FUNCTION
#' class_moving <- function(data,resting_rad){
#'   data$moving <- 0
#'   data$moving[which(data$dist_m >= resting_rad)] <- 1
#'   return(data)
#' }
#' 
#' #Read data
#' data_dir <- "/path/to/movedata.txt" #In this case, an ASCII file with multiple populations
#' dr <- read.table(data_dir,header=TRUE,sep=";")
#' pop_levels <- levels(dr$population)
#' pop_levels_n <- length(pop_levels)
#' 
#' #Differentiate data per population
#' for(i in 1:pop_levels_n){
#'   if(i == 1){
#'     pop_subset <- list(subset(dr, population == pop_levels[i]))
#'   }else{
#'     pop_subset <- c(pop_subset,list(subset(dr, population == pop_levels[i])))
#'   }
#' }
#' 
#' #Differentiate data per individual
#' indi_levels <- levels(dr$individual)
#' indi_levels_n <- length(indi_levels)
#' for(i in 1:indi_levels_n){
#'   if(i == 1){
#'     indi_subset <- list(subset(dr, individual == indi_levels[i]))
#'   }else{
#'     indi_subset <- c(indi_subset,list(subset(dr, individual == indi_levels[i])))
#'   }
#' }
#' 
#' #Compute animation input list with all individuals per population
#' pop_select <- 2 #Selectin a population within the data
#' match_indi_subs <- as.integer(na.omit(match(indi_levels,pop_subset[[pop_select]]$individual)))
#' for(i in 1:length(match_indi_subs)){
#'   if(i == 1){
#'     indi_subset_pop <- list(pop_subset[[pop_select]][match_indi_subs[i]:
#'     (match_indi_subs[i+1]-1),])
#'   }else{
#'     if(i != length(match_indi_subs)){
#'       indi_subset_pop[i] <- list(pop_subset[[pop_select]][match_indi_subs[i]:
#'          (match_indi_subs[i+1]-1),])
#'     }else{
#'       indi_subset_pop[i] <- list(pop_subset[[pop_select]][match_indi_subs[i]:
#'          length(pop_subset[[pop_select]][,1]),])
#'     }
#'   }
#' }
#' 
#' #Calculate dt stamps, extract start_dt and stop_dt for all arrays
#' for(i in 1:length(indi_subset_pop)){
#'   dt <- c(paste0(indi_subset_pop[[i]]$date, " ",indi_subset_pop[[i]]$time))
#'   dt_stamps <- as.POSIXct(strptime(dt, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
#'   indi_subset_pop[[i]]$dt <- align.time(dt_stamps, n=60)
#' }
#' start_dt <- st_times(indi_subset_pop)[[1]]
#' stop_dt <- st_times(indi_subset_pop)[[2]]
#' 
#' #Animal specifications, here for the White Stork
#' max_speed <- 45 
#' max_speed_min <- max_speed/60 #km/min
#' tolerance_m_min <- 50 #meter
#' temp_res <- 5 #for simplicity, we do not detect the temp. resolution automatically here
#' 
#' max_speed_spec <- max_speed_min*temp_res #km/temp_res
#' max_dist_m <- (max_speed_spec*1000)+tolerance_m_min #m/temp_res + tolerance range in m/temp_res
#' 
#' #Clean up individual data by defined props (eliminating peaks etc.), store them per individual
#' for(i in 1:length(indi_subset_pop)){
#'   data_clean <- indi_subset_pop[[i]]
#'   no_peaks <- FALSE
#'   while(no_peaks == FALSE){
#'     data_clean <- calc_dist(data_clean)
#'     dist_peaks <- which(is.na(data_clean$dist_m) == FALSE & data_clean$dist_m >= max_dist_m)
#'     if(length(dist_peaks) <= 1){
#'       no_peaks <- TRUE
#'     } else {
#'       data_clean <- data_clean[-dist_peaks[1:length(dist_peaks)],]
#'     }
#'   }
#'   if(i == 1){indi_subset_clean <- list(data_clean)
#'   }else{indi_subset_clean[i] <- list(data_clean)
#'   }
#'   indi_subset_clean[[i]]$peak <- 0
#'   indi_subset_clean[[i]]$peak[
#'      which(is.na(indi_subset_clean[[i]]$dist_m) == FALSE &
#'         indi_subset_clean[[i]]$dist_m >= max_dist_m)] <- 1 
#' }
#' 
#' #Recalculate start/stop times
#' start_dt <- st_times(indi_subset_clean)[[1]]
#' stop_dt <- st_times(indi_subset_clean)[[2]]
#' 
#' #Interpolate tracks
#' for(i in 1:length(indi_subset_clean)){
#'   out_n <- as.integer(difftime(stop_dt[i],start_dt[i],units="mins"))+1
#'   coords <- matrix(c(indi_subset_clean[[i]]$lon,indi_subset_clean[[i]]$lat),
#'      nrow=length(indi_subset_clean[[i]]$lon))
#'   lon_inter <- approx(coords[,1], n = out_n)
#'   lat_inter <- approx(coords[,2], n = out_n)
#'   dt_new <- seq(indi_subset_clean[[i]]$dt[1],
#'      indi_subset_clean[[i]]$dt[length(indi_subset_clean[[i]]$dt)], length.out = out_n)
#'   
#'   pop_name <- as.character(dt_new)
#'   pop_name[1:length(pop_name)] <- as.character(indi_subset_clean[[i]]$population[1])
#'   indi_name <- as.character(dt_new)
#'   indi_name[1:length(indi_name)] <- as.character(indi_subset_clean[[i]]$individual[1])
#'   if(i==1){
#'     indi_subset_int <- list(data.frame(lon_inter$y,lat_inter$y,pop_name,indi_name,dt_new))
#'     colnames(indi_subset_int[[i]]) <- c("lon","lat","population","individual","dt")
#'   }else{
#'     indi_subset_int[i] <- list(data.frame(lon_inter$y,lat_inter$y,pop_name,indi_name,dt_new))
#'     colnames(indi_subset_int[[i]]) <- c("lon","lat","population","individual","dt")
#'   }
#'   indi_subset_int[[i]] <- calc_dist(indi_subset_int[[i]])
#' }
#' 
#' #Segmentate moving/resting
#' resting_rad <- 20 #meters/min
#' for(i in 1:length(indi_subset_int)){
#'   if(i == 1){
#'     indi_subset_class <- list(class_moving(indi_subset_int[[i]],resting_rad))
#'   }else{
#'     indi_subset_class[i] <- list(class_moving(indi_subset_int[[i]],resting_rad))
#'   }
#' }
#' 
#' #Removing non-moving time periods...
#' for(i in 1:length(indi_subset_class)){
#'   if(i == 1){indi_subset_moving <- list(
#'      indi_subset_class[[i]][which(indi_subset_class[[i]]$moving == 1),])}
#'   else{indi_subset_moving[i] <- list(
#'      indi_subset_class[[i]][which(indi_subset_class[[i]]$moving == 1),])}
#' }
#' 
#' #Extract names of each individual
#' for(i in 1:length(indi_subset_moving)){
#'   if(length(indi_subset_moving[[i]][,1]) > 0){
#'     if(i==1){
#'       indi_names <- indi_subset_moving[[i]]$individual[1]
#'     }else{
#'       indi_names <- paste0(indi_names,", ",indi_subset_moving[[i]]$individual[1])
#'     }
#'   }
#' }
#' 
#' #Create move objects list
#' move_index <- 0
#' for(i in 1:length(indi_subset_moving)){
#'   maxi <- length(indi_subset_moving[[i]]$lon)
#'   #maxi <- 200 #If the maximum length should be defined by the user
#'   #if(length(indi_subset_moving[[i]]$lon) < maxi){
#'      maxi <- length(indi_subset_moving[[i]]$lon)
#'   }
#'   
#'   if(length(indi_subset_moving[[i]][,1]) > 0){
#'     if(i == 1){data_ani <- 
#'        list(move(x=indi_subset_moving[[i]]$lon[1:maxi],y=indi_subset_moving[[i]]$lat[1:maxi],
#'           time=indi_subset_moving[[i]]$dt[1:maxi],proj=CRS("+proj=longlat +ellps=WGS84"),
#'           animal=unlist(strsplit(indi_names,", "))[i]))
#'     }else{data_ani[i-move_index] <-
#'        list(move(x=indi_subset_moving[[i]]$lon[1:maxi],y=indi_subset_moving[[i]]$lat[1:maxi],
#'           time=indi_subset_moving[[i]]$dt[1:maxi],proj=CRS("+proj=longlat +ellps=WGS84"),
#'          animal=unlist(strsplit(indi_names,", "))[i]))}
#'   }else{move_index <- move_index+1}
#' }
#' 
#' #write out
#' file.create("moveVis_sample.txt")
#' for(i in 1:length(indi_subset_moving)){
#'   print(i)
#'   if(i == 1){
#'     writethis <- indi_subset_moving[[i]]
#'     write.table(writethis,file="samples.txt",sep=";")
#'   }else{
#'     writethis <- rbind(writethis,indi_subset_moving[[i]])
#'     write.table(writethis,file="samples.txt",sep=";")
#'   }
#' }
#' }
"move_data"