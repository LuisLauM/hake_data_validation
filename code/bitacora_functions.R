# myFunction <- function(data){
#   
#   .
#   .
#   .
# 
#   output <- list(index = output, ...) # index of rows
#              OR
#   output <- list(range = output,...) # names of haul index
# 
#   # Define error code
#   attr(x = output, which = "errorCode") <- 6
#   
#   return(output)
# }

# requiredVars: lo_i_grad, lo_i_min, ls_i_grad, ls_i_min
ptsOnLand <- function(data){
  
  # Calculate lon/lat
  data$lon <- with(data, -(lo_i_grad + lo_i_min/60))
  data$lat <- with(data, -(ls_i_grad + ls_i_min/60))
  
  # Get previous NA
  prevNA <- which(!complete.cases(data[,c("lon", "lat")]))
  
  # Get AIP to coast in nm
  data$aip <- ruisu::isopArea.assigner(dataPoints = data, old = FALSE)
  
  # Catch NA due ONLY to points on land
  output <- list(index = setdiff(which(is.na(data$aip)), prevNA))
  
  # Define error code
  attr(x = output, which = "errorCode") <- 5
  
  return(output)
}

# requiredVars: lo_i_grad, lo_i_min, ls_i_grad, ls_i_min
depthByType <- function(data){
  
  # Define depth limits by ship type
  depthLimits <- list(eac = c(20, 200),
                      eame = c(20, 300),
                      eme = c(20, 200))
  
  # Calculate lon/lat
  data$lon <- with(data, -(lo_i_grad + lo_i_min/60))
  data$lat <- with(data, -(ls_i_grad + ls_i_min/60))
  
  # Assign depth info using points
  data$depth <- raster::extract(x = depthData, y = as.matrix(data[,c("lon", "lat")]))
  
  # Merge catch data using haulIndex from embarcaciones to bitacora
  data <- merge(x = data, y = embarcaciones[,c("embarcacion", "tipo")], 
                by = "embarcacion", all.x = TRUE, sort = FALSE)
  
  output <- NULL
  for(i in seq_along(depthLimits)){
    index <- (data$tipo == names(depthLimits)[i] & !is.na(data$depth) & 
                (data$depth < depthLimits[[i]][1] | data$depth > depthLimits[[i]][2]))
    
    output <- sort(c(output, which(index)))
  }
  
  output <- list(index = output)
  
  # Define error code
  attr(x = output, which = "errorCode") <- 6
  
  return(output)
}

# requiredVars: n_lance, haulIndex
checkHaulNumber <- function(data){
  
  # Sort data by haul
  data <- data[order(data$n_lance, data$haulIndex),]
  
  # Get new haul index (avoiding to include haul number at the end)
  data$haulIndex2 <- sapply(strsplit(data$haulIndex, "-"), function(x) paste(x[-length(x)], collapse = "-"))
  
  # Get differences by haul
  haulDiffs <- tapply(data$n_lance, data$haulIndex2, diff)
  
  # Detect whether one of them are different than 1 (missing haul number)
  haulDiffs <- sapply(haulDiffs, function(x) any(x != 1))
  
  # Detect rows
  output <- list(index = match(names(haulDiffs)[haulDiffs], data$haulIndex))

  # Define error code
  attr(x = output, which = "errorCode") <- 8

  return(output)
}

# requiredVars: cubos, cajas
NACubosCajas <- function(data){
  
  # Check positions of double NA
  output <- list(index = which(is.na(data$cubos) & is.na(data$cajas)))

  # Define error code
  attr(x = output, which = "errorCode") <- 10

  return(output)
}

# requiredVars: embarcacion, cubos
checkCubos <- function(data){
  
  # Merge catch data using haulIndex from embarcaciones to bitacora
  data <- merge(x = data, y = embarcaciones[,c("embarcacion", "tipo")], 
                by = "embarcacion", all.x = TRUE, sort = FALSE)
  
  output <- list(index = which(data$tipo == "eame" & is.na(data$cubos)))
  
  # Define error code
  attr(x = output, which = "errorCode") <- 11
  
  return(output)
}

# requiredVars: embarcacion, cajas
checkCajas <- function(data){
  
  # Merge catch data using haulIndex from embarcaciones to bitacora
  data <- merge(x = data, y = embarcaciones[,c("embarcacion", "tipo")], 
                by = "embarcacion", all.x = TRUE, sort = FALSE)
  
  output <- list(index = which((data$tipo == "eac" | data$tipo == "eme") & is.na(data$cajas)))
  
  # Define error code
  attr(x = output, which = "errorCode") <- 12
  
  return(output)
}

# requiredVars: lo_i_grad, lo_i_min, ls_i_grad, ls_i_grad, ls_i_min, lo_f_grad, lo_f_min, ls_f_grad, ls_f_grad, ls_f_min
ptsStartEnd <- function(data, maxDistance_nm = 10){
  
  # Calculate lon/lat
  data$lon_i <- with(data, -(lo_i_grad + lo_i_min/60))
  data$lat_i <- with(data, -(ls_i_grad + ls_i_min/60))
  data$lon_f <- with(data, -(lo_f_grad + lo_f_min/60))
  data$lat_f <- with(data, -(ls_f_grad + ls_f_min/60))
  
  data$ptDistance <- apply(data[,c("lon_i", "lat_i", "lon_f", "lat_f")], 1, 
                           function(x) spDistsN1(pts = matrix(x[1:2], ncol = 2), 
                                                 pt = matrix(x[3:4], ncol = 2), 
                                                 longlat = TRUE))
  
  output <- which(data$ptDistance > maxDistance_nm*1.852)
  
  # Prepare output 
  output <- list(maxDistance_nm, index = output)
  
  # Define error code
  attr(x = output, which = "errorCode") <- 15
  
  return(output)
}

# Function that completes with zeros an integer number
completeZeros_aux <- function(x, desireNChar = 2){
  charX <- nchar(as.integer(x))
  
  naX <- is.na(x)
  
  if(charX < desireNChar && sum(!naX) > 0){
    output <- rep(NA, length(x))
    output[!naX] <- paste0(paste0(rep(0, desireNChar - charX)), x[!naX])
  }else{
    output <- x
  }
  
  return(list(index = output))
}

# requiredVars: date, hf_hora, hi_hora, hf_min, hi_min
checkTimes_startend <- function(data, maxTimeHaul = 5){
  
  # Get previous NA
  prevNA <- which(is.na(data$date))
  
  data$hf_hora[data$hf_hora == 24] <- 0
  data$hi_hora[data$hi_hora == 24] <- 0
  
  data$hi_hora <- sapply(data$hi_hora, completeZeros_aux)
  data$hf_hora <- sapply(data$hf_hora, completeZeros_aux)
  data$hi_min <- sapply(data$hi_min, completeZeros_aux)
  data$hf_min <- sapply(data$hf_min, completeZeros_aux)

  # Get start/end dates
  data$timeStart <- with(data, as.POSIXct(paste0(format(date, format = "%Y-%m-%d "), hi_hora, ":", hi_min, ":00")))
  data$timeEnd <- with(data, as.POSIXct(paste0(format(date, format = "%Y-%m-%d "), hf_hora, ":", hf_min, ":00")))
  
  # For those date-hour where End values are higher than Start, recalculate End date adding 1 day to catch date
  index <- data$timeEnd <= data$timeStart
  data$timeEnd[index] <- with(data[index,], as.POSIXct(paste0(format(date + 1, format = "%Y-%m-%d "), hf_hora, ":", hf_min)))
  
  # Calculate differerences (in hours) and determine which of them are higher than the threshold
  output <- which(as.numeric(difftime(time1 = data$timeEnd, time2 = data$timeStart, units = "hours")) > maxTimeHaul)
  
  # Prepare output 
  output <- list(maxTimeHaul, index = output)

  # Define error code
  attr(x = output, which = "errorCode") <- 16
  
  return(output)
}

# checkTimes_withinHauls
