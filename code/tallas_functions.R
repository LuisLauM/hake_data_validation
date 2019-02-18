# myFunction <- function(data){
#   
#   .
#   .
#   .
# 
#   output <- list(index = output) # index of rows
#              OR
#   output <- list(range = output) # names of haul index
#   
#   # Define error code
#   attr(x = output, which = "errorCode") <- 6
#   
#   return(output)
# }

# requiredVars: fr_abs, haulIndex, talla_cm
checkRepetitions <- function(data){
  
  # Get freq table by length-haul
  dataFreqs <- tapply(data$fr_abs, list(data$haulIndex, data$talla_cm), sum, na.rm = TRUE)
  dataFreqs[is.na(dataFreqs)] <- 0
  
  # Get index for duplicated freqs
  index <- duplicated(x = dataFreqs) | duplicated(x = dataFreqs, fromLast = TRUE)
  
  dataFreqs <- dataFreqs[index,]
  
  output <- list()
  i <- 1
  while(nrow(dataFreqs) > 0){
    
    if(sum(duplicated(dataFreqs)) > 1){
      comparedData <- apply(dataFreqs[-1,], 1, function(x, y) isTRUE(all.equal(target = x, current = y)), 
                            y = dataFreqs[1,])  
      
      comparedData <- c(rownames(dataFreqs)[1], names(comparedData[comparedData]))
      
      dataFreqs <- dataFreqs[-match(comparedData, rownames(dataFreqs)),]
    }else{
      comparedData <- rownames(dataFreqs)
      
      break
    }
    
    output[[i]] <- comparedData
    i <- i + 1
  }
  
  output <- list(range = output)
  
  # Define error code
  attr(x = output, which = "errorCode") <- 13
  
  return(output)
}


