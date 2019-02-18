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

# requiredVars: haulIndex, `nombre comun`
checkSppNames <- function(data){
  
  output <- as.data.frame.array(table(data$haulIndex, data$`nombre comun`))
  
  output <- which(apply(output, 1, function(x) any(x > 1)))
  
  output <- list(index = output)
  
  # Define error code
  attr(x = output, which = "errorCode") <- 13

  return(output)
}
