rm(list = ls()); gc(reset = TRUE)

require(ruisu)
source("code/auxiliar_functions.R")
load("data/auxiliarData.RData")

dataPath <- "data/BD_2018_19.xlsx"

fechaInicio <- "2018-7-1"
fechaFin <- "2019-6-30"

checkVariables <- readxl::read_excel(path = "data/checkVariables.xlsx", sheet = 1)
class(checkVariables) <- "data.frame"

checkVariables[checkVariables$variable == "fecha_desem", 5:6] <- c(fechaInicio, fechaFin)

messageCodes <- readxl::read_excel(path = "data/messageCodes.xlsx", sheet = 1)
class(messageCodes) <- "data.frame"

# Define the object to record events
output <- NULL

# Details of data that will be read
data2read <- list(path = rep(dataPath, 3),
                  sheet = c("BD_Bitacora", "BD_Compese", "BD_Tallas"),
                  name = c("bitacora", "compese", "tallas"))

# Loop for reading data
allData <- list()
for(i in seq_along(data2read$name)){
  # Read (Excel) file 
  object <- suppressWarnings(readxl::read_excel(path = data2read$path[i], sheet = data2read$sheet[i]))
  
  # Fixing class to data.frame
  class(object) <- "data.frame"
  
  # Making lower case to column names and replacing special characters
  colnames(object) <- tolower(colnames(object))
  colnames(object) <- chartr(old = "αινσϊόρ", new = "aeiouun", x = colnames(object))
  
  # Create a check object which will be used to indicate where some errors were found
  chkObject <- matrix(data = 0, nrow = nrow(object), ncol = ncol(object), dimnames = dimnames(object))
  
  # Making basic checks 
  basicChks <- chkDB(data = object, type = data2read$name[i], chkObject = chkObject)
  output <- rbind(output, basicChks$issues)
  chkObject <- basicChks$chkObject
  
  # Add date
  object$date <- with(object, as.Date(paste(ano_capt, mes_capt, dia_capt, sep = "-")))
  chkObject <- cbind(chkObject, date = rep(0, nrow(chkObject)))
  
  # Check out of limits dates
  index <- which(is.na(object$date) | object$date < as.Date(fechaInicio) | object$date > as.Date(fechaFin))
  
  # Add event
  if(length(index) > 0){
    output <- addEvent(list(paste(index, collapse = ", ")), type = data2read$name[i], logObject = output, code = 7)
    chkObject[index, "date"] <- 1  
  }
  
  # Add haul index
  haul <- match("n_lance", colnames(object))
  # object$haulIndex <- with(object, paste(as.numeric(date), hi_hora, embarcacion, object[,haul], sep = "-"))
  object$haulIndex <- with(object, paste(as.numeric(date), embarcacion, object[,haul], sep = "-"))
  
  # Set directory of functions that are going to be used, given a data type
  typeFx <- paste0("code/", data2read$name[i], "_functions.R")
  
  if(file.exists(typeFx)){
    # Load script
    source(typeFx)
    
    # Extract main functions and their names
    varsFx <- getFunctions(file = typeFx)
    typeFx <- names(varsFx)
    
    # Execute each function
    for(j in seq_along(typeFx)){
      # Set in memory each function
      criteriaFx <- match.fun(typeFx[j])
      
      # Filter data checking if there were any problem on required variables for function
      temporalVars <- varsFx[[j]]
      temporalVars <- temporalVars[!is.element(temporalVars, c("n_lance", "haulIndex"))]
      temporalVars <- match(temporalVars, colnames(chkObject))
      
      # Get index of rows for valid data
      indexData <- which(!(rowSums(as.matrix(chkObject[,temporalVars])) > 0))
      
      # Apply function
      out <- criteriaFx(data = object[indexData,])
      
      # Extract and convert outputs depending on they express an index for rows or hauls (range of rows)
      if(!is.null(out$index)){
        # Prepare output indices as a list separated by commas
        out$index <- indexData[out$index]
        out$index <- paste(out$index + 1, collapse = ", ")
        
        # Determine if it's usefull to create an event
        thereIsEvent <- out$index != "" && length(out$index) > 0 && !is.na(out$index) && !is.null(out$index)
      }else{
        # Prepare output range indices
        out$range <- paste(sapply(out$range, convertRange2Text), collapse = "; ")
        
        # Determine if it's usefull to create an event
        thereIsEvent <- length(out$range) > 0 && !is.na(out$range) && !is.null(out$range)
      }
      
      # Create an event
      if(isTRUE(thereIsEvent)){
        output <- addEvent(out, code = attr(x = out, which = "errorCode"), type = data2read$name[i], 
                           logObject = output)  
      }
    }  
  }
  
  # Compile data as a list
  allData[[i]] <- object
}

# Change names of list
names(allData) <- data2read$name

# Write results as a CSV file
write.csv(x = output, file = "outputs/logOut.csv", row.names = FALSE)

# Write results as An Excel file
output <- as.data.frame(output)
WriteXLS::WriteXLS(x = "output", ExcelFileName = "outputs/logOut.xlsx", AdjWidth = TRUE, 
                   FreezeRow = 1, Encoding = "latin1")