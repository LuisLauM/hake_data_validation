
chkDB <- function(data, type, chkObject){
  output <- NULL
  
  chkDB <- checkVariables[checkVariables$data_name == type,]
  
  # 1. Check if there are variables not considered in checkVariables.xlsx
  outVars <- colnames(data)[!is.element(colnames(data), chkDB$variable)]
  if(length(outVars) > 0){
    output <- addEvent(paste(outVars, collapse = ", "), type = type, logObject = output, code = 1)
  }
  
  # 2. Check if there are missing variables
  missingVars <- !is.element(chkDB$variable, colnames(data))
  if(any(missingVars)){
    output <- addEvent(paste(chkDB$variable[missingVars], collapse = ", "), type = type, 
                       logObject = output, code = 2)
  }
  
  # 3. Check NA for each variable
  outLimitsVars <- NULL
  for(j in seq(ncol(data))){
    if(!is.element(colnames(data)[j], chkDB$variable)) next
    
    # Check NA
    index <- which(is.na(data[,j]))
    
    if(isTRUE(as.logical(chkDB$report_na[j])) && sum(index) > 0){
      chkObject[index, j] <- 1
      
      index <- as.list(c(colnames(data)[j], paste(index + 1, collapse = ", ")))
      output <- addEvent(index, type = type, logObject = output, code = 3)
    }
    
    # Check NA due to wrong type of values 
    index <- withinType(variable = colnames(data)[j], db = data, chkDB = chkDB)
    
    if(length(index) > 0){
      chkObject[index, j] <- 1
      
      index <- as.list(c(colnames(data)[j], paste(index + 1, collapse = ", ")))
      output <- addEvent(index, type = type, logObject = output, code = 9)
    }
    
    # Check out of limits
    index <- withinLimits(variable = colnames(data)[j], db = data, chkDB = chkDB)
    
    if(length(index) > 0){
      chkObject[index, j] <- 1
      
      index <- as.list(c(colnames(data)[j], paste(index + 1, collapse = ", ")))
      output <- addEvent(index, type = type, logObject = output, code = 4)
    } 
  }
  
  output <- list(issues = output,
                 chkObject = chkObject)
  
  return(output)
}

addEvent <- function(..., type, logObject, code){
  
  if(is.null(list(...))) return(logObject)
  
  # Generate event text 
  index <- messageCodes$code == code
  
  if(length(...) > 1){
    theMessage <- do.call(what = sprintf, args = c(fmt = messageCodes$message[index], ...))
  }else{
    theMessage <- sprintf(fmt = messageCodes$message[index], ...)
  }
  
  eventText <- c(base = type,
                 codigo_error = code,
                 importancia = messageCodes$importance[index],
                 mensaje = theMessage)
  
  # sprintf(fmt = messageCodes$message[index], ...)
  
  if(is.null(logObject)){
    logObject <- rbind(logObject, eventText)
  }else if(!is.element(eventText["mensaje"], logObject[,"mensaje"])){
    logObject <- rbind(logObject, eventText)
  }
  
  return(logObject)
}

getFunctions <- function(file){
  scriptAsText <- readLines(con = file)
  
  index <- grepl(pattern = " <- function", x = scriptAsText) & !grepl(pattern = "^#|(_aux)", x = scriptAsText)
  allFunctions <- scriptAsText[index]
  allFunctions <- sapply(strsplit(x = allFunctions, split = " <- "), "[", 1)
  
  requiredVars <- scriptAsText[which(index) - 1]
  requiredVars <- sapply(strsplit(x = requiredVars, split = ": "), "[", 2)
  requiredVars <- strsplit(x = requiredVars, split = ", ")
  requiredVars <- lapply(requiredVars, gsub, pattern = "`", replacement = "")
  
  names(requiredVars) <- allFunctions
  
  return(requiredVars)
}

convertRange2Text <- function(x){
  x <- docall(rbind, strsplit(x = x, split = "-"))
  
  x <- apply(x, 1, function(x){
    date <- format(as.Date(as.numeric(x[1]), origin = "1970-1-1"), format = "%d/%m/%Y")
    ship <- x[2]
    haul <- paste0("Cala-", x[3])
    
    return(paste(date, ship, haul, sep = "_"))
  })
  
  return(paste0("[", paste(x, collapse = ", "), "]"))
}


# withinLimits ------------------------------------------------------------

withinLimits <- function(variable, db, chkDB){
  
  index <- chkDB$variable == variable
  
  type <- tolower(chkDB$type[index])
  
  chkDB <- chkDB[index,]
  
  output <- switch(type,
                   char = withinLimits_char(var = db[,variable], chkDB),
                   num  = withinLimits_num(var = db[,variable], chkDB),
                   date = withinLimits_date(var = db[,variable], chkDB),
                   int  = withinLimits_int(var = db[,variable], chkDB),
                   sprintf("No se ha definido un criterio de revisión de límites para el tipo %s.", type))
  
  return(output)
}

withinLimits_char <- function(var, chkDB){
  # Get previous NA
  prevNA <- which(is.na(var))
  
  # Get vector to compare
  compareVector <- tolower(as.character(chkDB[-(1:4)]))
  
  if(all(is.na(compareVector))){
    output <- NULL  
  }else{
    # Get positions with values out of limits
    if(tolower(compareVector[1]) == "pattern"){
      output <- which(!grepl(pattern = tolower(compareVector[2]), x = tolower(var)))
    }else{
      output <- which(!is.na(var) & !is.element(tolower(var), compareVector))
    }
  }
  
  return(setdiff(x = output, y = prevNA))
}

withinLimits_num <- function(var, chkDB){
  # Get previous NA
  prevNA <- which(is.na(var))
  
  # Convert data to numeric avoiding NA warnings
  var <- suppressWarnings(as.numeric(var))
  
  # Catch NA belonging ONLY to conversion 
  newNA <- setdiff(which(is.na(var)), prevNA)
  
  # Get positions with values out of limits
  output <- !is.na(var) & (var < as.numeric(chkDB[5]) | var > as.numeric(chkDB[6]))
  
  return(which(output))
}

withinLimits_date <- function(var, chkDB){
  # Get previous NA
  prevNA <- which(is.na(var))
  
  # Convert data to numeric avoiding NA warnings
  var <- suppressWarnings(as.Date(var))
  
  # Catch NA belonging ONLY to conversion 
  newNA <- setdiff(which(is.na(var)), prevNA)
  
  # Get positions with values out of limits
  output <- !is.na(var) & (var < as.Date(as.character(chkDB[5])) | var > as.Date(as.character(chkDB[6])))
  
  return(which(output))
}

withinLimits_int <- function(var, chkDB){
  # Get previous NA
  prevNA <- which(is.na(var))
  
  # Convert data to numeric avoiding NA warnings
  var <- suppressWarnings(as.numeric(var))
  
  # Catch NA belonging ONLY to conversion 
  newNA <- setdiff(which(is.na(var)), prevNA)
  
  # Get positions with values out of limits
  output <- !is.na(var) & (var < as.integer(chkDB[5]) | var > as.integer(chkDB[6])) & isTRUE(all.equal(var - as.integer(var), 0))
  
  return(sort(c(which(output), newNA)))
}


# withinType --------------------------------------------------------------

withinType <- function(variable, db, chkDB){
  
  # Get previous NA
  prevNA <- which(is.na(db[,variable]))
  
  index <- chkDB$variable == variable
  
  type <- tolower(chkDB$type[index])
  
  chkDB <- chkDB[index,]
  
  type <- switch(type,
                 char = as.character,
                 num  = as.numeric,
                 date = as.Date,
                 int  = as.integer,
                 sprintf("No se ha definido un criterio de revisión de tipo para %s.", type))
  
  output <- setdiff(which(is.na(suppressWarnings(type(db[,variable])))), prevNA)
  
  return(output)
}
