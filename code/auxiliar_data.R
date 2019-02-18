
# Depth data --------------------------------------------------------------

# Read nc file
ncFile <- ncdf4::nc_open(filename = "data/etopo180_4558_b013_12d8.nc")

# Get index of X positions for read nc 
index_x <- cut(x = c(-100, -70), breaks = c(ncFile$dim$longitude$vals, Inf), 
               labels = seq(ncFile$dim$longitude$len))
index_x <- as.numeric(as.character(index_x)) + c(0, 1)

# Get index of Y positions for read nc 
index_y <- cut(x = c(-19, -2), breaks = c(ncFile$dim$latitude$vals, Inf), seq(ncFile$dim$latitude$len))
index_y <- as.numeric(as.character(index_y)) + c(0, 1)

# Read nc depth data
depthData <- ncdf4::ncvar_get(nc = ncFile, varid = "altitude", start = c(index_x[1], index_y[1]), 
                              count = c(diff(index_x), diff(index_y)) + 1)

# Close nc file
ncdf4::nc_close(nc = ncFile)

# Convert data to raster object
depthData <- raster::raster(list(x = ncFile$dim$longitude$vals[seq(index_x[1], index_x[2])],
                                 y = ncFile$dim$latitude$vals[seq(index_y[1], index_y[2])],
                                 z = depthData))


# Embarcaciones -----------------------------------------------------------

embarcaciones <- readxl::read_excel(path = "data/especies_embarcaciones.xlsx", sheet = "embarcaciones")
class(embarcaciones) <- "data.frame"

# Making lower case to column names and replacing special characters
colnames(embarcaciones) <- tolower(colnames(embarcaciones))
colnames(embarcaciones) <- chartr(old = "αινσϊόρ", new = "aeiouun", x = colnames(embarcaciones))



# Save  all data ----------------------------------------------------------

save(depthData, embarcaciones, file = "data/auxiliarData.RData", compress = "xz")
