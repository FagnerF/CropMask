#' This package performs cropping of raster files
#'
#' @param Directory Location where the files will be saved
#' @param shapefile shapefile local
#'
#' @importFrom stats na.omit
#' @importFrom magrittr %>%
#' @importFrom readxl read_xlsx
#' @import remotes
#' @import utils
#' @import sp
#' @importFrom raster raster crs extent mask crop writeRaster
#' @import filesstrings
#' @importFrom geobr read_municipality
#' @importFrom rgdal readOGR
#'
#' @export
ScriptCropMask <- function(shapefile, Directory) {
  if (shapefile == "0") {
    pathNC <- file.choose()

    nc <- raster::raster(pathNC)

    raster::crs(nc) <-
      CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

    pathCondMun <- file.choose()

    CondMun <- list()

    CondMun <- readxl::read_xlsx(pathCondMun) %>%
      stats::na.omit()

    for (i in 1:length(CondMun$Municipio)) {
      cat("== Working on the data ==", "\n")
      cat("Wait...", "\n")
      cat(paste0("Crop from", " ", CondMun$Municipio[i], "\n"))

      shp <- list()
      MaskCrop <- list()

      shp <-
        geobr::read_municipality(code_muni =  CondMun$Codigo[i], year = 2020)

      # crop and mask
      MaskCrop <-
        raster::mask(raster::crop(nc, raster::extent(shp)), shp)

      # export para ".tif"
      raster::writeRaster(
        MaskCrop,
        paste0(Directory, "/Exit/" , "MasCrop_",  CondMun$Codigo[i]),
        format = "GTiff",
        bylayer = TRUE,
        options = c("COMPRESS=NONE", "TFW=YES"),
        overwrite = TRUE


      )

      cat("\014")

    }

  } else if (shapefile == "1") {
    all_files  <- list.files(path=paste0(Directory, "/Rasters"),
                         full.names = TRUE,
                         pattern = ".tif$")

    name_files  <- list.files(path=paste0(Directory, "/Rasters"),
                             full.names = FALSE,
                             pattern = ".tif$")

    shp <- list()

    shp <- list.files(path=paste0(Directory, "/BaseMap"),
                                full.names = TRUE,
                                pattern = ".shp")

    shp <- rgdal::readOGR(dsn=shp, verbose = FALSE,
                          stringsAsFactors=FALSE)

    for (i in 1:length(all_files)) {
      cat("== Working on the data ==", "\n")
      cat("Wait...", "\n")
      cat(paste0("Crop from", " ", all_files[i],"\n"))

      MaskCrop <- list()

      nc <- raster::raster(all_files[i])

      raster::crs(nc) <-
      CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

      # crop and mask
      MaskCrop <-
        raster::mask(raster::crop(nc, raster::extent(shp)), shp)

      # export para ".tif"
      raster::writeRaster(
        MaskCrop,
        paste0(Directory, "/Exit/" , "MasCrop_", name_files[i]),
        format = "GTiff",
        bylayer = TRUE,
        options = c("COMPRESS=NONE", "TFW=YES"),
        overwrite = TRUE


      )

      cat("\014")

    }

  }


  return(cat("Process finished!", "\n"))

}
