#' This package performs cropping of raster files
#'
#' @param Directory Location where the files will be saved
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
#'
#' @export
ScriptCropMask <- function(Directory) {
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
      paste0(Directory, "/" , "MasCrop_",  CondMun$Codigo[i]),
      format = "GTiff",
      bylayer = TRUE,
      options = c("COMPRESS=NONE", "TFW=YES"),
      overwrite = TRUE


    )

    cat("\014")

  }

  return(cat("Process finished!", "\n"))

}
