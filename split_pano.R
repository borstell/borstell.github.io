split_pano <- function(path, ratio="square") {
  img <- magick::image_read(path)
  vls <- magick::image_attributes(img)
  height <- vls$value[31]
  width <- vls$value[30]
  ratio_factor <- 1
  if (ratio=="wide") {
    ratio_factor <- 1.91
  }
  if (ratio=="tall") {
    ratio_factor <- .8
  }
  crop_width <- floor(as.integer(height)*ratio_factor)
  num <- as.integer(width) %/% crop_width
  file_name <- gsub(".*/", "", path)
  path_name <- gsub(file_name, "", path)
  extension <- paste0(".",tools::file_ext(file_name))
  fname <- gsub(extension, "", file_name)
  print(num)
  for (n in 1:num) {
    im <- magick::image_crop(img, geometry = paste0(crop_width,"x",height,"+",(n-1)*crop_width,"+",0))
    magick::image_write(im, paste0(path_name,fname,"_",n,extension))
  }
}