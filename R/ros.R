# Install and Load Packages
install_load <- function(packages){
  k <- packages[!(packages %in% installed.packages()
                  [,"Package"])];
  if(length(k))
  {
    install.packages(k, repos = 'https://cran.rstudio.com/');
  }

  for (package_name in packages) {
    library(package_name, character.only = TRUE, quietly = TRUE);
  }
}

install_load(c("jpeg", "png", "plot3D", "magick"))

insertImages <- function(raw_img_path)
  {

  #' Read raw image
  raw_img <- readJPEG(raw_img_path)

  #  plot image
  raw_img_green <- t(raw_img[,,2])#  greem channel
  raw_img_blue <- t(raw_img[,,3])#  blue channel
  raw_img_red <- t(raw_img[,,1])#  best visual contrast is 1, red channel

  thresh_img_green <- raw_img_green
  thresh_img_blue <- raw_img_blue
  thresh_img_red <- raw_img_red

  #  meaningless, just for dimensions
  thresh_img <- raw_img_red

  #' Apply thresholds
  log_thresh_db <- thresh_img_red < 0.15
  log_thresh1 <- thresh_img_red < 0.3 & thresh_img_red >= 0.15 & thresh_img_blue > 0.4
  log_thresh2 <- thresh_img_red < 0.4 & thresh_img_red >= 0.3 & thresh_img_blue > 0.5

  log_thresh3 <- thresh_img_red >= 0.6 & thresh_img_red < 0.65 & thresh_img_blue > 0.85
  log_thresh4 <- thresh_img_red > 0.4 & thresh_img_red < 0.6 & thresh_img_blue > 0.7
  log_thresh5 <- thresh_img_red > 0.4 & thresh_img_red < 0.6 & thresh_img_blue > 0.6 & thresh_img_green > 0.65

  for( i in 1:nrow(log_thresh1)){
    for( j in 1:ncol(log_thresh1)){
      if(log_thresh_db[i,j]){
        thresh_img[i,j] <- 3
      }else{
        if(log_thresh1[i,j]){
          thresh_img[i,j] <- 2
        }else{
          if(log_thresh2[i,j]){
            thresh_img[i,j] <- 2
          }else{
            if(log_thresh3[i,j]){
              thresh_img[i,j] <- 1
            }else{
              if(log_thresh4[i,j]){
                thresh_img[i,j] <- 1
              }else{
                if(log_thresh5[i,j]){
                  thresh_img[i,j] <- 1
                }else{
                  thresh_img[i,j] <- 0
                }
              }
            }
          }
        }
      }
    }
  }

  #' @return Return matrix with applied threshold
  return(thresh_img)
}

