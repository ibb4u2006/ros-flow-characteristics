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

install_load(c("jpeg"))

#' Image Processing of flow characteristics of rain-on-snow experiments
#'
#' Returns RDS data of the 2D plot of image inserted
#'
#' @return Return matrix with applied threshold
#'
#' @author Ibrahim Bello, \email{ibb4u2006@@yahoo.com}
#'
#' @examples
#' insert_images()
#'
#' @import jpeg
#' @export

apply_threshold <- function(raw_data)
  {

  if(!is.numeric(raw_data)) stop("Provided input is not numeric")

  # plot image
  raw_data_green <- t(raw_data[,,2])#  greem channel
  raw_data_blue <- t(raw_data[,,3])#  blue channel
  raw_data_red <- t(raw_data[,,1])#  best visual contrast is 1, red channel

  thresh_data_green <- raw_data_green
  thresh_data_blue <- raw_data_blue
  thresh_data_red <- raw_data_red

  # meaningless, just for dimensions
  thresh_data <- raw_data_red

  # Apply thresholds
  log_thresh_db <- thresh_data_red < 0.15
  log_thresh1 <- thresh_data_red < 0.3 & thresh_data_red >= 0.15 & thresh_data_blue > 0.4
  log_thresh2 <- thresh_data_red < 0.4 & thresh_data_red >= 0.3 & thresh_data_blue > 0.5

  log_thresh3 <- thresh_data_red >= 0.6 & thresh_data_red < 0.65 & thresh_data_blue > 0.85
  log_thresh4 <- thresh_data_red > 0.4 & thresh_data_red < 0.6 & thresh_data_blue > 0.7
  log_thresh5 <- thresh_data_red > 0.4 & thresh_data_red < 0.6 & thresh_data_blue > 0.6 & thresh_data_green > 0.65

  for( i in 1:nrow(log_thresh1)){
    for( j in 1:ncol(log_thresh1)){
      if(log_thresh_db[i,j]){
        thresh_data[i,j] <- 3
      }else{
        if(log_thresh1[i,j]){
          thresh_data[i,j] <- 2
        }else{
          if(log_thresh2[i,j]){
            thresh_data[i,j] <- 2
          }else{
            if(log_thresh3[i,j]){
              thresh_data[i,j] <- 1
            }else{
              if(log_thresh4[i,j]){
                thresh_data[i,j] <- 1
              }else{
                if(log_thresh5[i,j]){
                  thresh_data[i,j] <- 1
                }else{
                  thresh_data[i,j] <- 0
                }
              }
            }
          }
        }
      }
    }
  }
  return(thresh_data)
}
