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
#' Input data must be in matrix where the length of the matrix is 3 (rows, columns, three arrays)
#' each of the three arrays is the R, G and B values
#' Returns RDS data of the input
#'
#' @return Return matrix with applied threshold
#'
#' @author Ibrahim Bello, \email{ibb4u2006@@yahoo.com}
#'
#' @examples
#' ## To process raw image
#' raw_image <- readJPEG('path to image')
#' ## Apply threshold to the raw image data
#' apply_threshold(raw_image)
#'
#' @import jpeg
#' @export

apply_threshold <- function(raw_data)
  {

  if(!is.numeric(raw_data)) stop("Provided input is not numeric")
  if(length(dim(raw_data)) != 3) stop("Make sure your raw data comprises of the row, columns and RGB arrays")
  if (dim(raw_data)[3] != 3) stop("Make sure your raw data is in 3 arrays each for R,G, and B values")

  if (raw_data <= 1 && raw_data >= 0){

  } else {
    ifelse (raw_data <= 255 && raw_data >= 0, raw_data <- raw_data/255, stop("Invalid RGB Values"))
  }

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

blue_distribution <- function(thresh_data) {
  all_pixels <- as.data.frame(which(thresh_data == 0 | thresh_data == 1 | thresh_data == 2 | thresh_data == 3))
  total_pixels <- dim(all_pixels)[1]
  colnames(all_pixels) <- 'all_pixels'
  all_blues <- as.data.frame(which(thresh_data == 1 | thresh_data == 2 | thresh_data == 3))
  colnames(all_blues) <- 'all_blues'
  total_blues <- dim(all_blues)[1]
  blue_distr <- (total_blues / total_pixels) * 100

  #Light Blue
  light_blues <- as.data.frame(which(thresh_data == 1))
  colnames(light_blues) <- 'light_blues'
  light_blues <- dim(light_blues)[1]
  light_blue_distr <- (light_blues / total_pixels) * 100

  #Medium Blue
  medium_blues <- as.data.frame(which(thresh_data == 2))
  colnames(medium_blues) <- 'medium_blues'
  medium_blues <- dim(medium_blues)[1]
  medium_blue_distr <- (medium_blues / total_pixels) * 100

  #Dark Blue
  dark_blues <- as.data.frame(which(thresh_data == 3))
  colnames(dark_blues) <- 'dark_blues'
  dark_blues <- dim(dark_blues)[1]
  dark_blue_distr <- (dark_blues / total_pixels) * 100

  return(paste("Distribution of all blues =", blue_distr, "%,",
               "Distribution of light blues =", light_blue_distr, "%,",
               "Distribution of medium blues =", medium_blue_distr, "%,",
               "Distribution of dark blues =", dark_blue_distr, "%"))
}

identify_struct <- function(thresh_data) {
  dimensions <- dim(thresh_data)
  # To Identify Capilarry Barriers
    # Convert the data from apply_threshold to Data Frame
    main_data <- as.data.frame(thresh_data)
    # Create extra columns for the groups of neighbouring blues in all the columns
    # The columns are automatically named as V1 - V(number of columns)
    # Initiate the group with "Not a capillary barrier group"
    # Denote the initiation with NCG
    # main_data$group_V1 <- 'NCG'

    main_col <- paste0(rep("group_V", ncol(main_data)), 1:ncol(main_data))
    main_data[main_col] <- -999
    #main_data$
    # set j equal to the numeric value of the initialized group
    j <- 0
    # In the first column, Check for all the medium and dark neighbouring blues
    for (i in 1:dimensions[2]) {
      tmp_logic <- main_data[,i] != 0 & main_data[,i] != 1 &(
        main_data[1:(dimensions[1]-1),i] == main_data[2:dimensions[1],i] |
          main_data[1:(dimensions[1]-2),i] == main_data[3:dimensions[1],i] |
          main_data[1:(dimensions[1]-3),i] == main_data[4:dimensions[1],i] |
          main_data[2:dimensions[1],i] == main_data[1:(dimensions[1]-1),i] |
          main_data[3:dimensions[1],i] == main_data[1:(dimensions[1]-2),i] |
          main_data[4:dimensions[1],i] == main_data[1:(dimensions[1]-3),i]
      )
      ifelse(tmp_logic,
             main_data[tmp_logic,i+333] <- j, j <- j+1)
#      main_data[main_col] <- j
    }
}
