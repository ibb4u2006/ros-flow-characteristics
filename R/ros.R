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
#' apply_threshold(raw_data = raw_image)
#' ## To find the distribution of blues
#' threshold_data <- apply_threshold(raw_data = raw_image)
#' blue_distribution(thresh_data = threshold_data)
#' ## To plot the fractions of blue (all, dark blue, medium blue or light blue)
#' ## color accross the depth of the raw image
#' threshold_data <- apply_threshold(raw_data = raw_image)
#' By default blue_selections = 'all' for all blue fractions
#' blue_selections = 'dark blue' for dark blue fractions
#' blue_selections = 'medium blue' for medium blue fractions
#' blue_selections = 'light blue' for ligth blue fractions
#' Export the plot to your local disk
#' my_plot <- file.path("path to save the plotted image")
#' depth_blue_fractions(thresh_data = threshold_data, blue_selections = 'all', plot_path = my_plot)
#' #' ## To plot the fractions of blue (all, dark blue, medium blue or light blue)
#' ## color accross the length of the raw image
#' threshold_data <- apply_threshold(raw_data = raw_image)
#' By default blue_selections = 'all' for all blue fractions
#' blue_selections = 'dark blue' for dark blue fractions
#' blue_selections = 'medium blue' for medium blue fractions
#' blue_selections = 'light blue' for ligth blue fractions
#' Export the plot to your local disk
#' my_plot <- file.path("path to save the plotted image")
#' length_blue_fractions(thresh_data = threshold_data, blue_selections = 'all', plot_path = my_plot)
#'
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

  result <- rbind(c("all blue [%]", blue_distr),
                  c("light  blue [%]", light_blue_distr),
                  c("medium blue [%]", medium_blue_distr),
                  c("dark blue [%]", dark_blue_distr))
  print(paste("Distribution of all blues =", blue_distr, "%,",
        "Distribution of light blues =", light_blue_distr, "%,",
        "Distribution of medium blues =", medium_blue_distr, "%,",
        "Distribution of dark blues =", dark_blue_distr, "%"))

  return(result)
}

depth_blue_fractions <- function(thresh_data, blue_selection = 'all', plot_path){
  thresh_data_tmp <- thresh_data
  if(blue_selection == "all"){
    thresh_data_tmp[thresh_data == 1 | thresh_data == 2 | thresh_data == 3] <- 1
    main_title <- "all blues"
  }
  if (blue_selection == "dark blue"){
    thresh_data_tmp[thresh_data == 3] <- 1
    thresh_data_tmp[thresh_data == 1 | thresh_data == 2] <- 0
    main_title <- "dark blues"
  }
  if (blue_selection == "medium blue"){
    thresh_data_tmp[thresh_data == 2] <- 1
    thresh_data_tmp[thresh_data == 1 | thresh_data == 3] <- 0
    main_title <- "medium blues"
  }
  if (blue_selection == "light blue"){
    thresh_data_tmp[thresh_data == 1] <- 1
    thresh_data_tmp[thresh_data == 2 | thresh_data == 3] <- 0
    main_title <- "light blues"
  }
  data_mean <- as.data.frame(rev(1:ncol(thresh_data_tmp)))
  colnames(data_mean) <- "Depth"
  data_mean$Bluefraction <- colMeans(thresh_data_tmp)
  png(plot_path)
  plot(data_mean$Bluefraction,rev(1:ncol(thresh_data_tmp)),
       type = "l", col = "royalblue2",
       xlab = " Blue color fraction",
       ylab = " Depth [Pixel]",
       main = main_title)
  dev.off()
  result <- rbind(c("Blue Fraction Variance", var(data_mean$Bluefraction)))
  return(result)
}

length_blue_fractions <- function(thresh_data, blue_selection = 'all'){
  thresh_data_tmp <- thresh_data
  if(blue_selection == "all"){
    thresh_data_tmp[thresh_data == 1 | thresh_data == 2 | thresh_data == 3] <- 1
    main_title <- "all blues"
  }
  if (blue_selection == "dark blue"){
    thresh_data_tmp[thresh_data == 3] <- 1
    thresh_data_tmp[thresh_data == 1 | thresh_data == 2] <- 0
    main_title <- "dark blues"
  }
  if (blue_selection == "medium blue"){
    thresh_data_tmp[thresh_data == 2] <- 1
    thresh_data_tmp[thresh_data == 1 | thresh_data == 3] <- 0
    main_title <- "medium blues"
  }
  if (blue_selection == "light blue"){
    thresh_data_tmp[thresh_data == 1] <- 1
    thresh_data_tmp[thresh_data == 2 | thresh_data == 3] <- 0
    main_title <- "light blues"
  }
  data_mean <- as.data.frame(rev(1:nrow(thresh_data_tmp)))
  colnames(data_mean) <- "Length"
  data_mean$Bluefraction <- rowMeans(thresh_data_tmp)
  png(plot_path)
  plot(data_mean$Bluefraction,rev(1:nrow(thresh_data_tmp)),
       type = "l", col = "royalblue2",
       xlab = " Blue color fraction",
       ylab = " Length [Pixel]",
       main = main_title)
  dev.off()
  result <- rbind(c("Blue Fraction Variance", var(data_mean$Bluefraction)))
  return(result)
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
