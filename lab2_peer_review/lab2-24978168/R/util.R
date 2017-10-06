# Useful functions

# normalize
Normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x))) 
}

# round to nearest given base
roundToNearestX <- function(x, base){
  
  return(base * round(x / base))
  
}


# clean theme
blank_theme <- theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 


plotMap <- function(responses, state_df, my_title){
  # Plot responses on the map of US
  # Args:
  #   responses : data frame, contains log, lat, response to question
  #   state_df: data frame, contains state log and lat data
  #   my_title: string, title of this plot
  # Returns:
  #   plot : ggplot object
  
  plot <- ggplot() + 
    # plot answers
    geom_point(data = responses, aes(x = long, y = lat, color = ans), 
               size = .3, alpha = .5) + 
    # plot map
    geom_polygon(data = state_df, aes(x = long, y = lat, group = group),
                 color = "black", fill = NA) + 
    # make it pretty
    scale_color_discrete(name = "Answer") + 
    labs(subtitle = my_title) + 
    theme_void()
  
  return(plot)
  
}


getDataForQuestion <- function(ling_data, question_num, which_answers){
  # Make a dataframe that contains long, lat, and the responses to the questions given
  # Args:
  #   question_num : string, question number (e.g. "065")
  #   which_answers: vector, contains which answer categories to select 
  # (e.g. c(1,2) means only look at those who answered either category 1 or 2)
  # Returns:
  #   responses: dataframe, containing the data for the given question, plus long and lat
  
  # filter data for a given question number
  responses <- ling_data %>%
    # select relevant columns
    select(id, lat, long, response = paste0("q", question_num)) %>%
    # just look at observations who chose answers specified in which_answers
    filter(response %in% which_answers & !is.na(response)) %>%
    # convert answers to chracters
    mutate(response = as.character(response)) %>%
    #downsample
    sample_frac(1)
  
  
  # answer choices
  choices <- all.ans[[as.numeric(question_num)]] %>%
    # append a column to join responses by
    mutate(response = rownames(.))
  
  # join data
  responses <- responses %>%
    # join
    inner_join(choices, by = "response") %>%
    # update factors for answers since only a subset was chosen
    droplevels()
  
  
  return(responses)  
  
}



convertToBinary <- function(col_name, data){
  # Convert catgorical data to binary
  # Args:
  #   col_name : string, question number (e.g. "q065")
  #   data: dataframe, contains binary data
  # Returns:
  #   binary : dataframe, binary encoding of the categorical data

  binary <- data %>%
    # select id column and question column
    select(id, col_name) %>%
    # append a column of ones
    mutate(present = 1) %>%
    # spread the data to convert the categorical encoding to binary
    spread(col_name, present, fill = 0) %>%
    # remove the id column
    select(-id)
  
  # rename the columns
  colnames(binary) <- sapply(1:ncol(binary), function(x) paste0(col_name, x))
  
  return(binary)
  
}


rotateData <- function(X){
  # Run PCA on the given data, X, and return the projection of the data onto the PC space
  # Args:
  #   X : dataframe, contains the binary encoding of the data, along with long and lat
  # Returns:
  #   Y : dataframe, rotated data

  # run PCA
  pca <- X %>%
    # remove the location columns
    select(-long, -lat) %>%
    # scale data
    scale(center = TRUE, scale = TRUE) %>%
    # compute the covariance
    cov() %>%
    # find the eigenvalues and eigenvectors
    eigen()
  
  
  # project data onto PC space
  Y <- X %>%
    # remove the location columns
    select(-long, -lat) %>%
    # scale the data
    scale(center = TRUE, scale = TRUE) %>%
    # project onto the PC space
    as.matrix() %*% pca$vectors %>%
    # convert from matrix to dataframe
    data.frame() %>%
    # add back the location columns
    mutate(long = X$long, lat = X$lat)
  
  
  return(Y)
  
  
  
}


plotKmeans <- function(kmeans, data, alpha, sample_rate){
  # Plot kmeans clusters on the map of US
  # Args:
  #   kmeans : output of the kmeans function
  #   data: dataframe, contains long and lat
  # Returns:
  #   ggplot object
  
  data <- data %>%
    mutate(cluster = kmeans$cluster) %>%
    sample_frac(sample_rate)
  
  
  plot <- ggplot(data) + 
      # plot individuals color coded by kmeans cluster assignment
      geom_point(aes(x = long, y = lat, color = as.factor(cluster)), alpha = alpha) + 
      # draw map
      geom_polygon(data = state_df, aes(x = long, y = lat, group = group),
                   color = "black", fill = NA) + 
      # label
      labs(color = "Cluster") + 
      theme_void()

  return (plot)
  
  
  
}



