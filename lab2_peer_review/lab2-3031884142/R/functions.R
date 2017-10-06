######------ Some useful funtions

Distance <- function(df, metric = VarInfo){
    # Calculate distance matrix with any metric.
    #
    # Args:
    #  df: the data frame you want to calculate internal distance for
    #  metric: the metric you want to use
    # Return:
    #  A |df|x|df| matrix with distances of all pairs in df as entries
    outer(names(df), names(df), Vectorize(function(i, j){
        metric(df[[i]], df[[j]])
    }))
}

Entropy <- function(...){
    # Calculate entropy
    #
    # Args:
    #  ...: a list of variables for which you want to calculate entropy
    # Return:
    #  Entropy as a matrix
    args <- list(...)
    mat <- do.call(cbind, args) %>% as.matrix
    names(mat) <- NULL
    apply(mat, 1, function(x){
        if (all(x == 0)){
            return(0)
        }
        x <- x[x > 0]
        x <- x / sum(x)
        -sum(x * log(x))
    })
}

RelativeEntropy <- function(x){
    # Calculate relative entropym i.e. entropy / log(n)
    x <- x / sum(x)
    -sum(x * log(x)) / log(length(x))
}

VarInfo <- function(c1, c2){
    # Calculate variational information.
    T <- as.numeric(table(c1, c2))
    T1 <- table(c1)
    T2 <- table(c2)
    T <- T[T > 0] / sum(T)
    T1 <- T1[T1 > 0] / sum(T1)
    T2 <- T2[T2 > 0] / sum(T2)
    return(sum(T1 * log(T1)) + sum(T2 * log(T2)) - 2 * sum(T * log(T)))
}

RelVarInfo <- function(c1, c2){
    # Calculate relative Variational Information.
    T <- as.numeric(table(c1, c2))
    T1 <- table(c1)
    T2 <- table(c2)
    T <- T[T > 0] / sum(T)
    T1 <- T1[T1 > 0] / sum(T1)
    T2 <- T2[T2 > 0] / sum(T2)
    return((sum(T1 * log(T1)) + sum(T2 * log(T2)) - 2 * sum(T * log(T))) / (-sum(T * log(T))))
}

StatMode <- function(x){
    # Find mode number.
    freq <- as.factor(x) %>% summary
    which.max(freq) %>% names
}

StatModeNum <- function(x){
    # Find the frequency of mode number.
    freq <- as.factor(x) %>% summary
    max(freq)
}

KMedoids <- function(data, k, ...){
    # K-medoids clustering with the result of k-means algorithm as the initial medoids.
    params <- list(...)
    n <- nrow(data)
    init_model <- kmeans(data, centers = k, iter.max = 100)
    data$cluster <- init_model$cluster
    data$id <- 1:n
    init <- sapply(1: k, function(j){
        temp <- filter(data, cluster == j) %>% select(-(cluster))
        center <- init_model$center[j, ]
        ind <- apply(as.matrix(select(temp, -(id)), drop = FALSE), 1, function(x){
            return(crossprod(center - x))
        }) %>% which.min
        return(temp$id[ind])
    })
    D <- select(data, -(id), -(cluster)) %>% dist
    do.call(pam, c(list(x = D, k = k, medoids = init), params))
}

Zip2Location <- function(zip, root = "https://usa.youbianku.com/zipcode/"){
    # Web crawler to find latitude and longitude of zip-code.
    #
    # Args:
    #  zip: the zip code for which you want to find latitude and longitude
    #  root: source page
    # Return:
    #  the longitude, then the latitude
    #
    # This function may not work if the root changes its layout
    url <- paste0(root, zip)
    webpage <- getURL(url)
    webpage <- readLines(tc <- textConnection(webpage))
    close(tc)
    
    webpage <- unlist(webpage)
    long <- (grep("Longitude", webpage)[1]+1) %>% webpage[.] %>% as.numeric()
    lat <- (grep("Latitude", webpage)[1]+1) %>% webpage[.] %>% as.numeric()
    
    return (c(long, lat))
}

SampleData <- function(df, size, ...) {
    # A function that sample a data with a given size
    # 
    # Args:
    #  df: the data
    #  size: number of lines to choose
    #  ...: other args in sample()
    # Returns:
    #  A data frame of given size.
    
    df <- df[sample(nrow(df), size, ...),]
    return (df)
}

trunc <- function(x, floor, ceiling) {
    # A function that truncates data.
    #
    # Args:
    #  x: The value
    #  floor: The lower threshold.
    #  ceiling: The upper threshold.
    # Returns:
    #  If x < floor, then return floor; if x > ceiling, then return ceiling;
    #  Otherwise return x.
    
    x[x > ceiling] <- ceiling
    x[x < floor] <- floor
    return (x)
}

List2DataFrame <- function(list){
    # Transform list to dataframe.
    as.data.frame(rbindlist(list))
}

FirstIndex <- function(x, vec) {
    # A function that finds the rank of an element in a vector.
    # of the trunk.
    #
    # Args:
    #  x: the element
    #  vec: the vector
    # Returns:
    #  The rank of x in vec. Or 0 if not exists
    #
    # Fast but unstable:
    # temp <- sapply(x, function(x0){which(vec == x0)}) %>% 
    #     as.integer() %>% min()
    # if_else(is.na(temp), 0, temp)
    
    for(i in 1:length(vec)) {
        if(x == vec[i]) return(i)
    }
    return (0)
}

Not <- function(...) {
    # A functional operator as '!'
    return(!...)
}

ExistRData <- function(data_name, name_string = as.character(substitute(data_name))) {
    # A function that checks whether or not some R data exists under /data
    #
    # Arg:
    #  data_name: name of data
    #  name_string: name of data after loading
    # Returns:
    #  TRUE when the RData exists
    #  False when it does not exist.
    location <- paste0("data/", name_string, ".RData")
    return (file.exists(location))
}

LoadRData <- function(data_name, name_string = as.character(substitute(data_name))) {
    # A function that loads some R data under /data
    #
    # Arg:
    #  data_name: name of data
    #  name_string: name of data after loading
    # Returns:
    #  the data with name as name_string
    location <- paste0("data/", name_string, ".RData")
    data_name <- local(get(load(location)))
    return (data_name)
}


SaveRData <- function(data_name, name_string = as.character(substitute(data_name)))  {
    # A function that saves some R data under /data
    #
    # Arg:
    #   data_name: name of data
    #   name_string: name to save the data, default to be the same as name of data
    # Returns:
    #   the relative path of the saved .RData
    location <- paste0("data/", name_string, ".RData")
    save(file = location, data_name)
    return(location)
}

SavePNG <- function(picture_name, name_string = as.character(substitute(picture_name))) {
    # A function that saves a ggplot to a .png under figure/
    #
    # Arg:
    #   picture_name: a ggplot figure
    #   name_string: name to save the figure, default to be the same as name of the figure
    # Returns:
    #   the relative path of the saved figure
    location <- paste0("figure/", name_string, ".png")
    ggsave(filename = location, picture_name, width = 6, height = 7)
    return(location)
}
