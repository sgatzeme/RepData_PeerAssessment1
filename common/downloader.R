downloadFile <- function (directory = getwd(), dataset = NULL, url = NULL) {
 
    goTo <- url
    data <- dataset
    
    if (!file.exists(directory)) dir.create(directory)       
    
    if (!file.exists(data)) {
        download.file(goTo, destfile = data)
    }
    
}