trim <- function (file = NULL) {
    
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", file)
    
}