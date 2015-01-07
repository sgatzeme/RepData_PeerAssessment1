sessInfo <- function (clear = TRUE, loc = c(), log = TRUE) {    
    
    if (length(loc) > 0) {
        
        Sys.setlocale(loc[1], loc[2])
        
    }
    
    if (clear == FALSE) {
        
        rm(list = ls(.GlobalEnv), envir = .GlobalEnv)
        
    }
    
    if (log == TRUE) {

        session <- sessionInfo()
        
        return(session)
        
    } else {
        
        return(FALSE) 
        
    }

}