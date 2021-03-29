#check if two strings match using string distance
is_match <- function(x,y){
  jw <- stringdist(x, y, method = "jw", p = 0.1)
  if(is.na(jw) == TRUE){
    match <- NA
  } else {
    if(jw <= 0.15){
      match <- TRUE
    } else {
      if(jw > 0.15){
        match <- FALSE
      }
    }
  }
  return(match)
}