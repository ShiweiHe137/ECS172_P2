# default iDF
iDF <- rbind(c(2,5,1),c(3,5,4),c(2,5,2),c(6,1,5))

# function construct the "virtual_Ratings" class
virtualRatings <- function(x){
  result <- structure(list(inputDF = x), class="virtualRatings")
  return(result)
}

# overload [] method to search the ratings
'[.virtualRatings' = function(x,a,b){
  # generate an empty vector to store the found ratings
  result <- vector()
  for(i in 1:nrow(x$inputDF)){
    if(x$inputDF[i,1] == a){
      if(x$inputDF[i,2] == b){
        # if the user_id and item_id matched with parameters
        # append the rating into the vector result 
        result <- append(result, x$inputDF[i,3])
      }
    }
  }
  
  if(length(result) == 0){  # found no rating
    return(NA)  # return NA
  } else if(length(result) == 1){ # found one rating
    return(result[1]) # return the rating 
  } else if(length(result) > 1){  # found more than one ratings
    warning("muliple instances found")  # print the warning message
    return(result[1]) # return the first rating
  }
}
