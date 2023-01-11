#' Integrated ecological diversity indexes
#'
#' @param x  Community data
#' @param index diversity index, one of "richness", "shannon", "simpson",
#'  "invsimpson","uniquess_jaccard", "uniquess_bray", "uniquess_aitchison",
#'  "uniquess_kendel"; "all" indicates caluated all these index
#' @param margin row or col
#' @param ... lev parameter for uniquess function, indicates threshold to compute the uniquess
#'
#' @return data.frame
#' @import vegan
#' @import pcaPP
#' @export
#'
#' @examples
Mdiversity <- function(x, index = "shannon", margin = 1, ...){

  x <- drop(as.matrix(x))
  if (!is.numeric(x))
    stop("input data must be numeric")
  if (any(x < 0, na.rm = TRUE))
    stop("input data must be non-negative")
  if (MARGIN == 2)
      x <- t(x)

  IND <- c("all", "richness", "shannon", "simpson", "invsimpson",
           "uniquess_jaccard", "uniquess_bray", "uniquess_aitchison",
           "uniquess_kendel")

  index <- match.arg(index, IND)
  output <- matrix(NA, nrow = nrow(x), ncol = length(IND))
  rownames(output) <- rownames(x)
  colnames(output) <- IND

  if(index == "all" | index == "richness"){
    indexI <- richnessI(x)
    output[,1] <- indexI
  }
  if(index == "all" | index == "shannon"){
    indexI <- shannonI(x)
    output[,2] <- indexI
  }
  if(index == "all" | index == "simpson"){
    indexI <- simpson(x)
    output[,3] <- indexI
  }
  if(index == "all" | index == "invsimpson"){
    indexI <- invsimpson(x)
    output[,4] <- indexI
  }
  if(index == "all" | index == "uniquess_jaccard"){
    indexI <- uniquess_jaccard(x, ...)
    output[,5] <- indexI
  }
  if(index == "all" | index == "uniquess_bray"){
    indexI <- uniquess_bray(x, ...)
    output[,6] <- indexI
  }
  if(index == "all" | index == "uniquess_aitchison"){
    indexI <- uniquess_aitchison(x, ...)
    output[,7] <- indexI
  }
  if(index == "all" | index == "uniquesskendel"){
    indexI <- uniquess_kendall(x, ...)
    output[,8] <- indexI
  }

  if(index == "all")
    return(output)
  else
    output <- data.frame(indexI)
    rownames(index) <- x
    colnames(index) <- index
    return(output)

}
