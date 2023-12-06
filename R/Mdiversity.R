#' Integrated ecological diversity indexes
#'
#' @param x  Community data
#' @param index diversity index, one of "richness", "shannon", "simpson",
#'  "invsimpson","uniquess_jaccard", "uniquess_bray", "uniquess_aitchison",
#'  "uniquess_kendel"; "all" indicates calculated all these index
#' @param margin row is sample id (1) or col is sample id (2)
#' @param clr T/F, wheather use the clr transformation in uniquess kendell
#' @param lev lev parameter for uniquess function, indicates threshold to compute the uniquess
#'
#' @return
#' The function returns an data.frame with the integrated ecological dieversity index
#'
#' @author
#' Huahui Ren
#'
#' @import vegan
#' @import pcaPP
#' @export
#'
#' @examples
#' data(sp_data)
#' n100 <- sp_data[,1:100]
#' n100_t <- n100/colSums(n100)
#' Diversity <- Mdiversity(n100_t, index = "all", margin =2 )
Mdiversity <- function(x, index = "shannon", margin = 1, clr = T, lev = "min"){

  x <- drop(as.matrix(x))
  if (!is.numeric(x))
    stop("input data must be numeric")
  if (any(x < 0, na.rm = TRUE))
    stop("input data must be non-negative")
  if (margin == 2)
      x <- t(x)

  IND <- c("all", "richness", "shannon", "simpson", "invsimpson",
           "uniquess_jaccard", "uniquess_bray", "uniquess_aitchison",
           "uniquess_kendel")

  index <- match.arg(index, IND)
  if(index == "all"){
    output <- matrix(NA, nrow = nrow(x), ncol = length(IND)-1)
    rownames(output) <- rownames(x)
    colnames(output) <- IND[-1]
  }

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
    indexI <- uniquess_jaccard(x, lev = lev)
    output[,5] <- indexI
  }
  if(index == "all" | index == "uniquess_bray"){
    indexI <- uniquess_bray(x, lev = lev)
    output[,6] <- indexI
  }
  if(index == "all" | index == "uniquess_aitchison"){
    indexI <- uniquess_aitchison(x, lev = lev)
    output[,7] <- indexI
  }
  if(index == "all" | index == "uniquesskendel"){
    indexI <- uniquess_kendall(x, lev = lev, clr = clr)
    output[,8] <- indexI
  }

  if(index != "all"){
    output <- output[, index, drop=F]
  }
  return(output)

}
