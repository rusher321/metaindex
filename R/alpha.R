shannonI <- function(x){
  total <- apply(x, 1, sum, na.rm = T)
  x <- sweep(x, 1, total, "/")
  x <- -x*log(x, exp(1))
  shannon <- apply(x, 1, sum, na.rm = T)
  return(shannon)
}


richnessI <- function(x){
 richness <- apply(x, 1, function(x){sum(x!=0)})
 return(richness)
}

simpson <- function(x){
  total <- apply(x, 1, sum, na.rm = T)
  x <- sweep(x, 1, total, "/")
  x <- x*x
  H <- apply(x, 1, sum, na.rm = T)
  simp <- 1-H
  return(simp)
}

invsimpson <- function(x){
  total <- apply(x, 1, sum, na.rm = T)
  x <- sweep(x, 1, total, "/")
  x <- x*x
  H <- apply(x, 1, sum, na.rm = T)
  simp <- 1/H
  return(simp)
}

uniquess_bray <- function(x, lev = "min"){
  bray_dist <- as.matrix(vegan::vegdist(x, method = "bray"))
  bray_sort <- apply(bray_dist, 2, sort)
  if(lev == "min"){
    bray_uniq <- bray_sort[2, ]
  }else{
    index <- round(nrwo(x)*lev, 0)
    bray_uniq <- bray_sort[index, ]
  }
  return(bray_uniq)
}

uniquess_jaccard <- function(x, lev = "min"){
  jac_dist <- as.matrix(vegan::vegdist(x, method = "jaccard"))
  jac_sort <- apply(jac_dist, 2, sort)
  if(lev == "min")
    jac_uniq <- jac_sort[2, ]
  else
    index <- round(nrwo(x)*lev, 0)
    jac_uniq <- jac_sort[index, ]
  return(bray_uniq)
}

uniquess_aitchison <- function(x, lev = "min"){
  ### clr transformed
  total <- apply(x, 1, sum, na.rm = T)
  x <- sweep(x, 1, total, "/")
  min_x <- min(x[x!=0])
  x[x==0] <- min_x
  x_clr <- vegan::decostand(x, method = "clr", pseudocount = 0)

  ### distance
  ait_dist <- as.matrix(vegan::vegdist(x_clr, method = "euclidean"))
  ait_sort <- apply(ait_dist, 2, sort)
  if(lev == "min")
    ait_uniq <- ait_sort[2, ]
  else
    index <- round(nrwo(x)*lev, 0)
    ait_uniq <- ait_sort[index, ]
  return(ait_uniq)

}

uniquess_kendall <- function(x, lev = "min", clr = T){

  total <- apply(x, 1, sum, na.rm = T)
  x <- sweep(x, 1, total, "/")
  if(clr){
    min_x <- min(x[x!=0])
    x[x==0] <- min_x
    x_clr <- vegan::decostand(x, method = "clr", pseudocount = 0)
    kendel_uniq <- 1-pcaPP::cor.fk(x_clr)
  }else{
    kendel_uniq <- 1-pcaPP::cor.fk(x)
  }
  kendel_sort <- apply(kendel_uniq, 2, sort)
  if(lev == "min")
    kendel_uniq <- kendel_sort[2, ]
  else
    index <- round(nrwo(x)*lev, 0)
    kendel_uniq <- kendel_sort[index, ]
  return(kendel_uniq)

}

phenindex <- function(x, pos_spe, neg_spe){




}
