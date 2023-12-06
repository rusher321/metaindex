Network_cor <- function(dat, method = "spearman", clr = True, cutoff = 0.3){

  print("row is sample id, column is microbial features.")
  if(clr){
    dat_z <- zCompositions::cmultRepl(dat);
    dat <- robCompositions::cenLR(dat_z)$x.clr
  }
  dat_cor <- cor(dat, method = method)
  graph_raw <- adj2igraph(Matrix(dat_cor, sparse = TRUE))
  dat_cor[dat_cor > cutoff] <- 1
  dat_cor[dat_cor < -cutoff] <- -1
  graph_all <- adj2igraph(Matrix(dat_cor, sparse = TRUE))
  dat_cor_pos <- dat_cor
  dat_cor_pos[dat_cor_pos == 1] <- 0
  graph_pos <-  adj2igraph(Matrix(dat_cor_pos, sparse = TRUE))
  dat_cor_neg <- dat_cor
  dat_cor_neg[dat_cor_neg == -1] <- 0
  graph_neg <-  adj2igraph(Matrix(dat_cor_neg, sparse = TRUE))

  out <- list(graph_raw, graph_all, graph_pos, graph_neg)
  names(out) <- c("wighted", "all", "pos", "neg")

}

