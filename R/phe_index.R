#' phe_index
#'
#' @description
#' This function is provided for compute the combined index for a spefic phenotype
#' @param dat,  data frame for the metagenome data, row is sample and col is viarable
#' @param pos_spe, a vector of features which are positive association with phenotype
#' @param neg_spe, a vector of features which are negative association with phenotype
#'
#' @return data.frame including index
#'
#' @references
#' Gupta VK, Kim M, Bakshi U, et al. A predictive index for health status using species-level gut microbiome profiling. Nat Commun. 2020;11(1):4635.
#' @export
#'
#' @examples
#' data(MH)
#' data(MN)
#' data(sp_data)
#' sp_data_t <- t(sp_data)
#' sp_data_n <- sp_data_t/rowSums(sp_data_t)
#' GMHI <- Phe_index(sp_data_n, MH, MN)

Phe_index <- function(dat, pos_spe, neg_spe){

  pos <- dat[, pos_spe]
  neg <- dat[, neg_spe]

  MH_species <- pos_spe
  MN_species <- neg_spe
  # Extracting Health-prevalent species present in metagenome
  # Extracting Health-scarce species present in metagenome
  MH_species_metagenome <- t(pos)
  MN_species_metagenome <- t(neg)

  # Diversity among Health-prevalent species
  # Diversity among Health-scarce species
  alpha <- function(x){sum((log(x[x>0]))*(x[x>0]))*(-1)}
  MH_shannon <- apply((MH_species_metagenome), 2, alpha)
  MN_shannon <- apply((MN_species_metagenome), 2, alpha)

  fun <- function(x){-log(x) * x}
  a <- apply(MH_species_metagenome, 2, fun)
  b <- apply(MN_species_metagenome, 2, fun)
  a <- apply(a,1,function(x) mean(x,na.rm=T))
  b <- apply(b,1,function(x) mean(x,na.rm=T))
  a <- data.frame(Mean=a)
  b <- data.frame(Mean=b)

  # Richness of Health-prevalent species
  # Richness of Health-scarce species
  R_MH <- apply(MH_species_metagenome, 2, function(i) (sum(i > 0)))
  R_MN <- apply(MN_species_metagenome, 2, function(i) (sum(i > 0)))

  a <- data.frame(RMH=R_MH,RMN=R_MN)
  a <- a[order(a$RMN),]
  a <- a[order(a$RMH,decreasing = T),]

  # Median RMH from 1% of the top-ranked samples (see Methods)
  # Median RMN from 1% of the bottom-ranked samples (see Methods)
  MH_prime <- median(a[1:round(nrow(a)*0.01),"RMH"])
  MN_prime <- median(a[(round(nrow(a)*0.01):nrow(a)),"RMN"])


  # Collective abundance of Health-prevalent species
  # Collective abundance of Health-scarce species
  psi_MH <- ((R_MH/MH_prime)*MH_shannon)
  psi_MN <- ((R_MN/MN_prime)*MN_shannon)

  # final OS-index
  dat.index <- data.frame(
    Index = log10((psi_MH+0.00001)/(psi_MN+0.00001))
  )
  rownames(dat.index) <- rownames(dat)
  dat.index

}
