
#' Function for calculating group membership probabilities by chemical compositional distance using Mahalanobis distances and Hotellings T^2 statistic
#'
#' @param elements  transformed element data
#' @param assigned group designation by sample
#'
#' @return
#' @export
#'
#' @examples
group.mem.probs <- function(elements,assigned,method = "Hotellings", ID) {
  # Initialize libraries
  library(ICSNP)
  library(kableExtra)
  library(DataExplorer)
  library(mice)
  # elements = transformed element data
  # assigned = group designation by sample
  grps = assigned %>% unique %>% sort %>% as.character

  if(method == "Hotellings"){
    probs <- list()
    for (g in 1:length(grps)) {
      x <- elements[which(assigned==grps[g]),]
      probs[[g]] <- matrix(0,nrow(x),length(grps))
      colnames(probs[[g]]) <- grps
      rownames(probs[[g]]) <- rownames(x)

      for (gg in 1:length(grps)) {
        p.val <- NULL
        for (i in 1:nrow(x)) {p.val[i] <- HotellingsT2(x[i,],elements[which(assigned==grps[gg]),])$p.value}
        probs[[g]][,which(grps==grps[gg])] <- round(p.val,5)*100
      }
    }
  } else {
    probs <- list()
    for (g in 1:length(grps)) {
      x <- elements[which(assigned==grps[g]),]
      probs[[g]] <- matrix(0,nrow(x),length(grps))
      colnames(probs[[g]]) <- grps
      rownames(probs[[g]]) <- rownames(x)

      for (gg in 1:length(grps)) {
        tmp = elements[which(assigned==grps[gg]),]
        p.val <- NULL
        for (i in 1:nrow(x)) {
          tmp = dplyr::bind_rows(x[i, ],tmp)
          cov_matrix <- cov(tmp)
          mean_data <- colMeans(tmp)
          p.val[i] <- mahalanobis(x[i, ], mean_data, cov_matrix)
        }
        probs[[g]][,gg] <- round(p.val,3)
      }
    }
  }

  probs = getBestGroup(probs = probs)

  probs = purrr::map(1:length(probs),function(i){
    indx = which(assigned == grps[i])
    cbind(tibble::tibble(ID = ID[indx]),probs[[i]])
  })

  names(probs) = grps
  return(probs)
}

#' Calculate eligible groups
#'
#' number of rows must be 2 more than the number of columns per group
#'
#' @param elements dataframe with chemical elements
#' @param attrs dataframe with descriptive attributes
#' @param group selected group attribute
#'
#' @return vector of eligible groups for further analysis
#' @export
#'
#' @examples
getEligible = function(elements,attrs,group){
  nc = ncol(elements)
  eligible = attrs %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(group))) %>%
    dplyr::count() %>%
    dplyr::filter(n > (nc + 1)) %>%
    dplyr::pull(!!as.name(group)) %>%
    as.character
  return(eligible)
}

#' Find best group based on largest values
#'
#' @param probs
#'
#' @return
#' @export
#'
#' @examples
getBestGroup = function(probs){
  result = purrr::map(probs,function(p){
    bestVal = apply(p,1,which.max)
    best = sapply(bestVal,function(i) colnames(p)[i])
    return(cbind(p,best))
  })
  return(result)
}

# read in sample data INAA_test, create attribute and element data.frames, impute missing data and transform
# mydat <- read.csv('inst/INAA_test.csv',header=T,row.names=1)
# attr1 <- mydat[,c(1,3,5,7)] # pull out attributes for plotting
# chem1 <- mydat[,c(8:21,23:40)] # pull out element data (excluing Ni)
# chem1[chem1==0] <- NA # set 0 values to NA
# chem.imp <- tidyr::complete(mice::mice(chem1,method='rf')) # impute missing data using the random forest approach
# chem.t <- log10(chem.imp) # log-base-10 transform raw element data
# grps <- unique(attr1$CORE)

# run script and view output as "kable"
# knitr::kable(
# test2 =group.mem.probs(chem.t,attr1$CORE,grps)
# )
#
#
# ## Parallel processing version below, in progress
#
# library(foreach)
# library(doParallel)
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
#
# probs <- list()
# for (m in 1:length(grps)) {
# probs[[m]] <-     cbind(foreach(i=1:nrow(chem.t[which(attr1$CORE==grps[m]),]),.combine='c',.packages='ICSNP') %dopar% (round((HotellingsT2(chem.t[which(attr1$CORE==grps[m]),][i,],chem.t[which(attr1$CORE==grps[m]),][-i,])$p.value),5)*100),
#                     foreach(j=1:length(grps[-m]),.combine=cbind,.packages='foreach') %:% foreach(i=1:nrow(x),.combine='c',.packages='ICSNP')
#                     %dopar% (HotellingsT2(x[i,],chem.t[which(attr1$CORE==grps[-m][j]),])$p.value) )
# probs[[m]] <- probs[[m]][]
# colnames(probs[[m]]) <- grps
# row.names(probs[[m]]) <- row.names(x)
# }
# proc.time()-ptm


