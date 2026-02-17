
#' Function for calculating group membership probabilities by chemical compositional distance using Mahalanobis distances and Hotellings T^2 statistic
#'
#' @param elements  transformed element data
#' @param assigned group designation by sample
#'
#' @return data frame
#' @export
#'
#' @examples
#' group.mem.probs(elements,assigned)
group.mem.probs <- function(data,chem,group,eligible,method = "Hotellings", ID) {
  probsAlldf = NULL
  if (identical(method, "Hotellings") && !app_require_packages("ICSNP", feature = "Hotellings T2 group probabilities")) {
    return(NULL)
  }
  tryCatch({
  if("PC1" %in% names(data)){
    chem = data %>% dplyr::select(tidyselect::any_of(tidyselect::contains("PC"))) %>% names()
  }

  probsAll = matrix(nrow = nrow(data), ncol = length(eligible))
  colnames(probsAll) = eligible
  rownames(probsAll) = data[[ID]]
  p.val <- NULL
  for (r in 1:nrow(data)) {
    for(grp in eligible){
      grpindx = which(data[[group]]==grp)
      grpindx = setdiff(grpindx,r)
      if(method == "Hotellings"){
        p.val <- ICSNP::HotellingsT2(data[r,chem],data[grpindx,chem])$p.value %>%
          round(.,5)*100
      } else {
        p.val <- getMahalanobis(data[r,chem],data[grpindx,chem])
      }
      probsAll[r,which(eligible == grp)] <- p.val
    }

  }
  bg = getBestGroup(probsAll,eligible,method = method)
  probsAlldf = probsAll %>%
    tibble::as_tibble() %>%
    dplyr::mutate(ID = data[[ID]], Group = group, GroupVal = data[[group]],BestGroup = bg$nms, BestValue = bg$vals,InGroup = GroupVal == BestGroup,.before = 1)
  }, error = function(e){
    mynotification(glue::glue("unable to return group membership probabilities: {e}"), type = "error")
  })
  return(probsAlldf)
}

#' Calculate eligible groups
#'
#' number of rows must be 2 more than the number of columns per group
#'
#' @param group selected group attribute
#' @param data dataframe from imported data
#' @param chem names of columns with chemical elements
#'
#' @return vector of eligible groups for further analysis
#' @export
#'
#' @examples
#' getEligible(data,chem,group)
getEligible = function(data,chem,group){
  nc = length(chem)
  ng = length(unique(data[[group]]))
  m = max(nc,ng)
  eligible = data %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(group))) %>%
    dplyr::count() %>%
    dplyr::filter(n > (m + 1)) %>%
    dplyr::pull(!!as.name(group)) %>%
    as.character
  return(eligible)
}

#' Find best group based on largest values
#'
#' @param probsAll result from group probability
#' @param eligible groups
#'
#' @return vector of group names
#' @export
#'
#' @examples
#' getBestGroup(probsAll,eligible)
getBestGroup = function(probsAll,eligible, method){
  if(method == "Hotellings")
  bestVal = apply(probsAll,1,which.max) else
    bestVal = apply(probsAll,1,which.min)
  if(method == "Hotellings")
    val = apply(probsAll,1,max) else
      val = apply(probsAll,1,min)
  nms = sapply(bestVal,function(x)eligible[x]) %>% unname()
  return(list(nms = nms, vals = val))
}

#' Get Mahalanobis distance
#'
#' @param row to get distance for
#' @param data dataset to compare against
#'
#' @return distance
#' @export
#'
#' @examples
#' getMahalanobis(data[r,],data[,data[[group]] == grp])
getMahalanobis = function(row, data){
  cov_matrix <- cov(data)
  mean_data <- colMeans(data)
  result <- mahalanobis(row, mean_data, cov_matrix)
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


