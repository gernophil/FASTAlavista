index_consequence_severest <- function(conseqs, vep_cons) {
  conseq_list <- strsplit(conseqs, ",")[[1]]
  for (vep_con in vep_cons) {
    i <- 0
    for (conseq in conseq_list) {
      i <- i + 1
      conseq <- strsplit(conseq, "&")[[1]]
      if (is.element(vep_con, conseq)) {
        index <- i
        consequence <- vep_con
        string_index_consequence <- paste0(index, ";", consequence)
        return(string_index_consequence)
      }
    }
  }
  index <- NA
  consequence <- NA
  string_index_consequence <- paste0(index, ";", consequence)
  return(string_index_consequence)
}

pull_severest <- function(features, index) {
  feature_list <- strsplit(features, ",")[[1]]
  severest_feature <- feature_list[index]
  return(severest_feature)
}
