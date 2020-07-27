get_iucn_status <- function(species){
  status <- rep(NA, length(species))
  for(x in species){
    i <- which(species == x)
    if(is.null(rl_search(x)$result$category)){
      accepted <- rl_synonyms(x)$result$accepted_name[1]
      if(!is.null(accepted)){
        if(is.null(rl_search(accepted)$result$category)){
          status[i] <- NA
        } else{
          status[i] <- rl_search(accepted)$result$category
        }
      } else{
        accepted <- name_backbone(x)$species
        if(!is.null(accepted) & !is.null(rl_search(accepted)$result$category)){
          status[i] <- rl_search(accepted)$result$category
        } else{
          status[i] <- NA
        }
      }
    } else{
      status[i] <- rl_search(x)$result$category
    }
  }
  return(status)
}
