classifPloidy = function(x){
  x = as.numeric(x)
  x_u <- unique(x)
  x_u_sum <- sum(x_u)
  if(x_u_sum == 2){
    return("diploid")
  } else if(length(unique(x)) == 1){
    return("polyploid")
  }
  return("aneuploid")
}

return_chr_prop_matr <- function(chromosomes, ltr, maxPair){
  all_perms = as.data.frame(gtools::permutations(maxPair, 2, repeats.allowed = T)) %>%
    mutate(chrPaste=paste0(V1, V2))
  
  chromosomes2 <- chromosomes %>%
    filter(class == ltr) %>%
    group_by_(colnames(.)[2], colnames(.)[3]) %>%
    count() %>%
    ungroup %>%
    mutate(prop = n/sum(n), 
           prop.r =  round(prop*100, 1)) %>%
    unite(col = chrPaste, 1:2, sep = "",remove = FALSE) %>%
    left_join(all_perms, ., by="chrPaste") %>%
    mutate(prop.r.cl = ifelse(is.na(prop.r), "·", as.character(prop.r))) %>%
    replace(is.na(.), 0)
  return(chromosomes2)
}


create_perc_matr2 <- function(matr, title, minChr, maxChr, xlab, ylab){
  tot= sum(matr$n)
  gridSize <- maxChr - minChr + 1
  x <- ggplot(matr, aes(x = V1, y = V2, fill = log(prop*100+1, 10))) + 
    geom_tile(color = "black") +  
    theme_classic() +
    theme(axis.text=element_text(size=19, colour = "black"), 
          axis.line = element_blank(), axis.ticks = element_blank()) +
    scale_fill_gradient(low = "white", high = "firebrick3", limits = c(0,2)) + 
    geom_text(size = 4.5, aes(label = prop.r.cl)) + 
    coord_fixed() +
    xlab(xlab) + 
    ylab(ylab) + 
    scale_x_continuous(breaks=seq(minChr, maxChr, 1), labels=as.character(c(paste0("\u2264", minChr),{minChr+1}:{maxChr-1},paste0("\u2265", maxChr)))) + 
    scale_y_continuous(breaks=seq(minChr, maxChr, 1), labels=as.character(c(paste0("\u2264", minChr),{minChr+1}:{maxChr-1},paste0("\u2265", maxChr)))) + 
    ggtitle(paste0("% Aneuploidy Across ", tot, " ", title, "Observations"))
  #ggsave(filename = paste0(outDir, "/aneupl_", title, ".jpeg"), plot=x, device="jpeg", width = 6, height = 6, units = "in")
  return(x)
}


create_perc_matr3 <- function(matr, title, minChr, maxChr, xlab, ylab){
  tot= sum(matr$n)
  gridSize <- maxChr - minChr + 1
  x <- ggplot(matr, aes(x = V1, y = V2, fill = log(prop*100+1, 10))) + 
    geom_tile(color = "black") +  
    theme_classic() +
    theme(axis.text=element_text(size=19, colour = "black"), 
          axis.line = element_blank(), axis.ticks = element_blank()) +
    scale_fill_gradient(low = "white", high = "firebrick3", limits = c(0,2)) + 
    geom_text(size = 4.5, aes(label = prop.r.cl)) + 
    coord_fixed() +
    xlab(xlab) + 
    ylab(ylab) + 
    scale_x_continuous(breaks=seq(minChr, maxChr, 1), labels=as.character(c(paste0("\u2264", minChr),{minChr+1}:{maxChr-1},paste0("\u2265", maxChr)))) + 
    scale_y_continuous(breaks=seq(minChr, maxChr, 1), labels=as.character(c(paste0("\u2264", minChr),{minChr+1}:{maxChr-1},paste0("\u2265", maxChr)))) + 
    facet_grid(src~.) + 
    ggtitle(paste0("% Aneuploidy Across ", tot, " ", title, "Observations"))
  #ggsave(filename = paste0(outDir, "/aneupl_", title, ".jpeg"), plot=x, device="jpeg", width = 6, height = 6, units = "in")
  return(x)
}

