classifPloidy = function(x){
  x = as.numeric(x)
  x_u <- unique(x)
  x_u_sum <- sum(x_u)
  if(x_u_sum == 2){
    return("diploid")
  } else if(length(x_u) == 1 & (!all(x == 1))) { #all ones!
    return("polyploid")
  }
  return("aneuploid")
}

return_chr_prop_matr <- function(chromosomes, ltr, maxPair){
  
  #maxPair = maxChrPlus1
  #chromosomes = f1R.t %>% select(1:3, 6)#f1R.t2
  #ltr = classes[1]
  all_perms = as.data.frame(gtools::permutations(maxPair, 2, repeats.allowed = T)) %>%
    mutate(chrPaste=paste0(V1, V2))
  #chromosomes <- aneuDat_test %>% rename(clss=class)
  #ltr <- "test_aneupl_file_2.xlsx"
  chromosomes2 <- chromosomes %>%
    filter(clss == ltr) %>%
    mutate_at(2:3, .funs = ~ifelse(. > maxPair, maxPair, .)) %>%
    group_by_(colnames(.)[2], colnames(.)[3]) %>%
    count() %>%
    ungroup() %>%
    mutate(prop = n/sum(n), 
           prop.r =  round(prop*100, 1)) %>%
    unite(col = chrPaste, 1:2, sep = "",remove = FALSE) %>%
    left_join(all_perms, ., by="chrPaste") %>%
    mutate(prop.r.cl = ifelse(is.na(prop.r), "·", as.character(prop.r))) %>%
    replace(is.na(.), 0)
  return(chromosomes2)
}


return_chr_prop_matr2 <- function(chromosomes, ltr, maxPair){
  
  #maxPair = maxChrPlus1
  #chromosomes = f1R.t %>% select(1:3, 6)#f1R.t2
  #ltr = classes[1]
  temp_col_names <- colnames(chromosomes)
  colnames(chromosomes)[2:3] <- paste0("chr", temp_col_names[2:3])
  
  all_perms = as.data.frame(gtools::permutations(maxPair, 2, repeats.allowed = T)) %>%
    mutate(chrPaste=paste0(V1, V2))
  #chromosomes <- aneuDat_test %>% rename(clss=class)
  #ltr <- "test_aneupl_file_2.xlsx"
  chromosomes2 <- chromosomes %>%
    filter(category == ltr) %>%
    mutate_at(2:3, .funs = ~ifelse(. > maxPair, maxPair, .)) %>%
    group_by_(as.character(colnames(.)[2]), as.character(colnames(.)[3])) %>%
    count() %>%
    ungroup() %>%
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
          axis.line = element_blank(), axis.ticks = element_blank(),
          legend.position="none") +
    scale_fill_gradient(low = "white", high = "firebrick3", limits = c(0,2)) + 
    geom_text(size = 4.5, aes(label = prop.r.cl)) + 
    coord_fixed() +
    xlab(xlab) + 
    ylab(ylab) + 
    scale_x_continuous(breaks=seq(minChr, maxChr, 1), labels=as.character(c(paste0("\u2264", minChr),{minChr+1}:{maxChr-1},paste0("\u2265", maxChr)))) + 
    scale_y_continuous(breaks=seq(minChr, maxChr, 1), labels=as.character(c(paste0("\u2264", minChr),{minChr+1}:{maxChr-1},paste0("\u2265", maxChr)))) + 
    ggtitle(paste0("% aneuploidy across ", tot, " observations\nfile: ", title))
  #ggsave(filename = paste0(outDir, "/aneupl_", title, ".jpeg"), plot=x, device="jpeg", width = 6, height = 6, units = "in")
  return(x)
}


create_perc_matr2.1 <- function(matr, title, minChr, maxChr, xlab, ylab){
  tot <- sum(matr$n)
  gridSize <- maxChr - minChr + 1
  x <- ggplot(matr, aes(x = V1, y = V2, fill = log(prop*100+1, 10))) + 
    geom_tile(color = "black") +  
    theme_classic() +
    theme(axis.text=element_text(size=19, colour = "black", 
                                 face = c("plain", "bold", rep("plain", 7))), 
          axis.line = element_blank(), axis.ticks = element_blank(),
          legend.text=element_text(size=12)) +
          #legend.position="none") +
    scale_fill_gradient(low = "white", high = "firebrick3", limits = c(0,log(100+1, base = 10)),
                        breaks = c(0,log(c(11, 101), base = 10)),#1,log(100+1, base = 10)),
                        labels = c(0, 10, 100),
                        name = "Percentage") + 
    geom_text(size = 4.5, aes(label = prop.r.cl)) + 
    coord_fixed() +
    xlab(xlab) + 
    ylab(ylab) + 
    scale_x_continuous(breaks=seq(minChr, maxChr, 1), labels=as.character(c(paste0("\u2264", minChr),{minChr+1}:{maxChr-1},paste0("\u2265", maxChr)))) + 
    scale_y_continuous(breaks=seq(minChr, maxChr, 1), labels=as.character(c(paste0("\u2264", minChr),{minChr+1}:{maxChr-1},paste0("\u2265", maxChr)))) + 
    ggtitle(paste0("% aneuploidy across ", tot, " observations\nfile: ", title))
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


# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




############
#helper scripts - added 05-04-2018

calc_heterog_score <- function(chr_tbl, retChr = FALSE){ #also returns "n"
  #chr_tbl <- s2
  n_smpl_per_categ <- table(chr_tbl$category) / length(unique(chr_tbl$chr))# %>% data.frame
  n_smpl_per_categ.df <- as_tibble(n_smpl_per_categ) %>% rename(category = "Var1")
  
  if(retChr){
    heterog_tbl <-  chr_tbl %>%
      group_by(category, chr, num_chr, file_type) %>%
      summarize(mft=n()) %>% 
      arrange(category, chr, mft,  num_chr) %>% #category, bins,  mft, cp_nm
      ungroup() %>%
      group_by(category, chr, file_type) %>%
      mutate(f = rev(1:n()-1)) %>%
      mutate(mft_f = mft * f) %>%
      ungroup() %>%
      group_by(category, chr, file_type) %>%
      summarize(heterog_score_bakker_prelim = sum(mft_f)/ (length(unique(chr)))) %>% 
      left_join(n_smpl_per_categ.df, by="category") %>%
      mutate(heterog_score_bakker = heterog_score_bakker_prelim / n) %>%
      select(category, chr, heterog_score_bakker, n, file_type) # %>%
      #mutate(categ_file_type = paste0(category, "_",file_type))
    return(heterog_tbl)
  }
  
  chr_tbl %>%
    group_by(category, chr, num_chr, file_type) %>%
    summarize(mft=n()) %>% 
    arrange(category, chr, mft,  num_chr) %>% #category, bins,  mft, cp_nm
    ungroup() %>%
    group_by(category, chr, file_type) %>%
    mutate(f = rev(1:n()-1)) %>%
    mutate(mft_f = mft * f) %>%
    ungroup() %>%
    group_by(category, file_type) %>%
    summarize(heterog_score_bakker_prelim = sum(mft_f)/ (length(unique(chr)))) %>% 
    left_join(n_smpl_per_categ.df, by="category") %>%
    mutate(heterog_score_bakker = heterog_score_bakker_prelim / n) %>%
    select(category, heterog_score_bakker, n, file_type)  #%>%
   # mutate(categ_file_type = paste0(category, "_",file_type))
}


calc_aneupl_score <- function(chr_tbl, retChr = FALSE, numX=2, numY=1){
  if(retChr){
    aneupl_tbl <- chr_tbl %>%
      mutate(ideal_nchr = 2) %>%
      mutate(ideal_nchr = ifelse(chr == "X", numX, ideal_nchr)) %>% #2018-05-14
      mutate(ideal_nchr = ifelse(chr == "Y", numY, ideal_nchr)) %>% #2018-05-14
      mutate(ideal_obs_diff = abs(ideal_nchr - num_chr)) %>%
      group_by(category, chr, file_type) %>% 
      summarize(sum_ideal_obs_diff = sum(ideal_obs_diff), n_bins_times_n_cells_per_group = n()) %>%
      mutate(aneupl_score_bakker = sum_ideal_obs_diff / n_bins_times_n_cells_per_group) %>%
      select(category, chr, aneupl_score_bakker, file_type)  #%>%
      #mutate(categ_file_type = paste0(category, "_",file_type))
      #mutate(source)
    return(aneupl_tbl)
  }
  chr_tbl %>%
    mutate(ideal_nchr = 2) %>%
    mutate(ideal_nchr = ifelse(chr == "X", numX, ideal_nchr)) %>% #2018-05-14
    mutate(ideal_nchr = ifelse(chr == "Y", numY, ideal_nchr)) %>% #2018-05-14
    mutate(ideal_obs_diff = abs(ideal_nchr - num_chr)) %>%
    group_by(category, file_type) %>% 
    summarize(sum_ideal_obs_diff = sum(ideal_obs_diff), n_bins_times_n_cells_per_group = n()) %>%
    mutate(aneupl_score_bakker = sum_ideal_obs_diff / n_bins_times_n_cells_per_group) %>%
    select(category, aneupl_score_bakker, file_type)  #%>%
   # mutate(categ_file_type = paste0(category, "_",file_type))
}


calc_anca_score_normalized <-  function(chr_tbl, retChr = FALSE, numX=2, numY=1) {
  if(retChr){
    anca_tbl <- chr_tbl %>% 
      mutate(diploid_bin = num_chr == 2) %>%
      mutate(diploid_bin = ifelse(chr == "Y", num_chr == numY, diploid_bin)) %>% #& num_chr == numY, TRUE, diploid_bin)) %>%
      mutate(diploid_bin = ifelse(chr == "X", num_chr == numX, diploid_bin)) %>% #& num_chr == numY, TRUE, diploid_bin)) %>%
      group_by(category, diploid_bin, chr, file_type) %>% 
      summarise (n = n()) %>%
      spread(key = diploid_bin, value=n) %>%
      clean_names() %>%
      mutate_if(is.integer, funs(replace(., is.na(.), 0))) %>% #replace all NA with 0
      mutate(anca_score_normalized = false/ (true + false)) %>% 
      select(category, chr, anca_score_normalized, file_type) #%>%
      #mutate(categ_file_type = paste0(category, "_",file_type))
    return(anca_tbl)
  }
  #2018-05-12
  chr_tbl %>% 
    mutate(diploid_bin = num_chr == 2) %>%
    mutate(diploid_bin = ifelse(chr == "Y", num_chr == numY, diploid_bin)) %>% #& num_chr == numY, TRUE, diploid_bin)) %>%
    mutate(diploid_bin = ifelse(chr == "X", num_chr == numX, diploid_bin)) %>% #& num_chr == numY, TRUE, diploid_bin)) %>%
    group_by(category, diploid_bin, file_type) %>% 
    summarise (n = n()) %>%
    spread(key = diploid_bin, value=n) %>%
    clean_names() %>%
    mutate_if(is.integer, funs(replace(., is.na(.), 0))) %>% #replace all NA with 0
    mutate(anca_score_normalized = false / (true + false)) %>% 
    select(category, anca_score_normalized, file_type) #%>%
    #mutate(categ_file_type = paste0(category, "_",file_type))
}

calc_anca_score <-  function(chr_tbl, numX=2, numY=1) {
  #chr_tbl$file_type <- "sky"
  #chr_tbl %>% select(smpl, category) %>% distinct() %>% group_by(category)  %>% count()
  #unique(chr_tbl$smpl)
  
  n_smpl_per_categ <- table(chr_tbl$category) / length(unique(chr_tbl$chr))# %>% data.frame
  n_smpl_per_categ.df <- as_tibble(n_smpl_per_categ) %>% rename(category = "Var1")
  
  
  #2018-05-12
  chr_tbl %>% 
    mutate(diploid_bin = num_chr == 2) %>%
    #mutate(diploid_bin = ifelse(chr == "Y" & num_chr == 1, TRUE, diploid_bin)) %>%
    mutate(diploid_bin = ifelse(chr == "Y", num_chr == numY, diploid_bin)) %>% 
    mutate(diploid_bin = ifelse(chr == "X", num_chr == numX, diploid_bin)) %>% 
    group_by(category, diploid_bin, file_type) %>% 
    summarise (n = n()) %>%
    spread(key = diploid_bin, value=n) %>%
    clean_names() %>%
    mutate_if(is.integer, funs(replace(., is.na(.), 0))) %>% #replace all NA with 0
    left_join(n_smpl_per_categ.df, by = "category") %>%
    mutate(anca_score = false/n) %>%
    select(category, anca_score, file_type)
}




calc_perc_ploidy <-  function(chr_tbl, numX, numY) {
  cat_file_type <- chr_tbl %>% 
    select(category, file_type) %>% 
    distinct()
  
  chr_tbl %>%
    spread(chr, num_chr) %>% 
    mutate_at(vars(starts_with("Y", ignore.case = TRUE)), .funs=~ifelse(. == numY, 2, 3)) %>% #convert X and Y to appropriate ploidy
    mutate_at(vars(starts_with("X", ignore.case = TRUE)), .funs=~ifelse(. == numX, 2, 3)) %>%
    mutate(ploidy =  apply(.[,4:ncol(.)], 1, classifPloidy)) %>% 
    select(category, ploidy, file_type) %>% 
    mutate(ploidy = factor(ploidy, levels=c("diploid", "polyploid", "aneuploid"))) %>%
    group_by(category, ploidy) %>% 
    summarize(n=n()) %>% 
    mutate(freq=n/sum(n)) %>%
    tidyr::complete(ploidy, fill = list(n = 0, freq=0))%>%
    left_join(cat_file_type, by="category") %>%
    select(-n) %>%
    spread(ploidy, freq)
}

calc_perc_ploidy_old <-  function(chr_tbl) {
  cat_file_type <- chr_tbl %>% 
    select(category, file_type) %>% 
    distinct()
  
  chr_tbl %>%
    spread(chr, num_chr) %>% 
    mutate(ploidy =  apply(.[,4:ncol(.)], 1, classifPloidy)) %>% 
    select(category, ploidy, file_type) %>% 
    mutate(ploidy = factor(ploidy, levels=c("diploid", "polyploid", "aneuploid"))) %>%
    group_by(category, ploidy) %>% 
    summarize(n=n()) %>% 
    mutate(freq=n/sum(n)) %>%
    tidyr::complete(ploidy, fill = list(n = 0, freq=0))%>%
    left_join(cat_file_type, by="category") %>%
    select(-n) %>%
    spread(ploidy, freq)
}



calc_instab_idx <-  function(chr_tbl) {
  
  cat_file_type <- chr_tbl %>% 
    select(category, file_type) %>% 
    distinct()
  
  chr_tbl %>% 
    group_by(category, chr, num_chr) %>%
    summarise (n = n()) %>%
    mutate(freq = n / sum(n)) %>% 
    ungroup %>% 
    replace(is.na(.), 0) %>%  #get the max value from each group!
    group_by(category, chr) %>% 
    filter(freq == max(freq)) %>% 
    select(category, chr, freq) %>% 
    ungroup %>% 
    distinct() %>%
    mutate(one_minus_freq = 1-freq) %>%
    group_by(category) %>%
    summarize(instab_idx = mean(one_minus_freq)) %>%
    left_join(cat_file_type, by="category")
 }



##########
#functions for permutation
pvalFxn <- function(val, nPerm){
  if(val > nPerm/2){
    #pval = 2 * (nPerm - val + 1)/ nPerm
    pval = 2 * (nPerm - val)/ nPerm + 1/nPerm
    return(pval)
  } else if(val < nPerm/2){
    #pval = (2 * (val + 1)) / nPerm
    pval = (2*val) / nPerm + 1/nPerm
    return(pval)
  } else{
    return(1)
  }
}

pvalFxn2 <- function(val, nPerm){
  pval <- (nPerm-val +1)/nPerm
  if(pval > 1){
    return(1)
  } else {
    return(pval)
  }
}


retPermPlotDf <- function(input_df, fxn, nPerms){
  #test <- input_df %>% fxn
  input_df <- input_df %>% arrange(category)
  input_df_wide <- input_df %>% spread(chr, num_chr)
  obs_dist <- shufRetDist(input_df_wide, fxn, perm=FALSE)
  #obs_dist_log <- log(obs_dist+1, 2)
  shuf_dists <- lapply(1:nPerms, function(x) shufRetDist(input_df_wide, fxn))
    
  shuf_dists_aneupl <- shuf_dists %>% 
    lapply(function(x) obs_dist > x) %>%
    reduce(`+`) %>%
    as_tibble()
  shuf_dists_mean <- Reduce("+", shuf_dists) / length(shuf_dists)
  shuf_dists_mean2 <- as.vector(shuf_dists_mean) %>% as_tibble() %>% rename(perm_mean = value)
  obs_dist2 <- as.vector(obs_dist) %>% as_tibble() %>% rename(obs_val = value)
  
  shuf_dists_sd <- lapply(shuf_dists, as.vector) 
  shuf_dists_ci <- do.call(rbind, shuf_dists_sd) %>% 
    apply(2, quantile, c(0.025, 0.975)) %>% 
    t() %>% 
    as_tibble() %>%
    rename_all(.funs = ~paste0("perm_dist_", .))
  
  brk_lbls <- c("<0.001", "<0.01", "<0.05", ">0.05") 
  categs <- as_tibble(t(combn(x = unique(input_df$category), m = 2))) %>% 
    bind_cols(shuf_dists_aneupl) %>%
    mutate(pvalue = sapply(value, pvalFxn2, nPerms),
           pval_cut = cut(pvalue, 
                          breaks = c(0, 0.001, 0.01, 0.05, 1),
                          labels = brk_lbls))
  categs2 <- bind_cols(categs, shuf_dists_mean2, shuf_dists_ci, obs_dist2) %>% 
    mutate(fold_change = obs_val / (perm_mean)) %>%
    mutate(value = nPerms-value)
   return(categs2)
}





shufRetDist <- function(matr_wide, fxn, perm = TRUE){
  if(perm == TRUE){
    matr2 <- matr_wide %>% #matr_wide %>% # matr5 %>% 
      mutate(category = sample(category)) %>%
      gather("chr", "num_chr", 4:ncol(.))
    matr3 <- matr2 %>% fxn
  } else {
    matr3 <- matr_wide %>%
      gather("chr", "num_chr", 4:ncol(.)) %>% 
      fxn
  }
  matr3 %>% ungroup() %>% select(contains("score")) %>% dist()
}



######## shiny modules 2018-05-12 #####

#permPlotTbl

permPlotTblUI <- function(id, header) {
  ns <- NS(id)
  
  tagList(
    hr(),
    h3(header),
    h3("Are groups statistically significantly different from each other
                            in terms of the degree of numerical aneuploidy?"),
    sliderInput(ns("Nperms"), "Number of permutations:",
                min = 0, max = 5000, value = 500, step = 500
    ),
    selectInput(ns("fxn_to_perm"), "Select the score to permute", 
                choices=c("Aneuploidy Score" = "calc_aneupl_score",
                          "Heterogeneity Score" = "calc_heterog_score",
                          "Normalized ANCA Score" = "calc_anca_score_normalized",
                          "ANCA Score" = "calc_anca_score")),
    #"Instability index" = "calc_instab_idx")),
    actionButton(ns("permute_action"), "Permute"),
    p("Please wait for a few minutes for the permutation..."),
    tableOutput(ns("permTbl")),
    plotOutput(ns("permPlot")),
    
    p("How to use this page:"),
    p("Three steps: 1. Select the tab of the data type you would like to permute"),
    p("2. Select the # of desired permutations (default is 500). More perms will take longer."),
    p("3. Select the score to permute, then hit 'permute'. This may take a few minutes depending on 
      the number of permutations."),
    p("Methods: Generate random permutations of the category associated with each observed cell. 
      The difference in scores between all possible pairs of categories is calculated after each permutation. 
      A p-value is calculated by counting how many permuted ANCA scores are more extreme than
      the observed ANCA score."),
    p("The p-values is 1-sided, and tests the null hypothesis that there is no significant difference in scores
      between a given pair of groups. there two possible interpretations of the resulting p-value:
      not significantly different (p > 0.05, grey color) or significantly different (blue color)."),
    h4("Key for the table columns"),
    p("- Group 1 and Group 2 are the groups that are being compared"),
    p("- nperm_gr_thn_obs is the number of permutations greater than the observed normalized ANCA score"),
    p("- pvalue is the pvalue rounded to 2 decimal places,	pval_cut is the categorization of the pvalue into bins (for heatmap purposes)"),
    p("- perm_mean is the mean of the anca scores across all permuted samples"),	
    p("- perm_dist_2.5% and perm_dist_97.5% are the lower and upper 95% CI for the permuted ANCA scores"),
    p("- obs_val is the observed difference in ANCA score between the 2 groups"),
    p("- fold_change is the observed difference in ANCA scores divided by the mean permuted difference in ANCA scores. Analogous to fold enrichment above baseline noise.")
    
    
  )
}


permPlotTbl <- function(input, output, session, input_df, nPerms) { #removed file_input 2018-06-01
  #add file_type for validate
  # Yields the data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  #input_df2 <- reactive({
  #  input_df
  #})
  perms <- eventReactive(input$permute_action, {
    perm_df = retPermPlotDf(input_df = input_df(), 
                            fxn = match.fun(input$fxn_to_perm), nPerms = input$Nperms)
    return(perm_df)
  })
  
  
  output$permTbl <- renderTable({
    perms() %>% #mutate(value = nPerms-value) %>% 
      rename("Group 1" = V1, "Group 2" = V2, "nperm_gr_thn_obs" = value)
  })
  
  output$permPlot <- renderPlot({
    colorBlue <- RColorBrewer::brewer.pal(n = 9, name = "Blues")
    ggplot(perms(), aes(x=V1, y=V2, fill=pval_cut)) + 
      geom_tile() + 
      scale_fill_manual(values = rev(colorBlue[c(1,3,5,7)]), drop=FALSE) +
      geom_tile(color = "white", size = 1) + 
      geom_text(aes(label=round(pvalue, 3))) +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + xlab("") + ylab("") + scale_x_discrete(position = "top") 
  })

  return(perms)
}


########## 2018-06-01
retPermPlotDfMulti2 <- function(input_df, fxn, nPerms, chrInCommon = FALSE){
  #takes in a variable number of data matrices (from diff experimental types)
  #returns values that compares all pairwise comparisons
  #note that matrices must have same treatments
  # weighted by number of cells per group
  #dfs <- list(...)
  #df_master <- do.call(what = rbind, dfs)
  if(chrInCommon){
    chrInCom <- map(input_df, .f = ~pull(.x, chr) %>% as.character() %>% unique())# %>% unique# select()) intersect()
    chrInCom2 <- Reduce(intersect, chrInCom)
    input_df <- map(input_df, .f = ~filter(.x, as.character(chr) %in% chrInCom2)) # %>% 
    print(input_df)
  }
  
  input_df_wide <- lapply(input_df, FUN = function(x) x %>% spread(chr, num_chr))
  #input_df_wide
  print("input_df_wide")
  print(input_df_wide)
  print("input_df_wide_tail")
  print(tail(input_df_wide,50))
  obs_dist <- map(.x = input_df_wide, .f = ~shufRetDist(matr_wide = .x, fxn, perm=FALSE))
  #weight each element by the length of each df
  print("obs_dist") #2018-06-02 - issue here! FISH
  print(obs_dist)
  nrow_df <- map(.x = input_df_wide, .f = nrow)
  print("nrow_df")
  print(nrow_df)
  weighted_mean_dist_obs <- Reduce(`+`,Map(`*`, obs_dist, unlist(nrow_df))) / (sum(unlist(nrow_df)))
  print("weighted_mean_dist_obs")
  print(weighted_mean_dist_obs)
  obs_dist2 <- as.vector(weighted_mean_dist_obs) %>% as_tibble() %>% rename(obs_val = value)
  
  #now generate permuted distances
  weighted_mean_dist_perms_all <- lapply(1:nPerms, function(x){
    perm_dist <- map(.x = input_df_wide, .f = ~shufRetDist(matr_wide = .x, fxn, perm=TRUE))
    weighted_mean_dist_perm <- Reduce(`+`,Map(`*`, perm_dist, unlist(nrow_df))) / (sum(unlist(nrow_df)))
    return(weighted_mean_dist_perm)
  })
  print("weighted_mean_dist_perms_all")
  print(weighted_mean_dist_perms_all)
  shuf_dists_aneupl <- weighted_mean_dist_perms_all %>% 
    lapply(function(x) weighted_mean_dist_obs > x) %>%
    reduce(`+`) %>%
    as_tibble()
  print("shuf_dists_aneupl")
  print(shuf_dists_aneupl)
  shuf_dists_mean <- Reduce("+", weighted_mean_dist_perms_all) / length(weighted_mean_dist_perms_all)
  shuf_dists_mean2 <- as.vector(shuf_dists_mean) %>% as_tibble() %>% rename(perm_mean = value)
  print("shuf_dists_mean2")
  print(shuf_dists_mean2)
  shuf_dists_vect <- lapply(weighted_mean_dist_perms_all, as.vector) 
  shuf_dists_ci <- do.call(rbind, shuf_dists_vect) %>% 
    apply(2, quantile, c(0.025, 0.975)) %>% 
    t() %>% 
    as_tibble() %>%
    rename_all(.funs = ~paste0("perm_dist_", .))
  
  brk_lbls <- c("<0.001", "<0.01", "<0.05", ">0.05") 
  categs <- as_tibble(t(combn(x = unique(input_df[[1]]$category), m = 2))) %>% #always correct order?
    bind_cols(shuf_dists_aneupl) %>%
    mutate(pvalue = sapply(value, pvalFxn2, nPerms),
           pval_cut = cut(pvalue, 
                          breaks = c(0, 0.001, 0.01, 0.05, 1),
                          labels = brk_lbls))
  #print("categs:")
  #print(categs)
  categs2 <- bind_cols(categs, shuf_dists_mean2, shuf_dists_ci, obs_dist2) %>% 
    mutate(fold_change = obs_val / (perm_mean)) %>%
    mutate(value = nPerms-value)
 # print("categs2")
 # print(categs2)
  return(categs2)
}



permPlotTblMultiInputUI <- function(id, header) {
  ns <- NS(id)
  
  tagList(
    hr(),
    h3(header),
    
    sliderInput(ns("Nperms"), "Number of permutations:",
                min = 0, max = 5000, value = 500, step = 500
    ),
    checkboxInput(ns("chrInCommon"), label = "Use Chromosomes in Common", value = TRUE, width = NULL),
    
    selectInput(ns("fxn_to_perm"), "Select the score to permute", 
                choices=c("Aneuploidy Score" = "calc_aneupl_score",
                          "Heterogeneity Score" = "calc_heterog_score",
                          "Normalized ANCA Score" = "calc_anca_score_normalized",
                          "ANCA Score" = "calc_anca_score")),
    #"Instability index" = "calc_instab_idx")),
    actionButton(ns("permute_action"), "Permute"),
    p("Please wait for a few minutes for the permutation..."),
    tableOutput(ns("permTbl")),
    plotOutput(ns("permPlot"))
  )
}


permPlotTblMultiInput <- function(input, output, session, nPerms, sky_df, fish_df, wgs_df) {
  #add file_type for validate
  # Yields the data frame with an additional column "selected_"
  # that indicates whether that observation is brushed

  input_df <- reactive({
    s <- sky_df() #%>% arrange(category), NA) ifelse(!is.null(fish_df()), 
    f <- fish_df() #%>% arrange(category), NA) #fish_df() %>% arrange(category)
    w <- wgs_df() #ifelse(!is.null(wgs_df()), %>% arrange(category), NA) #wgs_df() %>% arrange(category)
    sfw_list <- list(f,s,w) %>% purrr::compact() %>% #remove empty elements
      map(.f = ~arrange(.x, category))
    return(sfw_list)
  })
  
  perms <- eventReactive(input$permute_action, {
    print(input$chrInCommon)
    print(class(input$chrInCommon))
    perm_df <- retPermPlotDfMulti2(input_df = input_df(), 
                                   fxn = match.fun(input$fxn_to_perm), 
                                   nPerms = input$Nperms,
                                   chrInCommon = input$chrInCommon)
    return(perm_df)
  })
  
  output$permTbl <- renderTable({
    perms() %>% #mutate(value = nPerms-value) %>% 
      rename("Group 1" = V1, "Group 2" = V2, "nperm_gr_thn_obs" = value)
  })
  
  output$permPlot <- renderPlot({
    colorBlue <- RColorBrewer::brewer.pal(n = 9, name = "Blues")
    ggplot(perms(), aes(x=V1, y=V2, fill=pval_cut)) + 
      geom_tile() + 
      scale_fill_manual(values = rev(colorBlue[c(1,3,5,7)]), drop=FALSE) +
      geom_tile(color = "white", size = 1) + 
      geom_text(aes(label=round(pvalue, 3))) +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + xlab("") + ylab("") + scale_x_discrete(position = "top") 
  })
  
  #return(perms)
}

css_style_conc_stat <- function(tag){
  #color: DarkBlue;
  paste0("#", tag, "{
         font-size: 18px;
         }")
  }

platformConcordanceUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    hr(),
    checkboxInput(ns("chrInCommonPC"), label = "Use Chromosomes in Common", value = TRUE, width = NULL),
    #p("Concordance between FISH and sc-WGS"),
   h3("Concordance between experimental platforms"),
    #p("The concordance between experimental platforms is listed below." A minimum of 2 or more experimental platforms, 
   #   each with 2 or more treatments, are required to calculate the concordance."), 
   #   #The support of the concordance statistic is [0-1], where 1 indicates maximal concordance."),
   # p("The concordance statistic (CS) is calculated between two experimental platforms. It can be described as the proportion of times that a given statistic 
  #    changes in the same direction between all possible pairwise combinations of treatments between two different experimental platforms. 
   #   All statistics listed as columns in the heatmap below are included in the calculation of the CS."),
    h4("Rationale"),
    p("The direction of change in the degree of aneuploidy between two treatment groups should be the same regardless of the experimental 
    approach used (e.g. FISH, SKY, or sc-WGS). In order to test the consistency of the results between experimental platforms (e.g. FISH vs sc-WGS), 
    we provide a summary of pairwise comparisons between all uploaded experimental platforms. The percentage of statistics (shown as columns in the 
    heatmap below) that are concordant (i.e that change in the same direction), between all pairwise combinations of treatment groups is reported below."),
    h4("Interpretation"),
    p("A value near 100% indicates that the direction of change between treatment groups across the two platforms is mostly the same, 
      which suggests that the two experimental platforms yield similar trends across the treatment groups."),
    h4("Requirements"),
    p("At least two platforms, each with at least two treatment groups, are required for this analysis."),
    h4("Results"),
    textOutput(ns("concStatSummaryFishScwgs")),
    textOutput(ns("concStatSummaryFishSky")),
    textOutput(ns("concStatSummaryScwgsSky")),
    tags$head(tags$style(css_style_conc_stat(ns("concStatSummaryFishScwgs")))),
    tags$head(tags$style(css_style_conc_stat(ns("concStatSummaryFishSky")))),
    tags$head(tags$style(css_style_conc_stat(ns("concStatSummaryScwgsSky")))),
    
    #paste0("#", ns("concStatSummaryFishScwgs"), "{color: red;
    #                             font-size: 20px;
    #       font-style: italic;
    #  }"
    #                     ))
    #
    #tags$head(tags$style(paste0("#", ns("concStatSummaryFishScwgs"), "{color: red;
    #                             font-size: 20px;
    #                             font-style: italic;
    #                             }"
    #                     ))
    #),
    
    plotOutput(ns("concPlot"), height="800px"),
  #plotOutput(ns("chrHeatS2"), height = "800px")
  
    p("The heatmap above displays the difference in the statistic (columns) between two treatments (rows) for a particular experimental platform (listed in parentheses)")
  )
}

platformConcordance2 <- function(input, output, session, sky_df, fish_df, wgs_df, numbX, numbY) {
  
  stsTbl2 <- reactive({ #eventReactive({input$submit_fish | input$submit_sky | input$reset_sky | input$reset_fish |
      #input$reset_wgs | input$submit_wgs |  as.numeric(input$numberOfX) | as.numeric(input$numberOfY) | 
      #input$chrInCommonPC}, {
        
        s <- sky_df() #%>% arrange(category), NA) ifelse(!is.null(fish_df()), 
        f <- fish_df() #%>% arrange(category), NA) #fish_df() %>% arrange(category)
        w <- wgs_df() #ifelse(!is.null(wgs_df()), %>% arrange(category), NA) #wgs_df() %>% arrange(category)
        
        validate(
          need(!is.null(s) | !is.null(f) | !is.null(w), "Please upload at least 1 file!")
        )     
        
        numX = as.numeric(numbX())
        numY = as.numeric(numbY())
        list_to_pass <- list(s, f, w) %>% purrr::compact() #2018-05-05 issue here?
        
        if(input$chrInCommonPC){
          chrInCom <- map(list_to_pass, .f = ~pull(.x, chr) %>% as.character() %>% unique())# %>% unique# select()) intersect()
          chrInCom2 <- Reduce(intersect, chrInCom)
          list_to_pass <- map(list_to_pass, .f = ~filter(.x, as.character(chr) %in% chrInCom2)) # %>% 
          print(list_to_pass)
        }
        
        aneupl_scores = purrr::map_df(.x = list_to_pass, .f = calc_aneupl_score, numX=numX, numY=numY)
        
        heterog_scores = purrr::map_df(.x = list_to_pass, .f = calc_heterog_score)
        anca_scores_normalized = purrr::map_df(.x = list_to_pass, .f = calc_anca_score_normalized, numX=numX, numY=numY)
        anca_scores = purrr::map_df(.x = list_to_pass, .f = calc_anca_score, numX=numX, numY=numY)
        instab_idx = purrr::map_df(.x = list_to_pass, .f = calc_instab_idx)
        perc_ploidy <- purrr::map_df(.x = list_to_pass, .f = calc_perc_ploidy, numX=numX, numY=numY)
        sumStats <- purrr::reduce(list(aneupl_scores, heterog_scores, anca_scores_normalized, anca_scores, instab_idx, perc_ploidy), full_join, by=c("category", "file_type")) %>%
          select(category, file_type, n, everything())
        return(sumStats)
        print(sumStats)
      })

  #can we just do an odds ratio type testo
  
  concDf <- reactive({
    #stsTbl <- read_csv("~/Downloads/2018-06-12-aneuvis-stats-by-group.csv")
    pairwise_combo_cats <- combn(unique(stsTbl2()$category), 2)
    sumry_tbl_compare_list <- list() #@sumry_tbl_compare
    #fish vs single cell wgs
    for(i in 1:ncol(pairwise_combo_cats)){
      #i=3
      stsTbl_split <- stsTbl2() %>% 
        filter(category %in% pairwise_combo_cats[,i]) %>% 
        split(f = .$file_type)
      stsTbl_split_diff <- stsTbl_split %>%
        map(.f = ~.[1,4:ncol(.)] - .[2,4:ncol(.)]) %>%
        plyr::ldply() %>%
        mutate(category = paste0(pairwise_combo_cats[,i], collapse = " - ")) #replace 1 with i
      sumry_tbl_compare_list[[i]] <- stsTbl_split_diff
    }
    sumry_tbl_compare_list.df <- sumry_tbl_compare_list  %>% plyr::ldply()
    sumry_tbl_compare_list.df.g <- sumry_tbl_compare_list.df %>% 
      gather(key = score_name, value = score, 2:(ncol(.)-1)) %>%
      arrange(category, .id, score_name) %>%
      mutate(.id2 = paste0("(", .id, ")")) %>%
      unite(col = category_new, category, .id2, sep = " ", remove = FALSE) %>%
      mutate(score_binary = ifelse(score > 0, 1, ifelse(score < 0 , -1, 0))) #%>%
    #print(sumry_tbl_compare_list.df.g)
    return(sumry_tbl_compare_list.df.g)
  })
  
  output$concStatSummaryFishScwgs <- renderText({
    if(!all(c("fish", "sc-wgs") %in% unique(concDf()$.id))){
      return(NULL)#"Please upload both FISH and SC-WGS data")
    }
    
   # Nperms = 1e2
   # tot_concord_rand_vect <- rep(0, Nperms)
   # for(i in 1:Nperms){
   #   sumry_tbl_compare_list.df.g2.rand <- sumry_tbl_compare_list.df.g %>%
   #     select(-category_new, -.id2, -score) %>%
   #     spread(key=.id, value = score_binary) %>%
   #     mutate(fish = sample(fish), `sc-wgs` = sample(`sc-wgs`)) %>%
   #     mutate(concordance = as.numeric({fish == `sc-wgs`})) 
   #   tot_concord_rand <- sumry_tbl_compare_list.df.g2.rand %>% {sum(.$concordance)/nrow(.)}
   #   tot_concord_rand_vect[i] <- tot_concord_rand
   # }

     
    sumry_tbl_compare_list.df.g2 <- concDf() %>% #sumry_tbl_compare_list.df.g %>%#
      select(-category_new, -.id2, -score) %>%
      spread(key=.id, value = score_binary) %>%
      mutate(concordance = as.numeric({fish == `sc-wgs`}))
    tot_concord <- sumry_tbl_compare_list.df.g2 %>% {sum(.$concordance)/nrow(.)}
    #return(paste0("Concordance between FISH and sc-WGS: ", round(tot_concord,3)))
    #return(paste0(round(tot_concord,3)*100, "% of the statistics between FISH and sc-WGS data shown below change in the same direction between treatment groups."))
    return(paste0(round(tot_concord,3)*100, "% concordance between FISH and sc-WGS data."))
  })
  
  output$concStatSummaryFishSky <- renderText({
    if(!all(c("fish", "sky") %in% unique(concDf()$.id))){
      return(NULL)#"Please upload both FISH and SKY data")
    }
    sumry_tbl_compare_list.df.g2 <- concDf() %>%
      select(-category_new, -.id2, -score) %>%
      spread(key=.id, value = score_binary) %>%
      mutate(concordance = as.numeric({fish == sky}))# collapse = ": "))##, .id=.id) #%>%
    tot_concord <- sumry_tbl_compare_list.df.g2 %>% {sum(.$concordance)/nrow(.)}
    #return(paste0("Concordance between FISH and SKY: ", round(tot_concord,3)))
    #return(paste0(round(tot_concord,3)*100, "% of the statistics between FISH and SKY data shown below change in the same direction between treatment groups."))
    return(paste0(round(tot_concord,3)*100, "% concordance between FISH and SKY data."))
    
    #return(tot_concord)
  })
  #
  output$concStatSummaryScwgsSky <- renderText({
    if(!all(c("sc-wgs", "sky") %in% unique(concDf()$.id))){
      return(NULL)#"Please upload both FISH and SKY data")
    }
    sumry_tbl_compare_list.df.g2 <- concDf() %>%
      select(-category_new, -.id2, -score) %>%
      spread(key=.id, value = score_binary) %>%
      mutate(concordance = as.numeric({sky == `sc-wgs`}))# collapse = ": "))##, .id=.id) #%>%
    tot_concord <- sumry_tbl_compare_list.df.g2 %>% {sum(.$concordance)/nrow(.)}
    #return(paste0(round(tot_concord,3)*100, "% of the statistics between sc-WGS and SKY data shown below change in the same direction between treatment groups."))
    return(paste0(round(tot_concord,3)*100, "% concordance between sc-WGS and SKY data."))
    
    #return(paste0("Concordance between sc-WGS and SKY: ", round(tot_concord,3)))
    
    #return(tot_concord)
  })
  #sum(sumry_tbl_compare_list.df.g2$concordance)/nrow(sumry_tbl_compare_list.df.g2)
  
  output$concPlot <- renderPlot({
    
    ggplot(concDf(), aes(x=score_name, y= category_new, fill=score)) + 
      geom_tile() + 
      scale_fill_gradient2() +
      geom_text(aes(label=round(score, 2))) +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text = element_text(size=16),
                              title = element_text(size=18),
                              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + xlab("") + ylab("") + #+ scale_x_discrete(position = "top") _
      labs(fill='Difference') + 
      ggtitle("Pairwise differences between treatment groups \nacross experimental platforms for 8 summary statistics")
  })
}



platformConcordance <- function(input, output, session, stsTbl) {

  concDf <- reactive({
    #stsTbl <- read_csv("~/Downloads/2018-06-12-aneuvis-stats-by-group.csv")
    pairwise_combo_cats <- combn(unique(stsTbl()$category), 2)
    sumry_tbl_compare_list <- list() #@sumry_tbl_compare
    #fish vs single cell wgs
    for(i in 1:ncol(pairwise_combo_cats)){
      #i=3
      stsTbl_split <- stsTbl() %>% 
        filter(category %in% pairwise_combo_cats[,i]) %>% 
        split(f = .$file_type)
      stsTbl_split_diff <- stsTbl_split %>%
        map(.f = ~.[1,4:ncol(.)] - .[2,4:ncol(.)]) %>%
        plyr::ldply() %>%
        mutate(category = paste0(pairwise_combo_cats[,i], collapse = " - ")) #replace 1 with i
      sumry_tbl_compare_list[[i]] <- stsTbl_split_diff
    }
    sumry_tbl_compare_list.df <- sumry_tbl_compare_list  %>% plyr::ldply()
    sumry_tbl_compare_list.df.g <- sumry_tbl_compare_list.df %>% 
      gather(key = score_name, value = score, 2:(ncol(.)-1)) %>%
      arrange(category, .id, score_name) %>%
      mutate(.id2 = paste0("(", .id, ")")) %>%
      unite(col = category_new, category, .id2, sep = " ", remove = FALSE) %>%
      mutate(score_binary = ifelse(score > 0, 1, ifelse(score < 0 , -1, 0))) #%>%
    #print(sumry_tbl_compare_list.df.g)
    return(sumry_tbl_compare_list.df.g)
  })
  
  output$concStatSummaryFishScwgs <- renderText({
     if(!all(c("fish", "sc-wgs") %in% unique(concDf()$.id))){
      return(NULL)#"Please upload both FISH and SC-WGS data")
    }

    sumry_tbl_compare_list.df.g2 <- concDf() %>%
      select(-category_new, -.id2, -score) %>%
      spread(key=.id, value = score_binary) %>%
      mutate(concordance = as.numeric({fish == `sc-wgs`}))
    tot_concord <- sumry_tbl_compare_list.df.g2 %>% {sum(.$concordance)/nrow(.)}
    return(paste0("Concordance between FISH and sc-WGS: ", round(tot_concord,3)))
  })
  
  output$concStatSummaryFishSky <- renderText({
    if(!all(c("fish", "sky") %in% unique(concDf()$.id))){
      return(NULL)#"Please upload both FISH and SKY data")
    }
    sumry_tbl_compare_list.df.g2 <- concDf() %>%
      select(-category_new, -.id2, -score) %>%
      spread(key=.id, value = score_binary) %>%
      mutate(concordance = as.numeric({fish == sky}))# collapse = ": "))##, .id=.id) #%>%
    tot_concord <- sumry_tbl_compare_list.df.g2 %>% {sum(.$concordance)/nrow(.)}
    return(paste0("Concordance between FISH and SKY: ", round(tot_concord,3)))
    
    #return(tot_concord)
  })
  #
  output$concStatSummaryScwgsSky <- renderText({
    if(!all(c("sc-wgs", "sky") %in% unique(concDf()$.id))){
      return(NULL)#"Please upload both FISH and SKY data")
    }
    sumry_tbl_compare_list.df.g2 <- concDf() %>%
      select(-category_new, -.id2, -score) %>%
      spread(key=.id, value = score_binary) %>%
      mutate(concordance = as.numeric({sky == `sc-wgs`}))# collapse = ": "))##, .id=.id) #%>%
    tot_concord <- sumry_tbl_compare_list.df.g2 %>% {sum(.$concordance)/nrow(.)}
    return(paste0("Concordance between sc-WGS and SKY: ", round(tot_concord,3)))
    
    #return(tot_concord)
  })
     #sum(sumry_tbl_compare_list.df.g2$concordance)/nrow(sumry_tbl_compare_list.df.g2)
    
  output$concPlot <- renderPlot({
      
    ggplot(concDf(), aes(x=score_name, y= category_new, fill=score)) + 
      geom_tile() + 
      scale_fill_gradient2() +
      geom_text(aes(label=round(score, 2))) +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4),
                              axis.text.y = element_text(vjust=0.3, hjust = 1)) +
      coord_fixed(ratio = 1) + xlab("") + ylab("") #+ scale_x_discrete(position = "top") 
  })
}



heatMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    p("This heatmap represents the number of distinct chromosomal states per group. Each column represents a chromosome, and each row represents a distinct chromosomal state per group. The proportion of cells within each group that have the given chromosomal state is shown on the rightmost plot (square black boxes). The darker the square, the greater the proportion of cells within that group that are in that state."),
    #p("Resize the width of your browser window to change the size of the plot"),
    plotOutput(ns("chrHeatS2"), height = "800px")
  )
}


heatMap <- function(input, output, session, input_df, file_type, orig_input){
  s4R <- reactive({
    
     #if (is.null(input_df())) { #2018-05-31
      if (is.null(input_df)) {
         
      return(NULL)
    }
    
    #s2_to_s4 <- input_df() %>% 
    s2_to_s4 <- input_df %>% 
      spread(chr, num_chr) %>%
      group_by(category)  %>%
      unite(colPaste, -category, -smpl, -file_type,remove = FALSE) %>% #added -file_type
      count(colPaste) %>%
      mutate(prop = n / sum(n)) %>%
      separate(colPaste, c(1:22, "X", "Y"), sep = "_") %>%
      ungroup() %>%
      mutate(category = paste(row_number(), category, sep="___")) %>%
      gather(key = chr, value=num_chr, 2:(ncol(.)-1)) %>%
      mutate(chr= factor(chr, levels=c(1:22, "X", "Y","n")))  %>%
      mutate(num_chr = as.numeric(num_chr)) %>%
      separate(category,into = c("row_numb", "categ"), sep = "___", remove = FALSE) %>%
      mutate(row_numb=as.numeric(row_numb)) %>%
      arrange(categ, row_numb) %>%
      mutate(category = factor(category,levels=unique(category))) 
    
    return(s2_to_s4)
  })
  
  
  output$chrHeatS2 <- renderPlot({
    
    validate(
      need(!is.null(orig_input()), paste0("Please upload at least 1 ", file_type, " file!"))
    ) 
    #print(head(s4R()))
    
    #if(is.null(g4R())
    s4.0 <- s4R() %>% 
      mutate(num_chr_filt = ifelse(num_chr > 9, 9, num_chr),
             num_chr_filt = factor(num_chr, levels = 0:9),
             prop2 = cut(prop, breaks = c(seq(0, 0.2, by = 0.05), 0.3, 0.4, 0.5, 1)),
             num_chr_filt2=ifelse(chr == "n", as.character(prop2), as.character(num_chr_filt))) %>%
      mutate(num_chr_filt3 = factor(num_chr_filt2, levels=c(levels(num_chr_filt), levels(prop2)))) #%>%
    
    labels_s4 <- s4R() %>% select(category, categ) %>% distinct()
    
    colors <- c(brewer.pal(n = 9, name = "Blues")[c(5,3)], 
                "white",
                brewer.pal(n = 9, name = "Reds")[3:9], 
                brewer.pal(n = 8, name = "Greys"))
    #2018-05-27
    s4.01 <- ggplot(s4.0, aes(x=chr, y=category, fill=num_chr_filt3)) + 
      geom_tile(color = "white", size = 1) + 
      
      scale_fill_manual(values = colors,drop=FALSE,name = "Copy Number") +
      theme_classic() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size=9),
                              axis.text.y = element_text(hjust = 1)) + #vjust=0.3, 
      xlab("Chromosome") + ylab("")+ 
      scale_y_discrete(breaks=labels_s4$category,
                       labels=labels_s4$categ, position = "right") +
      coord_fixed(ratio = 1) 
    return(s4.01)
  #}, #height = function() {
    #session$clientData$output_scwgs_test-chrHeatS2_width
  })
}


two_to_four <- function(df){
    df %>%
    spread(chr, num_chr) %>%
    group_by(category)  %>%
    unite(colPaste, -category, -smpl, -file_type,remove = FALSE) %>% #added -file_type
    count(colPaste) %>%
    mutate(prop = n / sum(n)) %>%
    separate(colPaste, c(1:22, "X", "Y"), sep = "_") %>%
    ungroup() %>%
    mutate(category = paste(row_number(), category, sep="___")) %>%
    gather(key = chr, value=num_chr, 2:(ncol(.)-1)) %>%
    mutate(chr= factor(chr, levels=c(1:22, "X", "Y","n")))  %>%
    mutate(num_chr = as.numeric(num_chr)) %>%
    separate(category,into = c("row_numb", "categ"), sep = "___", remove = FALSE) %>%
    mutate(row_numb=as.numeric(row_numb)) %>%
    arrange(categ, row_numb) %>%
    mutate(category = factor(category,levels=unique(category)))
}
 

####### new scripts as of 05-31-2018

retFishDf <- function(fish_name, fish_datapath){
  path_list <- as.list(fish_name)#as.list(input$fish_files$name)
  tbl_list <- lapply(fish_datapath, read_excel) #lapply(input$fish_files$datapath, read_excel)
  
  f1 <- map2(.x = path_list, .y= tbl_list,
             .f = ~data.frame(category=.x, .y) %>% clean_names) %>% #) %>%
    #rename_at(vars(names(.)), ~ unlist(regmatches(., gregexpr("Y|X|[[:digit:]]+", .))))) %>%
    do.call(rbind, .) %>% 
    as_tibble() %>% 
    clean_names() %>%
    mutate(smpl = paste0(1:n(), ";",category)) %>%
    mutate(category = as.character(category)) %>%
    gather(key = chr, value=num_chr, 2:(ncol(.)-1)) %>%
    mutate(chr = unlist(regmatches(chr, gregexpr("Y|X|[[:digit:]]+", chr)))) %>%
    mutate(chr = factor(chr, levels=c(1:22, "X", "Y"))) %>%
    mutate(file_type = "fish") %>%
    mutate(category = tools::file_path_sans_ext(category)) %>%
    .[ , order(names(.))] 
  return(f1)
}

retSkyDf <- function(sky_datapath){
  s1 <- read_excel(sky_datapath) %>% 
    clean_names() %>%
    filter(rowSums(is.na(.)) <= .50*ncol(.)) %>% #remove rows where > 50% of values are na
    filter(.[,1] != "Chr. No.") %>%
    set_names(nm=.[1,]) %>%
    .[-1,] %>%
    clean_names()
  
  category_s1 <- s1 %>% filter(.[,1] == "Category") %>% 
    gather(smpl, category, 2:ncol(.)) %>%
    select(-cell)
  
  s2 <- s1 %>% 
    filter(.[,1] != "Category") %>%
    mutate_at(vars(starts_with("x")), 
              .funs = funs(ifelse(str_detect(., ","), 
                                  str_split_fixed(., ",", n=2)[1,1], .))) %>%
    mutate_at(vars(starts_with("x")), .funs = as.numeric) %>%
    gather(key = smpl, value = num_chr, 2:ncol(.)) %>%
    rename(chr = "cell") %>%
    mutate(chr = unlist(regmatches(chr, gregexpr("Y|X|[[:digit:]]+", chr)))) %>%
    left_join(category_s1, by="smpl") %>%
    mutate(file_type = "sky") %>%
    mutate(chr = factor(chr, levels=c(1:22, "X", "Y"))) %>%
    .[ , order(names(.))]
  return(s2)
}

retWgsDf <- function(wgs_datapath, wgs_key_datapath){
  path_list <- as.list(wgs_datapath)
  #fileinput: 'name', 'size', 'type' and 'datapath'.
  tbl_list <- lapply(path_list, read_delim, delim="\t")
  
  g <- map2(.x = path_list, .y= tbl_list,
            .f = ~data.frame(category=.x, .y)) %>% 
    do.call(rbind, .) %>% 
    as_tibble() %>% 
    .[,colSums(!is.na(.)) > 0] %>%
    select(-category)
  print(g)
  gK <- read_excel(path = wgs_key_datapath[1], sheet = 1) #%>%
  
  g2 <- g %>% 
    gather(key = smpl, value=cp_nm, 4:ncol(.)) %>% 
    group_by(CHR, smpl) %>% 
    mutate(bin_size = END - START) %>%
    summarise(num_chr = round(weighted.mean(x = cp_nm, w = bin_size))) %>% 
    separate(CHR, c("chrRm", "chr"), sep=3) %>% 
    dplyr::select(-chrRm) %>% 
    mutate(chr = factor(chr, levels=c(1:22, "X", "Y")))%>% 
    left_join(gK, by=c("smpl" = "smpl_id")) %>%
    mutate(file_type = "sc-wgs") %>%
    .[ , order(names(.))]
  return(g2)
}
