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

permPlotTblUI <- function(id, header) {
  ns <- NS(id)
  
  tagList(
    hr(),
    h3(header),
    
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
     plotOutput(ns("permPlot"))
  )
}

#match.fun("calc_heterog_score")

permPlotTbl <- function(input, output, session, file_input, input_df, nPerms) {
  #add file_type for validate
  # Yields the data frame with an additional column "selected_"
  # that indicates whether that observation is brushed

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



heatMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    p("This heatmap represents the number of distinct chromosomal states per group. Each column represents a chromosome, and each row represents a distinct chromosomal state per group. The proportion of cells within each group that have the given chromosomal state is shown on the rightmost plot (square black boxes). The darker the square, the greater the proportion of cells within that group that are in that state."),
    p("Resize the width of your browser window to change the size of the plot"),
    plotOutput(ns("chrHeatS2"), height = "800px")
  )
}


heatMap <- function(input, output, session, input_df, file_type, orig_input){
  
  s4R <- reactive({
    
     if (is.null(input_df())) {
      return(NULL)
    }
    
    s2_to_s4 <- input_df() %>% 
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
 

#runApp(list(
#  ui = fluidPage(
#    plotOutput("plot1", height="auto")
#  ),
#  server = function(input, output, session) {
#    output$plot1 <- renderPlot(
#      {
#      plot(cars)
#    }, height = function() {
#      session$clientData$output_plot1_width
#    }
#    )
#  }
#))