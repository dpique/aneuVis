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


tabPanelFishMaster <- function(x){
  
  tabsetPanel(
    tabPanel("Grid Plots", uiOutput("gridPlots")),
    tabPanel("Grid Plots w grid.arrange", plotOutput("gridPlots2")),
    tabPanel(
      "Summary statistics",
      tableOutput("ft"),
      p(
        "Each column in this table represents
        a different file that was uploaded. The rows represent the following:",
        
        tags$ul(
          tags$li(
            "The first 3 rows represent
            the proportion of diploid (2n), polyploid, and aneuploid cells in
            each sample."
          ),
          tags$li(
            "The 4th row represents entropy, which was calculated
            using the values in the first 3 rows."
          ),
          tags$li(
            "The 5th row (n) represents the total number of cells analyzed within the file."
          ),
          tags$li(
            "The average number of copy alterations per group (anca_score) was calculated as in",
            tags$a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/pubmed/12775914", "Blegen et al 2003")
          ),
          tags$li(
            "The aneuploidy and heterogeneity scores were calculated as in",
            tags$a(
              target = "_blank",
              href = "https://www.ncbi.nlm.nih.gov/pubmed/27246460",
              "Bakker et al 2016 (Suppl.Methods & Table S2)"
            )
          ),
          tags$li(
            "The instability index (instab_idx_bayani) was calculated as in",
            tags$a(
              target = "_blank",
              href = "https://www.ncbi.nlm.nih.gov/pubmed/18813350",
              "Bayani et al 2008"
            ), "and", tags$a(
              target = "_blank",
              href = "https://www.ncbi.nlm.nih.gov/pubmed/9121588",
              "Langauer et al 1997"
            ), ". I believe this is equivalent to the anca_score."
          )
          ),
        "Also, none of these methods weigh the number of chromosomes/degree of aneuploidy, 
        which could present an opportunity for developing a new index."
      )
      ),
    tabPanel("Ternary Plot", plotOutput("ternPlot")),
    tabPanel("Entropy Plot", plotOutput("ploidyPlot")))
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


calc_aneupl_score <- function(chr_tbl, retChr = FALSE){
  if(retChr){
    aneupl_tbl <- chr_tbl %>%
      mutate(ideal_nchr = 2) %>%
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
    mutate(ideal_obs_diff = abs(ideal_nchr - num_chr)) %>%
    group_by(category, file_type) %>% 
    summarize(sum_ideal_obs_diff = sum(ideal_obs_diff), n_bins_times_n_cells_per_group = n()) %>%
    mutate(aneupl_score_bakker = sum_ideal_obs_diff / n_bins_times_n_cells_per_group) %>%
    select(category, aneupl_score_bakker, file_type)  #%>%
   # mutate(categ_file_type = paste0(category, "_",file_type))
}


calc_anca_score <-  function(chr_tbl, retChr = FALSE) {
  if(retChr){
    anca_tbl <- chr_tbl %>% mutate(diploid_bin = num_chr == 2) %>%
      group_by(category, diploid_bin, chr, file_type) %>% 
      summarise (n = n()) %>%
      spread(key = diploid_bin, value=n) %>%
      clean_names() %>%
      mutate_if(is.integer, funs(replace(., is.na(.), 0))) %>% #replace all NA with 0
      mutate(anca_score_blegen = false/ (true + false)) %>% 
      select(category, chr, anca_score_blegen, file_type) #%>%
      #mutate(categ_file_type = paste0(category, "_",file_type))
    return(anca_tbl)
  }
  chr_tbl %>% mutate(diploid_bin = num_chr == 2) %>%
    group_by(category, diploid_bin, file_type) %>% 
    summarise (n = n()) %>%
    spread(key = diploid_bin, value=n) %>%
    clean_names() %>%
    mutate_if(is.integer, funs(replace(., is.na(.), 0))) %>% #replace all NA with 0
    mutate(anca_score_blegen = false / (true + false)) %>% 
    select(category, anca_score_blegen, file_type) #%>%
    #mutate(categ_file_type = paste0(category, "_",file_type))
}