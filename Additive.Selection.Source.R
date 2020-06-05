
non_random_mate <- function(population, starting_pop,theta){
  #steps are: choose mates, form gametes, form new individual. For now we'll go constant population
  #size. So just always randomly choose 1000 pairs of parents.
  
  #So this should take a population df and return a new generation's pop level df.
  #So I'll need a function to build that df, then populate it row by row.
  
  #let's choose mates
  
  

  
  cols <- ncol(population)
  
  new_gen_df <- data.frame(matrix(nrow=starting_pop,ncol=cols-1))
  
  just_loci <- population[,-cols]
  
  names(new_gen_df) <- names(just_loci)
  
  max_ind <- nrow(just_loci)
  print(paste("Maximum index for parents is:",max_ind))
  
  n_all <- cols-1
  
  population$p <- apply(population,1,get_mate_probs,theta=theta)
  pop_size <- 1:max_ind
  
  print(range(population$p))
  
  for(offspring in 1:starting_pop){
    #honestly gonna let them self randomly for now cuz I don't feel like coding the check for it.
    
    
    
    parent_inds <- sample(pop_size,2,T,prob=population$p)
    
    
    if(parent_inds[1]>max_ind){
      parent_inds[1] <- max_ind
    }
    if(parent_inds[2]>max_ind){
      parent_inds[2] <- max_ind
    }
    
    
    parent_1 <- just_loci[parent_inds[1],]
    parent_2 <- just_loci[parent_inds[2],]
    
    #we are assuming that all of the loci sort indepedently, so we'll randomly separate pairs
    #of loci into haploid gametes, then combine for a diploid offspring
    gamete_1 <- gamete_formation(parent_1)
    gamete_2 <- gamete_formation(parent_2) 
    
    
    
    
    new_gen_df[offspring,] <- combine_gametes(gamete_1,gamete_2)

    
  }
  
  #not sure why I have the pop$trait thing here? seems...bad? maybe? it's outside the loop so it shouldn't
  #make a difference.
  new_gen_df$trait  <- rowSums(pop[,1:n_all])
  return(new_gen_df)
}



#for non-random mating, where distance from optimum affects mating success



get_mate_probs <- function(population_row,theta){

  
  len <- length(population_row)
  
  trait <- population_row[len]
  
  diff <- (trait-theta)^4
  
  fit <- exp(-0.01*diff)
  

  return(fit)
}



gamete_formation <- function(parent){
  pairs <- ncol(parent)
  run <- seq(1:pairs)
  even_ind <- which(run%%2==0)
  iter <- 1
  gam_ind <- numeric(length(even_ind))
  for(loc in even_ind){
    
    #make sure to unlist these guys
    all_1 <- unlist(parent[loc-1])
    all_2 <- unlist(parent[loc])
    
    gam_ind[iter] <- sample(c(all_1,all_2),1)
    iter <- iter+1
    
  }
  return(gam_ind)
  
}

combine_gametes <- function(gamete1,gamete2){
  loci <- length(gamete1)
  zygote <- numeric(loci)
  odds <- seq(1,loci*2,2)
  evens <- seq(2,loci*2,2)
  
  zygote[odds] <- gamete1
  zygote[evens] <- gamete2
  
  return(zygote)
  
}



####### selection function#######


#similar math to my other stuff, just now focusing on a single optimal value.


selection <- function(population_row,theta,alpha){
  len <- length(population_row)
  
  trait <- population_row[len]
  
  diff <- (trait-theta)^2
  
  fit <- exp(-alpha*diff)
  
  rando <- runif(1,0,1)
  
  if(fit>=rando){
    survival <- 1
  }else{
    survival <- 0
  }
  
  return(survival)
}


#### generation function ####


nr_single_generation <- function(population,theta, starting_pop,alpha){
  new_df <- non_random_mate(population, starting_pop, theta)
  new_df$survival <- apply(new_df,1,selection,theta=theta,alpha)
  surv_df <- new_df[which(new_df$survival==1),]
  cols <- ncol(surv_df)
  surv_df <- surv_df[,-cols]
  return(surv_df)
  
}

nr_multi_gen <- function(population,theta,n_gens,alpha){
  
  starting_pop <- nrow(population)
  
  #need to create a heterozygosity dataframe.
  #get number of columns
  cols <- ncol(population)
  #remove just the trait column
  alleles <- cols-1
  #get number of pairs
  loci <- alleles/2
  
  het_time_frame <- data.frame(matrix(NA,nrow=n_gens,ncol=loci))
  het_names<- c(rep(NA,loci))
  for(name in 1:loci){
    
    het_names[name] <- paste0("locus",name)
  }
  
  names(het_time_frame) <- het_names
  
  for(gen in 1:n_gens){
    
    print(paste("current generation:", gen))
    population <- nr_single_generation(population,theta, starting_pop,alpha)
    het_time_frame[gen,] <- het_all_loci(population)
    
    
  }
  
  return(list(population,het_time_frame))
}

multi_gen <- function(population,theta,n_gens,alpha){
  
  starting_pop <- nrow(population)
  
  #need to create a heterozygosity dataframe.
  #get number of columns
  cols <- ncol(population)
  #remove just the trait column
  alleles <- cols-1
  #get number of pairs
  loci <- alleles/2
  
  het_time_frame <- data.frame(matrix(NA,nrow=n_gens,ncol=loci))
  het_names<- c(rep(NA,loci))
  for(name in 1:loci){
    
    het_names[name] <- paste0("locus",name)
  }
  
  names(het_time_frame) <- het_names
  
  for(gen in 1:n_gens){
    
    print(paste("current generation:", gen))
    population <- single_generation(population,theta, starting_pop,alpha)
    het_time_frame[gen,] <- het_all_loci(population)
    
    
  }
  
  return(list(population,het_time_frame))
}

single_generation <- function(population,theta, starting_pop,alpha){
  new_df <- mate(population, starting_pop)
  new_df$survival <- apply(new_df,1,selection,theta=theta,alpha)
  surv_df <- new_df[which(new_df$survival==1),]
  cols <- ncol(surv_df)
  surv_df <- surv_df[,-cols]
  return(surv_df)
  
}





mate <- function(population, starting_pop){
  #steps are: choose mates, form gametes, form new individual. For now we'll go constant population
  #size. So just always randomly choose 1000 pairs of parents.
  
  #So this should take a population df and return a new generation's pop level df.
  #So I'll need a function to build that df, then populate it row by row.
  
  #let's choose mates
  
  
  
  
  cols <- ncol(population)
  
  new_gen_df <- data.frame(matrix(nrow=starting_pop,ncol=cols-1))
  
  just_loci <- population[,-cols]
  
  names(new_gen_df) <- names(just_loci)
  
  max_ind <- nrow(just_loci)
  print(paste("Maximum index for parents is:",max_ind))
  
  n_all <- cols-1
  
  pop_size <- 1:max_ind
  
  
  for(offspring in 1:starting_pop){
    #honestly gonna let them self randomly for now cuz I don't feel like coding the check for it.
    
    
    
    parent_inds <- sample(pop_size,2,T)
    
    

    
    
    parent_1 <- just_loci[parent_inds[1],]
    parent_2 <- just_loci[parent_inds[2],]
    
    #we are assuming that all of the loci sort indepedently, so we'll randomly separate pairs
    #of loci into haploid gametes, then combine for a diploid offspring
    gamete_1 <- gamete_formation(parent_1)
    gamete_2 <- gamete_formation(parent_2) 
    
    
    
    
    new_gen_df[offspring,] <- combine_gametes(gamete_1,gamete_2)
    
    
  }
  
  #not sure why I have the pop$trait thing here? seems...bad? maybe? it's outside the loop so it shouldn't
  #make a difference.
  new_gen_df$trait  <- rowSums(pop[,1:n_all])
  return(new_gen_df)
}


het_all_loci <- function(df){
  #assuming here that we have diploid traits, that each pair of loci are next to each other
  #in the frame (e.g. 1A, 1B, 2A, 2B, as opposed to 1A, 2A, 1B, 2B), and that the last column
  #is the additive trait value
  
  cols <- ncol(df)
  no_trait <- df[,-cols]
  loci <- ncol(no_trait)
  if(loci%%2!=0){
    stop("looks like we don't have an even number of columns after removing trait column, means our loci aren't correctly formatted")
  }
  pairs <- loci/2
  start <- 2
  het_vec <- numeric(pairs)
  for(locus in 1:pairs){
    het_vec[locus] <- get_het(df[,c(start,start-1)])
    start <- start+2
  }
  return(het_vec)
}



get_het <- function(locus){
  #here a locus should be two columns
  pop_size <- nrow(locus)
  hets <- length(which(locus[,1]!=locus[,2]))
  return(hets/pop_size)
}



generate_trait_loci <- function(n_loci,pop_size,p){
  
  n_alleles <- n_loci*2
  
  pop <- data.frame(matrix(NA,nrow=pop_size, ncol=n_alleles))
  
  for(i in 1:pop_size){
    pop[i,] <- rbinom(n_alleles,1,p)
  }
  allele_names <- c()
  iter <- 1
  for(name in 1:n_loci){
    
    allele_names[iter] <- paste0("locus_",name)
    iter <- iter+1
    allele_names[iter] <- paste0("locus_",name,"B")
    iter <- iter+1
  }
  
  print(allele_names)
  names(pop) <- allele_names
  #
  pop$trait <- rowSums(pop[,1:n_alleles])
  
  return(pop)
}
