samp_age <- function(input_age, nreaders, bias_mat, sd_mat){
  reader_id <- sample.int(nreaders,length(input_age), replace = TRUE)#apply a random reader to each age estimate
  output_age <- vector(length = length(input_age))
  
  for (i in 1:length(input_age)) {
    output_age[i] <- round(abs(rnorm(1, mean = bias_mat[input_age[i]+1,reader_id[i]], sd = sd_mat[input_age[i]+1,reader_id[i]])))
  }
  return(output_age)
}

boot_age <- function(n_boot, input_age, bias_mat, sd_mat){
  new_ages <- matrix(nrow = length(input_age), ncol = max(n_boot))
  
  for (i in n_boot) {
    new_ages[,i]<- samp_age(input_age, ncol(bias_mat), bias_mat, sd_mat)
  }
  
  return(new_ages)
}
