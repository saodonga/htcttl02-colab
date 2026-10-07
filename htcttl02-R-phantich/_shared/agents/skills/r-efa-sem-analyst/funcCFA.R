#----------------By Huong Trinh (TMU), 27_8_2021
# For: https://viasm.edu.vn/hdkh/hoi-thao-thong-ke-trong-khoa-hoc-xa-hoi-voi-phan-mem-ma-nguon-mo-r

funCFA <- function(modelCFA, ndigit )
{
  # modelCFA: A CFA mode, after using cfa function in Lavaan
 # ndigit: number of digits 
  #Find auto latents
 Varlatent <- standardizedSolution(modelCFA) %>% 
    filter(op == "=~") %>% 
    select(lhs) %>%
    unique()
 nVl <- length(Varlatent$lhs)
 # Find items 
 VarItems <- list()
 for ( i in Varlatent$lhs)
 {
   VarItems[[i]] <- standardizedSolution(modelCFA) %>%
                      filter(op == "=~" & lhs == i) %>% select(rhs) %>% unique() 
 }
 
 # Prepare table form
 TabMDF <- data.frame(Latent = Varlatent$lhs,
                      CR = rep(NA,  nVl),
                      AVE = rep(NA,  nVl),  MSV = rep(NA,  nVl))
 for ( i in Varlatent$lhs)
 {
   TabMDF[, i] <- NA
 }
 
#-----------------Fill cot CR
 sl <- standardizedSolution(modelCFA)
 sl <- sl$est.std[sl$op == "=~"] # estimated deviation
 names(sl) <- modelCFA@Model@ dimNames[[2]][[1]] # to take item's names of used model
 #Compute residual variance of each item
 re <- 1 - sl^2
 
 #CR for each latent
 
 for ( i in 1: length(Varlatent$lhs))
 {
   nlt <- Varlatent$lhs[i]
   vitems <- VarItems[[i]]$rhs
   TabMDF[TabMDF$Latent ==  nlt, "CR"] <- round(sum(sl[ vitems ])^2/(sum(sl[ vitems])^2 + sum(re[ vitems])), ndigit)
 } 
 
 
 #=======reliability and validity===============================================
 
 #Calculate reliability values of factors
 #AVE
 AVE <- semTools::reliability (modelCFA)
 
 AVE <- round(AVE["avevar", ],  ndigit)

 for ( i in 1: length(Varlatent$lhs))
 {
   nlt <- Varlatent$lhs[i]
   TabMDF[TabMDF$Latent ==  nlt, "AVE"] <-  AVE[ nlt ]
 } 

 #MSV
 
 MSV4a <- semTools::discriminantValidity (modelCFA) # He so tuong quan giua cac bien
 
 for ( i in 1: length(Varlatent$lhs))
 {
   nlt <- Varlatent$lhs[i]
   tempMSV <-  MSV4a %>% filter(lhs == nlt  |rhs== nlt ) %>% 
     select(est) %>% max()
   TabMDF[TabMDF$Latent ==  nlt, "MSV"] <- round(tempMSV^2, ndigit)
 }
 
 #MACOR
 MACOR <-  MSV4a %>%  select(c(lhs, op, rhs ,  est, 'Pr(>Chisq)'))
  pvalue <- ifelse(MACOR$'Pr(>Chisq)' <= 0.001, "***",
                        ifelse(MACOR$'Pr(>Chisq)' > 0.001 &MACOR$ 'Pr(>Chisq)'<0.01, "**", 
                               ifelse(MACOR$'Pr(>Chisq)' > 0.01 & MACOR$'Pr(>Chisq)'<0.05, "*", " ")))
 
 MACOR <- MACOR %>% mutate(estF = paste0(round(MACOR$est, ndigit), pvalue)) %>%
   select(lhs, op, rhs, estF)
 #MACOR
 
 for ( i in 1: length(Varlatent$lhs))
 {
   nlti <- Varlatent$lhs[i]
   for ( j in 1: length(Varlatent$lhs))
   {
     nltj <- Varlatent$lhs[j]
     
    MACORtempts <-  MACOR[ MACOR$lhs ==  nlti &   MACOR$rhs == nltj, "estF"] 
     TabMDF[TabMDF$Latent ==  nltj, nlti] <- 
       ifelse(i == j , round( sqrt(TabMDF[TabMDF$Latent ==  nlti, "AVE"]), ndigit ),  MACORtempts)
   }
 }
 
 return(TabMDF)
}



#===============================DONE======================================


#----------------By Huong Trinh (TMU), 27_8_2021
# For: https://viasm.edu.vn/hdkh/hoi-thao-thong-ke-trong-khoa-hoc-xa-hoi-voi-phan-mem-ma-nguon-mo-r

funCFA_resid <- function(modelCFA, cutoff_resid )
{
   # modelCFA: A CFA mode, after using cfa function in Lavaan
   # cutoff_resid: a positive value 
   #Find auto latents
   Varlatent <- standardizedSolution(modelCFA) %>% 
      filter(op == "=~") %>% 
      select(lhs) %>%
      unique()
   nVl <- length(Varlatent$lhs)
   # Find items 
   VarItems <- list()
   for ( i in Varlatent$lhs)
   {
      VarItems[[i]] <- standardizedSolution(modelCFA) %>%
         filter(op == "=~" & lhs == i) %>% select(rhs) %>% unique() 
   }
   
   resid_CFA <- data.frame(resid(modelCFA , type="normalized")$cov)
   
   #to find items to connect
   Resid_Final <- list()
   for (i in   Varlatent$lhs)
   {
      i_var <-  VarItems[[i]]$rhs
      residtempt <- resid_CFA[i_var, i_var]
      residtempt[ - cutoff_resid < residtempt& residtempt < cutoff_resid] <- NA
      Resid_Final[[i]] <-  residtempt
   }
   return(Resid_Final )
}


#----------------By Huong Trinh (TMU), 29_8_2021
# For: https://viasm.edu.vn/hdkh/hoi-thao-thong-ke-trong-khoa-hoc-xa-hoi-voi-phan-mem-ma-nguon-mo-r

funCFA_mi <- function(modelCFA, cutoff_mi )
{
   # modelCFA: A CFA mode, after using cfa function in Lavaan
   # cutoff_resid: a positive value 
   #Find auto latents
   Varlatent <- standardizedSolution(modelCFA) %>% 
      filter(op == "=~") %>% 
      select(lhs) %>%
      unique()
   nVl <- length(Varlatent$lhs)
   # Find items 
   VarItems <- list()
   for ( i in Varlatent$lhs)
   {
      VarItems[[i]] <- standardizedSolution(modelCFA) %>%
         filter(op == "=~" & lhs == i) %>% select(rhs) %>% unique() 
   }

   #to find items to connect
   Mi_Final <- list()
   for (i in   Varlatent$lhs)
   {
      i_var <-  VarItems[[i]]$rhs
      Mi_Final[[i]] <- modindices(modelCFA) %>% 
         filter(op == "~~" & mi > cutoff_mi & lhs %in% i_var  &rhs %in% i_var ) %>%
         select(lhs, op, rhs)
   }
   return(Mi_Final )
}
