# Trans Human Mortality Database to RData
# Organization: RUC
# Author: Yu-Teng Yan
# Time: 08/09/2023


###################################################################################################
###################################################################################################
####### 1. Environment ########
library(MortalityEstimate)
# Files from https://www.mortality.org


###################################################################################################
###################################################################################################
####### 2. Trans to List and Dataframe ########
setwd('~/Desktop/hmd_statistics_20230809/lt_both/bltper_5x5')
lf = list.files(pattern = '.txt$')
nf = gsub('\\.bltper.*','',lf)

hmd.t = list()
for (i in 1:length(nf)){
  hmd.t[[i]] = read.table(lf[i], skip=2,header = TRUE)
  hmd.t[[i]]$country = nf[i]
  hmd.t[[i]]$sex = 'total'
  hmd.t[[i]] = hmd.t[[i]][,c(12,11,1:10)]
  colnames(hmd.t[[i]]) = c("sex", "country", 
                         "year", "age", "mx",  "qx",  "ax",  "lx", "dx",  "Lx",  "Tx",  "ex"  )
}


setwd('~/Desktop/hmd_statistics_20230809/lt_male/mltper_5x5')
lf = list.files(pattern = '.txt$')
nf = gsub('\\.mltper.*','',lf)

hmd.m = list()
for (i in 1:length(nf)){
  hmd.m[[i]] = read.table(lf[i], skip=2,header = TRUE)
  hmd.m[[i]]$country = nf[i]
  hmd.m[[i]]$sex = 'male'
  hmd.m[[i]] = hmd.m[[i]][,c(12,11,1:10)]
  colnames(hmd.m[[i]]) = c("sex", "country", 
                           "year", "age", "mx",  "qx",  "ax",  "lx", "dx",  "Lx",  "Tx",  "ex"  )
}


setwd('~/Desktop/hmd_statistics_20230809/lt_female/fltper_5x5')
lf = list.files(pattern = '.txt$')
nf = gsub('\\.fltper.*','',lf)
hmd.f = list()
for (i in 1:length(nf)){
  hmd.f[[i]] = read.table(lf[i], skip=2,header = TRUE)
  hmd.f[[i]]$country = nf[i]
  hmd.f[[i]]$sex = 'female'
  hmd.f[[i]] = hmd.f[[i]][,c(12,11,1:10)]
  colnames(hmd.f[[i]]) = c("sex", "country", 
                           "year", "age", "mx",  "qx",  "ax",  "lx", "dx",  "Lx",  "Tx",  "ex"  )
}

HMD992.t = do.call(rbind, hmd.t)
HMD992.m = do.call(rbind, hmd.m)
HMD992.f = do.call(rbind, hmd.f)
HMD992 = rbind(HMD992.t.,HMD992.m,HMD992.f)

HMD992[, 5:12] <- sapply(HMD992[, 5:12], as.numeric) #71424
HMD991 = na.omit(HMD992) #71352/12/24

# rm(i,lf,nf)
# rm(hmd.t,hmd.m,hmd.f)
# rm(HMD992.t.,HMD992.m,HMD992.f)


###################################################################################################
###################################################################################################
####### 3. Build Log-Quadratic Model ########
setwd('~/Desktop')
load("~/Desktop/HMD991.RData")
HMD991.t = HMD991[HMD991$sex == "total", ]
HMD991.m = HMD991[HMD991$sex == "male", ]
HMD991.f = HMD991[HMD991$sex == "female", ]

x <- c(0,1, seq(5, 110, by = 5))
W_total <- wilmoth(x, LT = HMD991.t)
W_male <- wilmoth(x, LT = HMD991.m)
W_female <- wilmoth(x, LT = HMD991.f)

save.image("~/Desktop/Wilmoth991.RData")
# rm(HMD991,HMD991.t,HMD991.m,HMD991.f)


