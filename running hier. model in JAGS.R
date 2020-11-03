


#######################################################################################
##      PROJECT: Sub-national estimation BEL
##      -- by Benjamin-Samuel Schlüter --
##      UCLouvain
#######################################################################################
#
# Running the JAGS model
#
#######################################################################################
#
#
# Notes:
# 1) 
# 2) 
# 3) 
# 4) 

########################################################################################################################################################



rm(list = ls())



# ----- Load package ------------------------------------------------------------------------------------------------------------------------------------------------



# ----- Load data ------------------------------------------------------------------------------------------------------------------------------------------------

# arr units within pro
admin.pro.meta <- readRDS("./data/admin.pro.rda")
# PCS
pcs_m <- readRDS("./data/BeProvince_pcs_m.rda")
pcs_f <- readRDS("./data/BeProvince_pcs_f.rda")

# Counts of death
dth_m <- readRDS("./data/ArrPro_dth_m.rda")
dth_f <- readRDS("./data/ArrPro_dth_f.rda")

# Exposure
exp_m <- readRDS("./data/ArrPro_exp_m.rda")
exp_f <- readRDS("./data/ArrPro_exp_f.rda")


# ----- Required data in JAGS -------------------------------------------------------------------------------------------------------------------------------------

# Set parameters
admin.units <- readxl::read_excel("./data/admin units.xls")
admin.arr <- unique(admin.units$arrondissement)
admin.pro <- admin.pro.meta$province
arrwithinpro <- admin.pro.meta$nber

age_groups <- rownames(dth_m)
years <- colnames(dth_m)

jags.data <- list(y.xtas = dth_m, 
                  pop.xtas = exp_m, 
                  Yx = pcs_m,
                  S = length(admin.pro), X= length(age_groups), T = length(years), 
                  n.a = arrwithinpro, n.amax=max(arrwithinpro), P=3 )

parnames <- c("beta.tas", "mu.beta" ,"sigma.beta", "tau.mu", "u.xtas", "mx.xtas")


mod <- jags(data = jags.data, 
            parameters.to.save= parnames, 
            n.iter = 30000,
            model.file = "./code/HierarchicalModel.txt")






