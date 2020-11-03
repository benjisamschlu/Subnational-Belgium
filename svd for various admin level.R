


#######################################################################################
##      PROJECT: Sub-national estimation BEL
##      -- by Benjamin-Samuel Schlüter --
##      UCLouvain
#######################################################################################
#
# Perform SVD decomposition on mortality rates matrix for the selected admin level
#
#######################################################################################
#
#
# Notes:
# 1) Decided to perform SVD on province level (11 units)
# 2) Add 1991 and 1995 (drop interpolation step for 1995)
# 3) For the moment age lim = 95+ 
# 4) Drop is.infinite in log(mx): remove quite a lot of lines (more in female)

########################################################################################################################################################



rm(list=ls())


# ----- Load pkg -------------------------------------------------------------------------------------------------------------- 

packages <- c("tidyverse")
invisible( lapply(packages, library, character.only = TRUE))


# ----- Load data ---------------------------------------------------------------------------------------------------------------------

niscodes <- readxl::read_excel("./data/code commune.xls")
# different possible administrative units (province, arrondissement, commune)
admin <- "arrondissement"
admin <- "province"
admin <- "region"

if (admin == "arrondissement") {
        niscodes.admin <- niscodes[grep("Arrondissement", niscodes$`Entités administratives`), ] # first two digits defines arrondissement
} else if (admin == "province") {
        niscodes.admin <- niscodes[grep("Province|Région de Bruxelles-Capitale", niscodes$`Entités administratives`), ]
} else if (admin == "region") {
        niscodes.admin <- niscodes[grep("Région", niscodes$`Entités administratives`), ]
}


#----- Create array for pop (age*year*commune) -------------------------------------------------------------------------------------------------

# Set names for array dimensions 
age.names <- c(0, 1, seq(5, 100, 5)) # age grp defined in input data sets
year.names <- c(1992:2015)# (could be set with # folders)


admin.names <- niscodes.admin$`Entités administratives`

# Dth containers (male & female) 
pop_m <- pop_f <- array(0, # set 0 as default because if arrondissement not present in input data: n=0
                        dim = c(length(year.names), length(age.names), length(admin.names)), 
                        dimnames = list(year.names, age.names, admin.names))


# ----- Fill in pop array --------------------------------------------------------------------------------------------------------------------

# List files in order to extract data to fill "pop"
data.files <- list.files("./data")
for (i in seq_along(year.names)) {
        
        id <- grepl(year.names[i], data.files) & grepl("pop", data.files) # obtain dth data of a given year
        pop <- get(load(paste("./data/", data.files[id], sep = ""))) # load file corresponding to year
        
        for (j in seq_along(age.names)) {
                
                age.gp <- as.character(age.names[j]) # "age gp" to subset list
                pop.it <- as.data.frame(pop[[age.gp]]) 
                names(pop.it) <- c("code", "sex", "n")
                # aggregate at the arrondissement level
                if (admin == "province") {
                        pop.it <- pop.it %>% 
                                mutate(admin = ifelse(substr(code, 1, 2) == "21", "04000",
                                                    ifelse(substr(code, 1, 1) == "1", "10000",
                                                           ifelse(substr(code, 1, 2) %in% c("23", "24"), "20001",
                                                                  ifelse(substr(code, 1, 1) == "3", "30000",
                                                                         ifelse(substr(code, 1, 1) == "4", "40000",
                                                                                ifelse(substr(code, 1, 1) == "7", "70000",
                                                                                       ifelse(substr(code, 1, 2) == "25", "20002",
                                                                                              ifelse(substr(code, 1, 1) == "5", "50000",
                                                                                                     ifelse(substr(code, 1, 1) == "6", "60000",
                                                                                                            ifelse(substr(code, 1, 1) == "8", "80000",
                                                                                                                   ifelse(substr(code, 1, 1) == "9", "90000", NA
                                                                                                                   
                                                                  )))))))))))) %>%
                                group_by(admin, sex) %>% 
                                summarise(n = sum(n))
                        
                }
                else if (admin == "arrondissement") {
                        pop.it <- pop.it %>% 
                                mutate(admin = substr(code, 1, 2)) %>%
                                group_by(admin, sex) %>% 
                                summarise(n = sum(n))
                }
                else if (admin == "region") {
                        pop.it <- pop.it %>% 
                                mutate(admin = ifelse(substr(code, 1, 2) == "21", "04000",
                                                      ifelse(substr(code, 1, 1) %in% c("1", "3", "4", "7") | substr(code, 1, 2) %in% c("23", "24"), "02000",
                                                             ifelse(substr(code, 1, 1) %in% c("5", "6", "8", "9") | substr(code, 1, 2) %in% c("25"), "03000", NA)))) %>%
                                group_by(admin, sex) %>% 
                                summarise(n = sum(n))  
                }
               
                
                
                for (k in 1:dim(pop.it)[1] ) { 
                        
                        if (admin %in% c("province", "region")) {
                                admin.name <-  niscodes.admin$`Entités administratives`[niscodes.admin$`Code INS` == as.character(pop.it$admin[k])] # Obtain arrondissement name at each it
                        } else if (admin == "arrondissement") {
                                admin.name <-  niscodes.admin$`Entités administratives`[substr(niscodes.admin$`Code INS`, 1, 2) == as.character(pop.it$admin[k])] # Obtain arrondissement name at each it
                                
                        }
                        
                        admin.name <- as.character(admin.name) # used as 3rd dim in array
                        if (pop.it$sex[k] == 1) { # male
                                pop_m[i, j, admin.name] <- pop.it$n[k]
                                
                        }
                        else if (pop.it$sex[k] == 2) { # female
                                pop_f[i, j, admin.name] <- pop.it$n[k]
                        }
                }
        }
}


# ----- Transform pop into exposure --------------------------------------------------------------------------------------------------------------------

## Exp containers (male & female) 
y.lim <- dim(pop_m)[1]-1 # last year not accounted (exp(t)=(pop(t)+pop(t+1))/2)
exp_m <- pop_m[1:y.lim, , ]
exp_f <- pop_f[1:y.lim, , ]

for (k in seq_along(admin.names)) { # loop on 3rd dim of array
        
        admin.name <- as.character(admin.names[k])
        
        for( j in 1:y.lim ) { # loop on all years except last (see above)
                
                exp_m[j, , admin.name] <- (pop_m[j, ,admin.name] + pop_m[(j+1), , admin.name])/2
                exp_f[j, , admin.name] <- (pop_f[j, ,admin.name] + pop_f[(j+1), , admin.name])/2
        }
}


#----- Create array for dth (age*year*commune) -------------------------------------------------------------------------------------------------

year.names <- c(1992:1994, 1996:2015) 
# Dth containers (male & female) 
dth_m <- dth_f <- array(0, # set 0 as default because if arrondissement not present in input data: n=0
                        dim = c(length(year.names), length(age.names), length(admin.names)), 
                        dimnames = list(year.names, age.names, admin.names))


# ----- Fill in dth array --------------------------------------------------------------------------------------------------------------------

# List files in order to extract data to fill "dth"
data.files <- list.files("./data")
for (i in seq_along(year.names)) {
        
        id <- grepl(year.names[i], data.files) & grepl("dth", data.files) # obtain dth data of a given year
        dth <- get(load(paste("./data/", data.files[id], sep = ""))) # load file corresponding to year
        
        for (j in seq_along(age.names)) {
                
                age.gp <- as.character(age.names[j]) # "age gp" to subset list
                dth.it <- as.data.frame(dth[[age.gp]]) 
                names(dth.it) <- c("code", "sex", "n")
                # aggregate at the arrondissement level
                if (admin == "province") {
                        dth.it <- dth.it %>% 
                                mutate(admin = ifelse(substr(code, 1, 2) == "21", "04000",
                                                      ifelse(substr(code, 1, 1) == "1", "10000",
                                                             ifelse(substr(code, 1, 2) %in% c("23", "24"), "20001",
                                                                    ifelse(substr(code, 1, 1) == "3", "30000",
                                                                           ifelse(substr(code, 1, 1) == "4", "40000",
                                                                                  ifelse(substr(code, 1, 1) == "7", "70000",
                                                                                         ifelse(substr(code, 1, 2) == "25", "20002",
                                                                                                ifelse(substr(code, 1, 1) == "5", "50000",
                                                                                                       ifelse(substr(code, 1, 1) == "6", "60000",
                                                                                                              ifelse(substr(code, 1, 1) == "8", "80000",
                                                                                                                     ifelse(substr(code, 1, 1) == "9", "90000", NA
                                                                                                                            
                                                                                                                     )))))))))))) %>%
                                group_by(admin, sex) %>% 
                                summarise(n = sum(n))
                        
                }
                else if (admin == "arrondissement") {
                        dth.it <- dth.it %>% 
                                mutate(admin = substr(code, 1, 2)) %>%
                                group_by(admin, sex) %>% 
                                summarise(n = sum(n))
                }
                else if (admin == "region") {
                        dth.it <- dth.it %>% 
                                mutate(admin = ifelse(substr(code, 1, 2) == "21", "04000",
                                                      ifelse(substr(code, 1, 1) %in% c("1", "3", "4", "7") | substr(code, 1, 2) %in% c("23", "24"), "02000",
                                                             ifelse(substr(code, 1, 1) %in% c("5", "6", "8", "9") | substr(code, 1, 2) %in% c("25"), "03000", NA)))) %>%
                                group_by(admin, sex) %>% 
                                summarise(n = sum(n))  
                }
                
                
                
                for (k in 1:dim(dth.it)[1] ) { 
                        
                        if (admin %in% c("province", "region")) {
                                admin.name <-  niscodes.admin$`Entités administratives`[niscodes.admin$`Code INS` == as.character(dth.it$admin[k])] # Obtain arrondissement name at each it
                        } else if (admin == "arrondissement") {
                                admin.name <-  niscodes.admin$`Entités administratives`[substr(niscodes.admin$`Code INS`, 1, 2) == as.character(dth.it$admin[k])] # Obtain arrondissement name at each it
                                
                        }
                        
                        admin.name <- as.character(admin.name) # used as 3rd dim in array
                        if (dth.it$sex[k] == 1) { # male
                                dth_m[i, j, admin.name] <- dth.it$n[k]
                                
                        }
                        else if (dth.it$sex[k] == 2) { # female
                                dth_f[i, j, admin.name] <- dth.it$n[k]
                        }
                }
        }
}



# ----- Interpolate dth 1995 --------------------------------------------------------------------------------------------------------------------------------

# Issue with year 1991
year.names <- c(1992:2015)
dth.int_m <- dth_m; rm(dth_m)
dth.int_f <- dth_f; rm(dth_f)
# Now dth_m contains one additional col
dth_m <- dth_f <- array(0, 
                        dim = c(length(year.names), length(age.names), length(admin.names)), 
                        dimnames = list(year.names, age.names, admin.names))

for (k in seq_along(admin.names)) { # loop on 3rd dim of array
        
        admin.name <- as.character(admin.names[k])
        # male
        interpol <- (dth.int_m["1994", , admin.name] + dth.int_m["1996", , admin.name])/2
        it <- rbind(dth.int_m[, , admin.name], interpol)
        rownames(it)[rownames(it) == "interpol"] <- 1995
        dth_m[, , admin.name] <- it[as.character(year.names), ]
        # if exp(1995)=0 -- > dth(1995)=0
        if (sum(exp_m["1995", , admin.name] == 0) != 0) {
                
                id <- which(exp_m["1995", , admin.name] == 0)
                dth_m["1995", id, admin.name] <- 0
        }
        # female
        interpol <- (dth.int_f["1994", , admin.name] + dth.int_f["1996", , admin.name])/2
        it <- rbind(dth.int_f[, , admin.name], interpol)
        rownames(it)[rownames(it) == "interpol"] <- 1995
        dth_f[, , admin.name] <- it[as.character(year.names), ]
        # if exp(1995)=0 -- > dth(1995)=0
        if (sum(exp_f["1995", , admin.name] == 0) != 0) {
                
                id <- which(exp_f["1995", , admin.name] == 0)
                dth_f["1995", id, admin.name] <- 0
        }
}


# ----- Merge last age groups to avoid issues --------------------------------------------------------------------------------------------------------------

# If this step is not done, it creates Inf and Na values in mx
for (k in seq_along(admin.names)) { # loop on 3rd dim of array
        
        admin.name <- as.character(admin.names[k])
        
        for( j in 1:y.lim ) { # loop on all years except last (see above)
                
                # 21 corresponds to age gp 95
                exp_m[j, 21, admin.name] <- sum(exp_m[j, 21:22, admin.name])
                exp_f[j, 21, admin.name] <- sum(exp_f[j, 21:22, admin.name])
                dth_m[j, 21, admin.name] <- sum(dth_m[j, 21:22, admin.name])
                dth_f[j, 21, admin.name] <- sum(dth_f[j, 21:22, admin.name])
        }
}
exp_m <- exp_m[, -22, ]; exp_f <- exp_f[, -22, ] 
dth_m <- dth_m[, -22, ]; dth_f <- dth_f[, -22, ] 


# ----- Compute mx ------------------------------------------------------------------------------------------------------------------------------------------

age.names <- c(0, 1, seq(5, 95, 5))
# mx containers (male & female) 
mx_m <- mx_f <- array(0, 
                      dim = c(y.lim, length(age.names), length(admin.names)), 
                      dimnames = list(head(year.names, -1), age.names, admin.names))


for (k in seq_along(admin.names)) { # loop on 3rd dim of array
        
        admin.name <- as.character(admin.names[k])
        
        mx_m[,, admin.name] <- dth_m[1:y.lim,, admin.name]/exp_m[,, admin.name] 
        mx_f[,, admin.name] <- dth_f[1:y.lim,, admin.name]/exp_f[,, admin.name] 
}

# Lots of zero which will be an issue when converting into log()
sum(mx_m == 0)
which(mx_m == 0, arr.ind = TRUE)[, -3]
sum(mx_f == 0)
which(mx_m == 0, arr.ind = TRUE)[, -3]


# ------ Sensitive analysis on PCA for imputation of log(0) ------------------------------------------------------------------------------------------------------------

l.mx_m <- log(mx_m)
l.mx_f <- log(mx_f)

# containers matrices for log(mx)
X_m <- X_f <- matrix(NA, nrow = y.lim*length(admin.names), ncol = length(age.names))
up <- 0
for (k in seq_along(admin.names)) { # loop on 3rd dim of array
        
        admin.name <- as.character(admin.names[k])
        # identifies the lines to store
        low <- up +1
        up <- low + (y.lim-1)
        
        X_m[low:up, ] <- l.mx_m[, , admin.name]
        X_f[low:up, ] <- l.mx_f[, , admin.name]
}

plot(x = 1:21, y = X_m[100, ], type = "l")
# removing the line when NA
X1_m <- X_m[-which(is.infinite(X_m), arr.ind = T)[,1],]
X1_f <- X_f[-which(is.infinite(X_f), arr.ind = T)[,1],]


par(mfrow = c(2,3))
pcs <- svd(X1_m)$v[, 1:3]
plot(pcs[, 1], type = "o")
plot(pcs[, 2], type = "o")
plot(pcs[, 3], type = "o")
pcs <- svd(X1_f)$v[, 1:3]
plot(pcs[, 1], type = "o")
plot(pcs[, 2], type = "o")
plot(pcs[, 3], type = "o")


# approximates zero
X2_m <- X_m
X2_f <- X_f
X2_m[is.infinite(X2_m)] <- 1e-4 # selecting -4 or -8 does not impact the PCS
X2_f[is.infinite(X2_f)] <- 1e-4


par(mfrow = c(2,3))
pcs <- svd(X2_m)$v[, 1:3]
plot(pcs[, 1], type = "o")
plot(pcs[, 2], type = "o")
plot(pcs[, 3], type = "o")
pcs <- svd(X2_f)$v[, 1:3]
plot(pcs[, 1], type = "o")
plot(pcs[, 2], type = "o")
plot(pcs[, 3], type = "o")


# ----- Storing pcs -------------------------------------------------------------------------------------------------------------------------------------

X_m <- X_m[-which(is.infinite(X_m), arr.ind = T)[,1],]
X_f <- X_f[-which(is.infinite(X_f), arr.ind = T)[,1],]
pcs_m <- svd(X_m)$v[, 1:3]
pcs_f <- svd(X_f)$v[, 1:3]

saveRDS(matrix(pcs_m, ncol = 3), "./data/BeProvince_pcs_m.rda")
saveRDS(matrix(pcs_f, ncol = 3), "./data/BeProvince_pcs_f.rda")

par(mfrow = c(2,3))
plot(pcs_m[, 1], type = "o")
plot(pcs_m[, 2], type = "o")
plot(pcs_m[, 3], type = "o")
plot(pcs_f[, 1], type = "o")
plot(pcs_f[, 2], type = "o")
plot(pcs_f[, 3], type = "o")
