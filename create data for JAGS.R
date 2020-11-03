


#######################################################################################
##      PROJECT: Sub-national estimation BEL
##      -- by Benjamin-Samuel Schlüter --
##      UCLouvain
#######################################################################################
#
# Store data in the right format for JAGS : array (age*time*arrondissement*province) 
#
#######################################################################################
#
#
# Notes:
# 1) Possible to use as 4th dim:region; as 3rd dim commune
# 2) Default may be set as NAs instead of 0 in array containers
# 3) 
# 4) 

########################################################################################################################################################



rm(list=ls())


# ----- Load pkg -------------------------------------------------------------------------------------------------------------- 

packages <- c("tidyverse")
invisible( lapply(packages, library, character.only = TRUE))


# ----- Load and structure meta data ---------------------------------------------------------------------------------------------------------------------

admin.units <- readxl::read_excel("./data/admin units.xls")
admin.units$code.a <- substr(admin.units$code.c, 1, 2)

admin.pro <- unique(admin.units$province)
admin.arr <- admin.units[!duplicated(admin.units$arrondissement), ]
admin.arr <- admin.arr[,!colnames(admin.arr) %in% c("code.c", "commune")]

# Set order of arrond. in a given province: used in array later
admin.arr$order <- NA
for (i in seq_along(admin.pro)) {
        admin.arr$order[admin.arr$province == admin.pro[i]] <- order(as.numeric(admin.arr$code.a[admin.arr$province == admin.pro[i]]))
}
stopifnot(sum(is.na(admin.arr$order)) == 0)
# used to define arrays to contain data
max.arr <- max(admin.arr$order)

# get nber of arrond. for each province
admin.pro <- admin.arr %>% 
        group_by(province) %>% 
        summarize(nber = max(order))

#----- Create array for pop (age*year*arrond*province) -------------------------------------------------------------------------------------------------

# Set names for array dimensions 
age.names <- c(0, 1, seq(5, 100, 5)) # age grp defined in input data sets
year.names <- c(1992:2015)# (could be set with # folders)

arr.names <- admin.arr$arrondissement
pro.names <- admin.pro$province


# Pop containers (male & female) 
pop_m <- pop_f <- array(0, # set NA as default for JAGS
                        dim = c(length(age.names), length(year.names), max.arr, length(pro.names)), 
                        dimnames = list(age.names, year.names, NULL, pro.names))


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
                pop.it <- pop.it %>% 
                        mutate(admin = substr(code, 1, 2)) %>%
                        group_by(admin, sex) %>% 
                        summarise(n = sum(n))
                
                for (k in 1:dim(pop.it)[1] ) { 
                        
                        pro.name <- as.character( admin.arr$province[admin.arr$code.a == pop.it$admin[k]] ) # Obtain province name at each it
                        arr.name <-  as.character( admin.arr$arrondissement[admin.arr$code.a == as.character(pop.it$admin[k])] ) # Obtain arrondissement name at each it
                        position <- as.numeric( admin.arr$order[admin.arr$code.a == as.character(pop.it$admin[k])] ) # Set position for each arrond. within province
                        
                        if (pop.it$sex[k] == 1) { # male
                                pop_m[j, i, position, pro.name] <- pop.it$n[k]
                                
                        }
                        else if (pop.it$sex[k] == 2) { # female
                                pop_f[j, i, position, pro.name] <- pop.it$n[k]
                        }
                }
        }
}


# ----- Transform pop into exposure --------------------------------------------------------------------------------------------------------------------

## Exp containers (male & female) 
y.lim <- dim(pop_m)[2]-1 # last year not accounted (exp(t)=(pop(t)+pop(t+1))/2)
exp_m <- pop_m[, 1:y.lim, , ]
exp_f <- pop_f[, 1:y.lim, , ]

for (k in seq_along(pro.names)) { # loop on 3rd dim of array
        
        pro.name <- as.character(pro.names[k])
        n <- admin.pro$nber[admin.pro$province == pro.name]
        
        for(i in 1:n) {
                
                for( j in 1:y.lim ) { # loop on all years except last (see above)
                        
                        exp_m[, j, i, pro.name] <- (pop_m[, j, i, pro.name] + pop_m[, (j+1), i, pro.name])/2
                        exp_f[, j, i, pro.name] <- (pop_f[, j, i, pro.name] + pop_f[, (j+1), i, pro.name])/2
                } 
        }
}


#----- Create array for dth (age*year*arrond*province) -------------------------------------------------------------------------------------------------

year.names <- c(1992:1994, 1996:2015) 
# Dth containers (male & female) 
dth_m <- dth_f <- array(0, # set NA as default for JAGS
                        dim = c(length(age.names), length(year.names), max.arr, length(pro.names)), 
                        dimnames = list(age.names, year.names, NULL, pro.names))


# ----- Fill in dth array --------------------------------------------------------------------------------------------------------------------

# List files in order to extract data to fill "pop"
data.files <- list.files("./data")
for (i in seq_along(year.names)) {
        
        id <- grepl(year.names[i], data.files) & grepl("dth", data.files) # obtain dth data of a given year
        dth <- get(load(paste("./data/", data.files[id], sep = ""))) # load file corresponding to year
        
        for (j in seq_along(age.names)) {
                
                age.gp <- as.character(age.names[j]) # "age gp" to subset list
                dth.it <- as.data.frame(dth[[age.gp]]) 
                names(dth.it) <- c("code", "sex", "n")
                # aggregate at the arrondissement level
                dth.it <- dth.it %>% 
                        mutate(admin = substr(code, 1, 2)) %>%
                        group_by(admin, sex) %>% 
                        summarise(n = sum(n))
                
                for (k in 1:dim(dth.it)[1] ) { 
                        
                        pro.name <- as.character( admin.arr$province[admin.arr$code.a == dth.it$admin[k]] ) # Obtain province name at each it
                        arr.name <-  as.character( admin.arr$arrondissement[admin.arr$code.a == as.character(dth.it$admin[k])] ) # Obtain arrondissement name at each it
                        position <- as.numeric( admin.arr$order[admin.arr$code.a == as.character(dth.it$admin[k])] ) # Set position for each arrond. within province
                        
                        if (dth.it$sex[k] == 1) { # male
                                dth_m[j, i, position, pro.name] <- dth.it$n[k]
                                
                        }
                        else if (dth.it$sex[k] == 2) { # female
                                dth_f[j, i, position, pro.name] <- dth.it$n[k]
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
dth_m <- dth_f <- array(0, # set NA as default for JAGS
                        dim = c(length(age.names), length(year.names), max.arr, length(pro.names)), 
                        dimnames = list(age.names, year.names, NULL, pro.names))

for (k in seq_along(pro.names)) { # loop on 3rd dim of array
        
        pro.name <- as.character(pro.names[k])
        n <- admin.pro$nber[admin.pro$province == pro.name]
        
        for(i in 1:n) {
                
                # require to round() because poisson dist. do not allow for real numbers
                # male
                interpol <- round((dth.int_m[, "1994", i, pro.name] + dth.int_m[, "1996", i, pro.name])/2, 0)
                it <- cbind(dth.int_m[, , i, pro.name], interpol)
                colnames(it)[colnames(it) == "interpol"] <- 1995
                dth_m[, , i, pro.name] <- it[, as.character(year.names)]
                # if exp(1995)=0 -- > dth(1995)=0
                if (sum(exp_m[, "1995", i, pro.name] == 0) != 0) {
                        
                        id <- which(exp_m[, "1995", i, pro.name] == 0)
                        dth_m[id, "1995", , pro.name] <- 0
                }
                # female
                interpol <- round((dth.int_f[, "1994", i, pro.name] + dth.int_f[, "1996", i, pro.name])/2, 0)
                it <- cbind(dth.int_f[, , i, pro.name], interpol)
                colnames(it)[colnames(it) == "interpol"] <- 1995
                dth_f[, , i, pro.name] <- it[, as.character(year.names)]
                # if exp(1995)=0 -- > dth(1995)=0
                if (sum(exp_f[, "1995", i, pro.name] == 0) != 0) {
                        
                        id <- which(exp_f[, "1995", i, pro.name] == 0)
                        dth_f[id, "1995", , pro.name] <- 0
                }
        }
}


# ----- Merge last age groups to avoid issues --------------------------------------------------------------------------------------------------------------

# If this step is not done, it creates Inf and Na values in mx
for (k in seq_along(pro.names)) { # loop on 4th dim of array
        
        pro.name <- as.character(pro.names[k])
        n <- admin.pro$nber[admin.pro$province == pro.name]
        
        for(i in 1:n) {
                
                for( j in 1:y.lim ) { # loop on all years except last (see above)
                        
                        # 21 corresponds to age gp 95
                        exp_m[21, j, i, pro.name] <- sum(exp_m[21:22, j, i, pro.name])
                        exp_f[21, j, i, pro.name] <- sum(exp_f[21:22, j, i, pro.name])
                }
                for( j in 1:(y.lim+1) ) { # loop on all years 
                        
                        dth_m[21, j, i, pro.name] <- sum(dth_m[21:22, j, i, pro.name])
                        dth_f[21, j, i, pro.name] <- sum(dth_f[21:22, j, i, pro.name])
                        pop_m[21, j, i, pro.name] <- sum(pop_m[21:22, j, i, pro.name])
                        pop_f[21, j, i, pro.name] <- sum(pop_f[21:22, j, i, pro.name])
                }
        }
}
exp_m <- exp_m[-22, , , ]; exp_f <- exp_f[-22, , , ] 
dth_m <- dth_m[-22, , , ]; dth_f <- dth_f[-22, , , ]
pop_m <- pop_m[-22, , , ]; pop_f <- pop_f[-22, , , ]

stopifnot(all(dim(pop_m) == dim(dth_m)))


# ----- Save created data -----------------------------------------------------------------------------------------------------------------------------------------------

# nber of arr by province
saveRDS(admin.pro, "./data/admin.pro.rda")

# Due to poisson distribution, pop has to be interger -> not using exp
saveRDS(pop_m, "./data/ArrPro_pop_m.rda")
saveRDS(pop_f, "./data/ArrPro_pop_f.rda")

# exposure (carreful exp has less cols than pop and dth)
saveRDS(exp_m, "./data/ArrPro_exp_m.rda")
saveRDS(exp_f, "./data/ArrPro_exp_f.rda")

# death
saveRDS(dth_m, "./data/ArrPro_dth_m.rda")
saveRDS(dth_f, "./data/ArrPro_dth_f.rda")


