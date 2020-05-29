## load libraries
library(MASS)
library(tidyverse)
library(magrittr)
library(grid)
library(R6)
library(lhs)
library(rdist)
library(hexbin)

## R6 class for storing HM outputs
HM <- R6Class("HM", list(
    
    ## objects to store priors and data
    priors = NULL,
    data = NULL,
    
    ## objects to store observation error
    VO = NULL,
    
    ## objects to store design points
    design = NULL,
    
    ## object to store simulations
    sims = NULL,
    
    ## objects to store emulators
    ## and fits
    emulators = NULL,
    
    ## objects to store evaluation points,
    ## predictions and implausibilities
    eval = NULL,
    preds = NULL,
    I = NULL,
    
    ## object to store bounds
    bounds = NULL,
    
    ## object to store stages
    stages = tibble(
        stage1 = c("design", "bounds", "sims", "emulators", 
                   "eval", "preds", "I"), 
        group = c(1, 1, 2, 3, 4, 5, 6)
    ),
    
    ## initialiser function
    initialize = function(priors, data, VO) {
        
        ## check for inputs
        if(missing(priors)){
            stop("Must have 'priors' argument")
        }
        if(missing(data)){
            stop("Must have 'data' argument")
        }
        if(missing(VO)){
            stop("Must have 'VO' argument")
        }
        
        ## check priors
        checkInput(priors, "data.frame", ncol = 4)
        if(!identical(colnames(priors), c("parnames", "dist", "p1", "p2"))){
            stop("Column names of 'priors' must be: 'parnames', 'dist', 'p1' and 'p2'")
        }
        checkInput(priors$parnames, "character")
        checkInput(priors$dist, "character")
        checkInput(priors$p1, "numeric")
        checkInput(priors$p2, "numeric")
        if(!all(priors$dist %in% "unif")){
            stop("'priors' must be of form: 'unif'")
        }
        
        ## check data
        checkInput(data, "data.frame")
        for(i in 1:ncol(data)){
            checkInput(data[, i], c("vector", "numeric"), 1)
        }
        
        ## check VO
        checkInput(VO, "list", ncol(data))
        for(i in 1:length(VO)){
            checkInput(VO[[i]], c("vector", "numeric"), 1, gte = 0)
        }
        checkInput(names(VO), c("vector", "character"), ncol(data), inSet = names(data))
        VO <- as_tibble(VO)
        
        ## set inputs
        self$priors <- priors
        self$data <- data
        self$VO <- VO
    },
    
    ## print function
    print = function(...) {
        cat("An object of class: 'HM'\n")
        
        ## print prior information
        temp <- self$priors %>%
            mutate(p1 = as.character(signif(p1, 2))) %>%
            mutate(p2 = as.character(signif(p2, 2))) %>%
            mutate(temp = ifelse(dist == "unif", paste0("U(lower = ", p1, ", upper = ", p2, ")"), NA)) %>%
            mutate(temp = ifelse(dist == "gamma", paste0("G(shape = ", p1, ", rate = ", p2, ")"), temp)) %>%
            mutate(temp = ifelse(dist == "norm", paste0("N(mean = ", p1, ", sd = ", p2, ")"), temp)) %>%
            mutate(temp = paste0(parnames, " ~ ", temp)) %>%
            select(temp)
        colnames(temp) <- ""
        cat("\nPriors:\n")
        print(temp, row.names = F, col.names = F, quote = F)
        
        ## print data
        cat("\nData:\n\n")
        print(self$data, row.names = F, col.names = F, quote = F)
        
        ## summarise waves
        nwaves <- length(self$I)
        if(nwaves == 0) {
            cat("\nNo waves currently completed\n")
        } else {
            ## summarise acceptance rates
            cat(paste0("\n", nwaves, " complete ", ifelse(nwaves == 1, "wave has", "waves have"), " been run:\n\n"))
            accrates <- map_dbl(self$I[1:nwaves], ~{
                length(.[. <= 3]) / length(.)
            })
            accrates <- data.frame(Wave = 1:nwaves, Acc.rates = accrates) %>%
                mutate(Cum.acc.rates = cumprod(Acc.rates)) %>%
                mutate_at(2:3, signif, digits = 2)
            print(accrates, row.names = F, col.names = F, quote = F)
            
            ## extract non-implausible points
            preds <- self$preds[1:nwaves]
            I <- self$I[1:nwaves]
            preds <- map2(preds, I, ~{
                inds <- which(.y < 3)
                preds <- map(.x, function(preds, inds) {
                    slice(preds, inds)
                }, inds = inds)
                preds
            })
            
            ## extract other sources of uncertainty
            errors <- map(preds, ~{
                VO <- select(self$VO, one_of(names(.)))
                map2(., VO, ~{
                    select(.x, -pred) %>%
                    mutate(VO = .y)
                })
            })
            
            ## bind errors together and summarise
            errors <- map(errors, ~{
                nm <- names(.)
                map(., ~{
                    mutate(., par = 1:n()) %>%
                    gather(error, value, -par) %>%
                    mutate(error = ifelse(error != "VC", "other", "VC")) %>%
                    group_by(par, error) %>%
                    summarise(value = sum(value)) %>%
                    spread(error, value) %>%
                    ungroup() %>%
                    select(-par) %>%
                    transmute(smaller = ifelse(VC < other, 1, 0)) %>%
                    summarise(prop = sum(smaller) / length(smaller)) %>%
                    mutate(prop = signif(prop, 2))
                }) %>%
                map2(nm, ~set_colnames(.x, .y)) %>%
                bind_cols()
            }) %>%
            bind_rows(.id = "Wave")
            
            ## summarise sources of error
            cat("\nProportion of non-implausible points where VC < VS + VO + VM:\n\n")
            print(as.data.frame(errors), row.names = F, col.names = F, quote = F)
        }
        
        ## print partial wave information
        npartwaves <- map_int(self$stages$stage1, ~{
            length(self[[.]])
        })
        if(any(npartwaves > nwaves) > 0) {
            temp <- self$stages[npartwaves > nwaves, ] %>%
                filter(group == max(group)) %>%
                slice(1) %>%
                pluck("stage1")
            npartwaves <- npartwaves[npartwaves > nwaves]
            if(!all(npartwaves == npartwaves[1])) {
                stop("Error in partial waves")
            }
            if(!all(npartwaves[1] != (nwaves - 1))) {
                stop("Error in partial waves")
            }
            if(temp == "design") {
                temp <- "design points"
            } else {
                if(temp == "sims") {
                    temp <- "simulations"
                } else {
                    if(temp == "emulators") {
                        temp <- "emulator"
                    } else {
                        if(temp == "preds") {
                            temp <- "predictions"
                        } else {
                            stop("Error")
                        }
                    }
                }
            }
            cat(paste0("\nWave ", npartwaves[1], " partially completed up to: ", temp, "\n"))
        }
        
        ## return invisible object
        invisible(self)
    },
    
    ## function to normalise inputs
    fitBounds = function(xTrain, bounds, reverse = FALSE) {
        
        ## check inputs
        checkInput(xTrain, "data.frame")
        map(xTrain, checkInput, type = c("vector", "numeric"))
        checkInput(bounds, "data.frame", nrow = ncol(xTrain), ncol = 3)
        checkInput(colnames(bounds), c("vector", "character"), inSet = c("parnames", "p1", "p2"))
        bounds <- select(bounds, parnames, p1, p2)
        map(select(bounds, -parnames), checkInput, type = c("vector", "numeric"))
        map(select(bounds, parnames), checkInput, type = c("vector", "character"))
        checkInput(colnames(xTrain), c("vector", "character"), inSet = bounds$parnames)
        checkInput(reverse, c("vector", "logical"), 1)
        
        ## rescale inputs between 0 and 1
        xTrain %>%
            gather(parnames, value) %>%
            group_by(parnames) %>%
            nest() %>%
            inner_join(bounds, by = "parnames") %>%
            mutate(data = pmap(list(data, p1, p2), function(x, lb, ub, reverse) {
                if(!all(x >= lb & x <= ub) & !reverse) {
                    stop("Some points outside bounds")
                }
                if(!all(x >= 0 & x <= 1) & reverse) {
                    stop("Some points outside bounds")
                }
                if(!reverse) {
                    x <- (x - lb) / (ub - lb)
                } else {
                    x <- x * (ub - lb) + lb
                }
                x
            }, reverse = reverse)) %>%
            select(-p1, -p2) %>%
            unnest(cols = c(data)) %>%
            group_by(parnames) %>%
            mutate(id = 1:n()) %>%
            spread(parnames, value) %>%
            select(-id)
    },
    
    ## sample design points
    sampDesign = function(ndesign, nvalidate) {
        
        ## check other components have been sampled
        stages <- self$stages %>%
            filter(stage1 == "design") %>%
            pluck("group") %>%
            {filter(self$stages, group > .)} %>%
            pluck("stage1")
        if(
            any(
                map_int(stages, ~{
                    length(self[[.]])
                }) == (length(self$design) - 1)
            )
        ) {
            if((length(self$design) - 1) == 0) {
                self$design <- NULL
            } else {
                self$design <- self$design[1:(length(self$design) - 1)]
            }
            self$resetWave("design")
        } else {
            self$resetWave("design")
        }
        
        if(missing(ndesign)) {
            stop("Must provide 'ndesign' argument to 'sampDesign' function")
        }
        if(missing(nvalidate)) {
            stop("Must provide 'nvalidate' argument to 'sampDesign' function")
        }
        ## check ndesign input
        checkInput(ndesign, c("vector", "numeric"), 1, int = T, gt = 0)
        ## check nval input
        checkInput(nvalidate, c("vector", "numeric"), 1, int = T, gt = 0)
        
        ## if first wave, then sample from priors
        if(is.null(self$design)) {
            self$resetWave("design")
            
            ## sample design points
            self$design <- list(NULL)
            self$design[[1]] <- randomLHS(ndesign, nrow(self$priors)) %>%
                as.data.frame() %>%
                as.list() %>%
                {pmap(list(., self$priors$p1, self$priors$p2), function(x, LB, UB) {
                    (UB - LB) * x + LB
                })} %>%
                reduce(cbind) %>%
                set_colnames(self$priors$parnames) %>%
                as_tibble() %>%
                mutate(prevWave = 0) %>%
                mutate(val = 0)
            
            ## sample validation points
            self$design[[1]] <- rbind(self$design[[1]], 
                randomLHS(nvalidate, nrow(self$priors)) %>%
                    as.data.frame() %>%
                    as.list() %>%
                    {pmap(list(., self$priors$p1, self$priors$p2), function(x, LB, UB) {
                        (UB - LB) * x + LB
                    })} %>%
                    reduce(cbind) %>%
                    set_colnames(self$priors$parnames) %>%
                    as_tibble() %>%
                    mutate(prevWave = 0) %>%
                    mutate(val = 1)
            )
            
            ## add parameter index
            self$design[[1]] <- mutate(self$design[[1]], par = 1:n()) %>%
                select(par, everything())
            
            ## set bounds for completeness
            bounds <- self$priors %>%
                select(-dist)
            self$bounds <- list(NULL)
            self$bounds[[1]] <- bounds
        } else {
        
            ## sample non-implausible points
            nonimp <- self$eval[[length(self$eval)]] %>%
                cbind(I = self$I[[length(self$I)]]) %>%
                filter(I <= 3) %>%
                select(-I)
                
            ## set bounds
            bounds <- nonimp %>%
                summarise_all(list(min = min, max = max)) %>%
                gather(parnames, value) %>%
                separate(parnames, c("parnames", "type"), sep = -4) %>%
                mutate(type = gsub("_", "", type)) %>%
                spread(type, value) %>%
                select(parnames, min, max) %>%
                rename(p1 = min, p2 = max)
            
            ## extend bounds slightly for safety
            bounds <- mutate(bounds, p1 = ifelse(p1 < 0, p1 * 1.1, p1 * 0.9)) %>%
                mutate(p2 = ifelse(p2 < 0, p2 * 0.9, p2 * 1.1))
            
            ## check against priors
            bounds <- self$priors %>%
                select(parnames, p1, p2) %>%
                gather(bound, value, -parnames) %>%
                rbind(gather(bounds, bound, value, -parnames)) %>%
                group_by(parnames, bound) %>%
                summarise(min = min(value), max = max(value)) %>%
                ungroup() %>%
                mutate(value = ifelse(bound == "p1", max, min)) %>%
                select(parnames, bound, value) %>%
                spread(bound, value)
            
            ## keep same order as priors
            bounds <- inner_join(select(self$priors, parnames), bounds, by = "parnames")
            
            ## save bounds
            self$bounds[[length(self$bounds) + 1]] <- bounds
                
            ## check that we can sample points
            if(ndesign > nrow(nonimp)) {
                stop("Number of non-implausible points needs to be increased - ADD FUNCTION")
            }
            
            ## normalise inputs for sampling
            nonimp <- self$fitBounds(nonimp, bounds)
            
            ## sample initial non-implausible point
            ind <- sample.int(nrow(nonimp), 1)
            design <- nonimp[ind, ]
            nonimp <- nonimp[-ind, ]
            
            ## sample additional points               
            while(nrow(design) < ndesign) {
                ## calculate distances from other points 
                ind <- cdist(design, nonimp)
                
                ## find point according to maximin
                ind <- apply(ind, 2, min)
                ind <- which(ind == max(ind))[1]
                design <- rbind(design, nonimp[ind, ])
                nonimp <- nonimp[-ind, ]
            }
            
            ## sample initial non-implausible point for VALIDATION
            ind <- sample.int(nrow(nonimp), 1)
            validate <- nonimp[ind, ]
            nonimp <- nonimp[-ind, ]
            
            ## sample additional points               
            while(nrow(validate) < nvalidate) {
                ## calculate distances from other points 
                ind <- cdist(validate, nonimp)
                
                ## find point according to maximin
                ind <- apply(ind, 2, min)
                ind <- which(ind == max(ind))[1]
                validate <- rbind(validate, nonimp[ind, ])
                nonimp <- nonimp[-ind, ]
            }
            
            ## reverse normalise
            design <- self$fitBounds(design, bounds, T) %>%
                mutate(prevWave = 0)
            validate <- self$fitBounds(validate, bounds, T) %>%
                mutate(prevWave = 0)
            
            ## bind together
            design <- mutate(design, val = 0) %>%
                rbind(mutate(validate, val = 1)) %>%
                mutate(par = 1:n()) %>%
                select(par, everything()) %>%
                select(all_of(colnames(self$design[[1]])))
            
            ## return sampling points
            self$design[[length(self$design) + 1]] <- design
        }
        invisible(self)
    },
    
    ## set simulations based on design points
    setSims = function(sims, prevWave = T) {
        
        ## check other components have been sampled
        if(length(self$sims) >= length(self$design) & length(self$design) > 0) {
            if(length(self$design) == 1) {
                self$sims <- NULL
            } else {
                self$sims <- self$sims[1:(length(self$design) - 1)]
            }
            self$resetWave("sims")
        } else {
            self$resetWave("sims")
        }
        
        ## check other components have been sampled
        stages <- self$stages %>%
            filter(stage1 == "sims") %>%
            pluck("group") %>%
            {filter(self$stages, group < .)} %>%
            pluck("stage1")
        if(
            !all(
                map_int(stages, ~{
                    length(self[[.]])
                }) == (length(self$sims) + 1)
            )
        ) {
            stop("Not all necessary parts of HM process completed at this stage.")
        }
        
        ## check prevWave argument
        checkInput(prevWave, c("vector", "logical"), 1)
        
        ## check format
        design <- filter(self$design[[length(self$design)]], prevWave == 0)
        checkInput(sims, "data.frame")
        if(sum(colnames(sims) %in% "par") != 1) {
            stop("No 'par' column in 'sims'")
        }
        ## reorder columns so 'par' is first
        sims <- select(sims, par, everything()) %>%
            arrange(par)
        checkInput(sims$par, c("vector", "numeric"), int = T)
        ## check 'par' matches to design points
        if(!all.equal(unique(sims$par), design$par)) {
            stop("Unique values of 'par' in 'sims' do not match values of 'par' in 'design'")
        }
        ## check sims columns match data
        checkInput(colnames(select(sims, -par)), c("vector", "character"), inSet = names(self$data))
        
        ## augment with previous non-implausible samples if required
        if(prevWave & length(self$design) > 1) {
        
            ## extract fitted emulators
            emulators <- self$emulators[[length(self$emulators)]]
            
            ## extract evaluation points (design points at previous wave)
            eval <- self$design[[length(self$design) - 1]] %>%
                select(-prevWave, -val, -par)
            
            ## predict at evaluation points
            preds <- map(1:length(emulators), function(i, emulators, eval) {
                
                ## calculate predictions
                pred <- emulators[[i]]$predict(eval)
                
                ## return predictions
                pred
            }, emulators = emulators, eval = eval)
            names(preds) <- names(emulators)
            
            ## extract data
            data <- self$data %>%
                select(one_of(names(preds)))
            
            ## extract other sources of uncertainty
            VO <- select(self$VO, one_of(names(data)))
            
            ## calculate implausibility measures
            I <- pmap(list(preds, data, VO), function(p, d, VO) {
                if(!is.list(p)) {
                    I <- abs(p$pred - d) / sqrt(p$VC + p$VS + VO + p$VM)
                } else {
                    I <- map(p, function(p, d, VO) {
                        abs(p$pred - d) / sqrt(p$VC + p$VS + VO + p$VM)
                    }, d = d, VO = VO) %>%
                        bind_cols()
                    ## extract relevant prediction
                    inds <- apply(I, 1, function(x) {
                        which(x == min(x))[1]
                    })
                    inds <- tibble(id = inds, par = 1:length(inds))
                    p <- map(p, ~{
                        mutate(., par = 1:n())
                    }) %>%
                        bind_rows(.id = "id") %>%
                        mutate(id = as.numeric(id)) %>%
                        inner_join(inds, by = c("par", "id")) %>%
                        arrange(par)
                    ## check join
                    if(!all.equal(p$par, 1:nrow(p))) {
                        stop("Error when joining predictions")
                    }
                    p <- select(p, -par, -id)
                    ## extract relevant implausibility
                    I <- apply(I, 1, min)
                    I <- list(I = I, preds = p)
                }
                I
            })
            if(is.list(I)) {
                preds <- map(I, "preds")
                I <- map(I, "I")
            }
            I <- pmap_dbl(I, max)
                
            ## augment data
            ind <- which(I < 3)
            pars <- filter(self$design[[length(self$design) - 1]], par %in% ind) %>% 
                mutate(par = as.numeric(factor(par))) %>%
                mutate(par = max(sims$par) + par) %>%
                mutate(prevWave = 1) %>%
                mutate(val = 0)
            self$design[[length(self$design)]] <- rbind(self$design[[length(self$design)]], pars)
            sims <- filter(self$sims[[length(self$sims)]], par %in% ind) %>%
                mutate(par = as.numeric(factor(par))) %>% 
                mutate(par = max(sims$par) + par) %>%
                rbind(sims) %>%
                arrange(par)
        }
        
        ## store object
        if(is.null(self$sims)) {
            self$sims <- list(NULL)
            self$sims[[1]] <- sims
        } else {
            self$sims[[length(self$sims) + 1]] <- sims
        }
        invisible(self)
    },
    
    ## set emulator
    setEmulator = function(emulators) {
        
        ## check other components have been sampled
        if(length(self$emulators) >= length(self$sims) & length(self$sims) > 0) {
            if(length(self$sims) == 1) {
                self$emulators <- NULL
            } else {
                self$emulators <- self$emulators[1:(length(self$sims) - 1)]
            }
            self$resetWave("emulators")
        } else {
            self$resetWave("emulators")
        }
        
        ## check other components have been sampled
        stages <- self$stages %>%
            filter(stage1 == "emulators") %>%
            pluck("group") %>%
            {filter(self$stages, group < .)} %>%
            pluck("stage1")
        if(
            !all(
                map_int(stages, ~{
                    length(self[[.]])
                }) == (length(self$emulators) + 1)
            )
        ) {
            stop("Not all necessary parts of HM process completed at this stage.")
        }
        
        ## check emulators for each output
        outputs <- self$sims[[length(self$sims)]] %>%
            select(-par) %>%
            colnames()
        
        ## check emulators
        checkInput(emulators, "list")
        checkInput(names(emulators), c("vector", "character"), inSet = outputs, uni = T)
        
        ## check elements of emulators are R6 objects
        map(emulators, checkInput, type = "R6")
        
        ## check R6 objects have "predict" classes, and check
        ## predict classes take correct arguments
        map(emulators, ~{
            if("predict" %in% names(.)) {
                ## check predict object
                checkInput(.$predict, "function", 1)
                if(!identical(formalArgs(.$predict), "newdata")) {
                    stop("'predict' function must have argument: newdata")
                }
            } else {
                stop("No 'predict' element to R6 object")
            }
        })
        
        ## return emulator object
        if(is.null(self$emulators)) {
            self$emulators <- list(NULL)
            self$emulators[[1]] <- emulators
        } else {
            self$emulators[[length(self$emulators) + 1]] <- emulators
        }
        invisible(self)
    },
    
    ## sample evaluation points
    sampEval = function(neval) {
        
        ## check other components have been sampled
        if(length(self$eval) >= length(self$emulators) & length(self$emulators) > 0) {
            if(length(self$emulators) == 1) {
                self$eval <- NULL
            } else {
                self$eval <- self$eval[1:(length(self$emulators) - 1)]
            }
            self$resetWave("eval")
        } else {
            self$resetWave("eval")
        }
        
        ## check other components have been sampled
        stages <- self$stages %>%
            filter(stage1 == "eval") %>%
            pluck("group") %>%
            {filter(self$stages, group < .)} %>%
            pluck("stage1")
        if(
            !all(
                map_int(stages, ~{
                    length(self[[.]])
                }) == (length(self$eval) + 1)
            )
        ) {
            stop("Not all necessary parts of HM process completed at this stage.")
        }
        
        if(missing(neval)) {
            stop("Must provide 'neval' argument to 'sampEval' function")
        }
        ## check neval input
        checkInput(neval, c("vector", "numeric"), 1, int = T, gt = 0)
        
        ## if first wave, then sample from priors
        if(is.null(self$eval)) {
            self$eval <- list(NULL)
            self$eval[[1]] <- map2(self$priors$p1, self$priors$p2, runif, n = neval) %>%
                map(cbind) %>%
                reduce(cbind) %>%
                set_colnames(self$priors$parnames) %>%
                as_tibble()
        } else {
            
            ## sample from slice sampler
            
            ## extract bounds
            bounds <- self$bounds[[length(self$bounds)]]
            
            ## sample non-implausible points
            nonimp <- self$eval[[length(self$eval)]] %>%
                cbind(I = self$I[[length(self$I)]]) %>%
                filter(I <= 3) %>%
                select(-I)
            
            ## sample a set of points from the current non-implausible space
            ## (replace = T allows for more points to be sampled than
            ## are in the data set)
            imp <- sample_n(nonimp, neval - nrow(nonimp), replace = T) %>%
                as.data.frame()
            
            ## extract number of parameters
            npar <- ncol(imp)
            
            ## append index
            par <- rep(1, nrow(imp))
            
            ## set bounds matrix
            bounds <- as.matrix(bounds[, -1]) %>%
                t() %>%
                as.vector() %>%
                {matrix(rep(., length(par)), length(par), byrow = T)}
            
            ## get initial points
            ini_imp <- imp
            
            ## loop until all points are non-implausible
            while(length(par) > 0) {
                
                ## sample new design points in 'par'th dimension
                for(i in 1:length(par)) {
                    imp[i, par[i]] <- runif(1, bounds[i, (par[i] - 1) * 2 + 1], bounds[i, (par[i] - 1) * 2 + 2])
                    ## update bounds in case further sampling required
                    if(imp[i, par[i]] < ini_imp[i, par[i]]) {
                        bounds[i, (par[i] - 1) * 2 + 1] <- imp[i, par[i]]
                    } else {
                        bounds[i, (par[i] - 1) * 2 + 2] <- imp[i, par[i]]
                    }
                }
                
                ## work backwards through emulators
                inds <- 1:length(par)
                for(j in (length(self$emulators) - 1):1) {
                    
                    ## extract fitted emulators and additional emulator arguments
                    emulators <- self$emulators[[j]]
                    
                    ## predict at evaluation points
                    preds <- map(1:length(emulators), function(i, emulators, eval) {
                        
                        ## calculate predictions
                        pred <- emulators[[i]]$predict(eval)
                        
                        ## return predictions
                        pred
                    }, emulators = emulators, eval = imp[inds, , drop = F])
                    names(preds) <- names(emulators)
                    
                    ## extract data
                    data <- self$data %>%
                        select(one_of(names(preds)))
                    
                    ## extract other sources of uncertainty
                    VO <- select(self$VO, one_of(names(data)))
                    
                    ## calculate implausibility measures
                    I <- pmap(list(preds, data, VO), function(p, d, VO) {
                        if(!is.list(p)) {
                            I <- abs(p$pred - d) / sqrt(p$VC + p$VS + VO + p$VM)
                        } else {
                            I <- map(p, function(p, d, VO) {
                                abs(p$pred - d) / sqrt(p$VC + p$VS + VO + p$VM)
                            }, d = d, VO = VO) %>%
                                bind_cols()
                            ## extract relevant prediction
                            inds <- apply(I, 1, function(x) {
                                which(x == min(x))[1]
                            })
                            inds <- tibble(id = inds, par = 1:length(inds))
                            p <- map(p, ~{
                                mutate(., par = 1:n())
                            }) %>%
                                bind_rows(.id = "id") %>%
                                mutate(id = as.numeric(id)) %>%
                                inner_join(inds, by = c("par", "id")) %>%
                                arrange(par)
                            ## check join
                            if(!all.equal(p$par, 1:nrow(p))) {
                                stop("Error when joining predictions")
                            }
                            p <- select(p, -par, -id)
                            ## extract relevant implausibility
                            I <- apply(I, 1, min)
                            I <- list(I = I, preds = p)
                        }
                        I
                    })
                    if(is.list(I)) {
                        preds <- map(I, "preds")
                        I <- map(I, "I")
                    }
                    I <- pmap_dbl(I, max)
                    
                    ## extract non-implausible points
                    inds <- inds[I <= 3]
                }
                
                ## extract non-implausible points
                par[inds] <- par[inds] + 1
                nonimp <- rbind(nonimp, imp[par > npar, ])
                imp <- imp[par <= npar, , drop = F]
                ini_imp <- ini_imp[par <= npar, , drop = F]
                bounds <- bounds[par <= npar, , drop = F]
                par <- par[par <= npar]
            }
            
            ## return non-implausible points
            self$eval[[length(self$eval) + 1]] <- as_tibble(nonimp)
        }
        invisible(self)
    },
    
    ## predict at evaluation points
    predictEmulator = function(eval) {
        
        if(missing(eval)) {
            ## check other components have been sampled
            if(length(self$preds) >= length(self$eval) & length(self$eval) > 0) {
                if(length(self$eval) == 1) {
                    self$preds <- NULL
                } else {
                    self$preds <- self$preds[1:(length(self$eval) - 1)]
                }
                self$resetWave("preds")
            } else {
                self$resetWave("preds")
            }
            
            ## check other components have been sampled
            stages <- self$stages %>%
                filter(stage1 == "preds") %>%
                pluck("group") %>%
                {filter(self$stages, group < .)} %>%
                pluck("stage1")
            if(
                !all(
                    map_int(stages, ~{
                        length(self[[.]])
                    }) == (length(self$preds) + 1)
                )
            ) {
                stop("Not all necessary parts of HM process completed at this stage.")
            }
        } else {
            if(length(self$emulators) == 0) {
                stop("No emulators attached to produce predictions")
            }
            print(paste0("Predicting using wave ", length(self$emulators), " emulator(s)..."))
            ## check input data
            checkInput(eval, "data.frame")
            checkInput(colnames(eval), c("vector", "character"), nrow(self$priors), inSet = self$priors$parnames, uni = T)
            eval <- select(eval, one_of(self$priors$parnames))
            for(i in 1:ncol(eval)) {
                checkInput(pluck(eval, i), c("vector", "numeric"), gte = self$priors$p1[i], lte = self$priors$p2[i])
            }
        }
        
        ## extract fitted emulators and additional emulator arguments
        emulators <- self$emulators[[length(self$emulators)]]
        
        ## extract evaluation points
        if(missing(eval)) {
            eval1 <- self$eval[[length(self$eval)]]
        } else {
            eval1 <- eval
        }
        
        ## predict at evaluation points
        preds <- map(emulators, function(em, eval) {
            em$predict(eval)
        }, eval = eval1)
        names(preds) <- names(emulators)
        
        ## check outputs match
        map(preds, ~{
            if(is.data.frame(.)) {
                if(!identical(colnames(.), c("pred", "VS", "VM", "VC"))) {
                    stop("Colnames of predictions must be: pred, VS, VM, VC")
                }
            } else {
                if(!is.list(.)) {
                    stop("Predictions do not produce a data.frame or list")
                }
                map(., ~{
                    checkInput(., "data.frame", ncol = 4)
                    if(!identical(colnames(.), c("pred", "VS", "VM", "VC"))) {
                        stop("Colnames of predictions must be: pred, VS, VM, VC")
                    }
                })
            }
        })
        
        ## extract data
        data <- self$data %>%
            select(one_of(names(preds)))
        
        ## extract other sources of uncertainty
        VO <- select(self$VO, one_of(names(data)))
        
        ## calculate implausibility measures
        I <- pmap(list(preds, data, VO), function(p, d, VO) {
                if(is.data.frame(p)) {
                    I <- abs(p$pred - d) / sqrt(p$VC + p$VS + VO + p$VM)
                } else {
                    I <- map(p, function(p, d, VO) {
                            abs(p$pred - d) / sqrt(p$VC + p$VS + VO + p$VM)
                        }, d = d, VO = VO) %>%
                        bind_cols()
                    ## extract relevant prediction
                    inds <- apply(I, 1, function(x) {
                            which(x == min(x))[1]
                        })
                    inds <- tibble(id = inds, par = 1:length(inds))
                    p <- map(p, ~{
                            mutate(., par = 1:n())
                        }) %>%
                        bind_rows(.id = "id") %>%
                        mutate(id = as.numeric(id)) %>%
                        inner_join(inds, by = c("par", "id")) %>%
                        arrange(par)
                    ## check join
                    if(!all.equal(p$par, 1:nrow(p))) {
                        stop("Error when joining predictions")
                    }
                    p <- select(p, -par, -id)
                    ## extract relevant implausibility
                    I <- apply(I, 1, min)
                    I <- list(I = I, preds = p)
                }
                I
            })
        if(is.list(I[[1]])) {
            preds <- map(I, "preds")
            I <- map(I, "I")
        }
        I <- pmap_dbl(I, max)
        
        ## save predictions
        if(missing(eval)) {
            if(is.null(self$preds)) {
                self$preds <- list(NULL)
                self$preds[[1]] <- preds
                self$I <- list(NULL)
                self$I[[1]] <- I
            } else {
                self$preds[[length(self$preds) + 1]] <- preds
                self$I[[length(self$I) + 1]] <- I
            }
        } else {
            return(cbind(eval, I = I))
        }
        invisible(self)
    },
    
    ## function to plot implausibilities
    plotImplausibility = function(fixRanges = T) {
        
        ## check inputs
        checkInput(fixRanges, c("vector", "logical"), 1)
        
        ## extract evaluation points and implausibilities
        emul <- self$eval[[length(self$eval)]] %>%
            cbind(I = self$I[[length(self$I)]])
    
        ## layout function (used to set where you want to position
        ## each plot)
        vplayout <- function(x, y) {
            viewport(layout.pos.row = x, layout.pos.col = y)
        }
        
        ## open new custom graphics device
        grid.newpage()
        
        ## set grid layout for plots
        pushViewport(viewport(layout = grid.layout(ncol(emul), ncol(emul) - 1, 
                     heights = unit(c(0.5, rep(5, ncol(emul) - 1)), "null"))))
        
        ## add title
        wave <- length(self$eval)
        grid.text(
            paste0("Wave ", wave), 
            vp = viewport(layout.pos.row = 1, layout.pos.col = 1:(ncol(emul) - 1)), 
            gp = gpar(cex = 1.5, font = 2)
        )
        
        for(i in 1:(ncol(emul) - 2)) {
            for(j in (i + 1):(ncol(emul) - 1)) {
                
                ## extract data
                temp <- emul %>%
                    select(all_of(c(i, j)), I)
                
                ## set limits
                firstLim <- self$priors %>%
                    filter(parnames == colnames(temp)[1]) %>%
                    select(p1, p2) %>%
                    unlist()
                secLim <- self$priors %>%
                    filter(parnames == colnames(temp)[2]) %>%
                    select(p1, p2) %>%
                    unlist()
                
                ## plot implausibility measure
                p1 <- temp %>%
                    mutate(I = ifelse(I > 5, 5, I)) %>%
                    ggplot(aes_string(x = colnames(temp)[1], y = colnames(temp)[2])) +
                    geom_hex() + 
                    stat_summary_hex(aes(z = I), fun = "min") +
                    scale_fill_gradient2(low = "green", high = "red", mid = "yellow", 
                                         midpoint = 2, limits = c(0, 5)) +
                    theme(legend.position = "none") +
                    theme(
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank()
                        # axis.text.x = element_blank(),
                        # axis.ticks.x = element_blank()
                    )
                if(fixRanges) {
                    p1 <- p1 + xlim(firstLim) + ylim(secLim)
                }
                
                ## optical density plot
                p2 <- temp %>%
                    filter(I <= 3) %>%
                    ggplot(aes_string(x = colnames(temp)[2], y = colnames(temp)[1])) +
                    geom_hex() + 
                    scale_fill_gradient2(low = "white", high = "purple", mid = "blue") +
                    theme(legend.position = "none") + 
                    theme(
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank()
                        # axis.text.x = element_blank(),
                        # axis.ticks.x = element_blank()
                    )
                if(fixRanges){
                    p2 <- p2 + ylim(firstLim) + xlim(secLim)
                }
                
                ## put each plot in the specified place
                print(p1, vp = vplayout(j + 1, i))
                print(p2, vp = vplayout(i + 1, j))
            }
        }
        
        ## add input names
        for(i in 1:(ncol(emul) - 1)) {
            p <- ggplot() +
                theme_void() +
                annotate(geom = "text", x = 0, y = 0, label = colnames(emul)[i], size = 6)
            ## put each plot in the specified place
            print(p, vp = vplayout(i + 1, i))
        }
    },
    
    ## function to reset HM wave
    resetWave = function(stage) {
        
        ## check input
        checkInput(stage, c("vector", "character"), 1, inSet = self$stages$stage1)
        
        ## extract stages
        stages <- self$stages
        ## extract number of entries in current stage
        nentries <- filter(stages, stage1 == stage) %>%
            pluck("stage1") %>%
            {self[[.]]} %>%
            length()
        ## extract groups to reset
        groups <- filter(stages, stage1 == stage) %>%
            pluck("group") %>%
            {filter(stages, group >= .)} %>%
            pluck("stage1")
        map(groups, function(x, n) {
            if(n == 0) {
                self[[x]] <- NULL
            } else {
                self[[x]] <- self[[x]][1:n]
            }
        }, n = nentries)
        invisible(self)
    },

    ## function to extract wave
    extractWave = function(wave) {
        
        ## check input
        checkInput(wave, c("vector", "numeric"), 1, int = T, inSet = 1:length(self$design), gt = 0)
        
        ## copy object
        self1 <- self$clone()
        
        ## extract stages
        map(self1$stages$stage1, function(x, n) {
            self1[[x]] <- self1[[x]][1:n]
        }, n = wave)
        
        ## return subsetted object
        self1
    },

    ## function to extract element
    extractElement = function(element, prevWave = FALSE) {
        
        ## check inputs
        elements <- c(self$stages$stage1, "priors", "data", "VO", "VM", "VC", "VS")
        checkInput(element, c("vector", "character"), 1, inSet = elements)
        checkInput(prevWave, c("vector", "logical"), 1)
        if(prevWave & element != "design") {
            stop("'prevWave = TRUE' cannot be applied unless 'element == \"design\"'")
        }
        
        ## extract correct element
        wave <- length(self$design)
        
        ## extract variances if needed
        if(element == "VO" | element == "VM" | element == "VC" | element == "VS") {
        
            ## extract code variances
            if(element == "VC") {
                if(length(self$preds) < wave) {
                    stop(paste0("Element '", element, "' is not present in current wave"))
                }
                ## extract emulator variances
                VC <- self$preds[[wave]] %>%
                    bind_rows(.id = "output") %>%
                    select(output, VC)
                return(VC)
            }
            
            ## extract stochastic variances
            if(element == "VS") {
                if(length(self$preds) < wave) {
                    stop(paste0("Element '", element, "' is not present in current wave"))
                }
                ## extract emulator variances
                VS <- self$preds[[wave]] %>%
                    bind_rows(.id = "output") %>%
                    select(output, VS)
                return(VS)
            }
            
            ## extract observation variances
            if(element == "VO") {
                VO <- self$VO
                return(VO)
            }
            
            ## extract model discrepancy
            if(element == "VM") {
                if(length(self$preds) < wave) {
                    stop(paste0("Element '", element, "' is not present in current wave"))
                }
                ## extract emulator variances
                VM <- self$preds[[wave]] %>%
                    bind_rows(.id = "output") %>%
                    select(output, VM)
                return(VM)
            }
        }
        
        ## extract correct element
        temp <- self[[element]]
        if(element != "data" & element != "priors") {
            if(length(temp) < wave) {
                stop(paste0("Element '", element, "' is not present in current wave"))
            }
            temp <- temp[[wave]]
        }
        
        ## include augmented data if required
        if(!prevWave & element == "design") {
            temp <- filter(temp, prevWave == 0) %>%
                select(-prevWave)
        }        
        
        ## return correct element
        temp
    },
    
    ## function to plot simulations
    plotSims = function(outputs = NA, rep = NA, addPred = F) {
    
        ## extract correct element
        wave <- length(self$design)
        sims <- self$sims
        if(length(sims) < wave) {
            stop(paste0("'sims' is not present in current wave"))
        }
        sims <- sims[[wave]]
        
        ## extract outputs to plot
        if(is.na(outputs[1])) {
            outputs <- select(sims, -par) %>%
                colnames()
        } else {
            checkInput(outputs, c("vector", "character"), inSet = colnames(select(sims, -par)))
        }
        
        ## check addPred
        checkInput(addPred, c("vector", "logical"), 1)
        
        ## extract replicates to plot
        if(!is.na(rep[1])) {
            checkInput(rep, c("vector", "numeric"), inSet = unique(sims$par), uni = T, int = T)
        } else {
            rep <- unique(sims$par)
            if(addPred) {
                stop("'addPred' must be FALSE unless 'rep' is set")
            }
        }
        
        ## extract correct simulations and data
        sims <- select(sims, one_of(c("par", outputs))) %>%
            filter(par %in% rep)
        data <- select(self$data, one_of(outputs))
        
        ## format data correctly
        data <- data %>%
            gather(output, value) %>%
            mutate(y = 0)
        
        ## produce plot
        p <- sims %>%
            # group_by(par) %>%
            # summarise_all(mean) %>%
            select(-par) %>%
            gather(output, value) %>%
            ggplot(aes(x = value)) +
                geom_histogram() +
                facet_wrap(~ output) +
                xlab("Output") + ylab("Count") +
                geom_point(aes(y = y), data = data, colour = "red") +
                ggtitle(paste0("Wave ", wave))
        
        ## add predictions is required
        if(addPred) {
            if(length(self$fittedEm) != wave){
                stop(paste0("Haven't fitted emulator to wave ", wave, " yet, so can't set 'addPred = TRUE'"))
            }
            ## extract fitted emulators and additional emulator arguments
            emulators <- self$emulators[[length(self$emulators)]]
            fittedEm <- self$fittedEm[[length(self$fittedEm)]]
            fittedEmArgs <- self$fittedEmArgs[[length(self$fittedEmArgs)]]
            
            ## extract evaluation points
            eval <- self$design[[wave]] %>%
                select(-prevWave) %>%
                mutate(par = 1:n()) %>%
                filter(par %in% rep) %>%
                select(-par)
            
            ## predict at evaluation points
            preds <- map(1:length(emulators), function(i, emulators, eval) {
                
                ## calculate predictions
                x <- emulators[[i]]$predict(eval)
                
                ## check predictions
                checkInput(x, "data.frame", ncol = 2)
                map(x, ~checkInput(., c("vector", "numeric"), nrow(eval)))
                checkInput(names(x), c("vector", "character"), 2, inSet = c("pred", "var"), uni = T)
                x <- select(x, pred, var)
                
                ## return predictions
                x
            }, emulators = emulators, eval = eval)
            names(preds) <- names(emulators)
            preds <- map(preds, ~select(., pred)) %>%
                bind_cols() %>%
                set_colnames(names(preds)) %>%
                mutate(y = 0) %>%
                gather(output, value, -y)
            
            ## augment plot
            p <- p +
                geom_point(aes(y = y), data = preds, colour = "blue")
        }
        ## return plot
        p
    }
))
