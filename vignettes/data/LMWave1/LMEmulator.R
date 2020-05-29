#####################################################
########        FIT LINEAR EMULATOR          ########
#####################################################

## method to fit linear regression emulator to mixture model parameters
fitLM <- function(xTrain, yTrain, norder = 3, returnData = FALSE) {
    
    ## check inputs
    checkInput(xTrain, "data.frame")
    checkInput(yTrain, "data.frame", ncol = 1, nrow = nrow(xTrain))
    map(xTrain, checkInput, type = c("vector", "numeric"))
    map(yTrain, checkInput, type = c("vector", "numeric"))
    checkInput(norder, c("vector", "numeric"), 1, int = T)
    checkInput(returnData, c("vector", "logical"), 1)
    
    ## extract inputs
    inputs <- colnames(xTrain)
    
    ## create formula
    temp_form <- map(1:norder, function(order, inputs) {
        ints <- rep(list(inputs), order) %>%
            expand.grid() %>%
            apply(1, sort) %>%
            t()
        if(order > 1) {
            ints <- apply(ints, 1, table) %>%
                map(function(x) {
                    y <- x[x > 1]
                    x <- x[x == 1]
                    if(length(y) > 0) {
                        y <- paste0("I(", names(y), "^", y, ")")
                    }
                    x <- names(x)
                    x <- sort(c(x, y))
                    x <- paste0(x, collapse = ":")
                    x
                }) %>%
                unique()
        }
        ints
    }, inputs = inputs) %>%
        map(reduce, c) %>%
        reduce(c) %>%
        paste0(collapse = " + ") %>%
        {paste0("output ~ ", .)} %>%
        formula()
    
    ## run stepwise approach and return fitted model
    trainData <- cbind(yTrain, xTrain) %>%
        rename(output = 1)
    fits <- stepAIC(lm(output ~ 1, data = trainData), scope = list(lower = output ~ 1, upper = temp_form), k = log(nrow(trainData)))
    
    ## append data if required
    if(returnData) {
        fits <- list(LM = fits, trainData = trainData)
    }
    
    ## return fitted emulator
    fits
}

## function to check validation for LM
validateLM <- function(LM, xVal, yVal) {
    
    ## produce predictions
    preds <- predict(LM, newdata = xVal, se.fit = TRUE)
    
    ## extract sigma
    sigma <- summary(LM)$sigma
    
    ## bind to observations
    preds <- cbind(yVal, pred = preds$fit, var = preds$se.fit^2 + sigma^2) %>%
        rename(y = 1)
    
    ## produce prediction errors
    preds <- mutate(preds, res = (y - pred) / sqrt(var))
    
    ## plot predicted standardised residuals
    p <- preds %>%
        select(res, Predictions = pred) %>%
        cbind(xVal) %>%
        gather(var, value, -res) %>%
        ggplot(aes(y = res, x = value)) +
        geom_hline(yintercept = -2, linetype = "dashed") +
        geom_hline(yintercept = 2, linetype = "dashed") +
        geom_hline(yintercept = 0, colour = "red") +
        geom_point() + 
        facet_wrap(~var, scales = "free_x") +
        ylab("Predicted residuals")
    print(p)
}

## R6 class for storing emulator
LMemulator <- R6Class("LMemulator", list(
    
    mean = NULL,
    sd = NULL,
    trans = NULL,
    
    ## initialiser function
    initialize = function(mean, sd, trans) {
        checkInput(class(mean), c("vector", "character"), 1, inSet = "lm")
        checkInput(class(sd), c("vector", "character"), 1, inSet = "lm")
        checkInput(trans, "list", 2)
        if(!identical(names(trans), c("mean", "sd"))) {
            stop("Names in 'trans' must equal: 'mean' or 'sd'")
        }
        map(trans, ~{
            checkInput(., "list")
            if(names(.)[1] != "output") {
                stop("First element of 'trans' is not named 'output'")
            }
            # print("Need to check names of transformations")
            map(., ~{
                checkInput(., "function", 1)
                if(!identical(formalArgs(.), c("x", "inverse"))) {
                    stop("Arguments in 'trans' functions must equal: 'x' and 'inverse'")
                }
            })
        })
        
        self$mean <- mean
        self$sd <- sd
        self$trans <- trans
    },
    
    ## predict function
    predict = function(newdata) {
        
        ## predict mean
        trans <- self$trans$mean
        ind <- which(names(trans) == "output")
        ytrans <- trans[[ind]]
        xtrans <- trans[-ind]
        inputs <- newdata
        for(i in 1:length(xtrans)) {
            inputs <- mutate_at(inputs, names(xtrans)[i], xtrans[[i]])
        }
        mean <- predict(self$mean, inputs, se.fit = TRUE)
        mean <- mean[c("fit", "se.fit")] %>%
            bind_cols() %>%
            mutate(trans = list(ytrans)) %>%
            mutate(var = map2_dbl(fit, se.fit, function(mn, sd, tr) {
                x <- rnorm(100, mean = mn, sd = sd)
                x <- tr(x, inverse = TRUE)
                var(x)
            }, tr = ytrans)) %>%
            mutate(fit = ytrans(fit, inverse = TRUE)) %>%
            select(fit, var)
        
        ## predict sd
        trans <- self$trans$sd
        ind <- which(names(trans) == "output")
        ytrans <- trans[[ind]]
        xtrans <- trans[-ind]
        inputs <- newdata
        for(i in 1:length(xtrans)) {
            inputs <- mutate_at(inputs, names(xtrans)[i], xtrans[[i]])
        }
        sd <- predict(self$sd, inputs)
        sd <- ytrans(sd, inverse = TRUE)
        
        pred <- tibble(pred = mean$fit, VS = sd^2, VM = 0.1 * sd^2, VC = mean$var)
        pred
    }
))

