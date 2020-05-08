---
layout: default
---

**TODO:**

- use `set.seed`
- justify choice of parameters
- use more reasonable variation ranges
- upload and link design.csv file and output directory (do this when the other
  issues are resolved)


## Create a latin hypercube design for MetaWards parameters

This code creates a simple maximin latin hypercube design with uniform marginal
distributions, and then writes a corresponding design to a `csv` file that can
be used by MetaWards.  The MetaWards parameters are described in detail here:
<https://metawards.org/tutorial/part02/01_disease.html>   

For reference, the "disease" file `ncov.json` used for the simulation is

```bash
cat MetaWardsData/diseases/ncov.json

{ "name"             : "SARS-Cov-2",
  "version"          : "April 23rd 2020",
  "author(s)"        : "Leon Danon",
  "contact(s)"       : "l.danon@bristol.ac.uk",
  "reference(s)"     : "references...",  
  "beta"             : [0.0, 0.0, "1.0/1.15", "1.0/1.15", 0.0],
  "progress"         : [1.0, "1.0/5.2", "1.0/1.15", "1.0/1.15", 0.0],
  "too_ill_to_move"  : [0.0, 0.0, 0.0, 0.0, 0.0],
  "contrib_foi"      : [1.0, 1.0, 1.0, 1.0, 0.0],
  "start_symptom"    : 3
}
```

For this analysis we consider variations of the following parameters.

```r
library(lhs)

varnames <- c('beta[2]', 
              'beta[3]', 
	      'beta[4]', 
	      'progress[1]', 
	      'progress[2]', 
	      'progress[3]', 
	      'progress[4]',
              'too_ill_to_move[3]', 
	      'too_ill_to_move[4]')

# number of inputs and design points
k <- length(varnames)
n <- 10 * k

# Create a maximin latin hypercube
lhs <- lhs::maximinLHS(n, k, method = "build", dup = 1, eps = 0.05,
                       maxIter = 100, optimize.on = "grid", debug = FALSE)

# scale each parameter to their variation ranges
mins  <- rep(0, k)
maxes <- rep(1, k)
design <- sapply(1:k, function(i) mins[i] + lhs[,i] * (maxes[i] - mins[i]))

head(design)

##         beta[2]   beta[3]   beta[4] progress[1] progress[2] progress[3]
## [1,] 0.38710672 0.7569763 0.6833038   0.4779504   0.4627190   0.5614718
## [2,] 0.26436011 0.7365662 0.5343985   0.3851255   0.4941642   0.6908752
## [3,] 0.47295186 0.7043941 0.5603399   0.7304604   0.7053776   0.5090968
## [4,] 0.58942523 0.2583877 0.6218445   0.3134041   0.6259453   0.3612063
## [5,] 0.04531145 0.4148175 0.4693960   0.4387584   0.5902250   0.4196554
## [6,] 0.70659331 0.5639490 0.3867551   0.6536937   0.5817538   0.1563858
##      progress[4] too_ill_to_move[3] too_ill_to_move[4]
## [1,]   0.1994125          0.4282444          0.5700674
## [2,]   0.4559605          0.7743606          0.4756666
## [3,]   0.5422968          0.8408459          0.7618698
## [4,]   0.3936152          0.4984242          0.3895646
## [5,]   0.8571437          0.4555724          0.3709516
## [6,]   0.2028341          0.6502611          0.6407918
```

Save the design in a file

```r
write.matrix(design, file = 'design.csv')
```


## Run MetaWards at the design points

Run MetaWards with the disease specifications in
`MetaWardsData/diseases/ncov.json`, additional disease seeds in London. The
model is run for each line of the `design.csv` file.

```bash
metawards -d ncov -a ExtraSeedsLondon.dat --input design.csv
```

The results are written to the `output` directory.


