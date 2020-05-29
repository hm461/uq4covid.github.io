---
title: Guide to contributing vignettes
author: Stefan Siegert
layout: default
---

# Guide to contributing vignettes

Here is a quick style guide for contributing vignettes. Please try to follow
this as much as possible and let me know if something seems unreasonable or
doesn't make sense.



## Use rmarkdown::render to generate html

Vignettes are saved as html files in the `vignettes` subfolder. 
Use `knitr` as the rendering engine to convert Rmarkdown to
markdown. 

```r
rmarkdown::render("cool_analysis.Rmd") # generates cool_analysis.html
```


## Add your vignette to the vignette index 

Modify the file `vignettes.md` in the base directory to add a link to your new
vignette. 


## Pull request

To get your vignette added to the website, fork the repository, add your
vignette and edit the `vignettes.md` file, and then issue a pull request
through github.

To make any changes to your vignette, please also make the changes on your
local fork first, and then file a pull request.


## Folder structure

Vignettes are saved as html files or markdown files in the `vignettes`
subfolder. Data and additional source code files should be saved in individual
subfolders per vignette under the `vignettes/data` subfolder.

For example, if your vignette is called `cool_analysis.html`, it
should generate the following subdirectories and files:

```
vignettes
├── cool_analysis.Rmd
├── cool_analysis.html
└── data
    └── cool_analysis
        ├── cool_functions.R
        └── cool_data.csv
```


## Suppress package startup messages

To suppress the inclusion of package start up messages in the vignette, use the following:

````
```{r, eval=FALSE}
library(tidyverse)
```
```{r, include=FALSE}
library(tidyverse)
```
````


## Working with external files

Your vignette should not depend on data that the user cannot access.  If you
are using an external data file, make it easy for the user to download the file
by providing a download link, or instructions to generate the data contained in
the file.

To use a local copy of your data file when compiling the vignette, and also
include download instructions for others, you might write your vignette like
this:

````
Next we load the data

```{r, eval=FALSE}
load('data.Rdata')
```
```{r, include=FALSE}
load('/my/own/computers/path/to/data.Rdata')
```

where the file `data.Rdata` can be downloaded 
[here](http://link.to/data.Rdata).
````

which will render to

> Next we load the data
> ```
> load('data.Rdata')
> ```
> where the file `data.Rdata` can be downloaded [here](http://link.to/data.Rdata).


## Collapse big code chunks instead of hiding them

It's generally ok to show all the source code used in your vignette. If you
decide you want to hide parts of the code don't use chunk options like `echo =
FALSE` or `include = FALSE`, but rather use the `<details>` html tag to
collapse them:

````
<p><details><summary>Click here to see the big code chunk</summary>

```{r big_code_chunk}
# lots of code here
```

</details></p>
````

(Note: This only works when rendering the Rmarkdown file to html. It doesn't
seem to work with vignettes written in markdown.)


