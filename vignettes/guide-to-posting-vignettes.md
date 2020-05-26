---
title: Guide to contributing vignettes
author: Stefan Siegert
layout: default
---

# Guide to contributing vignettes

Here is a quick style guide for contributing vignettes. Please try to follow
this as much as possible and let me know if something seems unreasonable or
doesn't make sense.


## Use markdown and knitr

Vignettes are saved as markdown files (suffix `.md`) in the `vignettes`
subfolder. Your markdown file should include a yaml header that specifies
title, author, and the default layout, such as

```
---
author: Author name
title: Vignette title
layout: default
---
```


Use `knitr` as the rendering engine to convert Rmarkdown to
markdown. 

```r
knitr::knit("cool_analysis.Rmd")
```

## Add your vignette to the vignette index 

Modify the file `vignettes.md` in the base directory to add a link to your new
vignette. 


## Folder structure

All vignettes are saved as markdown files (suffix `.md`) in the `vignettes`
subfolder. Figures and data should be saved in individual subfolders per
vignette under in the `vignettes/figure` and `vignettes/data` folders.

For example, if your vignette is called `cool_analysis.md`, it
should generate the following subdirectories and files:

```
vignettes
├── cool_analysis.md
├── data
│   └── cool_analysis
│       └── cool_data.csv
└── figure
    └── cool_analysis
        └── cool_figure1.png
```

You can use the Rmarkdown template [`template.Rmd`]({{ site.baseurl
}}/vignettes/template.Rmd) that, among other things, sets chunk options so that
figures are saved in a new subdirectory. If you don't want to use the template
file, include the following chunk in your `Rmd` file:


````
```{r, include=FALSE}
my_fig_path = file.path('figure', knitr::current_input(), .Platform$file.sep)
knitr::opts_chunk$set(fig.path = my_fig_path)
```
````



## Latex maths

Use Latex syntax to typeset maths, i.e. single dollar signs `$` for inline
maths and double dollar signs `$$` for display style maths. 


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











