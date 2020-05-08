---
layout: default
---

# Running the MetaWards model

> MetaWards implements a national-scale stochastic metapopulation model of
> disease transmission in Great Britain.

- Main website of MetaWards: [metawards.org](https://metawards.org/index.html)
- Installation instructions: [metawards.org/install.html](https://metawards.org/install.html)

Below is a quick guide to install and run MetaWards. Note however that the
model is being actively developed, and for the latest information and
documentation, the official website should be consulted. 

The MetaWards model can be installed with `pip`. To install for a user without
root access, do


```bash
pip install --user metawards
```

The `metawards` executable is now available from the command line. 
At the time of writing this, the most recent version of MetaWards is

```bash
metawards --version


            ************************
            metawards version 0.10.0
            ************************

            -- Source information --
repository: https://github.com/metawards/MetaWards
                 branch: master
revision: 56949a79ae23b0b9a1310d2a74b37830c1ea3af5
    last modified: 2020-04-27T13:06:36+0100

          -- Additional information --
Visit https://metawards.github.io for more information
  about metawards, its authors and its license

```

To be able to run MetaWards simulations, we have to provide input data from the
[MetaWards data repository](https://github.com/metawards/MetaWardsData), and
model parameters through a configuration file:

```bash
git clone https://github.com/metawards/MetaWardsData
echo "0.9, 0.9, 0.2, 0.9, 0.9" > inputs.csv
```

The model parameters and different ways of specifying them are documented at
[metawards.org/tutorial/tutorial.html](https://metawards.org/tutorial/tutorial.html).

To run `metawards` with this input data and parameters, use

```bash
METAWARDSDATA=/path/to/MetaWardsData metawards --input inputs.csv
```

By default the output is written to a new subdirectory `output`.



