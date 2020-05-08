---
layout: default
---

# Running the MetaWards model

> MetaWards implements a national-scale stochastic metapopulation model of
> disease transmission in Great Britain.

- Main website of MetaWards: [metawards.org](https://metawards.org/index.html)
- Installation instructions: [metawards.org/install.html](https://metawards.org/install.html)

The MetaWards model can be easily installed via `pip`. To install for a user
without root access, do


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

To be able to run MetaWards simulations, we have to download the input data
from the [MetaWards data repository](https://github.com/metawards/MetaWardsData):

```bash
git clone https://github.com/metawards/MetaWardsData
```

We also have to set the model parameters governing the transmission rates. For
the following example we use a single setting of the 5 standard parameters
saved in the file `beta_table.csv`. 

```bash
echo "0.9, 0.9, 0.2, 0.9, 0.9" > beta_table.csv
```

The different ways of setting model parameters are documented at
[metawards.org/tutorial/tutorial.html](https://metawards.org/tutorial/tutorial.html).

To finally run `metawards` we have to point it to the data directory, and the
model parameter file as follows:

```bash
METAWARDSDATA=~/folders/MetaWardsData metawards --input beta_table.csv
```

By default the model output is written to a new subdirectory `output`.



