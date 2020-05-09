---
layout: default
---

**TODO:**

- add a small example


## Installing MOGP

We use the [Multi output Gaussian Process
(mogp)](https://mogp-emulator.readthedocs.io/en/latest/) emulator framework for
this project.  MOGP provides fast python modules to perform common UQ tasks
such as Gaussian Process emulation, Experimental design, History Matching, and
MCMC sampling. 

Installation instructions are at
[github.com/alan-turing-institute/mogp_emulator](https://github.com/alan-turing-institute/mogp_emulator).
In this project, we will use new functionionality from the development branch,
e.g. to specify mean functions for emulators. Therefore we have to use the
following non-standard way to install the module:

```bash
git clone https://github.com/alan-turing-institute/mogp_emulator/
cd mogp_emulator
git checkout devel
pip3 install -r requirements.txt 
python3 setup.py develop
```

(To uninstall, use `pip uninstall mogp_emulator`.)



