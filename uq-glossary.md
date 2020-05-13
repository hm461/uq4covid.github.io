---
layout: default
---

# Uncertainty quantification (UQ) glossary

The following glossary explains some of the central UQ concepts using quotes
from the seminal paper

[**Kennedy and Ohagan 2001 "Bayesian calibration of computer
models"**](https://rss.onlinelibrary.wiley.com/doi/abs/10.1111/1467-9868.00294)


## Calibration

> "Crudely put, calibration is the activity of adjusting the unknown [...]
> parameters until the outputs of the model fit the observed data."

> "More generally, a computer model will have a number of context-specific
> inputs that define a particular situation in which the model is to be used.
> When, as is often the case, the values of one or more of the context-specific
> inputs are unknown, observations are used to learn about them. This is
> calibration.  In current practice, calibration invariably consists of
> searching for a set of values of the unknown inputs such that the observed
> data fit as closely as possible, in some sense, to the corresponding outputs
> of the model. These values are considered as estimates of the context-
> specific inputs, and the model is then used to predict the behaviour of the
> process in this context by setting these inputs to their estimates.  Clearly,
> this 'plug-in' prediction treats the context-specific inputs as if they were
> known.  The reality is that they are only estimated, and residual uncertainty
> about these inputs should be recognized in subsequent predictions from the
> model."


## Model indadequacy (aka model discrepancy)

> "No model is perfect. Even if there is no parameter uncertainty [...] the
> predicted value will not equal the true value of the process. The discrepancy
> is model inadequacy. Since the real process may itself exhibit random
> variability, we define model inadequacy to be the difference between the true
> mean value of the real world process and the code output at the true values
> of the inputs. [...] Given that there is model inadequacy, we cannot think of
> the true input values as those which lead to perfect predictions being
> outputted, so how can we define true values for the uncertain input
> parameters?"


## Code uncertainty

> "[...] it is not realistic to say that the [computer model] output is known
> for given inputs before we actually run the code and see that output. It may
> not be practical to run the code to observe the output for every input
> configuration of interest, in which case uncertainty about code output needs
> to be acknowledged."


## Interpolation (aka emulation)

> "[...] given data comprising outputs at a sample of input configurations, the
> problem is to estimate the output at some other input configuration for which
> the code has not yet been run. This is relevant when the code is particularly
> large and expensive to run. [...] The only form of uncertainty accounted for
> [by interpolation] is code uncertainty."


## Uncertainty analysis

> "The objective of uncertainty analysis is to study the distribution of the
> code output that is induced by probability distributions on inputs. [...] The
> simplest approach to uncertainty analysis is a Monte Carlo solution in which
> configurations of inputs are drawn at random from their distribution. The
> code is then run for each sample input configuration and the resulting set of
> outputs is a random sample from the output distribution [...]. The Monte
> Carlo method for uncertainty analysis is simple but becomes impractical when
> the code is costly to run, because of the large number of runs required. More
> efficiency is claimed for Latin hypercube sampling."


## Sensitivity analysis

> "[...] whose goal is to characterize how the code output responds to changes
> in the inputs, with particular reference to identifying inputs to which the
> output is relatively sensitive or insensitive. [...] As with interpolation,
> these statistical approaches to sensitivity analysis only take account of
> [...] code uncertainty."


