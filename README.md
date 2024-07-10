# RcmdrPlugin.nestedancova

This is a plugin to the graphical user interface Rcmdr that gives support to
analysis of variance of experiments with nested designs (such as blocked or
split plot designs). Effectively it provides support for models that were 
fitted using the `Error()` term in `aov()` (i.e., models of class `aovlist`).

This package only works in combination with Rcmdr and it will add new menus and
statistical capabilities to Rcmdr. It also provides some convenient functions 
that can be use from R to extract residuals, fitted values and perform 
diagnostics on models of class `aovlist` (which are not provided with base R).

Please see documentation on Rcmdr on how to load this plugin after installation.
