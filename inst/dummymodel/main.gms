$title dummymodel

*' @title Dummy Model
*'
*' @description The *Dummy Model* (DumM) is a simple test model to test and
*' present the basic functionality of the gms and goxygen packages (@gms, @goxygen).
*'
*' It has the structure of a module GAMS model, and therefore can be interpreted
*' by goxygen, but has not content and cannot be solved with GAMS. It can serve as 
*' a template to build a modular GAMS model from scratch.
*'
*' The dummy model consists of three modules [01_fancymodule], [02_crazymodule],
*' and [03_Rmodule].

$setglobal fancymodule  default
$setglobal crazymodule  simple
$setglobal Rmodule  withr

$include "./core/sets.gms"
$include "./core/core.gms"
$batinclude "./modules/include.gms" calculations

*' @title{extrapage: "settings"}
*' Settings
*' @description{extrapage: "settings"}
*' We might want to move some documentation to a separate page called Settings.
