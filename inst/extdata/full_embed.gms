* #### CODE MERGED WITH FUNCTION gms::singleGAMSfile ####

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

*$include "./core/sets.gms" DONE!
sets
   i dummy set /x1,x2,x3/
;
*$include "./core/core.gms" DONE!

#' @code Here we are doing some core calculation stuff

parameter
  pm_global global parameter
;

pm_global = 1;

#' yihaaa!
*$batinclude "./modules/include.gms" calculations DONE!
$setglobal phase %1
$onrecurse
*######################## R SECTION START (MODULES) ############################
*$include "./modules/01_fancymodule/module.gms" DONE!
*' @title Fancy module
*'
*' @description This is the fancy module. Besides being fancy it does not do
*' anything. And even does not work! But we have a picture of a fancy cat:
*' 
*' ![Fancy cat](cat.png){ width=30% }
*' @authors Bruce Wayne

*###################### R SECTION START (MODULETYPES) ##########################
*$Ifi "%fancymodule%" == "default" $include "./modules/01_fancymodule/default/realization.gms" DONE!
*' @description This is the default realization. It does not work, but is
*' quite fancy
*' @limitations Does not work

*####################### R SECTION START (PHASES) ##############################
*$Ifi "%phase%" == "calculations" $include "./modules/01_fancymodule/default/calculations.gms" DONE!
*' @code

variables
  v01_fancy internal variable (kg)
  vm_exchange exchange variable (kg)
;

equations
  q01_calcme Equation 1;
;


*' @equations

*' Here we have some equations:

 q01_calcme ..
	vm_exchange + 12 =e= v01_fancy;

*' This was fancy, wasn't it?

*' @code Let's add some standard code

pm_global = v01_fancy.l;
*######################## R SECTION END (PHASES) ###############################
*###################### R SECTION END (MODULETYPES) ############################
*$include "./modules/02_crazymodule/module.gms" DONE!
*' @title Crazy Module

*' @description This module is crazy and therefore has two realizations
*' (crazy, right?). However, both are not working at all, so what's the point?

*' @authors Bruce Wayne, Max Mustermann

*###################### R SECTION START (MODULETYPES) ##########################
*$Ifi "%crazymodule%" == "complex" $include "./modules/02_crazymodule/complex/realization.gms" CONDITION WAS NEGATIVE!
*$Ifi "%crazymodule%" == "simple" $include "./modules/02_crazymodule/simple/realization.gms" DONE!

*' @description The simple realization is quite simple, which is nice (and simple)!

*' @limitations It is not really working as it is just an example.

*####################### R SECTION START (PHASES) ##############################
*$Ifi "%phase%" == "calculations" $include "./modules/02_crazymodule/simple/calculations.gms" DONE!
*' @code

variables
  v02_intern(i) internal variable (kg)
;

equations
  q02_equation1 Equation 1 (kg)
  q02_equation2 Equation 2 (kg)
;


*' @equations

*' Here we have some equations:

 q02_equation1 ..
	vm_exchange =e= sum(i,v02_intern(i));

 q02_equation2 ..
    sum(i,v02_intern(i)) =e= 12;

*' This was simple, wasn't it?

*' @code Let's add some standard code

display vm_exchange.l;
*######################## R SECTION END (PHASES) ###############################
*###################### R SECTION END (MODULETYPES) ############################
*$include "./modules/03_Rmodule/module.gms" DONE!
*' @title Module which uses R

*' @description This module uses an R script!

*' @authors Bruce Wayne, Max Mustermann

*###################### R SECTION START (MODULETYPES) ##########################
*$Ifi "%Rmodule%" == "withr" $include "./modules/03_Rmodule/withr/realization.gms" DONE!

*' @description This realization uses R!

*' @limitations It is not really working as it is just an example.

*####################### R SECTION START (PHASES) ##############################
*$Ifi "%phase%" == "calculations" $include "./modules/03_Rmodule/withr/calculations.gms" DONE!
*' @code Let's add running an R script, twice

$onecho > modules_03_Rmodule_withr_run_calculations.R
# a very important calculation, which can not possibly be done in GAMS
1 + 2
$offecho
Execute "Rscript modules_03_Rmodule_withr_run_calculations.R";
$onecho > modules_03_Rmodule_withr_run_calculations.R
# a very important calculation, which can not possibly be done in GAMS
1 + 2
$offecho
Execute.checkErrorLevel "Rscript modules_03_Rmodule_withr_run_calculations.R";
*######################## R SECTION END (PHASES) ###############################
*###################### R SECTION END (MODULETYPES) ############################
*######################## R SECTION END (MODULES) ##############################
$offrecurse

*' @title{extrapage: "settings"}
*' Settings
*' @description{extrapage: "settings"}
*' We might want to move some documentation to a separate page called Settings.
