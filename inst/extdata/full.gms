* This file was written with DUMPOPT=21 at 11/14/22 17:16:28
*
*      INPUT = /home/pflueger/work/gms/inst/dummymodel/main.gms
*       DUMP = /home/pflueger/work/gms/inst/dummymodel/main.dmp
*    RESTART =
*
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


* *** INCLUDE    /home/pflueger/work/gms/inst/dummymodel/core/sets.gms
sets
   i dummy set /x1,x2,x3/
;
* *** INCLUDE    /home/pflueger/work/gms/inst/dummymodel/core/core.gms

#' @code Here we are doing some core calculation stuff

parameter
  pm_global global parameter
;

pm_global = 1;

#' yihaaa!
* *** BATINCLUDE /home/pflueger/work/gms/inst/dummymodel/modules/include.gms
*######################## R SECTION START (MODULES) ############################
* *** INCLUDE    /home/pflueger/work/gms/inst/dummymodel/modules/01_fancymodule/module.gms
*' @title Fancy module
*'
*' @description This is the fancy module. Besides being fancy it does not do
*' anything. And even does not work! But we have a picture of a fancy cat:
*'
*' ![Fancy cat](cat.png){ width=30% }
*' @authors Bruce Wayne

*###################### R SECTION START (MODULETYPES) ##########################
* *** INCLUDE    /home/pflueger/work/gms/inst/dummymodel/modules/01_fancymodule/default/realization.gms
*' @description This is the default realization. It does not work, but is
*' quite fancy
*' @limitations Does not work

*####################### R SECTION START (PHASES) ##############################
* *** INCLUDE    /home/pflueger/work/gms/inst/dummymodel/modules/01_fancymodule/default/calculations.gms
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
* *** INCLUDE    /home/pflueger/work/gms/inst/dummymodel/modules/02_crazymodule/module.gms
*' @title Crazy Module

*' @description This module is crazy and therefore has two realizations
*' (crazy, right?). However, both are not working at all, so what's the point?

*' @authors Bruce Wayne, Max Mustermann

*###################### R SECTION START (MODULETYPES) ##########################
* *** INCLUDE    /home/pflueger/work/gms/inst/dummymodel/modules/02_crazymodule/simple/realization.gms

*' @description The simple realization is quite simple, which is nice (and simple)!

*' @limitations It is not really working as it is just an example.

*####################### R SECTION START (PHASES) ##############################
* *** INCLUDE    /home/pflueger/work/gms/inst/dummymodel/modules/02_crazymodule/simple/calculations.gms
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
* *** INCLUDE    /home/pflueger/work/gms/inst/dummymodel/modules/03_Rmodule/module.gms
*' @title Module which uses R

*' @description This module uses an R script!

*' @authors Bruce Wayne, Max Mustermann

*###################### R SECTION START (MODULETYPES) ##########################
* *** INCLUDE    /home/pflueger/work/gms/inst/dummymodel/modules/03_Rmodule/withr/realization.gms

*' @description This realization uses R!

*' @limitations It is not really working as it is just an example.

*####################### R SECTION START (PHASES) ##############################
* *** INCLUDE    /home/pflueger/work/gms/inst/dummymodel/modules/03_Rmodule/withr/calculations.gms
*' @code Let's add running an R script, twice

Execute "Rscript modules/03_Rmodule/withr/run_calculations.R";
Execute.checkErrorLevel "Rscript modules/03_Rmodule/withr/run_calculations.R";
*######################## R SECTION END (PHASES) ###############################
*###################### R SECTION END (MODULETYPES) ############################
*######################## R SECTION END (MODULES) ##############################
*** END OF DUMP ***
