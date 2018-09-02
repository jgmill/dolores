*_______________________________________________________________________________

*  This is DOLORES(Deployment of locationally optimal renewables), based on MORITS (MOdel for Renewable Integration Through Storage), DIETER's little brother (Dispatch and Investment Evaluation Tool with Endogenous Renewables).
*  It is used for the paper
*  "Doesn't have a name yet"
*  Coded by Andrew McConnell, Jonathan Muehlenpfordt, and Claudia Günther as additions to the original code by by Alexander Zerrahn and Wolf-Peter Schill
*
*
*  Date of this version: August 1st, 2018

*  This tool can do more than what is used in the paper. Any feedback is welcome.

*-------------------------------------------------------------------------------

*  This code is licensed under an MIT license.
*  Copyright (c) 2018 Alexander Zerrahn <azerrahn@diw.de>, Wolf-Peter Schill <wschill@diw.de>
*  SPDX-License-Identifier: MIT
*  Permission is hereby granted, free of charge, to any person obtaining a
*  copy of this software and associated documentation files (the "Software"),
*  to deal in the Software without restriction, including without limitation
*  the rights to use, copy, modify, merge, publish, distribute, sublicense,
*  and/or sell copies of the Software, and to permit persons to whom the
*  Software is furnished to do so, subject to the following conditions:

*  The above copyright notice and this permission notice shall be included in
*  all copies or substantial portions of the Software.

*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
*  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
*  IN THE SOFTWARE.

*_______________________________________________________________________________




* ------------- Base year ------------------------------------------------------
* Select year between 2012 and 2016:

$setglobal base_year "2014"

* ------------- Number of Regions ----------------------------------------------

* set number of regions which should correspond with the import file

$setglobal nregions "6"
Scalar nregions_scalar /6/ ;

* ------------- Name Import File -----------------------------------------------

* add the specifications regarding model run with the format "Region#ofgridpointsEXCELcolumn"
*                                                             eg "Germany150EU"

$setglobal modelrun "Germany_cap_from_lit_6G"

* ------------- Set import Excel -----------------------------------------------

* mark with a star to turn off excel import
* if only wanting to create a gdx file add a * to modelkill

$setglobal offXcel "*"
$setglobal modelkill ""


* ------------- Set EXCEL furthest right column

* add the alphabetic column name of the column which is furthest to right on excel to speed up data import

$setglobal colindex "I"

* ------------- Choose method to bind renewable capacities per region ----------

*   a star indicates it is on, only pick one

$setglobal onmin_res_reg  ""
$setglobal onmax_res_reg  ""
$setglobal equal_capacity ""
$setglobal max_capacity "*"

* ------------- Set Cluster Run ------------------------------------------------

* Set star to turn on or off cluster, * means cluster off

$setglobal offcluster "*"



* ------------- OBJECTIVE ------------------------------------------------------
* Select an objective function by setting an asterisk (*)
* Storage energy minimization (not used in the paper)?
$setglobal obj_min_sto_e ""

* Minimization of cost for storage only (not used in the paper)?
$setglobal obj_min_cost_sto ""

* Minimization of cost for total energy system?
$setglobal obj_min_cost_total "*"

* ------------- STORAGE --------------------------------------------------------
* Select if storage may only charge renewables or also conventional electricity by setting an asterisk (*)
* Storage only for renewables?
$setglobal sto_res_only "*"

* Storage also for conventional energy (not used in the paper)?
$setglobal sto_anything ""

* ------------- CONTEMPORANEOUS STORING IN AND OUT RESTRICTION -----------------
* Select if contemporaneous storage loading and discharging should be penalized by setting an asterisk (*)
* (only relevant for minimization of storage energy or storage costs)
$setglobal sto_inout_penalty ""

* ------------- Max curtailment or max loss? ----------------------------------
* Select if storage losses should also be considered in the curtailment restriction by setting an asterisk (*)
$setglobal max_loss ""
$setglobal max_curtailment ""

* ------------- P2X ------------------------------------------------------------
* Select if additional flexible power-to-x demand should be considered by setting an asterisk (*)
$setglobal p2x ""
* Auxiliary string (do not change):
$if "%p2x%" == "" $setglobal not_p2x "*"
$if "%p2x%" == "*" $setglobal not_p2x ""


* ------------- Sanity checks --------------------------------------------------
$if not "%obj_min_sto_e%%obj_min_cost_sto%%obj_min_cost_total%" == "*" $abort Please select obj_min_sto_e, obj_min_cost_sto or obj_min_cost_total!
$if "%sto_res_only%" == "%sto_anything%" $abort Please select sto_res_only or sto_anything!
$if "%max_loss%" == "max_curtailment" $abort Please select max_loss or max_curtailment!
$if "%nregions%" == "" $abort Enter Regions number
$if "%modelrun%" == "" $abort Enter Model Run

* ------------- Additional Formatting Organisation -----------------------------

*add additional model naming

$setglobal maxtype "noCap"


$if "%onmin_res_reg%" == "*" $set maxtype "mincap"
$if "%equal_capacity%" == "*" $set maxtype "EqualCap"
$if "%max_capacity%" == "*" $set maxtype "MaxCap"


* cluster on/ off

$setglobal backslash "\"

$if "%offcluster%" == "" $set backslash "/"
$if "%offcluster%" == "" $set offXcel "*"

* Auto set of input file

$setglobal inputfile "data%backslash%%modelrun%_upload_data"

* Auto set of output file

$setglobal outputfile "results%backslash%%modelrun%_%maxtype%_results"




*_______________________________________________________________________________



Sets
h                Hours                                   /h1*h8760/
ct               Dispatchable Technologies               /base, peak/
res              Renewable technologies                  /solar, wind/
sto              Storage technolgies                     /storage/
p2x              Power-to-x technologies                 /p2x/
r                Regions                                 /r1*r%nregions%/


%obj_min_cost_total%loop_curt        Solution loop for different curtailment or energy loss shares in per mille
%obj_min_cost_total%/0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 250, 300, 350, 400, 450, 500/

%obj_min_cost_total%$ontext
loop_curt        Solution loop for different curtailment or energy loss shares in per mille /1000/
$ontext
$offtext

loop_res_share   Solution loop for different shares of renewables
/20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100/

loop_p2x_power   Solution loop for different installed P2X capacities (MW)
/0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000/

loop_p2x_flh     Solution loop for different P2X full load hours
/0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000/

year             Base years
/2012, 2013, 2014, 2015, 2016/

alias(r,rr)
;

*-------------------------------------------------------------------------------

Variables
Z                        Objective
;

Positive variables
G_CON(ct,h)              Generation of conventional electricity
G_RENEWABLE(res,h,r)       Generation of renewable energy
CU(res,h,r)                Curtailment of renewable energy
N_RENEWABLE(res,r)         Capacities: renewable energy
N_CON(ct)                Capacities: conventional energy
N_STO_E(sto)             Capacities: storage energy
N_STO_P(sto)             Capacities: storage power
STO_L(sto,h)             Storage level
STO_IN(sto,h)            Storage intake
STO_IN_R(sto,res,h,r)    Storage intake regional level
STO_OUT(sto,h)           Storage generation
P2X_IN(p2x,h)            Power-to-x intake
;

Parameters
phi_sto_ini(sto)         Level of storage in first and last period of the analysis
eta_sto_in(sto)          Efficiency: storage in
eta_sto_out(sto)         Efficiency: storage out
phi_min_res              Minimum share of renewable electricity in net consumption
phi_min_res_region       Minimum share of renewable electricity per region, per tech in net consumption
phi_max_res_region       Maximum share of renewable electricity per region, per tech in net consumption
phi_max_curt             Maximum share of renewable electricity curtailed over the year
Max_RegCap(r)            Gridpoints per region
Max_RegCap_upload(r)     Upload gridpoints per region
Area_per_Gridpoint       How many square km are represented by one gridpoint
Area_per_Res(res)        Area required per installed MWH capacity per variable renewable
d(h)                     Electricity demand
d_upload(h,year)         Electricity demand - upload parameter
phi_res(res,h,r)         Hourly capacity factor renewable energy
phi_solar_upload(h,r,year) Hourly capacity factor renewable energy - upload parameter
phi_wind_upload(h,r,year)  Hourly capacity factor renewable energy - upload parameter
c_i_sto_e(sto)           Cost: investment into storage energy
c_i_sto_p(sto)           Cost: investment into storage power
c_i_res(res)             Cost: investment into renewable capacity
c_i_con(ct)              Cost: investment into conventional capacity
c_var_con(ct)            Cost: variable generation costs conventional energy
c_var_sto(sto)           Cost: variable generation costs storage
penalty                  Penalty term
share_solar(year)        Share of solar energy among total variable renewable energy
share_wind(year)         Share of wind energy among total variable renewable energy
p2x_power(p2x)           Power-to-x intake power
p2x_flh(p2x)             Power-to-x full-load hours
;

*-------------------------------------------------------------------------------

phi_sto_ini(sto) = 0.5 ;
eta_sto_in(sto) = 0.81 ;
eta_sto_out(sto) = 0.926 ;
penalty = 0 ;
phi_min_res_region = 0.05;
phi_max_res_region = 0.30;

*Historical energy shares of wind and solar PV in base years
*Source: OPSD (2017), see upload_data.xlsx
share_solar('2012') = 0.378304182 ;
share_solar('2013') = 0.385519442 ;
share_solar('2014') = 0.390535228 ;
share_solar('2015') = 0.310511627 ;
share_solar('2016') = 0.310454625 ;

share_wind('2012') = 0.621795818 ;
share_wind('2013') = 0.614480558 ;
share_wind('2014') = 0.609464772 ;
share_wind('2015') = 0.689488373 ;
share_wind('2016') = 0.689545375 ;

%sto_inout_penalty%$ontext
penalty = 1000000 ;
$ontext
$offtext

* Values assumed for 2035 - base analogous to lignite, peak analogous to OCGT efficient
* Source:  W.-P. Schill, A. Zerrahn, F. Kunz, 2017. Prosumage of solar electricity: pros, cons, and the system perspective.
*          Economic of Energy & Environmental Policy 6(1), 7-31, and the sources mentioned therein.
*          https://doi.org/10.5547/2160-5890.6.1.wsch
*$ontext
c_i_sto_e(sto) = 5418.14 ;
c_i_sto_p(sto) = 50995.48 ;
c_i_res('solar') = 60526.64 ;
c_i_res('wind') =  108869.81 ;
c_i_con('base') = 102393.68 ;
c_i_con('peak') = 47840.27 ;
c_var_con('base') = 31.03 ;
c_var_con('peak') = 78.36 ;
c_var_sto(sto) = 0.5 ;
*$offtext

* Estimates taken from XXXXX, elaborated upon in paper

Area_per_Gridpoint = 357000 / 140;
Area_per_Res('solar') = 1.21843;
Area_per_Res('wind') = 1;

*------------------------------ Upload Data ------------------------------------------------

* remember to change the excel read in dimensions to match with colindex

$onecho >%inputfile%.tmp

par=d_upload             rng=demand!a3:f8763     rdim=1 cdim=1
par=phi_solar_upload     rng=solar!a3:%colindex%8764      rdim=1 cdim=2
par=phi_wind_upload      rng=wind!a3:%colindex%8764       rdim=1 cdim=2
par=Max_RegCap_upload    rng=maxcap!b3:%colindex%4     rdim=0 cdim=1

$offecho


%offXcel%$call "gdxxrw %inputfile%.xlsx squeeze=N @%inputfile%.tmp  o=%inputfile%.gdx  ";
$GDXin %inputfile%.gdx
$load d_upload phi_solar_upload, phi_wind_upload
$load Max_RegCap_upload
;

* Initialize base year
phi_res('solar',h,r) = phi_solar_upload(h,r,'%base_year%') ;
phi_res('wind',h,r) = phi_wind_upload(h,r,'%base_year%') ;
d(h) = d_upload(h,'%base_year%') ;
Max_RegCap(r) = Max_RegCap_upload(r);
*$stop

*_______________________________________________________________________________


$if "%modelkill%" == "*"  $abort Check GDX upload

Equations
objective                       Objective function
energy_balance                  Energy balance (market clearing)
renewable_generation            Use of renewable energy generation
renewable_generation_region     Use of renewable energy generation with regions
storage_region                  Regional storage infeed
minRES                          Constraint on minimum share of renewables
minRESREG                       Constraint on minimum share of renewables per region
equal_cap                       All regions must have the same renewable share per region
maxRESREG                       Maximum capacity constrain per region per renewables
maximum_curtailment             Constraint on maximum share of renewables curtailment
maximum_loss                    Constraint on maximum share of renewable energy loss

maximum_generation_con   Capacity constraint - conventional generation
stolev_start_end         Storage: storage level in the first and last period
stolev                   Storage: storage level dynamics
stolev_max               Storage: capacity constraint on maximum energy
maxin_power              Storage: capacity constraint on maximum power - storing in
maxout_power             Storage: capacity constraint on maximum power - storing out

maxin_p2x                Power-to-x: constraint on maximum power - energy intake
flh_p2x                  Power-to-x: full-load hours
;

*-------------------------------------------------------------------------------
** OBJECTIVES

%obj_min_sto_e%$ontext
objective..
         Z =E= sum( sto , N_STO_E(sto) ) + sum( (sto,h) , penalty * STO_IN(sto,h) )
;
$ontext
$offtext

%obj_min_cost_sto%$ontext
objective..
         Z =E= sum( sto , c_i_sto_e(sto) * N_STO_E(sto) + c_i_sto_p(sto) * N_STO_P(sto) ) + sum( (sto,h) , penalty * STO_IN(sto,h) )
;
$ontext
$offtext

%obj_min_cost_total%$ontext
objective..
         Z =E= sum( sto , c_i_sto_e(sto) * N_STO_E(sto) + c_i_sto_p(sto) * N_STO_P(sto) )
         + sum( (res,r) , c_i_res(res) * N_RENEWABLE(res,r) )
         + sum( ct , c_i_con(ct) * N_CON(ct) )
         + sum( (ct,h) , c_var_con(ct) * G_CON(ct,h) )
         + sum( (sto,h) , c_var_sto(sto) * (STO_IN(sto,h) + STO_OUT(sto,h)) )
;
$ontext
$offtext

*-------------------------------------------------------------------------------
** ENERGY BALANCE AND RENEWABLES USE

%sto_res_only%$ontext
energy_balance(h)..
         sum( ct , G_CON(ct,h)) + sum( (res,r) , G_RENEWABLE(res,h,r)) + sum( sto , STO_OUT(sto,h))
         =E= d(h)
;


renewable_generation_region(res,h,r)..
         phi_res(res,h,r) * N_RENEWABLE(res,r)
         =E= G_RENEWABLE(res,h,r) + CU(res,h,r) +  sum( sto , STO_IN_R(sto,res, h,r))
;


storage_region(sto,h)..
         STO_IN(sto,h)
         =E= sum( (res,r) , STO_IN_R(sto,res, h,r))
;

renewable_generation(res,h)..
         sum( r , (phi_res(res,h,r) * N_RENEWABLE(res,r)))
         =E= sum( r , (G_RENEWABLE(res,h,r) + CU(res,h,r))) + sum( sto , STO_IN(sto,h))
%not_p2x%        + sum( p2x , P2X_IN(p2x,h))
;
$ontext
$offtext

%sto_anything%$ontext
energy_balance(h)..
          sum( ct , G_CON(ct,h)) + sum( (res,r) , G_RENEWABLE(res,h,r)) + sum( sto , STO_OUT(sto,h)) =E= d(h) + sum( sto , STO_IN(sto,h) )
;

renewable_generation(res,h,r)..
         phi_res(res,h,r) * N_RENEWABLE(res,r) =E= G_RENEWABLE(res,h,r) + CU(res,h,r)

;


storage_region(sto,h)..
         STO_IN(sto,h)
         =E= sum( (res,r) , STO_IN_R(sto,res, h,r))
;


renewable_generation_region(res,h,r)..
         phi_res(res,h,r) * N_RENEWABLE(res,r)
         =E= G_RENEWABLE(res,h,r) + CU(res,h,r) +  sum( sto , STO_IN_R(sto,res, h,r))
;

$ontext
$offtext

*-------------------------------------------------------------------------------
** MINIMUM AND MAXIMUM SHARES

minRES..
         sum( (ct,h) , G_CON(ct,h) ) =L= (1-phi_min_res) * sum( h , d(h) )
;

%onmin_res_reg%$ontext
minRESREG(res,r)..
        N_RENEWABLE(res,r) =g= (phi_min_res_region)*sum(rr , N_RENEWABLE(res,rr))
;
$ontext
$offtext

%onmax_res_reg%$ontext
maxRESREG(res,r)..
        N_RENEWABLE(res,r) =l= (phi_max_res_region)*sum(rr , N_RENEWABLE(res,rr))
;
$ontext
$offtext

%equal_capacity%$ontext
equal_cap(res,r)..
        N_RENEWABLE(res,r) =E= sum( rr, N_RENEWABLE(res,rr)) / nregions_scalar
;
$ontext
$offtext

%max_capacity%$ontext
maxRESREG(res,r)..
         N_RENEWABLE(res,r) =L= Max_RegCap(r) / Area_per_Res(res) * Area_per_Gridpoint
;
$ontext
$offtext



maximum_curtailment..
         sum( (res,h,r) , CU(res,h,r) ) =L= phi_max_curt * sum( (res,h,r) , phi_res(res,h,r) * N_RENEWABLE(res,r) )
;

maximum_loss..
         sum( (res,h,r) , CU(res,h,r) ) + sum( (sto,h) , STO_IN(sto,h) - STO_OUT(sto,h) )
                 =L= phi_max_curt * sum( (res,h,r) , phi_res(res,h,r) * N_RENEWABLE(res,r) )
;

*-------------------------------------------------------------------------------
** CONVENTIONAL GENERATION

maximum_generation_con(ct,h)..
         G_CON(ct,h) =L= N_CON(ct)
;

*-------------------------------------------------------------------------------
** STORAGE

stolev(sto,h)$( ord(h) > 1 )..
         STO_L(sto,h) =E= STO_L(sto,h-1) + STO_IN(sto,h) * eta_sto_in(sto) - STO_OUT(sto,h)/eta_sto_out(sto)
;

stolev_start_end(sto)..
         STO_L(sto,'h1') =E= STO_L(sto,'h8760')
;

stolev_max(sto,h)..
        STO_L(sto,h) =L= N_STO_E(sto)
;

maxin_power(sto,h)..
         STO_IN(sto,h) =L= N_STO_P(sto)
;

maxout_power(sto,h)..
         STO_OUT(sto,h) =L= N_STO_P(sto)
;

*-------------------------------------------------------------------------------
** POWER-TO-X

maxin_p2x(p2x,h)..
         P2X_IN(p2x,h) =L= p2x_power(p2x)
;

flh_p2x(p2x)..
         sum( h , P2X_IN(p2x,h)) =E= p2x_flh(p2x) * p2x_power(p2x)
;

*_______________________________________________________________________________


Model morits_min_sto_e /
objective

energy_balance
renewable_generation
renewable_generation_region
minRES

%max_loss%$ontext
maximum_loss
$ontext
$offtext

%onmin_res_reg%$ontext
minRESREG
$ontext
$offtext

%onmax_res_reg%$ontext
maxRESREG
$ontext
$offtext

%equal_capacity%$ontext
equal_cap
$ontext
$offtext

%max_capacity%$ontext
maxRESREG
$ontext
$offtext

%max_curtailment%$ontext
maximum_curtailment
$ontext
$offtext

stolev_start_end
stolev
stolev_max

%not_p2x%maxin_p2x
%not_p2x%flh_p2x
/
;

Model morits_min_cost_sto /
objective

energy_balance
renewable_generation
renewable_generation_region
storage_region
minRES

%max_loss%$ontext
maximum_loss
$ontext
$offtext

%onmin_res_reg%$ontext
minRESREG
$ontext
$offtext

%equal_capacity%$ontext
equal_cap
$ontext
$offtext

%max_capacity%$ontext
maxRESREG
$ontext
$offtext

%max_curtailment%$ontext
maximum_curtailment
$ontext
$offtext

stolev_start_end
stolev
stolev_max
maxin_power
maxout_power

%not_p2x%maxin_p2x
%not_p2x%flh_p2x
/
;

Model morits_min_cost_all /
objective

energy_balance
renewable_generation_region
renewable_generation
minRES
maximum_generation_con

%onmin_res_reg%$ontext
minRESREG
$ontext
$offtext

%equal_capacity%$ontext
equal_cap
$ontext
$offtext

%max_capacity%$ontext
maxRESREG
$ontext
$offtext

%max_loss%$ontext
maximum_loss
$ontext
$offtext

%max_curtailment%$ontext
maximum_curtailment
$ontext
$offtext

stolev_start_end
stolev
stolev_max
maxin_power
maxout_power

%not_p2x%maxin_p2x
%not_p2x%flh_p2x
/


*_______________________________________________________________________________


options
optcr = 0.00
reslim = 10000000
lp = cplex
mip = cplex
nlp = conopt
dispwidth = 15
limrow = 0
limcol = 0
solprint = off
sysout = off
optcr = 1e-3
optca = 10
;

$onecho > cplex.opt
lpmethod 4
threads 4
epgap 1e-3
epagap 10
parallelmode -1
$offecho

morits_min_sto_e.OptFile = 1;
morits_min_sto_e.holdFixed = 1 ;

morits_min_cost_sto.OptFile = 1;
morits_min_cost_sto.holdFixed = 1 ;

morits_min_cost_all.OptFile = 1;
morits_min_cost_all.holdFixed = 0 ;


*_______________________________________________________________________________

* Preparation of GUSS tool for scenario analysis
phi_min_res = eps ;
phi_max_curt = eps ;
p2x_power(p2x) = eps ;
p2x_flh(p2x) = eps ;

$eval superscencount 1000

Set
modelstats       model stats collection          /modelstat, solvestat, resusd/
superscen        Scenarios                       /scen1*scen%superscencount%/
%p2x%map(superscen,loop_res_share,loop_curt)     /#superscen:(#loop_res_share.#loop_curt)/
%not_p2x%map(superscen,loop_res_share,loop_curt,loop_p2x_power,loop_p2x_flh) /#superscen:(#loop_res_share.#loop_curt.#loop_p2x_power.#loop_p2x_flh)/
;

Set
scen(superscen);
%p2x%scen(superscen) = yes$( sum((loop_res_share,loop_curt) , map(superscen,loop_res_share,loop_curt)) )    ;
%not_p2x%scen(superscen) = yes$( sum((loop_res_share,loop_curt,loop_p2x_power,loop_p2x_flh) , map(superscen,loop_res_share,loop_curt,loop_p2x_power,loop_p2x_flh)) )    ;

Parameters
gussoptions                              /Logoption 2, Optfile 1, Skipbasecase 1/
modstats(superscen, modelstats)
min_res
max_curt
p2x_power_scen
p2x_flh_scen
;

%p2x%min_res(scen) = sum( (loop_res_share,loop_curt)$map(scen,loop_res_share,loop_curt) , loop_res_share.val/100 ) ;
%p2x%max_curt(scen) = sum( (loop_res_share,loop_curt)$map(scen,loop_res_share,loop_curt) , loop_curt.val/1000 ) ;
%not_p2x%min_res(scen) = sum( (loop_res_share,loop_curt,loop_p2x_power,loop_p2x_flh)$map(scen,loop_res_share,loop_curt,loop_p2x_power,loop_p2x_flh) , loop_res_share.val/100 ) ;
%not_p2x%max_curt(scen) = sum( (loop_res_share,loop_curt,loop_p2x_power,loop_p2x_flh)$map(scen,loop_res_share,loop_curt,loop_p2x_power,loop_p2x_flh) , loop_curt.val/1000 ) ;
%not_p2x%p2x_power_scen(scen,p2x) = sum( (loop_res_share,loop_curt,loop_p2x_power,loop_p2x_flh)$map(scen,loop_res_share,loop_curt,loop_p2x_power,loop_p2x_flh) , loop_p2x_power.val ) ;
%not_p2x%p2x_flh_scen(scen,p2x) = sum( (loop_res_share,loop_curt,loop_p2x_power,loop_p2x_flh)$map(scen,loop_res_share,loop_curt,loop_p2x_power,loop_p2x_flh) , loop_p2x_flh.val ) ;

Parameters
marginal_minRES(superscen)
marginal_energy_balance(superscen,h)

lev_Z(superscen)
lev_G_CON(superscen,ct,h)
lev_G_RENEWABLE(superscen,res,h,r)
lev_CU(superscen,res,h,r)
lev_STO_IN(superscen,sto,h)
lev_STO_OUT(superscen,sto,h)
lev_STO_L(superscen,sto,h)
lev_N_RENEWABLE(superscen,res,r)
lev_N_CON(superscen,ct)
lev_N_STO_E(superscen,sto)
lev_N_STO_P(superscen,sto)
lev_P2X_IN(superscen,p2x,h)

marg_N_RENEWABLE(superscen,res,r)
marg_N_STO_E(superscen,sto)
marg_N_STO_P(superscen,sto)
marg_N_CON(superscen,ct)
;


* Definition of dictionary set for GUSS tool
Set dict(*,*,*) /
scen             .scenario       .''
gussoptions      .opt            .modstats

phi_min_res      .param          .min_res

%max_loss%%max_curtailment%$ontext
phi_max_curt     .param          .max_curt
$ontext
$offtext

minRES           .marginal       .marginal_minRES

%not_p2x%p2x_power       .param          .p2x_power_scen
%not_p2x%p2x_flh         .param          .p2x_flh_scen

Z                .level          .lev_Z
G_CON            .level          .lev_G_CON
G_RENEWABLE      .level          .lev_G_RENEWABLE
CU               .level          .lev_CU
STO_IN           .level          .lev_STO_IN
STO_OUT          .level          .lev_STO_OUT
STO_L            .level          .lev_STO_L

N_RENEWABLE      .level          .lev_N_RENEWABLE
N_STO_E          .level          .lev_N_STO_E

N_RENEWABLE      .marginal       .marg_N_RENEWABLE
N_STO_E          .marginal       .marg_N_STO_E

%not_p2x%P2X_IN  .level          .lev_P2X_IN

%obj_min_cost_sto%$ontext
N_STO_P          .level          .lev_N_STO_P
N_STO_P          .marginal       .marg_N_STO_P
$ontext
$offtext

%obj_min_cost_total%$ontext
N_CON            .level          .lev_N_CON
N_STO_P          .level          .lev_N_STO_P

N_STO_P          .marginal       .marg_N_STO_P
N_CON            .marginal       .marg_N_CON
$ontext
$offtext
/
;

*_______________________________________________________________________________
*Solve

%obj_min_sto_e%$ontext
solve morits_min_sto_e using lp min Z scenario dict;
$ontext
$offtext

%obj_min_cost_sto%$ontext
solve morits_min_cost_sto using lp min Z scenario dict;
$ontext
$offtext

%obj_min_cost_total%$ontext
solve morits_min_cost_all using lp min Z scenario dict;
$ontext
$offtext

*$stop

*_______________________________________________________________________________
*Reporting

* Parameters for the report file

Scalar eps_rep_rel Sensitivity for shares defined between 0 and 1        / 1e-4 / ;
Scalar eps_rep_abs Sensitivity for absolute values - e.g. hourly         / 1e-2 / ;
Scalar eps_rep_ins Sensitivity for absolute values - e.g. installed MW   / 1 /    ;

Parameter
report
report_tech
report_tech_r
report_hours
report_hours_r
report_cost
report_marginal
report_marginal_r
;

%p2x%$setglobal reportset "loop_res_share,loop_curt"
%not_p2x%$setglobal reportset "loop_res_share,loop_curt,loop_p2x_power,loop_p2x_flh"

*-------------------------------------------------------------------------------
*Report

         report('model status',%reportset%) = sum(scen$(map(scen,%reportset%)) , modstats(scen, 'modelstat')) ;
         report('solve time',%reportset%) = sum(scen$(map(scen,%reportset%)) , modstats(scen, 'resusd')) ;
         report('obj value',%reportset%) = sum(scen$(map(scen,%reportset%)) , lev_Z(scen) ) ;
         report('net energy demand',%reportset%) = sum( scen$(map(scen,%reportset%)) , sum( h , d(h) )) ;
         report('renewables available',%reportset%) = sum( scen$(map(scen,%reportset%)) , sum( (res,h,r) , phi_res(res,h,r) * lev_N_RENEWABLE(scen,res,r) )) ;
         report('renewables used directly',%reportset%) = sum( scen$(map(scen,%reportset%)) , sum( (res,h,r) , lev_G_RENEWABLE(scen,res,h,r) )) / report('renewables available',%reportset%)  ;
*sum over regions ok?
*         report('renewables curtailed absolute',%reportset%) = sum((res,h,r), sum(scen$(map(scen,%reportset%)) , lev_CU(scen,res,h,r))) ;
*         report('renewables curtailed relative',%reportset%)$(sum((res,h,r), sum(scen$(map(scen,%reportset%)) , phi_res(res,h,r) * lev_N_RENEWABLE(scen,res,r) ) ) > eps_rep_abs*card(res,r)*card(h)) = sum((res,h,r), sum(scen$(map(scen,%reportset%)) , lev_CU(scen,res,h,r) )) / sum((res,h,r), sum(scen$(map(scen,%reportset%)) , phi_res(res,h,r) * lev_N_RENEWABLE(scen,res,r)  )) ;

         report('renewables stored',%reportset%) = sum((sto,h), sum(scen$(map(scen,%reportset%)) , lev_STO_IN(scen,sto,h))) / report('renewables available',%reportset%) ;
%not_p2x%report('renewables used for p2x',%reportset%) = sum((p2x,h), sum(scen$(map(scen,%reportset%)) , lev_P2X_IN(scen,p2x,h))) / report('renewables available',%reportset%) ;
         report('renewables lost in storage',%reportset%) = sum((sto,h), sum(scen$(map(scen,%reportset%)) , lev_STO_IN(scen,sto,h) - lev_STO_OUT(scen,sto,h))) / report('renewables available',%reportset%)  ;
         report('renewables lost total',%reportset%) = (sum((sto,h), sum(scen$(map(scen,%reportset%)) , lev_STO_IN(scen,sto,h) - lev_STO_OUT(scen,sto,h))) + sum((res,h,r), sum(scen$(map(scen,%reportset%)) , lev_CU(scen,res,h,r)))) / report('renewables available',%reportset%)  ;
         report('"efficiency"',%reportset%) = 1 - report('renewables lost total',%reportset%) ;
         report('renshare total',%reportset%) = sum( h, sum(scen$(map(scen,%reportset%)) , sum( (res,r) , lev_G_RENEWABLE(scen,res,h,r)) + sum( sto , lev_STO_OUT(scen,sto,h)) )) / sum( scen$(map(scen,%reportset%)) , sum( h , d(h) )) ;    ;
         report('hours with contemp storing in and out',%reportset%) = sum(scen$(map(scen,%reportset%)) , sum( (sto,h)$(lev_STO_IN(scen,sto,h) > 0 AND lev_STO_OUT(scen,sto,h) > 0) , 1 )) ;

         report_marginal_r('capacity renewables',%reportset%,res,r) =  sum( scen$(map(scen,%reportset%)) , marg_N_RENEWABLE(scen,res,r)) ;
         report_marginal('capacity storage energy',%reportset%,sto) =  sum( scen$(map(scen,%reportset%)) , marg_N_STO_E(scen,sto)) ;

         report_hours('demand',%reportset%,'demand',h) = d(h) ;
         report_hours('generation conventional',%reportset%,ct,h) =  sum(scen$(map(scen,%reportset%)) , lev_G_CON(scen,ct,h) ) ;

         report_hours_r('generation renewable',%reportset%,res,h,r) = sum(scen$(map(scen,%reportset%)) , lev_G_RENEWABLE(scen,res,h,r) ) ;
         report_hours_r('curtailment of fluct res',%reportset%,res,h,r) =  sum(scen$(map(scen,%reportset%)) , lev_CU(scen,res,h,r) ) ;

         report_hours('generation storage',%reportset%,sto,h) =  sum(scen$(map(scen,%reportset%)) , lev_STO_OUT(scen,sto,h)   ) ;
         report_hours('storage loading',%reportset%,sto,h) =  sum(scen$(map(scen,%reportset%)) , lev_STO_IN(scen,sto,h)  ) ;
         report_hours('storage level',%reportset%,sto,h) =  sum(scen$(map(scen,%reportset%)) , lev_STO_L(scen,sto,h)  ) ;
%not_p2x%report_hours('p2x loading',%reportset%,p2x,h) =  sum(scen$(map(scen,%reportset%)) , lev_P2X_IN(scen,p2x,h)  ) ;
         report_hours('residual load (before curt and sto)',%reportset%,'demand',h) =  sum(scen$(map(scen,%reportset%)) , d(h) - sum( (res,r) , phi_res(res,h,r) * lev_N_RENEWABLE(scen,res,r)) ) / 1000 ;
         report_hours('residual load (after curt)',%reportset%,'demand',h) =  sum(scen$(map(scen,%reportset%)) , d(h) - sum( (res,r) , phi_res(res,h,r) * lev_N_RENEWABLE(scen,res,r) - lev_CU(scen,res,h,r) ) ) / 1000 ;
         report_hours('residual load (after curt and sto)',%reportset%,'demand',h) =  sum(scen$(map(scen,%reportset%)) , d(h) - sum( (res,r) , phi_res(res,h,r) * lev_N_RENEWABLE(scen,res,r) - lev_CU(scen,res,h,r)) - sum( sto , lev_STO_OUT(scen,sto,h) - lev_STO_IN(scen,sto,h))  ) / 1000 ;
%not_p2x%report_hours('residual load (after p2x)',%reportset%,'demand',h) =  sum(scen$(map(scen,%reportset%)) , d(h) - sum( (res,r) , phi_res(res,h,r) * lev_N_RENEWABLE(scen,res,r)) + sum( p2x , lev_P2X_IN(scen,p2x,h)) ) / 1000 ;
%not_p2x%report_hours('residual load (after curt and sto and p2x)',%reportset%,'demand',h) =  sum(scen$(map(scen,%reportset%)) , d(h) - sum( (res,r) , phi_res(res,h,r) * lev_N_RENEWABLE(scen,res,r) - lev_CU(scen,res,h,r)) - sum( sto , lev_STO_OUT(scen,sto,h) - lev_STO_IN(scen,sto,h)) + sum( p2x , lev_P2X_IN(scen,p2x,h)) ) / 1000 ;

         report_tech_r('capacities renewable GW',%reportset%,res,r) = 0 + sum( scen$(map(scen,%reportset%)) , lev_N_RENEWABLE(scen,res,r)) / 1000 ;
         report_tech_r('renewables curtailed absolute',%reportset%,res,r) =  sum(h, sum(scen$(map(scen,%reportset%)) , lev_CU(scen,res,h,r) )) ;
         report_tech_r('renewables curtailed relative',%reportset%,res,r)$(report_tech_r('renewables curtailed absolute',%reportset%,res,r) AND sum(h, sum(scen$(map(scen,%reportset%)) , lev_G_RENEWABLE(scen,res,h,r) ) ) + sum(h, sum(scen$(map(scen,%reportset%)) , lev_CU(scen,res,h,r))) > card(h)*eps_rep_abs ) =  sum(h, sum(scen$(map(scen,%reportset%)) , lev_CU(scen,res,h,r)  ))/( sum(h , sum(scen$(map(scen,%reportset%)) , phi_res(res,h,r)*(lev_N_RENEWABLE(scen,res,r) ))) ) ;

         report_tech('capacities storage GWh',%reportset%,sto) =  sum( scen$(map(scen,%reportset%)) , lev_N_STO_E(scen,sto)) / 1000 ;
%not_p2x%report_tech('check flh p2x',%reportset%,p2x) =  sum( scen$(map(scen,%reportset%)) , sum( h , lev_P2X_IN(scen,p2x,h)) / p2x_power_scen(scen,p2x) ) ;


*sum over regions ok?   maybe 'cost renewables in bn euro' also per region?
         report_cost('cost renewables in bn euro',%reportset%,res) = sum(scen$(map(scen,%reportset%)) , c_i_res(res) * sum( r, lev_N_RENEWABLE(scen,res,r))) / 1e9 ;
         report_cost('cost storage energy in bn euro',%reportset%,sto) = sum(scen$(map(scen,%reportset%)) , c_i_sto_e(sto) * lev_N_STO_E(scen,sto)) / 1e9 ;

%obj_min_sto_e%$ontext
         report_tech('implicit capacities storage GW',%reportset%,sto) =  0 + sum( scen$(map(scen,%reportset%)) , max( smax( h , lev_STO_IN(scen,sto,h)), smax( h, lev_STO_OUT(scen,sto,h)))) / 1000 ;
         report_tech('implicit capacities conventional GW',%reportset%,ct) = 0 + sum( scen$(map(scen,%reportset%)) , smax( h , lev_G_CON(scen,ct,h)) ) / 1000 ;
         report_tech('implicit EP ratio storage',%reportset%,sto) = report_tech('capacities storage GWh',%reportset%,sto) / report_tech('implicit capacities storage GW',%reportset%,sto) ;
         report_tech('implicit FLH storage',%reportset%,sto) = sum( scen$(map(scen,%reportset%)) , sum( h , lev_STO_OUT(scen,sto,h))) / (1000 * report_tech('implicit capacities storage GW',%reportset%,sto))   ;

         report_cost('cost storage power (implicit) in bn euro',%reportset%,sto) = sum(scen$(map(scen,%reportset%)) , c_i_sto_p(sto) * 1000 * report_tech('implicit capacities storage GW',%reportset%,sto)) / 1e9 ;
         report_cost('cost investment conventional (implicit) in bn euro in bn euo',%reportset%,ct) = sum(scen$(map(scen,%reportset%)) , c_i_con(ct) * 1000 * report_tech('implicit capacities conventional GW',%reportset%,ct)) / 1e9 ;
         report_cost('cost operation conventional (implicit) in bn euro',%reportset%,ct) = sum(scen$(map(scen,%reportset%)) , sum( h , c_var_con(ct) * lev_G_CON(scen,ct,h))) / 1e9 ;
         report_cost('cost total (implicit) in bn euro',%reportset%,'total') = sum( res , report_cost('cost renewables in bn euro',%reportset%,res)) + sum( sto , report_cost('cost storage energy in bn euro',%reportset%,sto) + report_cost('cost storage power (implicit) in bn euro',%reportset%,sto)) + sum( ct , report_cost('cost investment conventional (implicit) in bn euro in bn euo',%reportset%,ct) + report_cost('cost operation conventional (implicit) in bn euro',%reportset%,ct)) ;
$ontext
$offtext

%obj_min_cost_sto%$ontext
         report_tech('capacities storage GW',%reportset%,sto) =  sum( scen$(map(scen,%reportset%)) , lev_N_STO_P(scen,sto)) / 1000 ;
         report_tech('implicit capacities conventional GW',%reportset%,ct) = 0 + sum( scen$(map(scen,%reportset%)) , smax( h , lev_G_CON(scen,ct,h)) ) / 1000 ;
         report_tech('EP ratio storage',%reportset%,sto) = report_tech('capacities storage GWh',%reportset%,sto) / report_tech('capacities storage GW',%reportset%,sto) ;
         report_tech('FLH storage',%reportset%,sto) = sum( scen$(map(scen,%reportset%)) , sum( h , lev_STO_OUT(scen,sto,h))) / sum( scen$(map(scen,%reportset%)) ,lev_N_STO_P(scen,sto) )   ;

         report_cost('cost storage power in bn euro',%reportset%,sto) = sum(scen$(map(scen,%reportset%)) , c_i_sto_p(sto) * lev_N_STO_P(scen,sto)) / 1e9 ;
         report_cost('cost investment conventional (implicit) in bn euro in bn euo',%reportset%,ct) = sum(scen$(map(scen,%reportset%)) , c_i_con(ct) * 1000 * report_tech('implicit capacities conventional GW',%reportset%,ct)) / 1e9 ;
         report_cost('cost operation conventional (implicit) in bn euro',%reportset%,ct) = sum(scen$(map(scen,%reportset%)) , sum( h , c_var_con(ct) * lev_G_CON(scen,ct,h))) / 1e9 ;
         report_cost('cost total (implicit) in bn euro',%reportset%,'total') = sum( res , report_cost('cost renewables in bn euro',%reportset%,res)) + sum( sto , report_cost('cost storage energy in bn euro',%reportset%,sto) + report_cost('cost storage power in bn euro',%reportset%,sto)) + sum( ct , report_cost('cost investment conventional (implicit) in bn euro in bn euo',%reportset%,ct) + report_cost('cost operation conventional (implicit) in bn euro',%reportset%,ct)) ;

         report_marginal('capacity storage power',%reportset%,sto) =  sum( scen$(map(scen,%reportset%)) , marg_N_STO_P(scen,sto)) ;
$ontext
$offtext

%obj_min_cost_total%$ontext
         report_tech('capacities storage GW',%reportset%,sto) =  sum( scen$(map(scen,%reportset%)) , lev_N_STO_P(scen,sto)) / 1000 ;
         report_tech('capacities conventional GW',%reportset%,ct) = 0 + sum( scen$(map(scen,%reportset%)) , lev_N_CON(scen,ct)) / 1000 ;


         report_cost('cost storage power in bn euro',%reportset%,sto) = sum(scen$(map(scen,%reportset%)) , c_i_sto_p(sto) * lev_N_STO_P(scen,sto)) / 1e9 ;
         report_cost('cost investment conventional in bn euro',%reportset%,ct) = sum(scen$(map(scen,%reportset%)) , c_i_con(ct) * lev_N_CON(scen,ct)) / 1e9 ;
         report_cost('cost operation conventional in bn euro',%reportset%,ct) = sum(scen$(map(scen,%reportset%)) , sum( h , c_var_con(ct) * lev_G_CON(scen,ct,h))) / 1e9 ;
         report_cost('cost total in bn euro',%reportset%,'total') = report('obj value',%reportset%) / 1e9 ;

         report_marginal('capacity conventional',%reportset%,ct) =  sum( scen$(map(scen,%reportset%)) , marg_N_CON(scen,ct)) ;
         report_marginal('capacity storage power',%reportset%,sto) =  sum( scen$(map(scen,%reportset%)) , marg_N_STO_P(scen,sto)) ;
$ontext
$offtext


*-------------------------------------------------------------------------------

                 report('obj value',%reportset%)$(report('obj value',%reportset%) < eps_rep_abs) = 0 ;
                 report('net energy demand',%reportset%)$(report('net energy demand',%reportset%) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report('renewables available',%reportset%)$(report('renewables available',%reportset%) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report('renewables used directly',%reportset%)$(report('renewables used directly',%reportset%) < eps_rep_rel OR report('model status',%reportset%) <> 1) = 0 ;
                 report('renewables curtailed absolute',%reportset%)$(report('renewables curtailed absolute',%reportset%) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report('renewables curtailed relative',%reportset%)$(report('renewables curtailed relative',%reportset%) < eps_rep_rel OR report('model status',%reportset%) <> 1) = 0 ;
                 report('renewables stored',%reportset%)$(report('renewables stored',%reportset%) < eps_rep_rel OR report('model status',%reportset%) <> 1) = 0 ;
%not_p2x%        report('renewables used for p2x',%reportset%)$(report('renewables used for p2x',%reportset%) < eps_rep_rel OR report('model status',%reportset%) <> 1) = 0 ;
                 report('renewables lost in storage',%reportset%)$(report('renewables lost in storage',%reportset%) < eps_rep_rel OR report('model status',%reportset%) <> 1) = 0 ;
                 report('renewables lost total',%reportset%)$(report('renewables lost total',%reportset%) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report('"efficiency"',%reportset%)$(report('"efficiency"',%reportset%) < eps_rep_rel OR report('model status',%reportset%) <> 1) = 0 ;
                 report('renshare total',%reportset%)$(report('renshare total',%reportset%) < eps_rep_rel OR report('model status',%reportset%) <> 1) = 0 ;
                 report('hours with comtemp storing in and out',%reportset%)$(report('hours with comtemp storing in and out',%reportset%) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;

                 report_hours('generation conventional',%reportset%,ct,h)$(report_hours('generation conventional',%reportset%,ct,h) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_hours('generation renewable',%reportset%,res,h)$(report_hours('generation renewable',%reportset%,res,h) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_hours('curtailment of fluct res',%reportset%,res,h)$(report_hours('curtailment of fluct res',%reportset%,res,h) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_hours('generation storage',%reportset%,sto,h)$(report_hours('generation storage',%reportset%,sto,h) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_hours('storage loading',%reportset%,sto,h)$(report_hours('storage loading',%reportset%,sto,h) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_hours('storage level',%reportset%,sto,h)$(report_hours('storage level',%reportset%,sto,h) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_hours('residual load (before storage)',%reportset%,'demand',h)$(report('model status',%reportset%) <> 1) = 0 ;
                 report_hours('residual load (after storage)',%reportset%,'demand',h)$(report('model status',%reportset%) <> 1) = 0 ;
%not_p2x%        report_hours('residual load (after p2x)',%reportset%,'demand',h)$(report('model status',%reportset%) <> 1) = 0 ;
%not_p2x%        report_hours('residual load (after storage and p2x)',%reportset%,'demand',h)$(report('model status',%reportset%) <> 1) = 0 ;

                 report_tech('capacities renewable GW',%reportset%,res)$(report_tech('capacities renewable GW',%reportset%,res) < eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
                 report_tech('capacities storage GWh',%reportset%,sto)$(report_tech('capacities storage GWh',%reportset%,sto) < eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
%not_p2x%        report_tech('check flh p2x',%reportset%,p2x)$(report_tech('check flh p2x',%reportset%,p2x) < eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
                 report_tech('renewables curtailed absolute',%reportset%,res)$(report_tech('renewables curtailed absolute',%reportset%,res) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_tech('renewables curtailed relative',%reportset%,res)$(report_tech('renewables curtailed relative',%reportset%,res) < eps_rep_rel OR report('model status',%reportset%) <> 1) = 0 ;

                 report_cost('cost renewables in bn euro',%reportset%,res)$(report_cost('cost renewables in bn euro',%reportset%,res) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_cost('cost storage energy bn euro',%reportset%,sto)$(report_cost('cost storage energy in bn euro',%reportset%,sto) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;

%obj_min_sto_e%$ontext
                 report_tech('implicit capacities storage GW',%reportset%,sto)$(report_tech('implicit capacities storage GW',%reportset%,sto) < eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
                 report_tech('implicit capacities conventional GW',%reportset%,ct)$(report_tech('implicit capacities conventional GW',%reportset%,ct) < eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
                 report_tech('implicit EP ratio storage',%reportset%,sto)$(report_tech('implicit EP ratio storage',%reportset%,sto) < eps_rep_rel OR report('model status',%reportset%) <> 1) = 0 ;
                 report_tech('implicit FLH storage',%reportset%,sto)$(report_tech('implicit FLH storage',%reportset%,sto) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;

                 report_cost('cost storage power (implicit) in bn euro',%reportset%,sto)$(report_cost('cost storage power (implicit) in bn euro',%reportset%,sto) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_cost('cost investment conventional (implicit) in bn euro in bn euo',%reportset%,ct)$(report_cost('cost investment conventional (implicit) in bn euro in bn euo',%reportset%,ct) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_cost('cost operation conventional (implicit) in bn euro',%reportset%,ct)$(report_cost('cost operation conventional (implicit) in bn euro',%reportset%,ct) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_cost('cost total (implicit) in bn euro',%reportset%,'total')$(report_cost('cost total (implicit) in bn euro',%reportset%,'total') < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
$ontext
$offtext

%obj_min_cost_sto%$ontext
                 report_tech('capacities storage GW',%reportset%,sto)$(report_tech('capacities storage GW',%reportset%,sto)< eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
                 report_tech('implicit capacities conventional GW',%reportset%,ct)$(report_tech('implicit capacities conventional GW',%reportset%,ct) < eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
                 report_tech('EP ratio storage',%reportset%,sto)$(report_tech('EP ratio storage',%reportset%,sto) < eps_rep_rel OR report('model status',%reportset%) <> 1) = 0 ;
                 report_tech('FLH storage',%reportset%,sto)$(report_tech('FLH storage',%reportset%,sto) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;

                 report_cost('cost storage power in bn euro',%reportset%,sto)$(report_cost('cost storage power in bn euro',%reportset%,sto) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_cost('cost investment conventional (implicit) in bn euro in bn euo',%reportset%,ct)$(report_cost('cost investment conventional (implicit) in bn euro in bn euo',%reportset%,ct) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_cost('cost operation conventional (implicit) in bn euro',%reportset%,ct)$(report_cost('cost operation conventional (implicit) in bn euro',%reportset%,ct) < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
                 report_cost('cost total (implicit) in bn euro',%reportset%,'total')$(report_cost('cost total (implicit) in bn euro',%reportset%,'total') < eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;
$ontext
$offtext

%obj_min_cost_total%$ontext
                 report_tech('capacities storage GW',%reportset%,sto)$(report_tech('capacities storage GW',%reportset%,sto)< eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
                 report_tech('capacities conventional GW',%reportset%,ct)$(report_tech('capacities conventional GW',%reportset%,ct)< eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
                 report_tech('EP ratio storage',%reportset%,sto)$(report_tech('EP ratio storage',%reportset%,sto)< eps_rep_rel OR report('model status',%reportset%) <> 1) = 0 ;
                 report_tech('FLH storage',%reportset%,sto)$(report_tech('FLH storage',%reportset%,sto)< eps_rep_abs OR report('model status',%reportset%) <> 1) = 0 ;

                 report_cost('cost storage power in bn euro',%reportset%,sto)$(report_cost('cost storage power in bn euro',%reportset%,sto)< eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
                 report_cost('cost investment conventional in bn euro',%reportset%,ct)$(report_cost('cost investment conventional in bn euro',%reportset%,ct)< eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
                 report_cost('cost operation conventional in bn euro',%reportset%,ct)$(report_cost('cost operation conventional in bn euro',%reportset%,ct)< eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
                 report_cost('cost total',%reportset%,'total')$(report_cost('cost total',%reportset%,'total')< eps_rep_ins OR report('model status',%reportset%) <> 1) = 0 ;
$ontext
$offtext

Execute_Unload '%outputfile%'

report
report_cost
report_tech
report_hours
report_marginal
report_tech_r
report_hours_r
report_marginal_r
;

%offcluster%$ontext
$onecho >%outputfile%.tmp
par=report            rng=report!A1             rdim=2 cdim=1
par=report_cost       rng=report_cost!A1        rdim=3 cdim=1
par=report_hours      rng=report_hours!A1       rdim=4 cdim=1
par=report_tech       rng=report_tech!A1        rdim=3 cdim=1
par=report_marginal   rng=report_marginal!A1    rdim=3 cdim=1
par=report_hours_r    rng=report_hours_r!A1     rdim=5 cdim=1
par=report_tech_r     rng=report_tech_r!A1      rdim=3 cdim=2
par=report_marginal_r rng=report_marginal_r!A1  rdim=3 cdim=2
$offecho
execute "gdxxrw i=%outputfile%.gdx o=%outputfile%.xlsx @%outputfile%.tmp squeeze=N";
$ontext
$offtext
