

*----------------------------------------------------------------------------------

* set modelrun name just like in main file format "region#ofregionsExcelcolumn_typeofcapacityconstrain"
*												e.g "Germany10K_EqualCap"

* capacity constraints:
*						noCap
*						mincap
*						EqualCap
*						MaxCap

$setglobal modelrun "Germany2C_mincap"

* Auto set of output file 

$setglobal outputfile "%modelrun%_results"


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