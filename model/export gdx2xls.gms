$setglobal base_year "2014"
*$call gdx2xls results_base_year_%base_year%_regions.gdx

$onecho >temp_output.tmp
par=report            rng=report!A1             rdim=2 cdim=1
par=report_cost       rng=report_cost!A1        rdim=3 cdim=1
par=report_hours      rng=report_hours!A1       rdim=4 cdim=1
par=report_tech       rng=report_tech!A1        rdim=3 cdim=1
par=report_marginal   rng=report_marginal!A1    rdim=3 cdim=1
par=report_hours_r    rng=report_hours_r!A1     rdim=5 cdim=1
par=report_tech_r     rng=report_tech_r!A1      rdim=3 cdim=2
par=report_marginal_r rng=report_marginal_r!A1  rdim=3 cdim=2
$offecho

execute 'gdxxrw i=results_base_year_%base_year%_regions.gdx o=results_base_year_%base_year%_regions.xlsx @temp_output.tmp' ;
