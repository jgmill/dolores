* Example how to load data from CSV

* So far, reads only one time series (one region, one technology, one year)
* Requires CSV files to be formatted differently from how they come out of renewables ninja.


Sets
h                Hours                                   /h1*h8760/
res              Renewable technologies                  /wind, solar/
r                Regions                                 /north, south/


parameters
phi_res(res,h,r)         Hourly capacity factor renewable energy
phi_solar_upload(h)
;

$call csv2gdx north14_pv.csv id=phi_solar_upload Index=(1) Values=(2) UseHeader=Y StoreZero=Y
$gdxin north14_pv.gdx

$load phi_solar_upload
$gdxin

phi_res('solar',h,'north') = phi_solar_upload(h)

display phi_res
