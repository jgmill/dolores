alias (*,d_upload,phi_res_upload);
parameter val(d_upload,phi_res_upload) /
$ondelim offlisting
$include upload_data.csv
$offdelim  onlisting
/;
sets d_upload(s), phi_res_upload(d);
option d_upload<val, phi_res_upload<val;
