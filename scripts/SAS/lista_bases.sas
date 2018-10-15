proc delete data = _all_; run;

%macro data_create;
	%do i = 1 %to 9;
		data esbu_20180&i.;
			call streaminit(6969+&i);
				do j = 1 to 10;
					xi = rand("Weibull", 2, &i + 0.4);
					output;
				end;
		run;
	%end;
%mend;

%data_create;

data list_data;
	set sashelp.vtable(keep = libname memname where = (libname = "WORK" and memname =* "esbu_" ));
	cd_safra = input(substr(memname, 6), 12.);
run;
proc sort data = list_data; 
	by cd_safra; 
run;
data _null_;
	set list_data end = fim;
	if fim then call symputx('aux4', cd_safra);
run;
%put &aux4;

