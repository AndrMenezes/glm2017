%include "C:\Users\André Felipe\Dropbox\4° Série\Modelos Lineares Generalizados\trabalho\scripts\tablatex.sas";

/*Dados de contagem*/
proc import out = dados
			datafile = "C:\Users\André Felipe\Dropbox\4° Série\Modelos Lineares Generalizados\trabalho\couart2.dta";
run;

/*Medidas descritivas por estratos*/
%uplatex(x="table1.tex");
proc means data = dados min max mean var cv maxdec=4;
	class fem mar;
	var art;
run;
%downlatex;

%uplatex(x="table2.tex");
proc means data = dados min max mean var cv maxdec=4;
	class kid5;
	var art;
run;
%downlatex;
