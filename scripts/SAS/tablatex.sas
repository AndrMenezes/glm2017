%macro uplatex(x=);
	ods tagsets.tablesonlylatex file=&x (notop nobot) newfile=table;
%mend;
%macro downlatex;
	ods tagsets.tablesonlylatex close;
%mend;
