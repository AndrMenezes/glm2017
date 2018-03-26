%include "C:\Users\User\Dropbox\4° Série\Modelos Lineares Generalizados\trabalho\scripts\tablatex.sas";

proc import datafile 	= 'C:\Users\User\Dropbox\4° Série\Modelos Lineares Generalizados\trabalho\idh_pt2010.txt' replace
			out  		= dados
			dbms 		= tab;
			getnames	= yes;
run;

data parana(where = (ufn = 'Paraná'));
	set dados;
	if (pt2006 = 0) then delete;
run;

* Descritiva das covariáveis;
%uplatex(x="bounded.tex");
proc means data = parana stackodsoutput min q1 median mean q3 max cv maxdec = 4;
	var pt2010 pt2006 gini idhm_e idhm_l idhm_r urbprop pdes18m pibperc;
run;
%downlatex;

* Regressão Beta;
proc nlmixed data = parana tech = tr update = bfgs df=999999;
	parms b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0, b7 = 0, b8 = 0, disp = 1;
	y	  	= pt2010;
	eta   	= b0 + b1 * pt2006 + b2 * gini + b3 * idhm_e + b4 * idhm_l + b5 * idhm_r + b6 * urbprop + b7 * pdes18m + b8 * pibperc;
	mu    	= exp(eta) / (1 + exp(eta));
	phi   	= disp;
	t 	  	= mu * phi;
	w	  	= (1 - mu) * phi;
	ll   	= lgamma(phi) - lgamma(t) - lgamma(w) + (t - 1) * log(y) + (w - 1) * log(1 - y);
	*ll	  	= logpdf('BETA', pt2014, t, w);
	model y ~ general(ll);
	predict mu out = betapred(rename = (pred = betapred Lower = betalower upper = betaupper) drop = DF StdErrPred tValue Probt alpha);
	ods output ParameterEstimates = betaest(drop = df alpha lower upper gradient tValue Probt
			   rename = (estimate = estbeta StandardError = stdbeta));
	ods output FitStatistics = betafit;
run; 

proc glimmix data = parana;
	model pt2010 = pt2006 gini idhm_e idhm_l idhm_r urbprop pdes18m pibperc / dist = beta solution;
	output out=outbeta / allstats;
run;

* Regressão Simplex;
proc nlmixed data = parana tech = tr update = bfgs df=999999;
	parms b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0, b7 = 0, b8 = 0, disp = 1;
	y	  	= pt2010;
	eta   	= b0 + b1 * pt2006 + b2 * gini + b3 * idhm_e + b4 * idhm_l + b5 * idhm_r + b6 * urbprop + b7 * pdes18m + b8 * pibperc;
	mu    	= exp(eta) / (1 + exp(eta));
	phi   	= disp;
	pi		= constant("pi");
	ll 	  	= log(1/sqrt(2 * pi * phi**2 * (y * (1 - y))**3)) - 0.5 / phi**2 * ((y - mu)**2 / (y * (1 - y) * mu**2 * (1 - mu)**2));
	model y ~ general(ll);
	predict mu out = simppred(rename = (pred = simppred Lower = simplower upper = simpupper) drop = DF StdErrPred tValue Probt alpha);
	ods output ParameterEstimates = simpest(drop = df alpha lower upper gradient tValue Probt
			   rename = (estimate = estsimp StandardError = stdsimp));
	ods output FitStatistics = simpfit(rename = (value = svalue) drop = descr);
run; 

data est;
	merge betaest simpest;
run;

%uplatex(x = 'est.tex');
proc print data = est noobs;
	format _numeric_ comma10.4;
run;
%downlatex;

data fits;
	merge betafit simpfit;
run;
%uplatex(x = 'fits.tex');
proc print data = fits noobs;
	format _numeric_ comma10.4;
run;
%downlatex;

* Verificando se as covariáveis pib e idhm_l são conjuntamente significativas ;
proc nlmixed data = parana tech = tr update = bfgs df=999999;
	parms b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0, b7 = 0, b8 = 0, disp = 1;
	y	  	= pt2010;
	eta   	= b0 + b1 * pt2006 + b2 * gini + b3 * idhm_e + b4 * idhm_l + b5 * idhm_r + b6 * urbprop + b7 * pdes18m + b8 * pibperc;
	mu    	= exp(eta) / (1 + exp(eta));
	phi   	= disp;
	pi		= constant("pi");
	ll 	  	= log(1/sqrt(2 * pi * phi**2 * (y * (1 - y))**3)) - 0.5 / phi**2 * ((y - mu)**2 / (y * (1 - y) * mu**2 * (1 - mu)**2));
	model y ~ general(ll);
*	contrast 'b4 = b5 = b8 = 0' b4, b5, b8;
	contrast 'b4 = b8 = 0'          b4, b8;
	ods output Contrasts 	= Wald(drop = label DenDF rename = (NumDF = df FValue = est ProbF = pvalue));
	ods output FitStatistics = H1(rename = (value = llh1 ) where = (descr = '-2 Log Likelihood'));
run; 

* Ajuste sem as covariáveis pib e idhm_l;
proc nlmixed data = parana tech = tr update = bfgs df=999999;
	parms b0 = 0, b1 = 0, b2 = 0, b3 = 0, b5 = 0, b6 = 0, b7 = 0, disp = 1;
	y	  	= pt2010;
	eta   	= b0 + b1 * pt2006 + b2 * gini + b3 * idhm_e + b5 * idhm_r + b6 * urbprop + b7 * pdes18m;
	mu    	= exp(eta) / (1 + exp(eta));
	phi   	= disp;
	pi		= constant("pi");
	ll 	  	= log(1/sqrt(2 * pi * phi**2 * (y * (1 - y))**3)) - 0.5 / phi**2 * ((y - mu)**2 / (y * (1 - y) * mu**2 * (1 - mu)**2));
	model y ~ general(ll);
	predict mu out = simppred(rename = (pred = simppred Lower = simplower upper = simpupper) drop = DF StdErrPred tValue Probt alpha);
	ods output ParameterEstimates = simpest2(drop = df alpha gradient);
	ods output FitStatistics = simpfit2;
	ods output FitStatistics = H0(rename = (value = llh0 ) where = (descr = '-2 Log Likelihood'));
run; 
data simpest2;
	retain  estimate StandardError lower upper tValue Probt;
	set simpest2;
	drop parameter;
run;

%uplatex(x='est2.tex');
proc print data = simpest2 noobs; format _numeric_ comma10.4; run;
%downlatex;

* Teste de Wald e LR sob a hipótese H0: b4 = b8 = 0 vs H1: pelo menos um é diferente;
data LR(drop = Descr llH0 llH1);
	merge H0 H1;
	df		= 3;
	Est 	= llH0 - llH1;
	pvalue	= 1 - cdf('CHISQUARE', Est, df);
run;	
data wald;
	set wald;
	est = df * est;
run;
data testes;
	set Wald LR;
run;
* Exibindo resultados;
proc print data = testes noobs;
	format _numeric_ comma10.4;
run;

* Teste de especificação RESET;
data simppred;
	set simppred;
	simppred2 = simppred**2;
run;
proc nlmixed data = simppred tech = tr update = bfgs df=999999;
	parms b0 = 0, b1 = 0, b2 = 0, b3 = 0, b5 = 0, b6 = 0, b7 = 0, disp = 1, b8 = 0;
	y	  	= pt2010;
	eta   	= b0 + b1 * pt2006 + b2 * gini + b3 * idhm_e + b5 * idhm_r + b6 * urbprop + b7 * pdes18m + b8 * simppred2;
	mu    	= exp(eta) / (1 + exp(eta));
	phi   	= disp;
	t 	  	= mu * phi;
	w	  	= (1 - mu) * phi;
	ll   	= lgamma(phi) - lgamma(t) - lgamma(w) + (t - 1) * log(y) + (w - 1) * log(1 - y);
	model y ~ general(ll);
	ods output ParameterEstimates = TRESET(drop = df alpha gradient lower upper);
run; 
%uplatex(x='est2.tex');
proc print data = TRESET noobs; format _numeric_ comma10.4; run;
%downlatex;

* Exportação dos valores preditos (muhat);
proc export data = simppred outfile = "pred.txt" replace dbms = tab; run;
