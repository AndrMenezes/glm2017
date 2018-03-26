proc delete data = _all_ ; run;
proc import out = dados
			datafile = "C:\Users\User\Dropbox\4° Série\Modelos Lineares Generalizados\trabalho\couart2.dta";
run;
%include "C:\Users\User\Dropbox\4° Série\Modelos Lineares Generalizados\trabalho\scripts\tablatex.sas";
%include "C:\Users\User\Dropbox\4° Série\Modelos Lineares Generalizados\trabalho\scripts\envelope_macro.sas";
%include "C:\Users\User\Dropbox\4° Série\Modelos Lineares Generalizados\trabalho\scripts\voung-test.sas";

******** Comparação de modelos;

/*Modelo Poisson*/
proc genmod data = dados;
	class fem mar;
	model art = fem mar kid5 phd ment / dist = poisson link = log;
	ods output ParameterEstimates=Estpoi(keep=parameter estimate StdErr where=(estimate <> 0));
	ods output ModelFit=gofpoi(drop = df ValueDF);
quit;

/*Modelo Binomial Negativa*/
proc genmod data = dados;
	class fem mar;
	model art = fem mar kid5 phd ment / dist = negbin link = log;
	ods output ParameterEstimates=EstBn(keep= estimate StdErr where=(estimate <> 0));
	ods output ModelFit=gofbn(drop = df ValueDF);
quit;

/*Modelo Quase-Poisson*/
proc glimmix data = dados ;
	class fem mar;
	_variance_ = _mu_;
	model art = fem mar kid5 phd ment / link = log solution;
	random _residual_;
	ods output ParameterEstimates=estQP(keep= estimate StdErr where=(estimate <> 0));
	ods output FitStatistics=gofqp;
quit;


*** Tabelas com estimativas e erros padrões;
data estpoi;
	set estpoi;
	rename estimate = Estimativa stderr = EP;
run;
data estqp;
	set estqp;
	rename estimate = Estimativa2 stderr = EP2;
run;
data estbn;
	set estbn;
	rename estimate = Estimativa3 stderr = EP3;
run;
data estimativa;
	merge estpoi estqp estbn;
run;
%uplatex(x = "est.tex");
proc print data = estimativa nobs;
var estimativa--ep3;
run;
%downlatex;


*** Tabelas com os goodness of fit;
proc transpose data = gofpoi out = gofpoi(drop = _NAME_ _LABEL_ col2 col4 col6 col8);
run;
proc transpose data = gofbn out = gofbn(drop = _NAME_ _LABEL_ col2 col4 col6 col8);
run;
proc transpose data = gofqp out = gofqp(drop = _NAME_  col3 col5 col6 col8 rename = (col2 = col3 col4 = col5));
run;
data gof;
	set gofpoi gofbn gofqp;
run;
%uplatex(x = "gof.tex");
proc print data = gof nobs;
run;
%downlatex;


/*Análise do Modelo Binomial Negativo*/

****** Ajuste e avaliação dos parâmetros;

proc genmod data = dados plots = predicted;
	class fem mar;
	model art = fem mar kid5 ment / dist = negbin link = log covb corrb type3;
	ods output  ParameterEstimates=estbn(drop = df level1);
	ods output	corrb = correst;
quit;

proc export data = correst outfile = "corrpar.txt" dbms = tab replace; run;

%uplatex(x='parbn.tex');
proc print data = estbn noobs;
run;
%downlatex;

****** Crítica do modelo;
proc genmod data = dados;
	class fem mar;
	model art = fem mar kid5 ment / dist = negbin link = log;
	output out=bn predicted=predito xbeta=xb stdxbeta=stderror lower=lo upper=up
	reschi=rpearson resdev=rdeviance reslik=rlik stdreschi=sdpearson stdresdev=sdeviance 
	hesswgt=weightmat leverage=leverage dfbetas=dfbeta cooksd=cook;
quit;

proc export data = bn outfile = "residuos.csv" dbms = csv replace; run;

* Avaliando a função de ligação;
data bn;
	set bn;
	z = xb + (art - predito) / predito;
	xb2 = xb**2;
run;
proc gplot data=bn;
	title 'Verificando a função de ligação';
	plot z * xb;
run;
proc genmod data = bn;
	class fem mar;
	model art = fem mar kid5 ment xb2 / dist = negbin link = log type1 type3;
	ods output  ParameterEstimates=estbnxb2(drop = df level1 LowerWaldCL UpperWaldCL);
quit;
%uplatex(x='parbnxb2.tex');
proc print data = estbnxb2 noobs; run;
%downlatex;

* Avaliando a normalidade;
proc univariate data = bn normal;
	var rdeviance;
	histogram rdeviance / kernel normal;
	qqplot rdeviance / normal(mu=0 sigma=1);
run;
ods listing;
%envelope(data=bn, predict=predito, resid=rdeviance, class_v= fem mar, plinear=fem mar kid5 ment,
dispersion=0.4417, family=nb,
		  link=log, type=HN);


%envelope(data=bn, predict=predito, resid=rdeviance, class_v= fem mar, plinear=fem mar kid5 ment, dispersion= 0.4417, family=nb,
		  link=log, type=N);


* Discriminação entre os modelos P, NB, ZIP e ZINB via teste de Voung;
proc countreg data=dados;
	model art=fem mar kid5 ment / d=zip; 
    zeromodel art~fem mar kid5 ment; 
    output out=outzip pred=predzip probzero=p0zip;
run;
    
proc countreg data=outzip method = qn;
	model art=fem mar kid5 ment / d=zinb; 
    zeromodel art~fem mar kid5 ment; 
    output out=outzinb pred=predzinb probzero=p0zinb;
run;

proc countreg data=outzinb;
    model art=fem mar kid5 ment / d=Poisson;
    output out=outp pred=predp;
run;

proc countreg data=outp method=qn;
    model art=fem mar kid5 ment / d=negbin(p = 2);
	output out=out pred=pnb;
run;
 
* BN vs Poisson;
%vuong(data=out, response=art,
       model1=neg. bin, p1=pnb,   dist1=nb,  nparm1=6, scale1=0.441673,
	   model2=Poisson,  p2=predp, dist2=poi, nparm2=5) 

* BN vs ZIP;
%vuong(data=out, response=art,
       model1=neg. bin, p1=pnb,     dist1=nb,  nparm1=6, scale1=0.441673,
	   model2=zip,  	p2=predzip, dist2=zip, nparm2=10, pzero2=p0zip) 

* BN vs ZINB;
%vuong(data=out, response=art,
       model1=neg. bin, p1=pnb,      dist1=nb, 	 nparm1=6,  scale1=0.441673,
	   model2=zip,  	p2=predzinb, dist2=zinb, nparm2=11, scale2=0.376253, pzero2=p0zinb) 

* ZINB vs NB;
%vuong(data=out, response=art,
       model2=neg. bin, p2=pnb,      dist2=nb, 	 nparm2=6,  scale2=0.441673,
	   model1=zip,  	p1=predzinb, dist1=zinb, nparm1=11, scale1=0.376253, pzero1=p0zinb) 




