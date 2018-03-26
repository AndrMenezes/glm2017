/*****************************************************************************************************************/
/*								SIMULATED ENVELOPE SAS MACRO FOR GLM  (%envelope)                                */   
/*****************************************************************************************************************/
/*			
/*			AUTHOR: PAULO HENRIQUE DOURADO DA SILVA
/*			INSTITUTE: University of Brasilia (UnB) - Brazil
/*
******************************************************************************************************************/


%macro envelope(data=, 								
				predict=, 						
				resid=,							
				m=1, 							
				pzero=,							
				plinear=, 						
				zlinear=, 						
				class_v=,						
				offset=,						
				dispersion=,					
				scale=, 						
				family=Normal, 					
				sim=19, 						
				link=identity, 					
				zlink=logit, 					
				type=HN, 						
				quasi=F, 						
				off=F);							
%if %upcase(&family)=BINOMIAL %then %do;
/*Binomial*/
	%do i=1 %to &sim;
		/*simulating the response variable*/
		data &data; set &data;
			resp=ranbin(&i., &m., &predict);
		run;
		
		/*Residual generating*/
		%if %upcase(&quasi)=F %then %do;
		ods select none;
		proc genmod data=&data descending;
			class &class_v;
			%if &m=1 %then %do;
			model resp= &plinear / dist=binomial link=&link;
			%end; %else %do;
			model resp/&m.= &plinear / dist=binomial link=&link;
			%end;
			output out=&data STDRESDEV=deviance&i;
		run;

		%end; %else %if %upcase(&quasi)=T %then %do;
		ods select none;
		proc genmod data=&data;
			class &class_v;
			%if &m=1 %then %do;
			model resp= &plinear / dist=binomial link=&link noscale scale=&scale;
			%end; %else %do;
			model resp/&m.= &plinear / dist=binomial link=&link noscale scale=&scale;
			%end;
			output out=&data STDRESDEV=deviance&i;
		run;
		%end;
	%end;
%end; %if %upcase(&family)=GAMMA %then %do;
/*Gamma*/
	%do i=1 %to &sim;
		/*simulating the response variable*/
		data &data; set &data;
			resp=(&predict./&scale.)*rangam(&i,&scale.);
		run;
		
		/*Residual generating*/
		ods select none;
		proc genmod data=&data;
			class &class_v;
			model resp= &plinear / dist=gamma link=&link;
			output out=&data STDRESDEV=deviance&i;
		run;

	%end;
%end; %else %if %upcase(&family)=GEOM %then %do;
/*Geometric*/
	%do i=1 %to &sim;
		/*simulating the response variable*/
		data &data; set &data;
			gama=(&predict.*rangam(&i, 1));
			resp=ranpoi(&i, gama);
		run;
		
		/*Residual generating*/
		ods select none;
		proc genmod data=&data;
			class &class_v;
			model resp= &plinear / dist=geom link=&link;
			output out=&data STDRESDEV=deviance&i;
		run;

	%end;
%end; %else %if %upcase(&family)=NB %then %do;
/*Negative Binomial*/			
	%do i=1 %to &sim;
		/*simulating the response variable*/
		data &data; set &data;
			theta=1/&dispersion.;
			gama=(&predict.*rangam(&i, theta))/theta;
			resp=ranpoi(&i, gama);
		run;
		
		/*Residual generating*/
		%if %upcase(&quasi)=F %then %do;
		ods select none;
		proc genmod data=&data;
			class &class_v;
			model resp= &plinear / dist=nb link=&link;
			output out=&data STDRESDEV=deviance&i;
		run;
		%end; %else %if %upcase(&quasi)=T %then %do;
		ods select none;
		proc genmod data=&data;
			class &class_v;
			model resp= &plinear / dist=nb link=&link noscale scale=&scale;
			output out=&data STDRESDEV=deviance&i;
		run;
		%end;
	%end;
%end; %else %if %upcase(&family)=IG %then %do;
/*Inverse Gaussian*/
	proc sql noprint;
		select COUNT(*) into :nobs from &data;
	quit;
	%do i=1 %to &sim;
		/*simulating the response variable*/
		data ig;
			do i=1 to &nobs;
				v=rannor(&i);
				z=ranuni(&i);
				output;
			end;
		run;

		data &data; set &data; set ig;
			y=v**2;
			mu2=&predict.**2;
			scale=1/(&scale.);
			x=&predict.+(y*(mu2)/(2*scale))-((&predict.)/(2*scale))*sqrt(4*&predict.*scale*y+(mu2)*(y**2));
			if z <= &predict./(x+&predict.) then rig=x; else rig=(mu2)/x;
		run;

		data &data; set &data; set ig;
		run;
		
		/*Residual generating*/
		ods select none;
		proc genmod data=&data;
			class &class_v;
			model rig= &plinear / dist=igaussian link=&link;
			output out=&data STDRESDEV=deviance&i;
		run;

	%end;
%end; %else %if %upcase(&family)=NORMAL %then %do;
/*Normal*/
	proc sql noprint;
		select COUNT(*) into :nobs from &data;
	quit;
	%do i=1 %to &sim;
		/*simulating the response variable*/
		data rnorm;
			do j=1 to &nobs;
				norm=rannorm(&i);
				output;
			end;
			drop j;
		run;

		data &data; set &data; set rnorm;
			resp=&predict.+&scale.*norm;
		run;
		
		/*Residual generating*/
		ods select none;
		proc genmod data=&data;
			class &class_v;
			model resp= &plinear / dist=normal link=&link;
			output out=&data STDRESDEV=deviance&i;
		run;

	%end;
%end; %else %if %upcase(&family)=POISSON %then %do;
/*Poisson*/
	%do i=1 %to &sim;
		/*simulating the response variable*/
		data &data; set &data;
			resp=ranpoi(&i, &predict);
		run;
		
		/*Residual generating*/
		%if %upcase(&quasi)=F %then %do;  
			%if %upcase(&off)=F %then %do;
			ods select none;
			proc genmod data=&data;
				class &class_v;
				model resp= &plinear / dist=poisson link=&link;
				output out=&data STDRESDEV=deviance&i;
			run;
			%end; %else %if %upcase(&off)=T %then %do;
			ods select none;
			proc genmod data=&data;
				class &class_v;
				model resp= &plinear / dist=poisson link=&link offset=&offset;
				output out=&data STDRESDEV=deviance&i;
			run;
			%end;
		%end; %else %if %upcase(&quasi)=T %then %do;
			%if %upcase(&off)=F %then %do;
			ods select none;
			proc genmod data=&data;
				class &class_v;
				model resp= &plinear / dist=poisson link=&link noscale scale=&scale;
				output out=&data STDRESDEV=deviance&i;
			run;
			%end; %else %if %upcase(&off)=T %then %do;
			ods select none;
			proc genmod data=&data;
				class &class_v;
				model resp= &plinear / dist=poisson link=&link offset=&offset noscale scale=&scale;
				output out=&data STDRESDEV=deviance&i;
			run;
			%end;
		%end;
	%end;
%end; %else %if %upcase(&family)=ZINB %then %do;
/*Zero Inflated Negative Binomial*/			
	%do i=1 %to &sim;
		/*simulating the response variable*/
		data &data; set &data;
			theta=1/&scale.;
			gama=(&predict.*rangam(&i, theta))/theta;
			negbin=ranpoi(&i, gama);
			ben=ranbin(&i, 1, &pzero);
			if ben=1 then resp=0; else
			resp=negbin;
		run;
		
		/*Residual generating*/
		ods select none;
		proc genmod data=&data;
			class &class_v;
			model resp= &plinear / dist=zinb link=&link;
			zeromodel &zlinear / link=&zlink;
			output out=&data reschi=deviance&i;
		run;
	%end;
%end; %else %if %upcase(&family)=ZIP %then %do;
/*Zero Inflated Poisson*/;
	%do i=1 %to &sim;
		/*simulating the response variable*/
		data &data; set &data;
			pois=ranpoi(&i, &predict.);
			ben=ranbin(&i, 1, &pzero);
			if ben=1 then resp=0; else
			resp=pois;
		run;
		
		/*Residual generating*/
		ods select none;
		proc genmod data=&data;
			class &class_v;
			model resp= &plinear / dist=zip link=&link;
			zeromodel &zlinear / link=&zlink;
			output out=&data reschi=deviance&i;
		run;

	%end;
%end;
	data dev; set &data;
		keep deviance1 -- deviance&sim;
	run;

	proc iml;
	use dev;
		read all into e;
	close;

	use &data;
		read all var{ &resid. } into r;
	close;

	n=nrow(e);

	%if &type=HN %then %do;

	r=abs(r);
	do i=1 to &sim;
		m=abs(e[,i]);
		call sort(m);
		e[,i]=m;
	end;
	p=(n+(1:n)-0.125)/(2*n+0.5);

	%end; %else %if &type=N %then %do;
	
	do i=1 to &sim;
		m=e[,i];
		call sort(m);
		e[,i]=m;
	end;

	p=((1:n)-0.375)/(n+0.25);
	%end;

	q=quantile("Normal", p);
	te=t(e);
	k=j(n,3,0);
	do i=1 to n;
		h=te[,i];
		k[i,1]=min(h);
		k[i,2]=median(h);
		k[i,3]=max(h);
	end;
	call sort(r);
	base=r||t(q)||k;
	*print base;
	create envelope from base;
	append from base;
quit;

data envelope1; set envelope;
	if col1 < col3 or col1 > col5 then color=1;
	else color=2;
run;

%if &type=HN %then %do;

ods select all;
proc sgplot data=envelope1 noautolegend;	
	title "Half-Normal plot with simulated envelope";
	scatter y=COL1 x=COL2 /  group=color;
	series y=COL3 x=COL2 / lineattrs=(color=black);
	series y=COL4 x=COL2 / lineattrs=(color=black pattern=2);
	series y=COL5 x=COL2 / lineattrs=(color=black);
	xaxis label="N(0,1) quantile";
	yaxis label="Standardized deviance residual";
run;
quit;	

%end; %else %if &type=N %then %do;

ods select all;
proc sgplot data=envelope1 noautolegend;	
	title "Normal plot with simulated envelope";
	scatter y=COL1 x=COL2 /  group=color;
	series y=COL3 x=COL2 / lineattrs=(color=black);
	series y=COL4 x=COL2 / lineattrs=(color=black pattern=2);
	series y=COL5 x=COL2 / lineattrs=(color=black);
	xaxis label="N(0,1) quantile";
	yaxis label="Standardized deviance residual ";
run;
quit;
%end;
%mend envelope;
