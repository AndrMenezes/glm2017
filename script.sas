/* Gerando dados  */
%let b0 = 1; %let b1 = 1.4; %let b2 = 0.3; %let b3 = -1.4;
data dados;
	call streaminit(1212);
	do j = 1 to 1000;
		x1 			= rand("Normal", 5, 2);
		x2 			= rand("Normal", 6, 3);		
		x3 			= rand("Normal", 7, 4);	
		pi			= exp(&b0 + &b1 * x1 + &b2 * x2 + &b3 * x3) / (1 + exp(&b0 + &b1 * x1 + &b2 * x2 + &b3 * x3));
		ai 			= rand("Bernouli", pi);
		if ai = 0 then yi = "Mau";
		else yi = "Bom";
		if rand("Bernouli", 0.08) = 1 then do;
			idx = 1;
			x1  = .;
		end;
		else idx = 0;
		output;
	end;
run;

proc freq data = dados;
	table yi;
	table idx;
run;

/* Selecionando amostra treino e teste */
proc surveyselect data 	 	= dados 
				  out 		= training_data 
				  method 	= srs   
				  n 		= 800 
				  seed 		= 1212;
run;

proc sql noprint;
	select j into :id separated by ", " from training_data;
quit;

data test_data;
	set dados(where=(j not in (&id)));
run;

/* LDA: Linear Discriminat Analysis  */
proc discrim data   	= training_data 
			 method 	= normal
			 testdata 	= test_data
			 testout    = out_discrim;
	class yi;
	var x1 x2 x3;
run;

/* KNN: K-Nearest-Neighbors k = 8 */
proc discrim data   	= training_data 
			 method 	= npar
			 k			= 8
			 testdata 	= test_data
			 testout    = out_kernel;
	class yi;
	var x1 x2 x3;
run;

/* Logistic regression */
proc logistic data = training_data;
	class yi;
	model yi = x1 x2 x3;
	score data = test_data
		  out  = out_logistic;
run;

/* Random Forest */
proc hpforest data = training_data 	
				maxtrees 		= 200 
				vars_to_try 	= 3
				seed        	= 1212
				maxdepth		= 40
				leafsize		= 6
				trainfraction	= 0.7
				alpha			= 0.7;
	target yi / level = binary;
	input x1 x2 x3 / level = interval;
	ods output fitstatistics = fit;
	save file = "/home/andrefelipemarin0/model_fit.bin";
run;

proc hp4score data = test_data;
	id yi j;
	score file = "/home/andrefelipemarin0/model_fit.bin"
		  out  = out_rf;
run;

/* Estrutura predições */
data out_predicts;
	merge out_discrim(in = a rename = (Mau 	= p_mau_discrim 
									 Bom 	= p_bom_discrim 
									 _into_ = yi_discrim))
	   	  out_kernel(in = b rename = (Mau    = p_mau_kernel 
	   	  							  Bom 	 = p_bom_kernel 
	   	  							  _into_ = yi_kernel))
	   	  out_logistic(in=c rename = (P_Mau = p_mau_logistic 
	   	  							  p_Bom = p_bom_logistic 
	   	  							  I_yi  = yi_logistic))
		  out_rf(in = d rename = (P_yiMau = p_mau_rf 
	   	  						  p_yiBom = p_bom_rf 
	   	  						  I_yi  = yi_rf));	   	  							  
	by j;
	if a and b and c and d;
	if yi_rf = "BOM" then yi_rf = "Bom"; else yi_rf = "Mau";
run;

/* Matriz de confusão */
proc freq data = out_predicts;
	table yi * yi_logistic 	/ missing nocol norow;
	table yi * yi_kernel 	/ missing nocol norow;
	table yi * yi_discrim 	/ missing nocol norow;
	table yi * yi_rf 		/ missing nocol norow;	
run;

/* Acurácia */
proc sql;
	select "Logistica" as metodo, mean(yi = yi_logistic) as acuracia 
		from out_predicts
			outer union
	select "KNN" as metodo, mean(yi = yi_kernel) as acuracia 
		from out_predicts
			outer union
	select "Discrim" as metodo, mean(yi = yi_discrim) as acuracia 
		from out_predicts
			outer union
	select "RF" as metodo, mean(yi = yi_rf) as acuracia 
		from out_predicts;
quit;
	










	
