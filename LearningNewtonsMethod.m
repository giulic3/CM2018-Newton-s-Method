(* ::Package:: *)

(* :Title: LearningNewtonsMethod.m *)
(* :Summary: A package providing function useful for learning newton's method *)
(* :Author:	Matteo Sanfelici, Anna Avena,	Giulia Cantini 	*)
(* :Copyright: \[Copyright] <2018> by <Matteo Sanfelici> *)
(* :Package Version: 1.0 *)
(* :Mathematica Version: 11.2 *)
(* :Keywords: programming style, newton, root calculation *)
(* :History: *)

BeginPackage["LearningNewtonsMethod`"];

interactiveNewton[]:=
	Manipulate[
	newton[funcMenu, N[x0], 0, 2*Pi, iteration],

		(** Definition of Subscript[Graphi, Subscript[\[Placeholder], \[Placeholder]]]cal Controller **)
		(* Define a Graphics for PopupMenu "funcMenu", for an interactive selection of function to plot *)
		{{funcMenu, f2, "function"}, {
			f1 -> TraditionalForm[Cos[x]],
			f2 -> TraditionalForm[Sin[x]],
			f3 -> TraditionalForm[-7 + x^2],
			f4 -> TraditionalForm[-1 + x - 3*x^2 + x^3],
			f5 -> TraditionalForm[Sin[x]*Cos[x]],
			f6 -> TraditionalForm[-1 + x^2*Log2[x]]
			}, ControlType -> PopupMenu},
		(* Define a Graphics for SetterBar "iteration" controlling Newton's Iteration *)
		{{iteration, 6, "n"}, {0, 1, 2, 3, 4, 5, 6}, ControlType -> SetterBar},
		(* Define a Graphics for Slider "x0" controlling starting root *)
		{{x0, 0.29000000000000004, Subscript["x", "0"]}, 0.01, 6.11, Appearance -> "Labeled"},
		ControllerLinking -> True,

		(* Initialization of Plot and graphical controller*)
		Initialization :> {
			newton[f_, x0_, a_, b_, n_] :=
				Module[
					(* Compute a n-iteration list of root *)
					{list = NestList[#1 - f[#1]/Derivative[1][f][#1] & , x0, n]},
					Column[{
						(* Show n-iteration lit of root calculated*)
						TraditionalForm[Text[Style[Row[{HoldForm[Subscript[{Subscript[x, k]}, k = 0]^n], " = ", list}], 14]]],
						(* Plot function selected in PopupMenu in a given range *)
						Plot[
							f[x], {x, a, b},
							PlotRange -> All,
							AxesLabel -> {Style[x, 16], Style[y, 16]},
							PlotStyle -> Thickness[0.006],
							Epilog -> {
								{Red, Thickness[0.002],
									Arrowheads[0.03],
									Arrow[Most[Flatten[({{#1, 0}, {#1, f[#1]}} & ) /@ list, 1]]]},
								{PointSize[0.015],
									Point[{x0, 0}]}
							},
							ImageSize -> {600, 325}
						]},
					Center]
				],
				Attributes[Derivative] = {NHoldAll, ReadProtected},
				Attributes[Subscript] = {NHoldRest}, Subscript[w, opt] = {2.706, 3.686},
				Attributes[PlotRange] = {ReadProtected},
					f1[x_] := Cos[x],
					f2[x_] := Sin[x],
					f3[x_] := x^2 - 7,
					f4[x_] := x^3 - 3*x^2 + x - 1,
					f5[x_] := Sin[x]*Cos[x],
					f6[x_] := x^2*Log[x] - 1
				}]

(*NewtonCalcolatrice[]:=
	Manipulate[
		newton[calcolatrice,N[x0], 0, 2*Pi, iteration],
		{calcolatrice[]},
		{{iteration, 6, "n"}, {0, 1, 2, 3, 4, 5, 6}, ControlType -> SetterBar},
		{{x0, 0.29000000000000004, Subscript["x", "0"]}, 0.01, 6.11, Appearance -> "Labeled"},
		ControllerLinking -> True,
	]*)
	
				
calcolatrice[] :=
		Module[{funzione,funzioneFinale},
			funzione = "";
			funzioneFinale[x_]= "";
			buttonVuoto = "";buttonSin =Button["sin",funzione = funzione<>"sin"];buttonCos =Button["cos",funzione = funzione<>"cos"];buttonLog =Button["Log",funzione = funzione<>"Log"];buttonTan =Button["tan",funzione = funzione<>"tan"];buttonLn = Button["ln",funzione = funzione<>"ln"];button1 =Button[" 1 ",funzione = funzione<>"1"];button2 = Button[" 2 ",funzione = funzione<>"2"];button3 = Button[" 3 ",funzione = funzione<>"3"];buttonFINE = Button["FINE",funzioneFinale[x_]=ToExpression[funzione,TraditionalForm]];buttonAzzera =Button["Azzera",{funzione="",funzioneFinale[x_]=""}];
			buttonx = Button["x",funzione = funzione<>"x"];buttonLpar =Button["(",funzione = funzione<>"("];buttonRpar =Button[")",funzione = funzione<>")"];buttonPi=Button["\[Product]",funzione = funzione<>"Pi"];buttonElev=Button["^",funzione = funzione<>"^"];buttonNepero=Button["\[ScriptE]",funzione = funzione<>"nonloso"];button4 =Button["4",funzione = funzione<>"4"];button5=Button["5",funzione = funzione<>"5"];button6=Button["6",funzione = funzione<>"6"];buttonVirgola=Button[",",funzione = funzione<>","];
			buttonPlus =Button["+",funzione = funzione<>"+"];buttonTimes=Button["*",funzione = funzione<>"*"];buttonMinus=Button["-",funzione = funzione<>"-"];buttonDivide=Button["/",funzione = funzione<>"/"];buttonSqrt=Button["Sqrt",funzione = funzione<>"Sqrt"];button7=Button["7",funzione = funzione<>"7"];button8=Button["8",funzione = funzione<>"8"];button9=Button["9",funzione = funzione<>"9"];button0=Button["0",funzione = funzione<>"0"];
			Grid[
			{
				{buttonVuoto,buttonSin,buttonCos,buttonLog,buttonTan,buttonLn,button1,button2,button3,buttonFINE,buttonAzzera,buttonVuoto,Dynamic[funzione]},
				{buttonx,buttonLpar,buttonRpar,buttonPi,buttonElev,buttonNepero,button4,button5,button6,buttonVirgola,buttonVuoto,buttonVuoto},
				{buttonVuoto,buttonPlus,buttonTimes,buttonMinus,buttonDivide,buttonSqrt,button7,button8,button9,button0,buttonVuoto,buttonVuoto,Dynamic[funzioneFinale["x"]]}
			},
			Alignment->Center
			]
		];
EndPackage[];
