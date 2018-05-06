(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 07-May-2018 *)

BeginPackage["LearningNewtonsMethod`"]
(* Exported symbols added here with SymbolName::usage *) 
NewtonInteractive::usage = "Usalo";
Calcolatrice::usage = "Usalo";
ConvertImageToFullyScaledNinePatch::usage = "Usalo";

Begin["`Private`"]

(* Function that shows an interactive manipulate *)
NewtonInteractive[]:=
	Manipulate[
		newton[input,N[x0], 0, 2*Pi, iteration],
		{{input, funzioneFinale1, "Funzione: "}, {
			funzioneFinale1 -> TraditionalForm[Cos["x"]],
			funzioneFinale2 -> TraditionalForm[Sin["x"]],
			funzioneFinale3 -> TraditionalForm[-7 + "x"^2],
			funzioneFinale4 -> TraditionalForm[-1 + "x" - 3*"x"^2 + "x"^3],
			funzioneFinale5 -> TraditionalForm[Sin["x"]*Cos["x"]],
			funzioneFinale6 -> TraditionalForm[-1 + "x"^2*Log2["x"]]
			}, ControlType -> PopupMenu},
		{{iteration, 0, "Numero di Iterazioni: "}, {0, 1, 2, 3, 4, 5, 6}, ControlType -> SetterBar},
		{{x0, 0.3, Subscript["x", "0"]}, 0.01, 6.11, Appearance -> "Labeled"},
		ControllerLinking -> True,
		Initialization:>{
			newton[funzioneFinale_,x0_,a_,b_,n_] :=
				Module[
					{list = NestList[ #1 - funzioneFinale[#1]/Derivative[1][funzioneFinale][#1] & ,x0, n]},
					Column[{
						TraditionalForm[Text[Style[Row[{HoldForm[Subscript[{Subscript["x", "k"]}, "k" = 0]^n], " = ", list}], 14]]],
						Plot[
							funzioneFinale[x], {x, a, b},
							PlotRange -> All,
							AxesLabel -> {Style["x", 16], Style["y", 16]},
							PlotStyle -> Thickness[0.006],
							Epilog -> {
								{Red, Thickness[0.002],
									Arrowheads[0.03],
									Arrow[Most[Flatten[({{#1, 0}, {#1, funzioneFinale[#1]}} & ) /@ list, 1]]]},
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
				funzioneFinale1[x_] := Cos[x],
				funzioneFinale2[x_] := Sin[x],
				funzioneFinale3[x_] := x^2 - 7,
				funzioneFinale4[x_] := x^3 - 3*x^2 + x - 1,
				funzioneFinale5[x_] := Sin[x]*Cos[x],
				funzioneFinale6[x_] := x^2*Log[x] - 1
		}];

(* Simply plot a pretty calculator way to write function *)
Calcolatrice[] :=
	Module[{},
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


(* helper function that converts an image to a nine-patch image to be used as background*)
ConvertImageToFullyScaledNinePatch[img_] :=
Module[{paddedImage = ImagePad[img,1,Black]},
ReplaceImageValue[paddedImage,
Flatten[Outer[List,{0,#1},{0,#2}]&@@ImageDimensions[paddedImage],1] -> White]];

End[]

EndPackage[]
