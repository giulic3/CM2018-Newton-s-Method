(* ::Package:: *)

(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 07-May-2018 *)

(* Copyright *)

BeginPackage["LearningNewtonsMethod`"]
(* Exported symbols added here with SymbolName::usage *) 

NewtonInteractive::usage = "Usalo";
Calcolatrice::usage = "Usalo";
ConvertImageToFullyScaledNinePatch::usage = "Set notebook background image";
RootExampleGraphic::usage = "Shows a example plot about root concept";
BisectionAnimated::usage = "Animated BIsection method";
SecantAnimated::usage = "Animated Secant Method";
NewtonAnimated::usage = "Animated Newton Method";
FirstExample::usage = "";
SecondExample::usage = "";

i::usage = "";

Begin["`Private`"]

(* Function that shows an interactive manipulate *)
NewtonInteractive[] :=
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
                Module[ {list = NestList[ #1 - funzioneFinale[#1]/Derivative[1][funzioneFinale][#1] & ,x0, n]},
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
                            ImageSize -> {800,500}
                            ]},
                    Center]
                ],
                Attributes[Derivative] = {NHoldAll, ReadProtected},
                Attributes[Subscript] = {NHoldRest}, Subscript[w, opt] = {2.706, 3.686},
                Attributes[PlotRange] = {ReadProtected},
                funzioneFinale1[x_] :=
                    Cos[x],
                funzioneFinale2[x_] :=
                    Sin[x],
                funzioneFinale3[x_] :=
                    x^2 - 7,
                funzioneFinale4[x_] :=
                    x^3 - 3*x^2 + x - 1,
                funzioneFinale5[x_] :=
                    Sin[x]*Cos[x],
                funzioneFinale6[x_] :=
                    x^2*Log[x] - 1
        }];

    (*NewtonInteractive[]:=
        Manipulate[
            newton[input,N[x0], 0, 2*Pi, iteration],
            {{input,Null,"function"},{InputField[Dynamic[func],String]},ControlType -> InputField},
            {{iteration, 0, "n"}, {0, 1, 2, 3, 4, 5, 6}, ControlType -> SetterBar},
            {{x0, 0.2, Subscript["x", "0"]}, 0.01, 6.11, Appearance -> "Labeled"},
            ControllerLinking -> True,
            Initialization:>{
                newton[input_,x0_,a_,b_,n_] :=
                    Module[
                        {
                        list = NestList[ #1 - funzioneFinale[#1]/funzioneFinale'[#1] & ,x0, n]
                        },
                        funzioneFinale[x_] := Sin[x];
 
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
                    Attributes[PlotRange] = {ReadProtected}
            }];*)

(* Simply plot a pretty calculator way to write function *)
Calcolatrice[] :=
    Module[ {},
        funzione = "";
        funzioneFinale[x_] = "";
        buttonVuoto = "";
        buttonSin = Button["sin",funzione = funzione<>"sin"];
        buttonCos = Button["cos",funzione = funzione<>"cos"];
        buttonLog = Button["Log",funzione = funzione<>"Log"];
        buttonTan = Button["tan",funzione = funzione<>"tan"];
        buttonLn = Button["ln",funzione = funzione<>"ln"];
        button1 = Button[" 1 ",funzione = funzione<>"1"];
        button2 = Button[" 2 ",funzione = funzione<>"2"];
        button3 = Button[" 3 ",funzione = funzione<>"3"];
        buttonFINE = Button["FINE",funzioneFinale[x_] = ToExpression[funzione,TraditionalForm]];
        buttonAzzera = Button["Azzera",{funzione = "",funzioneFinale[x_] = ""}];
        buttonx = Button["x",funzione = funzione<>"x"];
        buttonLpar = Button["(",funzione = funzione<>"("];
        buttonRpar = Button[")",funzione = funzione<>")"];
        buttonPi = Button["\[Product]",funzione = funzione<>"Pi"];
        buttonElev = Button["^",funzione = funzione<>"^"];
        buttonNepero = Button["\[ScriptE]",funzione = funzione<>"nonloso"];
        button4 = Button["4",funzione = funzione<>"4"];
        button5 = Button["5",funzione = funzione<>"5"];
        button6 = Button["6",funzione = funzione<>"6"];
        buttonVirgola = Button[",",funzione = funzione<>","];
        buttonPlus = Button["+",funzione = funzione<>"+"];
        buttonTimes = Button["*",funzione = funzione<>"*"];
        buttonMinus = Button["-",funzione = funzione<>"-"];
        buttonDivide = Button["/",funzione = funzione<>"/"];
        buttonSqrt = Button["Sqrt",funzione = funzione<>"Sqrt"];
        button7 = Button["7",funzione = funzione<>"7"];
        button8 = Button["8",funzione = funzione<>"8"];
        button9 = Button["9",funzione = funzione<>"9"];
        button0 = Button["0",funzione = funzione<>"0"];
        Grid[
        {
            {buttonVuoto,buttonSin,buttonCos,buttonLog,buttonTan,buttonLn,button1,button2,button3,buttonFINE,buttonAzzera,buttonVuoto,Dynamic[funzione]},
            {buttonx,buttonLpar,buttonRpar,buttonPi,buttonElev,buttonNepero,button4,button5,button6,buttonVirgola,buttonVuoto,buttonVuoto},
            {buttonVuoto,buttonPlus,buttonTimes,buttonMinus,buttonDivide,buttonSqrt,button7,button8,button9,button0,buttonVuoto,buttonVuoto,Dynamic[funzioneFinale["x"]]}
        },
        Alignment->Center
        ]
    ];


RootExampleGraphic[] :=
    Module[ {},
        Legended[
            Plot[
                x^6 - 1221,
                {x, 0, 6},
                PlotStyle -> 
                Directive[RGBColor[0.84`, 0.`, 0.1`], AbsoluteThickness[1.415`]],
                PlotTheme -> "Classic",
                ImageSize -> {800,500},
                PlotRange -> {-1221, 1221},
                LabelStyle -> {GrayLevel[0]},
                AxesLabel -> {Style["x", 16], Style["y", 16]}
            ],
            Placed[Style[HoldForm[Global`f["x"] = "x"^6 - 1221]], {Right, Top}]
        ]
    ];

BisectionAnimated[] :=
    Module[ 
    	{a,b,m,steps,f},
	    f[x_] := Sin[x];
	    a = Pi/4;
	    b = (3/2)Pi;
	    m = (a + b)/2;
	    steps = 
		    Reap[
		        While[ Abs[f[m]] > 0.001, 
		            Sow[m] If[ f[m]*f[a] < 0,
		                       b = m,
		                       a = m
		                   ];
		            m = (a + b)/2
		            ]
		        ][[2, 1]];
	        
	    Animate[
	    	Plot[
	    		f[x], {x, 0, 2 Pi},
		        Epilog -> {
	          		Directive[{Thick, Red, Dashed}], 
		            InfiniteLine[{steps[[i]], 0}, {0, 1}],
		            Red, 
		            PointSize[0.02],
		            Point@{steps[[i]], f@steps[[i]]}
		        },
	          	ImageSize->{800,500}
	    	], 
	      	{i, 1, Length[steps], 1}
	    ]
    ];
    
    
SecantAnimated[] :=
    Module[
	     (* Protected Variable *)
	     {poly, xValues,x0,x1},
	     (* Initialization of protected variable *)
	     x0 = 0.4;
	     x1 = 1.8;
	     poly[x_] := x^4 - 2;
	     
	     Animate[
		    Plot[
		        poly[x], {x, 0, 3},
		        PlotRange -> {{0, 2}, {-3, 10}},
		     	Epilog -> {
		       	 	xValues = 
			      		N[NestList[
			      		{
			      			#1[[1]],
			      			#1[[1]] - poly[#1[[1]]]*((#1[[1]] - #1[[2]]) / (poly[#1[[1]]] -  poly[#1[[2]]]))
			      		} &,
			      		{x1, x0},
			      		i
			      	]];
		       		{PointSize[0.01], (Point[{#1, 0}] &) /@ Flatten[xValues]},
			    	{Thickness[0.002], 
		          		MapIndexed[{ Hue[0.76*(#2[[1]]/6)], 
		             		Line[{  {#1[[1]], poly[#1[[1]]]},  {#1[[2]], 
		                	poly[#1[[2]]]}  }]} &, xValues  ]},
		         	{Thickness[0.002], Black, 
		          		Line[({{#1, 0}, {#1, poly[#1]}} &) /@ Flatten[xValues]]}
		     	},
		       	Axes -> True,
		       	ImageSize -> {800,500},
		       	AxesLabel -> {Style["x", 16], Style["y", 16]}
		    ],
		    {i,1,6,1}
	     ]       
    ];

NewtonAnimated[] :=
	Module[
		{list, funz, x0},
		x0 = 0.30;
		funz[x_] = x^2 - 7;
		
		Animate[
			Plot[
				list = NestList[#1 - funz[#1]/Derivative[1][funz][#1] &, x0, i];
				funz[x], {x, 0, 12}, PlotRange -> All,
				AxesLabel -> {Style["x", 16], Style["y", 16]},
				PlotStyle -> Thickness[0.006],
				Epilog -> {
					{
						PointSize[0.01], 
						Point[({#1, 0}) & /@ list], 
						Text[DisplayForm@RowBox[{Subscript["x", i - 1]}], {list[[i]], -10}]
					},
					{
						Red,
						Thickness[0.002], 
						Arrowheads[0.03],
						Arrow[Most[Flatten[({{#1, 0}, {#1, funz[#1]}} &) /@ list, 1]]]
					}
				},
				ImageSize -> {800, 500}
			],
			{i, 1, 5, 1}
		]
	];

(* these two functions show cases/examples in which the Newton's method fails *)
(* xlog(x)-1*)
FirstExample[]:=
	Module[{},
		f[x_]=x Log[x]-1//N;
		list = NestList[(#-f[#]/f'[#])&,0.1,3];
		Plot[{x Log[x]-1},{x,-3,3}, 
			PlotLegends->"f(x) = xlog(x)-1",
			Epilog->{
				PointSize[Large],
				Point[{list[[1]],0}],
				Point[{list[[2]],0}],
				Text[DisplayForm@RowBox[{Subscript["x","0"]}],{0.3,0.3}],
				Text[DisplayForm@RowBox[{Subscript["x","1"]}],{-0.6,0.3}],
				Directive[{Thick,Red,Dashed}],
				InfiniteLine[{{list[[1]],0},{list[[1]],1}}],
				InfiniteLine[{{list[[2]],0},{list[[2]],1}}],
				ImageSize->Large
			},
			ImageSize -> {800,500}
		]
	]
	
(* cos(x) *)
SecondExample[]:=
		Module[{},
			Plot[
				{Cos[x], y=1},{x,-Pi,Pi}, 
				PlotRange->{-1.5,1.5},
				PlotLegends->{"f(x) = cos(x)","f(x) = 1"},
				Epilog->{
					PointSize[Large],
					Point[{0,1}],
					Text[DisplayForm@RowBox[{Subscript["x","0"]}],{0.3,1.2}]
				},
				ImageSize -> {800,500}
			]
		]


(* helper function that converts an image to a nine-patch image to be used as background*)
ConvertImageToFullyScaledNinePatch[img_] :=
    Module[ {paddedImage = ImagePad[img,1,Black] },
        ReplaceImageValue[
            paddedImage,
            Flatten[Outer[List,{0,#1},{0,#2}]&@@ImageDimensions[paddedImage],1] -> White]
    ];

(* SetBackground[img_] :=
        SetOptions[SelectedNotebook[], 
         System`BackgroundAppearance -> ConvertImageToFullyScaledNinePatch[img_]];*)
     
End[]

EndPackage[]

