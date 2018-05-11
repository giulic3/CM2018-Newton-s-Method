(* ::Package:: *)

(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 07-May-2018 *)

(* Copyright *)

BeginPackage["LearningNewtonsMethod`"]
(* Exported symbols added here with SymbolName::usage *) 

NewtonInteractive::usage = "NewtonInteractive[]";
Calcolatrice::usage = "Calcolatrice[]";
ConvertImageToFullyScaledNinePatch::usage = "ConvertImageToFullyScaledNinePatch[img] Set notebook background image";
RootExampleGraphic::usage = "Shows a example plot about root concept";
BisectionAnimated::usage = "Animated BIsection method";
SecantAnimated::usage = "Animated Secant Method";
NewtonAnimated::usage = "Animated Newton Method";
FirstExample::usage = "";
SecondExample::usage = "";
Calculator::usage = "";
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
Calculator[] :=
    Module[ {buttonOptions = {ImageSize->Full, Background->White}},
        f = "";
        finalFunction[x_] = "";
        buttonEmpty = "";
        buttonSin = Button["sin",f = f<>"sin", buttonOptions];
        buttonCos = Button["cos",f = f<>"cos", buttonOptions];
        buttonLog = Button["log",f = f<>"log", buttonOptions];
        buttonTan = Button["tan",f = f<>"tan", buttonOptions];
        buttonLn = Button["ln",f = f<>"ln", buttonOptions];
        button1 = Button[" 1 ",f = f<>"1", buttonOptions];
        button2 = Button[" 2 ",f = f<>"2", buttonOptions];
        button3 = Button[" 3 ",f = f<>"3", buttonOptions];
        buttonEnd = Button["=",finalFunction[x_] =N[ToExpression[f,TraditionalForm],2], buttonOptions];
        buttonAllClear = Button["AC",{f = "",finalFunction[x_] = ""}, buttonOptions];
   buttonClearEntry = Button["CE",{f = StringDrop[f,-1]}, buttonOptions];
        buttonLpar = Button["(",f = f<>"(", buttonOptions];
        buttonRpar = Button[")",f = f<>")", buttonOptions];
        buttonPi = Button["\[Product]",f = f<>"Pi", buttonOptions];
        buttonElev = Button["^",f = f<>"^", buttonOptions];
   buttonSquare = Button["\!\(\*SuperscriptBox[\(x\), \(2\)]\)",f=f<>"^2", buttonOptions];
        buttonNepero = Button["\[ScriptE]",f = f<> "\[ExponentialE]", buttonOptions];
        button4 = Button["4",f = f<>"4", buttonOptions];
        button5 = Button["5",f = f<>"5", buttonOptions];
        button6 = Button["6",f = f<>"6", buttonOptions];
        buttonComma = Button[",",f = f<>",", buttonOptions];
        buttonPlus = Button["+",f = f<>"+", buttonOptions];
        buttonTimes = Button["\[Times]",f = f<>"*", buttonOptions];
        buttonMinus = Button["-",f = f<>"-", buttonOptions];
        buttonDivide = Button["\[Divide]",f = f<>"/", buttonOptions];
        buttonSqrt = Button["\[Sqrt]",f = f<>"sqrt", buttonOptions];
        button7 = Button["7",f = f<>"7", buttonOptions];
        button8 = Button["8",f = f<>"8", buttonOptions];
        button9 = Button["9",f = f<>"9", buttonOptions];
        button0 = Button["0",f = f<>"0", buttonOptions];
        Grid[
{
{button7, button8, button9, buttonDivide, buttonEmpty, buttonEmpty, buttonSin, buttonCos, buttonTan,buttonEmpty, buttonEmpty, buttonClearEntry},
{button4, button5, button6, buttonTimes,buttonEmpty, buttonEmpty, buttonLn, buttonLog, buttonSquare,buttonEmpty, buttonEmpty, buttonAllClear},
{button1, button2, button3, buttonMinus,buttonEmpty, buttonEmpty, buttonPi, buttonNepero, buttonElev,buttonEmpty, buttonEmpty,buttonEmpty,buttonEmpty, Dynamic[f]},
{buttonComma, button0, buttonEnd, buttonPlus,buttonEmpty, buttonEmpty, buttonLpar, buttonRpar, buttonSqrt, buttonEmpty, buttonEmpty, buttonEmpty, buttonEmpty, Dynamic[finalFunction["x"]]}
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

Esercizio[funzione_, a_, b_,x0_] :=
	Module[
		{calculator,plot,testoRow1,testoRow2,buttonNew},
	  	fun[x_] := ToExpression[funzione];
	  	
		calculator = Calculator[];	
		plot = 
			Plot[
				fun[x], {x, a, b},
				ImageSize -> Large
			];
		testoRow1 = "Calcolare un'approssimazione dello zero usando il Metodo di Newton,";
		testoRow2 = "con due iterazioni, 	 partendo dalla prima approssimazione data";
		buttonNew = 
			Button[Style["Nuovo Esercizio", FontSize -> 20], ImageSize -> 150];
		  
		Column[{
			Row[{
				" ",
				Column[{
					Row[{TextCell[testoRow1, "Text", FontSize -> 28]}],
					Row[{TextCell[testoRow2, "Text", FontSize -> 28]}]
				}],
		  		buttonNew
		  	}, "                         "],
			Row[{
				Column[{plot}],
			  	"               ",
			  	Column[{
			    	Row[{TextCell["Stai lavorando sulla funzione:", "Text", FontSize -> 30]}],
			    	Row[{
			    		TextCell["    f(x) = ", "Text", FontSize -> 30], 
			    		TextCell[TraditionalForm[fun[x]], "Text", FontSize -> 30](*,
			    		TextCell["     f(", "Text", FontSize -> 30], TextCell[ToString[x0], "Text", FontSize -> 30], TextCell[ ") = ", "Text", FontSize -> 30],
			    		TextCell[TraditionalForm[fun[x]/.x->x0], "Text", FontSize -> 30] *)
			    		
			    	}],
			        Row[{calculator}],
			        Row[{
			       		TextCell["Inserisci il risultato: ", "Text", FontSize -> 30], 
			    		InputField[Dynamic[Risultato],Number,ImageSize->100],
			    		"  ",
			    		Button[Style["Verifica", FontSize -> 20], ImageSize -> 150]
			       	}]
			   	}, Spacings -> 3]
			}]
		}, 
		Spacings -> 4,
		Frame -> True
		]
	];

EndPackage[]


