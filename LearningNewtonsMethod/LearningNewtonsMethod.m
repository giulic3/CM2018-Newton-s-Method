(* ::Package:: *)

(* Wolfram Language Package *)
(* Created by the Wolfram Workbench 07-May-2018 *)
(* Copyright *)

(* Made by: Anna Avena (INF), Giulia Cantini (INF), Roberto Ferraro (MAT), Nicola Mainetti (MAT), Matteo Sanfelici (INF) *)

BeginPackage["LearningNewtonsMethod`"]
(* Exported symbols added here with SymbolName::usage *) 


GoForward::usage = "Allows user to go to the next slide";
GoBack::usage = "Allows user to go back to the previous slide ";
GoHomepage::usage = "Return to the homepage";
ReadInputFile::usage = "Reads expressions from 'inputExp' file";
NewtonInteractive::usage = "Animated Newtom Method";
ConvertImageToFullyScaledNinePatch::usage = "Set notebook background image";
RootExampleGraphic::usage = "Shows a example plot about root concept";
BisectionInteractive::usage = "Interactive Bisection Method";
BisectionAnimated::usage = "Animated Bisection Method";
SecantAnimated::usage = "Animated Secant Method";
NewtonAnimated::usage = "Animated Newton Method";
FirstExample::usage = "Shows the cases in which the Newton's method fails ";
SecondExample::usage = "Shows the cases in which the Newton's method fails";
Calculator::usage = "Shows a scientific calculator that allows user to write functions";
MethodsComparison::usage = "Show convergence to solution using three different methods";
i::usage = "Global variable used for the exercises part";
SecantInteractive::usage = "Interactively show step of iteration in the secant method";

Begin["`Private`"]

(* Function that links a slide with the next,
gets in input the Cell Tag of the slide to be linked 
and a boolean value that indicates if the link must be abled or disabled *)
GoForward[nextSlide_, bool_] :=
    Module[ {},
        If[ bool== Boole[True], (* check on the boolean value *)
            Hyperlink[Import["Images/forward.png"], {EvaluationNotebook[], ToString[nextSlide]}, ImageSize->{100,100}],
            Import["Images/forwardgrey.png",ImageSize->{100,100}]
        ]
    ];
 
(* Function that links a slide with the previous,
gets in input the Cell Tag of the slide to be linked 
and a boolean value that indicates if the link must be abled or disabled *)
GoBack[prevSlide_, bool_] :=
    Module[ {},
        If[ bool== Boole[True], (* check on the boolean value *)
            Hyperlink[Import["Images/back.png"], {EvaluationNotebook[], ToString[prevSlide]},ImageSize->{100,100}],
            Import["Images/backgrey.png",ImageSize->{100,100}]
        ]
    ];
    
(* Function that links a slide with the homepage, gets in input the Cell Tag of the main slide *)
GoHomepage[homeSlide_] :=
    Module[ {},
        Hyperlink[Import["Images/home.png", ImageSize->{100,100}], {EvaluationNotebook[], ToString[homeSlide]}]
    ];
    
(* Function that reads expressions from 'inputExp' file *)
(* The file contains N rows composed in this way:
function, initial interval point, final interval point, point of the first iteration *)
ReadInputFile[] :=
    Module[ {expressions,esp,func,a,b,x0,d}, (* local variables *)
        esp = {}; (* array that will contain all the expressions and other attributes mentioned above *)
        expressions = Import["inputExp"];
        For[i = 1, i <= Length[expressions], i++,
             (* Appends every row contained in the file to esp array *)
             esp = Append[esp,expressions[[i]]]; 
            ];
        i = 1;
        func = ToString[esp[[i]][[1]]]; (* gets the first parameter of exp that corresponds to the function *)
        a = esp[[i]][[2]]; (* gets the second parameter of exp that corresponds to the initial interval point *)
        b = esp[[i]][[3]]; (* gets the second parameter of exp that corresponds to the final interval point *)
        x0 = esp[[i++]][[4]]; (* gets the third parameter of exp that corresponds to the point of the first iteration *)
        d = Esercizio[func,a,b,x0]; (* Shows the first exercise *)
        Column[{
            Row[{
                Button[ (* Button that allows user to change exercise *)
                    Style["Nuovo Esercizio", FontSize -> 25],
                    {d = Esercizio[ (* sets input parameters that are passed to Esercizio function *)
                        ToString[esp[[1 + Mod[i, Length[esp]]]][[1]]], 
                        esp[[1 + Mod[i, Length[esp]]]][[2]],
                        esp[[1 + Mod[i, Length[esp]]]][[3]],
                        esp[[1 + Mod[i++, Length[esp]]]][[4]]
                    ],
                    Clear[xn]},
                    ImageSize -> 200
                ]
            }],
            Row[{
                Dynamic[d] (* Display the exercise *)
            }]
        }]
    ];
 

(* Function that shows an interactive manipulate *)
NewtonInteractive[] :=
    Module[ 
    	{newton,listaIntervalli,listaFunzioni},
    	
    	listaIntervalli={
    		{0, 2*Pi},
    		{0, 2*Pi},
    		{0, 6},
    		{0, 6},
    		{0, 2*Pi},
    		{0, 6},
    		{0, 50}
    	};
    	listaFunzioni={
    		TraditionalForm[Cos[x]],
    		TraditionalForm[Sin[x]],
    		TraditionalForm[-7 + x^2],
    		TraditionalForm[-1 + x - 3*x^2 + x^3],
    		TraditionalForm[Sin[x]*Cos[x]],
    		TraditionalForm[-1 + x^2*Log2[x]]
    	};
        Manipulate[
        	
            newton[input,N[x0], -100, 100, n],
            
            {{input,listaFunzioni[[1]], "Funzione: "}, 
                listaFunzioni,
                ControlType -> PopupMenu},
            {{n, 0, "Numero di Iterazioni: "}, {0, 1, 2, 3, 4, 5, 6}, ControlType -> SetterBar},
            {{x0, 0.3, Subscript["x", "0"]}, 0.01, 50, Appearance -> "Labeled"},
            
            Initialization:>{
                newton[input_,x0_,a_,b_,n_] :=
                	
                    Module[ 
                    	{list,funzioneFinale,listArrow},
                    	
                    	funzioneFinale = ToExpression[ToString[input]];
                        list = NestList[ (#1 - ((funzioneFinale/.x->#1)/(D[funzioneFinale,x]/.x->#1))) & ,x0, n];
                        
                        Clear[listArrow];
                        listArrow = {{list[[1]],0}};
                        For[i=1,i<=n,i++,
                    		listArrow = Append[listArrow,{list[[i]],funzioneFinale/.x->list[[i]]}];
                    		listArrow = Append[listArrow,{list[[i+1]],0}];
                    	];
                    	
                        Column[{
                            Row[{TextCell[HoldForm[Subscript[{Subscript["x", "k"]}, "k" = 0]^n],FontSize->17],TextCell[ " = ",FontSize->17], TextCell[list,FontSize->17]}],
                            Plot[
                                funzioneFinale, {x, a, b},
                                PlotRange -> All,
                                AxesLabel -> {Style["x", 16], Style["y", 16]},
                                PlotStyle -> Thickness[0.006],
                                Epilog -> {
                                    {
                                    	Red, 
                                    	Thickness[0.002],
                                        
                                        If[n>0,Line[{listArrow}]]
                                    },
                                    {
                                    	PointSize[0.015],
                                        Point[{list[[i]], 0}]
                                    }
                                },
                                ImageSize -> {800,500}
                          	]
                        }]
                    ]
            	}
            ]
    ];

(* Simply plot a pretty calculator way to write function *)
Calculator[] :=
    Module[  (* local variables *)
    	{buttonOptions = {ImageSize->Full, Background->White},
		f,finalFunction,buttonEmpty,buttonSin,buttonCos,buttonLog,
		buttonTan,buttonLn,button1,button2,button3,buttonEnd,
		buttonAllClear,buttonClearEntry,buttonLpar,buttonRpar,
		buttonPi,buttonElev,buttonSquare,buttonNepero,button4,
		button5,button6,buttonComma,buttonPlus,buttonTimes,
		buttonMinus,buttonDivide,buttonSqrt,button7,button8,button9,button0},
		(* buttons definition *)	
		f = ""; (* function that has been composed *)
        finalFunction[x_] = ""; (* result *)
        buttonEmpty = "";
        buttonSin = Button["sin",f = f<>"sin", buttonOptions];
        buttonCos = Button["cos",f = f<>"cos", buttonOptions];
        buttonLog = Button["log",f = f<>"log", buttonOptions];
        buttonTan = Button["tan",f = f<>"tan", buttonOptions];
        buttonLn = Button["ln",f = f<>"ln", buttonOptions];
        button1 = Button[" 1 ",f = f<>"1", buttonOptions];
        button2 = Button[" 2 ",f = f<>"2", buttonOptions];
        button3 = Button[" 3 ",f = f<>"3", buttonOptions];
        buttonEnd = Button["=",finalFunction[x_] = N[ToExpression[f,TraditionalForm],2], buttonOptions];
        buttonAllClear = Button["AC",{f = "",finalFunction[x_] = ""}, buttonOptions];
        buttonClearEntry = Button["CE",{f = StringDrop[f,-1]}, buttonOptions];
        buttonLpar = Button["(",f = f<>"(", buttonOptions];
        buttonRpar = Button[")",f = f<>")", buttonOptions];
        buttonPi = Button["\[Product]",f = f<>"Pi", buttonOptions];
        buttonElev = Button["^",f = f<>"^", buttonOptions];
        buttonSquare = Button["\!\(\*SuperscriptBox[\(x\), \(2\)]\)",f = f<>"^2", buttonOptions];
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
        
        Grid[ (* button arrangement on screen *) 
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
    Module[ 
    	{},
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
            Placed[
            	Row[{
            		TextCell["f(x) ="],
        			TextCell[TraditionalForm["x^6 - 1221"]]
            	}],            			
            	{Right,Top}
            ]
        ]
    ];

BisectionInteractive[] :=
	Module[{quad,plotoptions,pl,linie,bisec,intervalle,vertline},
		Manipulate[
	    	Show[{
	    		Plot[
	    			quad[x], {x, 0, 2.5},
	                PlotStyle -> {{Thickness[0.005]}, {Thickness[0.001]}, {Thickness[0.001]}, {Thickness[0.001]}, {Thickness[0.001]}},
	                Evaluate[plotoptions], Filling -> {1 -> Axis},
	                PlotLabel -> pl[f, {aP[[1]], bP[[1]]}, nn]
	            ],
	            Graphics[{Red, Line /@ intervalle[f, {aP[[1]], bP[[1]]}, nn]}],
	            Graphics[{Dashed, vertline /@ linie[f, {aP[[1]], bP[[1]]}, nn]}]}, 
	            ImageSize->{800,500}
	      	],
	        {{f, quad,""},{quad->"\!\(\*SuperscriptBox[\(x\), \(4\)]\)-2"}, ControlType -> None},
	    	{{aP, {0.5, 0}}, {0, 0}, {2.5, 0}, ControlType -> Locator, Appearance -> Style["|", 20, Bold, RGBColor[1, 0, 0]]},
	        {{bP, {2, 0}}, {0,0}, {2.5, 0}, ControlType -> Locator, Appearance -> Style["|", 20, Bold, RGBColor[1, 0, 0]]},
	        {{nn, 1,"Iterazione"}, 1, 12, 1, Appearance -> "Labeled"},
	        Initialization:>{
	          	plotoptions = {PlotRange->{{0,2.5},{-1,1}},PlotRangePadding->0.25, AspectRatio->Automatic,ImageSize->{800,500}},
	            Attributes[PlotRange] = {ReadProtected},
	        	pl[f_,{a_,b_},n_] :=
				If[ f[a]*f[b]<=0,
	                   Row[{"Intervallo: [",linie[f,{a,b},n][[1]],", ",linie[f,{a,b},n][[2]],"]"}],
	                   Row[{"Errore"}]
				],
	            linie[f_,{a_,b_},1] = {Min[a,b],Max[a,b]},
	           	linie[f_,{a_,b_},n_] :=
	            	If[ f[a]*f[b]<=0,
	                	bisec[f,linie[f,{a,b},n-1]],
	                   	{a,b}
	               	],
	                linie[f$_,1] = {0,2},
	                linie[f_,n_] := bisec[f,linie[f,n-1]],
	                bisec[f_,{a_,b_}] :=
	              		If[ f[a]*f[b]<=0,
	                  		If[ f[(a+b)/2]*f[a]<=0,
	                      		{a,(a+b)/2},
	                  			If[ f[(a+b)/2]*f[b]<0,
	                          		{(a+b)/2,b}
	                      		]
	                  		],
	                  		{a,b}
	              		],
	              		intervalle[f_,{a_,b_},nn_] := 
	              			Table[
	              				{
	              					{linie[f,{a,b},i][[1]],-(i/30)},
	          						{linie[f,{a,b},i][[2]],-(i/30)}
	          					},
	          					{i,1,nn}
	          				],               
	                    intervalle[f_,nn_] :=
	          				Table[
	          					{
	          						{linie[f,i][[1]],-(i/50)},
	          						{linie[f,i][[2]],-(i/50)}
	          					},
	          					{i,1,nn}
	          				],
	                  	vertline[a_] := Line[{{a,-1},{a,1}}], 
				       	quad[x_] := (x^4 - 2)
			}    
		]
    ]; 
                 
                                         
BisectionAnimated[] :=
    Module[ {a,b,m,steps,f},
        f[x_] :=
            Sin[x];
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
         (* Protected Variable *) {poly, xValues,x0,x1},
         (* Initialization of protected variable *)
        x0 = 0.4;
        x1 = 1.8;
        poly[x_] :=
            x^4 - 2;
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
    Module[ {list, funz, x0},
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
    
    SecantInteractive[] :=
Module[{},
	Manipulate[
Secant[iteration],
  {{iteration,0, "Numero di iterazioni: "},{0,1,2,3,4,5,6},ControlType->SetterBar},
Initialization:>{
Secant[i_] :=
Module[{xValues,x0,x1,f},
x0= 0.4;
x1 = 1.8;
f[x_] := x^4 - 2;
xValues =
			NestList[{
				#[[1]],
				#[[1]] - f[#[[1]]]*((#[[1]] - #[[2]]) / (f[#[[1]]] -  f[#[[2]]]))
			} &,
			 {x1, x0},
			   i
			];
 Plot[
		        f[x], {x, 0, 3},
		        PlotRange -> {{0, 2}, {-3, 10}},
		     	Epilog -> {
			{PointSize[0.01], 
			(Point[{#1, 0}] &) /@ Flatten[xValues]
		},
		{Thickness[0.002],
		   MapIndexed[{ Hue[0.76*(#2[[1]]/6)],
		   Line[{
{#1[[1]], f[#[[1]]]},
  {#1[[2]], f[#1[[2]]]}  }]} &, xValues  ]
	},
		         	{Thickness[0.002], Black,
		          		Line[({{#1, 0}, {#1, f[#1]}} &) /@ Flatten[xValues]]}
		     	},
		       	Axes -> True,
		       	ImageSize -> {800,500},
		       	AxesLabel -> {Style["x", 16], Style["y", 16]}
		    ]

]
}
]	   
    ];

(* these two functions show cases/examples in which the Newton's method fails *)
(* xlog(x)-1*)
FirstExample[] :=
    Module[ {list,f},
        f[x_] = x Log[x]-1//N;
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
SecondExample[] :=
    Module[ {},
        Plot[
            {Cos[x], y = 1},{x,-Pi,Pi},
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
         
(* Function that calculate the value of f(x) for a specific given x 
and display it on screen *)        
AddIteration[i_,fun_,x0_] :=
    Module[ {xn},
        xn = Null;
        (*NewtonList = NestList[N[Rationalize[(Rationalize[#1] - ((fun /. x->Rationalize[#1])/(D[fun, x]/.x->Rationalize[#1])))],3] &, Rationalize[x0], 10];*)
        Row[{
            TextCell[ Subscript["x", i], FontSize -> 25],
              TextCell["=", FontSize -> 25],
              InputField[Dynamic[xn], BaseStyle->FontSize->25, ImageSize -> 130],
              TextCell["-", FontSize -> 25],
              FractionBox[ (* area that allows to insert the value of f(x) and f'(x) *)
                RowBox[{
                      TextCell["f(", FontSize -> 25],
                      InputField[Dynamic[xn], BaseStyle->FontSize->25, ImageSize -> 130],
                      TextCell[")", FontSize -> 25]}],
                    RowBox[{TextCell["f'(", FontSize -> 25],
                      InputField[Dynamic[xn], BaseStyle->FontSize->25, ImageSize -> 130],
                      TextCell[")", FontSize -> 25]}]
            ] // DisplayForm, (* shows calculations on screen *) 
              TextCell["  ->  ", FontSize -> 25],
              TextCell[ Subscript["x", i], FontSize -> 25, FontColor->Blue],
              TextCell["=", FontSize -> 25, FontColor->Blue],
              If[ xn==Null,
                  xn = Subscript[x,i-1]
              ];
              TextCell[Dynamic[N[(Rationalize[xn] - ((fun /. x->Rationalize[xn])/(D[fun, x]/.x->Rationalize[xn])))]], FontSize -> 25, FontColor->Blue]
        }]
    ];  
                   
End[]

(* Function that manage the exercise area,
gets in input the function, the initial interval point, the final interval point and 
the first point from which start the iteration *)
Esercizio[funzione_, a_, b_,x0_] :=
    Module[ {calculator,plot,testoRow1,testoRow2,buttonNew,fun,i,IterationList,Iter2Result,Risultato},
        fun = ToExpression[funzione]; (* the current function *)
        calculator = Calculator[];
        (* plot the current function *)
        plot = 
            Plot[
                fun, {x, a, b},
                PlotStyle -> Thickness[0.006],
                Epilog->{
                    PointSize[Large], (* plots the point of the first iteration *)
                    Point[{x0,0}](*    ,
                    Text[DisplayForm@RowBox[{Subscript["x","0"]}], {x0, -0.20}] *)
                },
                ImageSize -> Large
            ];
        (* exercise's text *) 
        testoRow1 = "Calcolare un'approssimazione dello zero usando il Metodo di Newton,";
        testoRow2 = "con due iterazioni, 	 partendo dalla prima approssimazione data";
        buttonNew = Button[Style["Nuovo Esercizio", FontSize -> 20], ImageSize -> 150];
        Off[FindRoot::cvmit];
        i = 1;
        (*Iter2Result = FindRoot[fun, {x, Rationalize[x0]}, Method -> "Newton", MaxIterations -> , WorkingPrecision -> 3][[1]][[2]]    ;*)
        Iter2Result = NestList[ (* calculate the second iteration result *) 
            N[(Rationalize[#1] - ((fun /. x->Rationalize[#1])/(D[fun, x]/.x->Rationalize[#1])))] &, x0, i+1];
        Print[Dynamic[Iter2Result]];
        IterationList = {AddIteration[i,fun,x0]}; (* manage all the exercise area *)
        Column[{
            Row[{"   ",
                Column[{
                    Row[{
                        " ",
                        Column[{
                            Row[{TextCell[testoRow1, "Text", FontSize -> 28]}], (* text exercise *)
                            Row[{TextCell[testoRow2, "Text", FontSize -> 28]}] (* text exercise *)
                        }]
                      }, "                         "],
                    Row[{
                        Column[{plot}], (* plot the current function *)
                          "               ",
                          Column[{
                              Row[{}],
                            Row[{
                                TextCell["Funzione Data:", "Text", FontSize -> 30], 
                                TextCell["    f(x) = ", "Text", FontSize -> 30,FontColor->Blue], 
                                TextCell[TraditionalForm[fun], "Text", FontSize -> 30,FontColor->Blue]}],  (* function *)
                            Row[{
                                TextCell["Prima approssimazione data:", "Text", FontSize -> 30], 
                                TextCell["    ", "Text", FontSize -> 30,FontColor->Blue], 
                                TextCell[Subscript["x","0"],FontSize -> 30,FontColor->Blue],
                                TextCell[" = ", "Text", FontSize -> 30,FontColor->Blue],
                                TextCell[x0,FontSize->30,FontColor->Blue] (* first iteration point *)
                                
                            }],
                            Dynamic[Column@IterationList], (* add new iteration *)
                            Button[
                                Style["Aggiungi Iterazione", FontSize -> 20], 
                                {
                                    AppendTo[IterationList, AddIteration[++i,fun,x0]],
                                    Iter2Result = 
                                        NestList[N[(Rationalize[#1] - ((fun /. x->Rationalize[#1])/(D[fun, x]/.x->Rationalize[#1])))] &, x0, i+1]
                                }, ImageSize->200],
                            (*Row[{calculator}],*)
                            Row[{ (* in this section is verified the result inserted by the user *)
                                   TextCell["Inserisci il risultato: ", "Text", FontSize -> 30], 
                                InputField[Dynamic[Risultato], String, BaseStyle->FontSize->25, ImageSize->150],
                                "  ",
                                Print[Dynamic[Iter2Result[[i+1]]]];
                                Button[Style["Verifica", FontSize -> 20],
                                    {
                                        If[ ToString[Risultato] == ToString[Iter2Result[[i+1]]], (* verification of the entered value *) 
                                            CreateDialog[{ (* if correct *)
                                                Column[{
                                                    TextCell["Complimenti!", FontSize -> 25],
                                                    TextCell[\[HappySmiley], FontSize -> 200, FontColor -> Green],
                                                    TextCell["Hai risolto l'esercizio correttamente!", FontSize -> 25],
                                                    DefaultButton[]
                                                },Alignment->Center]
                                            },WindowTitle->"Corretto"],
                                            CreateDialog[{ (* if wrong *)
                                                Column[{
                                                    TextCell["Errore!", FontSize -> 25],
                                                    TextCell[\[WarningSign], FontSize -> 200, FontColor -> Red],
                                                    TextCell["Riprova e stai pi\[UGrave] attento", FontSize -> 25],
                                                    DefaultButton[]
                                                },Alignment->Center]
                                            },WindowTitle->"Sbagliato"]
                                        ]
                                    },
                                    ImageSize -> 150]
                               }]
                           }, Spacings -> 3]
                    }]
                }],
                "   "
            }]
        },
        Spacings -> 4,
        Frame -> True
        ]
    ];

(* Function that compares the bisection, secant and Newton's methods *)
MethodsComparison[] :=
    Module[ {a,b,n,f},
        Clear[f];
        f[x_] :=
            Sin[x];
        (* initial interval *)
        a = 2.5;
        b = 1.4Pi;
        (* maximum number of iterations *)
        n = 10;
        bisectionRoots = (* initial root of the bisection method *)
            N[NestList[
                If[ f[#[[1]]]*f[(#[[1]]+#[[2]])/2]<0,
                    {#[[1]],(#[[1]]+#[[2]])/2},
                    {(#[[1]]+#[[2]])/2,#[[2]]}
                ] &,
                {a,b},
                n
            ]];
        (* costruisco una lista annidata di coppie {xi-1, xi}, i due estremi su cui lavora secanti *)
        (* a, b - f(b)(b-a)/(f(b)-f(a)) *)
        secantRoots = (* initial root of the bsecant method *)
            N[NestList[
            {#[[1]],#[[2]]-f[#[[2]]](#[[2]]-#[[1]])/(f[#[[2]]]-f[#[[1]]])} &,
            {a,b},
            n]];
        newtonRoots = NestList[ #1 - f[#1]/Derivative[1][f][#1] &, a, n]; (* initial root of the Newton's method *)
        Manipulate[
            Row[{
            (* bisection *)
                Column[{
                    Plot[f[x],{x,1.5,4.5},
                        Epilog -> {
              Directive[{Thick, Gray, Dashed}],
              (* calculation and plot of the subsequent roots for the bisection method *)
                    InfiniteLine[{(bisectionRoots[[i]][[1]]+bisectionRoots[[i]][[2]])/2, 0}, {0, 1}],
                    {
                    Red,
                    PointSize[.015],
                    Point[{(bisectionRoots[[i]][[1]]+bisectionRoots[[i]][[2]])/2,0}]
                    }
                },
        ImageSize->500,
        PlotLabel->"Bisezione"]," ",
        (* display the current root value for the bisection method *)
        TextCell[Row[{Subscript[x,i]," = ",(bisectionRoots[[i]][[1]]+bisectionRoots[[i]][[2]])/2},Alignment->Center],"Text",TextAlignment->Center,CellBaseline->Center, CellSize->{500,50}]}],
        (* secant *)
        Column[{
        Plot[f[x],{x,1.5,4.5},
        Epilog -> {
                  Directive[{Thick, Gray, Dashed}],
                  (* calculation and plot of the subsequent roots for the secant method *)
                    InfiniteLine[{secantRoots[[i]][[2]], 0}, {0, 1}],
        {
        Red,
        PointSize[.015],
        Point[{secantRoots[[i]][[2]],0}]
        }
                },
        ImageSize->500,
        PlotLabel->"Secanti"]," ",
        (* display the current root value for the secanti method *)
        TextCell[Row[{Subscript[x,i]," = ",secantRoots[[i]][[2]]},Alignment->Center],"Text",TextAlignment->Center,CellBaseline->Bottom, CellSize->{500,50}]}],
        (* Newton *)
        Column[{
        Plot[f[x],{x,1.5,4.5}, 
        Epilog -> {
                  Directive[{Thick, Gray, Dashed}],
                  (* calculation and plot of the subsequent roots for the bNewton'a method *)
                    InfiniteLine[{newtonRoots[[i]], 0}, {0, 1}],
        {
        Red,
        PointSize[.015],
        Point[{newtonRoots[[i]],0}]
        }
                },
        ImageSize->500,
        PlotLabel->"Newton"],
        (* display the current root value for the Newton's method *)
        TextCell[Row[{Subscript[x,i]," = ", newtonRoots[[i]]},Alignment->Center],"Text", TextAlignment->Center, CellBaseline->Bottom,CellSize->{500,50}]
        }]}],
        (* slider that shows the iteration steps *)
        {i,1,10,1,Appearance->{"Open","Labeled"}}
        ]
    ];

EndPackage[]


