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
NewtonInteractive::usage = "Animated Newton Method";
ConvertImageToFullyScaledNinePatch::usage = "Set notebook background image";
RootExampleGraphic::usage = "Shows a example plot about root concept";
BisectionInteractive::usage = "Interactive Bisection Method BisectionMethod[pm=0/1,it=0/1] pm->PopupMenu, it->Interactive";
BisectionAnimated::usage = "Animated Bisection Method";
SecantAnimated::usage = "Animated Secant Method";
NewtonAnimated::usage = "Animated Newton Method";
FirstExample::usage = "Shows the cases in which the Newton's method fails ";
SecondExample::usage = "Shows the cases in which the Newton's method fails";
Calculator::usage = "Shows a scientific calculator that allows user to write functions";
MethodsComparison::usage = "Show convergence to solution using three different methods";
i::usage = "Global variable used for the exercises part";
AlgoNewton::usage="";
SecantInteractive::usage = "Interactively show step of iteration in the secant method";
ShowDetails::usage = "ciao";
Bolzano::usage = "Show root graph example step by step: press a button to advance";
AlgoBisez::usage = "a";
AlgoSec::usage = "a";


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
and a boolean value that indicates if the link must be enabled or disabled *)
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


ShowDetails[] :=
    Module[
      {text = Null, buttonStatus = "Closed"},
      NotebookWrite[EvaluationNotebook[],
        Cell[BoxData@ToBoxes@Dynamic[text], "Text",
          FontFamily -> Source Sans Pro, FontSize -> 36]];

      Row[{
      button = Button["Approfondimento 1",
        If[Equal[buttonStatus, "Closed"],
          (text =
            ToString [
              StringForm["Provate a considerare la funzione f(x) = `1`",
                Superscript["x",
                  "2"]]]; buttonStatus = "Open"),
          (text = ""; buttonStatus = "Closed")
        ]
      ]
      }]
    ];

(* Function that progressively introduces and explains Bolzano's Theorem *)
Bolzano[] :=
    Module[{
      buttonStatus1 = "closed",
      buttonStatus2 = "closed",
      buttonStatus3 = "closed",
      buttonStatus4 = "closed",
      plot = "",
      f,
      x,
      a,
      b,
      textCellStyle = {}
    },

      f[x_] := x^2-2;
      a = 1;
      b = 2;
      textCellStyle = {FontSize->36, FontFamily->"Source Sans Pro"};

      Row[{
      Column[{
        Row[{
          TextCell["Notiamo che la ", textCellStyle],
          TextCell["funzione f", textCellStyle, Blue],
          TextCell["(x) = ", textCellStyle],
          TextCell[TraditionalForm[ToExpression["x^2 - 2"]], textCellStyle],
          TextCell[" \[EGrave] ", textCellStyle],
          Button["continua",
            If[
              buttonStatus1 == "closed" ,
              (buttonStatus1 = "open";
              plot =
                  Plot[x^2 - 2, {x, -2.5, 2.5},
                    ImageSize -> {800, 500},
                    Filling -> Axis,
                    FillingStyle -> {LightGreen, LightCyan}]
              )
            ],
            ImageSize->{200,70},
            BaseStyle->{FontFamily->"Source Sans Pro", FontSize->36}
          ]
        }],
        Row[{}], (* linefeed *)
        Row[{
          TextCell[" e che f calcolata in a = 1 \[EGrave] ", textCellStyle],
          Button["negativa",
            If[
              buttonStatus2 == "closed" && buttonStatus1 == "open",
              ( buttonStatus2 = "open";
              plot = Show[
                plot,
                Epilog -> {
                  {
                    PointSize[0.012],
                    {
                      Blue,
                      Point[{a, f[a]}],
                      Text[{a, f[a]}, Offset[{40, 10}, {a, f[a]}]]
                    }
                  }
                }
              ]
              )
            ]
            ImageSize->{230,70},
            BaseStyle->{FontFamily->"Source Sans Pro", FontSize->36}
          ],
          TextCell[" .", textCellStyle]
        }],
        Row[{}],
        Row[{
          TextCell["Inoltre notiamo che f calcolata in b = 2 \[EGrave] ", textCellStyle],
          Button["positiva",
            If[
              buttonStatus3 == "closed" && buttonStatus1 == "open" && buttonStatus2 == "open",
              (buttonStatus3 = "open";
              plot = Show[
                plot,
                Epilog -> {
                  {
                    PointSize[0.012],
                    {
                      Blue,
                      Point[{a, f[a]}],
                      Text[{a, f[a]}, Offset[{40, 10}, {a, f[a]}]],
                      Point[{b, f[b]}],
                      Text[{b, f[b]}, Offset[{40, 10}, {b, f[b]}]]
                    }
                  }
                }
              ]
              )
            ],
            ImageSize->{200,70},
            BaseStyle->{FontFamily->"Source Sans Pro", FontSize->36}
          ],
          TextCell[" .", textCellStyle]
        }],
        Row[{}],
        Row[{
          TextCell["Ci\[OGrave] che abbiamo appena osservato sono le ipotesi del ", textCellStyle],
          TextCell["Teorema di Bolzano.", textCellStyle, FontWeight->Bold]
        }],
        Row[{}],
        Row[{
        (* TODO riquadro colorato interattivo, holy shit. *)
        }],
        Row[{}],
        Row[{
          TextCell["Quindi possiamo esser certi che tra ", textCellStyle],
          TextCell["1", textCellStyle, Green],
          TextCell[" e ", textCellStyle],
          TextCell["2", textCellStyle, Green],
          TextCell[" \[EGrave] compreso un suo ", textCellStyle],
          Button["zero",
            If[
              buttonStatus4 == "closed" && buttonStatus1 == "open" && buttonStatus2 == "open" && buttonStatus3 == "open",
              (buttonStatus4 = "open";
              plot = Show[
                plot,
                Epilog -> {
                  PointSize[0.012],

                  {
                    Blue,
                    Point[{a, f[a]}],
                    Text[{a, f[a]}, Offset[{40, 10}, {a, f[a]}]],
                    Point[{b, f[b]}],
                    Text[{b, f[b]}, Offset[{40, 10}, {b, f[b]}]]
                  },
                  {
                    Red,
                    Point[{Sqrt[2], 0}],
                    Text[{Sqrt[2], 0}, Offset[{40, 10}, {Sqrt[2], 0}]]
                  }
                }
              ])
            ],
            ImageSize->{200,70},
            BaseStyle->{FontFamily->"Source Sans Pro", FontSize->36},
            Background->Red
          ],
          TextCell[" .", textCellStyle]
        }],

        Row[{}]
      }], (* end column *)
        Column[{
          Dynamic[plot]
        }]
      }] (* end external row *)
      ];



(* Function that reads expressions from 'inputExp' file *)
(* The file contains N rows composed in this way:
function, initial interval point, final interval point, point of the first iteration *)
ReadInputFile[] :=
    DynamicModule[ {expressions,esp,func,a,b,x0,d,i}, (* local variables *)
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
NewtonInteractive[pm_,it_] :=
    DynamicModule[ 
        {newton,ff,passInput,listaIntervalli,listaFunzioni,selectedInput,aa,bb,interv},
        (* plotting interval for each function *)
        listaIntervalli={
            {0.375, 3},
            {0, 2*3.14},
            {0, 2*3.14},
            {-4, 8},
            {0, 6},
            {0, 2*3.14},
            {-3, 6}
        };

        (* list of functions that can be selected from the popup menu *)
        listaFunzioni={
            TraditionalForm[x^2-2],
            TraditionalForm[Cos[x]],
            TraditionalForm[Sin[x]],
            TraditionalForm[-9 + (x-2)^2],
            TraditionalForm[-4 + x - 3*(x)^2 + (x)^3],
            TraditionalForm[Sin[x]*Cos[x]],
            TraditionalForm[-1 + x^2*Log2[x]]
        };

        ff=TraditionalForm[x^2-2];

        Manipulate[
            passInput[ff],
            Row[{
                TextCell["Funzione: ", FontSize->23],
                If[pm==1,
                    PopupMenu[Dynamic[ff], listaFunzioni, MenuStyle->{FontSize->23}],
                    TextCell[TraditionalForm[x^2-2], FontSize->23]
                ]
            }],
            Initialization:>{
                passInput[input_]:= DynamicModule[
                    {nn,x00},
                    (* select the position (index) of the first occurence of input inside listaFunzioni *)
                    selectedInput = Position[listaFunzioni,input][[1]];
                    (* select the corresponding plotting interval *)
                    interv = listaIntervalli[[ selectedInput ]];
                    (* aa and bb store bounds of the selected interval *)
                    aa = interv[[1]][[1]];
                    bb = interv[[1]][[2]];

                    Manipulate[
                        newton[input, N[x00], aa, bb, nn],
                        Column[{
                            Row[{
                                TextCell[Subscript[x,0], FontSize->23],
                                Slider[Dynamic[x00],{aa+0.01,bb-0.01,0.01}],
                                TextCell[" ",FontSize->25],
                                TextCell[Dynamic[x00],FontSize->23]
                            }],
                            Row[{
                                If[it==1,TextCell["Iterazioni ", FontSize->23]],
                                If[it==1,Slider[Dynamic[nn],{1,12,1}]],
                                If[it==1,TextCell[" ",FontSize->25]],
                                If[it==1,TextCell[Dynamic[nn],FontSize->23],nn=1;]
                            }]
                        }],

                        Initialization:>{
                            newton[inputFun_,x0_,a_,b_,n_] := Module[
                                {list,funzioneFinale,listArrow},

                                funzioneFinale = ToExpression[ToString[inputFun]];
                                list = NestList[ (#1 - ((funzioneFinale/.x->#1)/(D[funzioneFinale,x]/.x->#1))) & ,x0, n];
                                Clear[listArrow];
                                listArrow = {{list[[1]],0}};
                                For[i=1,i<=n,i++,
                                    listArrow = Append[listArrow,{list[[i]],funzioneFinale/.x->list[[i]]}];
                                    listArrow = Append[listArrow,{list[[i+1]],0}];
                                ];

                                Column[{
                                    Row[{
                                        TextCell[HoldForm[Subscript[{Subscript["x", "k"]}, "k" = 0]^n],FontSize->17],
                                        TextCell[ " = ",FontSize->17],
                                        TextCell[list,FontSize->17]
                                    }],
                                    Plot[
                                        funzioneFinale, {x, aa, bb},
                                        PlotRange -> {{aa,bb},Full},
                                        AxesLabel -> {Style["x", 16], Style["y", 16]},
                                        PlotStyle -> Thickness[0.006],

                                        Epilog -> {
                                            {
                                                Red,
                                                Thickness[0.002],
                                                If[n>0,Line[{listArrow}]]
                                            },
                                            {
                                                Red,
                                                PointSize[0.015],
                                                Point[{list[[i]], 0}]
                                            }
                                        },
                                        ImageSize -> {800,500}
                                    ]
                                }]
                            ]

                        },
                        Paneled->False
                    ]
                ]
            },
            Paneled->False
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

(* Call with pm = 1 to display a version of the graph with a popup-menu with a list of functions to choose from,
call with it = 1 to display graph with multiple iterations *)
BisectionInteractive[pm_,it_] :=
    DynamicModule[
        {listFunctions,listIntervals,passf,ff},

        listIntervals={
          {0.5,2},
          {(1/2*3.14),(1.5*3.14)}
        };

        listFunctions={
          TraditionalForm[x^2 - 2],
          TraditionalForm[Sin[x]]
        };

        ff=TraditionalForm[x^2-2];

        Manipulate[
            passf[ff],
            Row[{
                TextCell["Funzione: ", FontSize->25],
                If[pm==1,
                    PopupMenu[Dynamic[ff], listFunctions, MenuStyle->{FontSize->23}],
                    TextCell[TraditionalForm[x^2-2], FontSize->25]
                ]
            }],

            Initialization :> {
                passf[fun_]:=	DynamicModule[
                    {ax,bx,stepx,BisezMethod,selectedInput,interv,interva,intervb},
                        selectedInput = Position[listFunctions,ff][[1]];
                        interv = listIntervals[[ selectedInput ]];
                        interva = interv[[1]][[1]];
                        intervb = interv[[1]][[2]];
                        ax=((interva+intervb)*0.33);
                        bx=((interva+intervb)*0.66);

                    Manipulate[
                        BisezMethod[fun, ax, bx, stepx],
                        Column[{
                            Row[{
                                TextCell["a ", FontSize->23],
                                Slider[Dynamic[ax],{(interva+0.01), (intervb-0.01),0.01}],
                                TextCell[" ",FontSize->25],
                                TextCell[Dynamic[ax],FontSize->23]
                            }],
                            Row[{
                                TextCell["b ", FontSize->23],
                                Slider[Dynamic[bx],{(interva+0.01), (intervb-0.01),0.01}],
                                TextCell[" ",FontSize->25],
                                TextCell[Dynamic[bx],FontSize->23]
                            }],
                            Row[{
                                If[it==1,TextCell["Iterazioni ", FontSize->23]],
                                If[it==1,Slider[Dynamic[stepx],{1,12,1}]],
                                If[it==1,TextCell[" ",FontSize->25]],
                                If[it==1,TextCell[Dynamic[stepx],FontSize->23],stepx=1;]
                            }]
                        }],


                        Initialization :> {
                        BisezMethod[ffx_, aa_, bb_, nn_] :=
                            Module[
                                {line, bisec, intervals, vertline, fx},

                                fx = ToExpression[ToString[ffx]];

                                line[f_,{a_,b_},1] := {Min[aa, bb], Max[aa, bb]};

                                line[f_,{a_,b_},n_] :=
                                    If[ (f/.x->a) * (f/.x->b) <= 0,
                                        bisec[f, line[f,{a,b},n -1]],
                                        {a, b}
                                    ];

                                bisec[f_,{a_, b_}] :=
                                    If[(f/.x->a)*(f/.x->b) <= 0,
                                        If[(f/.x->((a + b)/2)) * (f/.x->a) <= 0,
                                              {a, (a + b)/2},
                                              If[(f/.x->((a + b)/2)) * (f/.x->b) < 0,
                                                  {(a + b)/2, b}
                                              ]
                                        ],
                                        {a, b}
                                    ];

                                intervals[f_,{a_,b_},step_] :=
                                    Table[
                                        {
                                          {line[f,{a,b},i][[1]], -(i/30)},
                                          {line[f,{a,b},i][[2]], -(i/30)}
                                        },
                                        {i, 1, step}
                                    ];

                                vertline[a_] := Line[{{a, -1}, {a, 1}}];

                                If[(fx /. x -> aa)*(fx /. x -> bb) <= 0,
                                    Row[{"Intervallo: [", line[fx, {aa, bb}, nn][[1]], ", ", line[fx, {aa, bb}, nn][[2]], "]"}],
                                    Row[{"Errore"}]
                                ];

                                Plot[
                                    fx, {x, interva, intervb},
                                    PlotRange -> All,
                                    ImageSize -> {800, 500} ,
                                    Epilog -> {
                                        {Red, Line /@ intervals[fx, {aa, bb}, nn]},
                                        {Dashed, vertline /@ line[fx, {aa, bb}, nn]}
                                    }
                                ]
                            ]
                        },
                        Paneled->False
                    ]
                ]
            },
            Paneled->False
        ]
    ];

BisectionAnimated[] :=
    DynamicModule[ {a,b,m,steps,f},
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
    DynamicModule[
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
    DynamicModule[ {list, funz, x0},
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
    
     (* TODO problema setterbar, l'iterazione 0 in realt\[AGrave] \[EGrave] la 1
    non mi piace che gli slider a e b dipendano dai valori di plotting,
    parte la ventola quando manipolo il grafico 
      *)
SecantInteractive[pm_,it_] :=  DynamicModule[
    {Secant, passInput, intervalsList, functionsList, selectedInput, aa, bb, interval,ff},

    intervalsList = {
        {0,4},
        {0.4,1.8},
        {0,10},
        {-1,1.5},
        {(-1/4)Pi, Pi},
        {0.4,1.8}
    };

    functionsList = {
        TraditionalForm[x^2 - 2],
        TraditionalForm[x^4 - 2],
        TraditionalForm[(x/2)^2+2Sin[x]+Cos[x]-10],
        TraditionalForm[3x+Sin[x]-Exp[x]],
        TraditionalForm[Cos[x]],
        TraditionalForm[x-Sin[x]-(1/2)]
    };

    ff=TraditionalForm[x^2-2];

    Manipulate[
        passInput[ff],
        Row[{
            TextCell["Funzione: ", FontSize->25],
            If[pm==1,
                PopupMenu[Dynamic[ff], functionsList, MenuStyle->{FontSize->23}],
                TextCell[TraditionalForm[x^2-2], FontSize->25]
            ]
        }],
        Initialization:> {
            passInput[input_]:= DynamicModule[
                {ax,bx,iteration},
                selectedInput = Position[functionsList,input][[1]];
                interval = intervalsList[[selectedInput]];
                aa = interval[[1]][[1]];
                bb = interval[[1]][[2]];
                ax = aa+0.01;
                bx = bb-0.01;
                Manipulate[
                    Secant[input,N[ax],N[bx],aa,bb,iteration],
                    Column[{
                        Row[{
                            TextCell["a ", FontSize->23],
                            Slider[Dynamic[ax],{(aa+0.01), (bb-0.01),0.01}],
                            TextCell[" ",FontSize->25],
                            TextCell[Dynamic[ax],FontSize->23]
                        }],
                        Row[{
                            TextCell["b ", FontSize->23],
                            Slider[Dynamic[bx],{(aa+0.01), (bb-0.01),0.01}],
                            TextCell[" ",FontSize->25],
                            TextCell[Dynamic[bx],FontSize->23]
                        }],
                        Row[{
                            If[it==1,TextCell["Iterazioni ", FontSize->23]],
                            If[it==1,Slider[Dynamic[iteration],{1,6,1}]],
                            If[it==1,TextCell[" ",FontSize->25]],
                            If[it==1,TextCell[Dynamic[iteration],FontSize->23],iteration=1;]
                        }]
                    }],
                    (*{{iteration, 0, TextCell["Numero di Iterazioni: ", FontSize->17]}, {0, 1, 2, 3, 4, 5, 6}, ControlType -> SetterBar},
                    {{a, aa+0.01, TextCell["a",FontSize->30]}, aa+0.01, bb-0.01, Appearance -> "Labeled"},
                    {{b, bb-0.01, TextCell["b",FontSize->30]}, aa+0.01, bb-0.01, Appearance -> "Labeled"},*)

                    Initialization:>{

                        Secant[inputFun_,x0_,x1_,a_,b_,i_] := Module[
                            {xValues,finalFunction},
                            finalFunction = ToExpression[ToString[inputFun]];
                            xValues =
                                NestList[
                                    {
                                        #[[1]],
                                        (*x1 - f(x1) * { (x1-x0) /  [ f(x1) - f(x0) ] }*)
                                        #[[1]] - (finalFunction/.x->#[[1]])*((#[[1]] - #[[2]]) / ((finalFunction/.x->#[[1]]) -  (finalFunction/.x->#[[2]])))
                                    } &,
                                    {x1, x0},
                                    i
                                ];

                            Plot[
                                finalFunction, {x, a, b},
                                PlotRange -> {{a,b},Full},
                                AxesLabel -> {Style["x", 16], Style["y", 16]},
                                PlotStyle -> Thickness[0.006],
                                Epilog -> {
                                    {
                                        PointSize[0.01],
                                        (Point[{#1, 0}] &) /@ Flatten[xValues]
                                    },
                                    {
                                        Thickness[0.002],
                                        MapIndexed[
                                            {
                                                Red,
                                                Line[{
                                                    {#1[[1]], finalFunction/.x->#[[1]]},
                                                    {#1[[2]], finalFunction/.x->#1[[2]]}
                                                }]
                                            } &,
                                            xValues
                                        ]
                                    },
                                    {
                                        Thickness[0.002], Black,
                                        Line[({{#1, 0}, {#1, finalFunction/.x->#1}} &) /@ Flatten[xValues]]
                                    }
                                },
                                Axes -> True,
                                ImageSize -> {800,500},
                                AxesLabel -> {Style["x", 16], Style["y", 16]}
                            ]
                        ]
                    },
                    Paneled->False
                ]
            ]
        },
        Paneled->False
    ]
];

(* these two functions show cases/examples in which the Newton's method fails *)
(* xlog(x)-1*)
FirstExample[] :=
    DynamicModule[ {list,f},
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
         
         
AlgoBisez[] :=
  DynamicModule[{ww, zz, tt1, ff},
                                     ww = ToExpression["a"];
                                     zz = ToExpression["b"];
                                     tt1 = ToExpression["\[Tau]"];
                                     ff = x^2 - 2;
                                     Manipulate[BisectionAlgorithm[ww, zz, tt1],
                                                Column[{Row[{TextCell["Sia f = ", FontSize -> 25],
                                         TextCell[TraditionalForm[ff], FontSize -> 25]}],
                                         Row[{TextCell["Inserisci punto iniziale: ", FontSize -> 25],
                                             InputField[Dynamic[ww], ImageSize -> 150,Alignment->Center,
                                                        BaseStyle -> FontSize -> 25],
                                             TextCell[" e punto finale: ", FontSize -> 25],
                                             InputField[Dynamic[zz], ImageSize -> 150,Alignment->Center,
                                                        BaseStyle -> FontSize -> 25]}] ,
                                         Row[{TextCell["Tolleranza \[Tau] ", FontSize -> 25],
                                             InputField[Dynamic[tt1], ImageSize -> 100, Alignment->Center,
                                                        BaseStyle -> FontSize -> 25]}]}],
                                                Initialization :> {
                                                    BisectionAlgorithm[w_, z_, t1_] := DynamicModule[{cValB, fcValB},
                                                                                                    a1 = N[ToExpression[w]];
                                                                                                    If[w != ToExpression["a"], w= a1];
                                                                                                    b1 = N[ToExpression[z]];
                                                                                                    If[z != ToExpression["b"], z = b1];
                                                                                                    If[t1 == ToString["\[Tau]"], "", ""];
                                                                                                    
                                                                                                    Column[{
                                                        Row[{
                                                            TextCell["Finch\[EAcute] | a - b | > \[Tau]", FontSize -> 25]
                                                        }],
                                                        Row[{
                                                            Column[{
                                                                Row[{
                                                                    TextCell["  1. Calcolo c = ", FontSize -> 25],
                                                                    FractionBox[
                                                                                RowBox[{
                                                                        TextCell["a+b", FontSize -> 25]
                                                                    }],
                                                                                RowBox[{
                                                                        TextCell["2", FontSize -> 25]
                                                                    }]
                                                                                ] // DisplayForm}],
                                                                    Row[{
                                                                        
                                                                        TextCell["  2a. Se f(c) = 0 ho la soluzione ",
                                                                                 FontSize -> 25]
                                                                    }],
                                                                    Row[{
                                                                        TextCell["  2b. Se segno(c) = segno(a) ", FontSize -> 25]
                                                                    }],
                                                                    Row[{
                                                                        TextCell["         a = c", FontSize -> 25]
                                                                    }],
                                                                    Row[{
                                                                        TextCell["      Altrimenti", FontSize -> 25]
                                                                    }],
                                                                    Row[{
                                                                        TextCell["         b = c", FontSize -> 25]
                                                                    }]
                                                                }],
                                                                
                                                                Column[{
                                                                    Spacer[20],
                                                                    Row[{
                                                                        
                                                                        TextCell["    c = ", FontSize -> 25,
                                                                                 FontColor -> Gray],
                                                                        Column[{
                                                                            TextCell[
                                                                                     FractionBox[
                                                                                                 RowBox[{
                                                                                         TextCell[w + z, FontSize -> 25]
                                                                                     }],
                                                                                                 RowBox[{
                                                                                         TextCell["2", FontSize -> 25]
                                                                                     }]
                                                                                                 ] // DisplayForm, FontSize -> 25,
                                                                                     FontColor -> Gray
                                                                                     ]
                                                                        }],
                                                                        Column[{
                                                                            
                                                                            If[ToString[w] != ToString["a"] &&
                                                                               ToString[z] != ToString["b"] &&
                                                                               ToString[t1] != ToString["\[Tau]"],
                                                                               If[Abs[w - z] > ToExpression[ToString[t1]],
                                                                                  cVal1B = ((w + z)/2) // N,
                                                                                  cValB = "";
                                                                                  
                                                                                  CreateDialog[
                                                                                               Column[{Row[{TextCell["Approssimazione ",
                                                                                                                     FontSize -> 25]}],
                                                                                      Row[{TextCell["con tolleranza ", FontSize -> 25],
                                                                                          TextCell[t1, FontSize -> 25]}],
                                                                                      Row[{TextCell["Raggiunta", FontSize -> 25]}]},
                                                                                                      Alignment -> Center]]
                                                                                  ],
                                                                               cVal1B = "";
                                                                               ];
                                                                            cValB = cVal1B;
                                                                            TextCell[" = ", FontSize -> 25, FontColor -> Gray]
                                                                        }],
                                                                        Column[{
                                                                            TextCell[cValB, FontSize -> 25, FontColor -> Gray]
                                                                        }]
                                                                    }],
                                                                    
                                                                    Spacer[50],
                                                                    Spacer[30],
                                                                    Row[{
                                                                        
                                                                        If[ToString[w] != ToString["a"] &&
                                                                           ToString[z] != ToString["b"] &&
                                                                           ToString[t1] != ToString["\[Tau]"],
                                                                           If[Abs[w - z] > ToExpression[ToString[t1]],
                                                                              Row[{
                                                                               
                                                                               TextCell[" in questo caso f(c) = ", FontSize -> 25,
                                                                                        FontColor -> Gray],
                                                                               fcValB = ff /. x -> cValB;
                                                                               
                                                                               TextCell[ToExpression[ToString[fcValB]],
                                                                                        FontSize -> 25, FontColor -> Gray]
                                                                           }],
                                                                              TextCell[""]
                                                                              ],
                                                                           TextCell[""]
                                                                           ]
                                                                    }],
                                                                    Spacer[50],
                                                                    Row[{
                                                                        If[ToString[cValB] != ToString[""],
                                                                           Row[{
                                                                            
                                                                            TextCell["Controllo se segno(", FontSize -> 25,
                                                                                     FontColor -> Gray],
                                                                            TextCell[cValB, FontSize -> 25, FontColor -> Gray],
                                                                            
                                                                            TextCell[") = segno(", FontSize -> 25,
                                                                                     FontColor -> Gray],
                                                                            
                                                                            TextCell[Dynamic[w], FontSize -> 25,
                                                                                     FontColor -> Gray],
                                                                            TextCell[")", FontSize -> 25, FontColor -> Gray]
                                                                        }],
                                                                           ""]
                                                                    }],
                                                                    Spacer[50]
                                                                }]
                                                            }],
                                                            
                                                            Row[{
                                                                "                       ",
                                                                Button[TextCell["Reitera", FontSize -> 25],
                                                                       If[Abs[w - z] >= ToExpression[ToString[t1]],
                                                                          If[(ff /. x -> w)*(ff /. x -> cValB) >= 0,
                                                                             {
                                                                                 If[ToString[cValB] != ToString[""],
                                                                                    ww = cValB,
                                                                                    Break[]
                                                                                    ];
                                                                                 zz = z;
                                                                                 tt1 = t1
                                                                             },
                                                                             {
                                                                                 If[ToString[cValB] != ToString[""], zz = cValB, Break[]];
                                                                                 ww = w;
                                                                                 tt1 = t1
                                                                             }
                                                                             ],
                                                                          
                                                                          CreateDialog[
                                                                                       Column[{Row[{TextCell["Approssimazione ",
                                                                                                             FontSize -> 25]}],
                                                                              Row[{TextCell["con tolleranza ", FontSize -> 25],
                                                                                  TextCell[t1, FontSize -> 25]}],
                                                                              Row[{TextCell["Raggiunta", FontSize -> 25]}]},
                                                                                              Alignment -> Center]]
                                                                          ], ImageSize -> 200
                                                                       
                                                                       ]
                                                            }]
                                                            
                                                        }]
                                                        ]
                                                    }, Paneled -> False
                                             ]
                                ] ;
AlgoSec[] :=
    DynamicModule[{aa, bb, cc, \[Tau]\[Tau], ff, faVal, fbVal, cVal1},
                                                                  faVal = ""; fbVal = ""; cVal1 = "";
                                                                  aa = ToExpression["a"];
                                                                  bb = ToExpression["b"];
                                                                  \[Tau]\[Tau] = ToExpression["\[Tau]"];
                                                                  ff = x^2 - 2;
                                                                  Manipulate[SecAlgorithm[aa, bb, \[Tau]\[Tau]],
                                                                             Column[{Row[{TextCell["Sia f = ", FontSize -> 25],
                                                                      TextCell[TraditionalForm[ff], FontSize -> 25]}],
                                                                      Row[{TextCell["Inserisci punto iniziale: ", FontSize -> 25],
                                                                          InputField[Dynamic[aa], ImageSize -> 150, Alignment -> Center,
                                                                                     BaseStyle -> FontSize -> 25],
                                                                          TextCell[" e punto finale: ", FontSize -> 25],
                                                                          InputField[Dynamic[bb], ImageSize -> 150, Alignment -> Center,
                                                                                     BaseStyle -> FontSize -> 25]}] ,
                                                                      Row[{TextCell["Tolleranza \[Tau] ", FontSize -> 25],
                                                                          InputField[Dynamic[\[Tau]\[Tau]], ImageSize -> 100,
                                                                                     Alignment -> Center, BaseStyle -> FontSize -> 25]}]}],
                                                                             Initialization :> {
                                                                                 SecAlgorithm[a_, b_, t_] :=
                                                                                 DynamicModule[{ cVal, fcVal, r, rr},
                                                                                               a1 = N[ToExpression[a]];
                                                                                               If[a != ToExpression["a"], a = a1];
                                                                                               b1 = N[ToExpression[b]];
                                                                                               If[b != ToExpression["b"], b = b1];
                                                                                               If[t == ToString["\[Tau]"], "", ""];
                                                                                               
                                                                                               Column[{
                                                                                     Row[{
                                                                                         TextCell["Finch\[EAcute] | a - b | > \[Tau]", FontSize -> 25]
                                                                                     }],
                                                                                     Row[{
                                                                                         Column[{
                                                                                             Row[{
                                                                                                 TextCell["  1. Calcolo c = a - ", FontSize -> 25],
                                                                                                 FractionBox[
                                                                                                             RowBox[{TextCell["f(a)*(b-a)", FontSize -> 25]}],
                                                                                                             RowBox[{TextCell["f(b)-f(a)", FontSize -> 25]}]
                                                                                                             ] // DisplayForm
                                                                                             }],
                                                                                             Row[{
                                                                                                 Column[{
                                                                                                     
                                                                                                     TextCell["  2a. Se f(c) = 0 ho la soluzione ",
                                                                                                              FontSize -> 25],
                                                                                                     Spacer[30],
                                                                                                     Row[{
                                                                                                         
                                                                                                         TextCell["  2b. Se segno(c) = segno(a) ",
                                                                                                                  FontSize -> 25]
                                                                                                     }],
                                                                                                     Row[{
                                                                                                         TextCell["         a = c", FontSize -> 25]
                                                                                                     }],
                                                                                                     Row[{
                                                                                                         TextCell["      Altrimenti", FontSize -> 25]
                                                                                                     }],
                                                                                                     Row[{
                                                                                                         TextCell["         b = c", FontSize -> 25]
                                                                                                     }]
                                                                                                 }]
                                                                                             }]
                                                                                         }],
                                                                                         Column[{
                                                                                             Spacer[20],
                                                                                             Row[{
                                                                                                 Column[{
                                                                                                     Row[{
                                                                                                         Column[{
                                                                                                             Row[{
                                                                                                                 
                                                                                                                 If[ToString[a] != ToString["a"] &&
                                                                                                                    ToString[b] != ToString["b"] &&
                                                                                                                    ToString[t] != ToString["\[Tau]"],
                                                                                                                    If[Abs[a - b] >= ToExpression[ToString[t]],
                                                                                                                       {
                                                                                                                           faVal = ff /. x -> a;
                                                                                                                           fbVal = ff /. x -> b;
                                                                                                                           cVal1 = a - ((faVal*(b - a))/(fbVal - faVal));
                                                                                                                           r = x /. Solve[ff == 0, x];
                                                                                                                           rr = N[r[[2]]];
                                                                                                                           
                                                                                                                           If[ToString[cVal1] ==
                                                                                                                              ToString[rr], {CreateDialog[
                                                                                                                                                          Column[{Row[{TextCell["Trovata soluzione: ",
                                                                                                                                                                                FontSize -> 25]}],
                                                                                                                               Row[{TextCell["x = ", FontSize -> 25],
                                                                                                                                   TextCell[cVal1, FontSize -> 25]}]},
                                                                                                                                                                 Alignment -> Center]]}, ""]
                                                                                                                       },
                                                                                                                       {
                                                                                                                           (*cVal1="";*)
                                                                                                                           
                                                                                                                           CreateDialog[
                                                                                                                                        Column[{Row[{TextCell["Approssimazione ",
                                                                                                                                                              FontSize -> 25]}],
                                                                                                                               Row[{TextCell["con tolleranza ", FontSize -> 25],
                                                                                                                                   TextCell[t, FontSize -> 25]}],
                                                                                                                               Row[{TextCell["Raggiunta", FontSize -> 25]}]},
                                                                                                                                               Alignment -> Center]]
                                                                                                                       }]
                                                                                                                    ];
                                                                                                                 cVal = N[cVal1];
                                                                                                                 
                                                                                                                 If[ToString[faVal] != ToString[""] &&
                                                                                                                    ToString[fbVal] != ToString[""],
                                                                                                                    Row[{
                                                                                                                     
                                                                                                                     TextCell[" c = ", FontSize -> 25,
                                                                                                                              FontColor -> Gray],
                                                                                                                     TextCell[a, FontSize -> 25, FontColor -> Gray],
                                                                                                                     TextCell[" - ", FontSize -> 25, FontColor -> Gray],
                                                                                                                     FractionBox[
                                                                                                                                 
                                                                                                                                 RowBox[{TextCell[faVal, FontSize -> 25,
                                                                                                                                                  FontColor -> Gray],
                                                                                                                         TextCell["*(", FontSize -> 25,
                                                                                                                                  FontColor -> Gray],
                                                                                                                         TextCell[b, FontSize -> 25, FontColor -> Gray],
                                                                                                                         TextCell["-", FontSize -> 25, FontColor -> Gray],
                                                                                                                         TextCell[a, FontSize -> 25, FontColor -> Gray],
                                                                                                                         TextCell[")", FontSize -> 25, FontColor -> Gray]}],
                                                                                                                                 
                                                                                                                                 RowBox[{TextCell[fbVal, FontSize -> 25,
                                                                                                                                                  FontColor -> Gray],
                                                                                                                         If[faVal >= 0,
                                                                                                                            TextCell["-", FontSize -> 25, FontColor -> Gray];
                                                                                                                            TextCell[faVal, FontSize -> 25,
                                                                                                                                     FontColor -> Gray],
                                                                                                                            TextCell[faVal, FontSize -> 25, FontColor -> Gray]]
                                                                                                                     }]
                                                                                                                                 ] // DisplayForm,
                                                                                                                     (*cVal=cVal1;*)
                                                                                                                     
                                                                                                                     TextCell[" = ", FontSize -> 25, FontColor -> Gray],
                                                                                                                     TextCell[cVal, FontSize -> 25, FontColor -> Gray]
                                                                                                                 }], ""
                                                                                                                    ]
                                                                                                             }]
                                                                                                         }]
                                                                                                     }],
                                                                                                     Spacer[50],
                                                                                                     Spacer[30],
                                                                                                     Row[{
                                                                                                         
                                                                                                         If[ToString[a] != ToString["a"] &&
                                                                                                            ToString[b] != ToString["b"] &&
                                                                                                            ToString[t] != ToString["\[Tau]"],
                                                                                                            If[Abs[a - b] > ToExpression[ToString[t]],
                                                                                                               Row[{
                                                                                                                
                                                                                                                TextCell[" in questo caso f(c) = ",
                                                                                                                         FontSize -> 25, FontColor -> Gray],
                                                                                                                fcVal = ff /. x -> cVal;
                                                                                                                
                                                                                                                TextCell[ToExpression[ToString[fcVal]],
                                                                                                                         FontSize -> 25, FontColor -> Gray]
                                                                                                            }],
                                                                                                               ""
                                                                                                               ],
                                                                                                            ""
                                                                                                            ]
                                                                                                     }],
                                                                                                     Spacer[50],
                                                                                                     Row[{
                                                                                                         
                                                                                                         If[ToString[cVal] != ToString[""] &&
                                                                                                            ToString[cVal] != ToString["cVal1"] &&
                                                                                                            ToString[a] != ToString[""],
                                                                                                            Row[{
                                                                                                             
                                                                                                             TextCell["Controllo se segno(", FontSize -> 25,
                                                                                                                      FontColor -> Gray],
                                                                                                             
                                                                                                             TextCell[cVal, FontSize -> 25,
                                                                                                                      FontColor -> Gray],
                                                                                                             
                                                                                                             TextCell[") = segno(", FontSize -> 25,
                                                                                                                      FontColor -> Gray],
                                                                                                             
                                                                                                             TextCell[Dynamic[a], FontSize -> 25,
                                                                                                                      FontColor -> Gray],
                                                                                                             TextCell[")", FontSize -> 25, FontColor -> Gray]
                                                                                                         }],
                                                                                                            ""]
                                                                                                     }],
                                                                                                     Spacer[50]
                                                                                                 }]
                                                                                             }]
                                                                                         }]
                                                                                     }],
                                                                                     Row[{
                                                                                         "                       ",
                                                                                         Button[TextCell["Reitera", FontSize -> 25],
                                                                                                If[Abs[a - b] >= ToExpression[ToString[t]],
                                                                                                   If[(ff /. x -> a)*(ff /. x -> cVal) >= 0,
                                                                                                      {
                                                                                                          If[
                                                                                                             ToString[cVal] != ToString[""],
                                                                                                             aa = cVal,
                                                                                                             Break[]
                                                                                                             ];
                                                                                                          bb = b;
                                                                                                          \[Tau]\[Tau] = t
                                                                                                      },
                                                                                                      {
                                                                                                          If[ToString[cVal] != ToString[""],
                                                                                                             bb = cVal,
                                                                                                             Break[]
                                                                                                             ];
                                                                                                          aa = a;
                                                                                                          \[Tau]\[Tau] = t
                                                                                                      }
                                                                                                      ],
                                                                                                   CreateDialog[
                                                                                                                Column[{Row[{TextCell["Approssimazione3 ",
                                                                                                                                      FontSize -> 25]}],
                                                                                                       Row[{TextCell["con tolleranza ", FontSize -> 25],
                                                                                                           TextCell[t, FontSize -> 25]}],
                                                                                                       Row[{TextCell["Raggiunta", FontSize -> 25]}]},
                                                                                                                       Alignment -> Center]]
                                                                                                   ], ImageSize -> 200
                                                                                                
                                                                                                ]
                                                                                     }]
                                                                                     
                                                                                 }]
                                                                                               
                                                                                               ]
                                                                             }, Paneled -> False]];
         

AlgoNewton[] :=
    DynamicModule[
				{xii, \[Tau]\[Tau], ff,ii},
				ii=0;
				xii = Subscript[x,ii];
				\[Tau]\[Tau] = ToExpression["\[Tau]"];
				ff=(x^2)-2;
				Manipulate[
						drawAlgo[xii,\[Tau]\[Tau],ff,ii],
						Column[{
                                Row[{
                                    TextCell["Sia f = ", FontSize -> 25], TextCell[TraditionalForm[ff], FontSize -> 25]
                                }],
								Row[{
										TextCell[" Approssimazione ", FontSize -> 25],
										InputField[Dynamic[xii], ImageSize -> 200,	Alignment -> Center, BaseStyle -> FontSize -> 25]
								}],
								Spacer[50],
								Row[{
										TextCell[" Tolleranza ", FontSize -> 25],
										TextCell["\[Tau] ", FontSize -> 35],
										InputField[Dynamic[\[Tau]\[Tau]], ImageSize -> 200,	Alignment -> Center,	BaseStyle -> FontSize -> 25]
								}]
            }],
						Initialization :> {
								drawAlgo[xi_,\[Tau]_,f_,i_]:= Module[
										{xi1},
										If[xi!=Subscript[x,0],
												xi=N[ToExpression[xi]]
										];
                    Column[{
                        Row[{
                            TextCell["Per k = 0, 1, 2, ...", FontSize -> 25]
                        }],
                        Row[{
                            Column[{

                                Row[{
                                    TextCell["  1. Ho k = 0", FontSize -> 25]
                                }],
                                Row[{
                                    TextCell["  2. ",FontSize -> 25],
                                    TextCell[Subscript[x, "k+1"], FontSize -> 25],
                                    TextCell[" = ", FontSize -> 25],
                                    TextCell[Subscript[x, "k"], FontSize -> 25],
                                    TextCell[" - ", FontSize -> 25],
                                    FractionBox[
                                        RowBox[{
                                            TextCell["f(", FontSize -> 25],
                                            TextCell[Subscript[x, "k"], FontSize -> 25],
                                            TextCell[")", FontSize -> 25]
                                        }],
                                        RowBox[{
                                            TextCell["f'(", FontSize -> 25],
                                            TextCell[Subscript[x, "k"], FontSize -> 25],
                                            TextCell[")", FontSize -> 25]
                                        }]
                                    ] // DisplayForm
                                }],
                                Row[{
                                    TextCell["  3. Finch\[EGrave] ", FontSize -> 25],
                                    TextCell["|", FontSize -> 30],
                                    TextCell[Subscript[x,"k+1"], FontSize -> 25],
                                    TextCell[ " - "],
                                    TextCell[Subscript[x,"k"], FontSize -> 25],
                                    TextCell["|",FontSize -> 30],
                                    TextCell[" > ", FontSize -> 25],
                                    TextCell["\[Tau]", FontSize -> 25, 	FontWeight->Bold]
                                }],
                                Spacer[50],
                                Row[{
                                    TextCell["       k = k + 1 ",FontSize->25]
                                }],
                            }],
                            Column[{
                                Spacer[50],
                                Row[{
                                    TextCell["    k = ", FontSize -> 25, FontColor -> Gray],
                                    TextCell[i, FontSize -> 25, FontColor -> Gray]
                                }],
                                Spacer[50],
                                Spacer[30],
                                Row[{
                                    TextCell["    cio\[EGrave] ", FontSize -> 25, FontColor -> Gray],
                                    TextCell[Subscript[x,i+1],FontSize -> 25, FontColor -> Gray],
                                    TextCell[" = ", FontSize -> 25, FontColor -> Gray],
                                    TextCell[Dynamic[xi], FontSize -> 25, FontColor -> Gray],
                                    TextCell[" - ", FontSize -> 25, FontColor -> Gray],
                                    FractionBox[
                                        RowBox[{TextCell[Dynamic[TraditionalForm[(f /. x -> xi)]], FontSize -> 25, FontColor -> Gray]}],
                                        RowBox[{TextCell[
                                            Dynamic[
                                                TraditionalForm[(D[f, x] /. x -> xi)]],
                                            FontSize -> 25, FontColor -> Gray]}]
                                    ] // DisplayForm,
                                    TextCell[" = ", FontSize -> 25, FontColor -> Gray],
                                    TextCell[Dynamic[
                                        (xi - ((f /. x -> xi) / (D[f, x] /. x -> xi))) // N
                                    ], FontSize -> 25, FontColor -> Gray]
                                }],
                                Spacer[50],
                                Row[{
                                    TextCell["    cio\[EGrave] ", FontSize -> 25, FontColor ->Gray],
                                    TextCell["|", FontSize -> 30, FontColor -> Grey],
                                    xi1 = N[(xi - ((f /. x -> xi) / (D[f, x] /. x -> xi)))];
                                    If[xi==Subscript[x,0],
                                        xi1 = Subscript[x,1]
                                    ];
                                    TextCell[xi1, FontSize -> 25, FontColor -> Gray],
                                    TextCell[ " - ", FontColor -> Grey],
                                    TextCell[xi, FontSize -> 25, FontColor -> Gray],
                                    TextCell["|", FontSize -> 30, FontColor -> Gray],
                                    TextCell[" > ", FontSize -> 25, FontColor -> Gray],
                                    TextCell[\[Tau], FontSize -> 25, FontColor -> Gray]
                                }],
                                Spacer[50]
                            }]
                            
                        }],
                                    Row[{
                                        "                       ",
                                        Button[TextCell["Reitera",
                                                        FontSize -> 25], {xi1 =
                                            N[(xi - ((f /. x -> xi)/(D[f, x] /. x -> xi)))];
                                            If[(ff /. x -> xi1) != 0,
                                               If[Abs[xi - xi1] > \[Tau], {xia := ToExpression[xi1];
                                                xii = xia;
                                                ii = i + 1;},
                                                  CreateDialog[
                                                               Column[{Row[{TextCell["Approssimazione ", FontSize -> 25]}],
                                                      Row[{TextCell["con tolleranza ", FontSize -> 25],
                                                          TextCell[\[Tau], FontSize -> 25]}],
                                                      Row[{TextCell["Raggiunta", FontSize -> 25]}]},
                                                                      Alignment -> Center]]],
                                               CreateDialog[TextCell["Soluzione Trovata", FontSize -> 25]]]},
                                               ImageSize -> 120]}]
                    }]

								]
						},
						Paneled -> False
				]
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
    
EndPackage[]





