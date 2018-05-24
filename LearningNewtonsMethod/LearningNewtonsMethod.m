(* ::Package:: *)

(* Wolfram Language Package *)
(* Created by the Wolfram Workbench 07-May-2018 *)
(* Copyright *)

(* Made by: Anna Avena (INF), Giulia Cantini (INF), Roberto Ferraro (MAT), Nicola Mainetti (MAT), Matteo Sanfelici (INF) *)

BeginPackage["LearningNewtonsMethod`"];
(* Exported symbols added here with SymbolName::usage *) 


GoForward::usage = "Allows user to go to the next slide";
GoBack::usage = "Allows user to go back to the previous slide ";
GoHomepage::usage = "Return to the homepage";
ReadInputFile::usage = "Reads expressions from 'inputExp' file";
NewtonInteractive::usage = "Animated Newton Method";
ConvertImageToFullyScaledNinePatch::usage = "Set notebook background image";
BisectionInteractive::usage = "Interactive Bisection Method BisectionMethod[pm=0/1,it=0/1] pm->PopupMenu, it->Interactive";
FirstExample::usage = "Shows the cases in which the Newton's method fails ";
SecondExample::usage = "Shows the cases in which the Newton's method fails";
MethodsComparison::usage = "Show convergence to solution using three different methods";
AlgoNewton::usage="";
SecantInteractive::usage = "Interactively show step of iteration in the secant method";
Bolzano::usage = "Show root graph example step by step: press a button to advance";
AlgoBisez::usage = "a";
AlgoSec::usage = "a";


Begin["`Private`"];

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

(* Function that progressively introduces and explains Bolzano's Theorem *)
Bolzano[] :=
    Module[
        {
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
                        If[buttonStatus1 == "closed" ,
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
                        If[buttonStatus2 == "closed" && buttonStatus1 == "open",
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
                        ],
                        ImageSize->{230,70},
                        BaseStyle->{FontFamily->"Source Sans Pro", FontSize->36}
                    ],
                    TextCell[" .", textCellStyle]
                }],
                Row[{}],
                Row[{
                    TextCell["Inoltre notiamo che f calcolata in b = 2 \[EGrave] ", textCellStyle],
                    Button["positiva",
                        If[buttonStatus3 == "closed" && buttonStatus1 == "open" && buttonStatus2 == "open",
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
                Row[{}],
                Row[{}],
                Row[{
                    TextCell["Quindi possiamo esser certi che tra ", textCellStyle],
                    TextCell["1", textCellStyle, Green],
                    TextCell[" e ", textCellStyle],
                    TextCell["2", textCellStyle, Green],
                    TextCell[" \[EGrave] compreso un suo ", textCellStyle],
                    Button["zero",
                        If[buttonStatus4 == "closed" && buttonStatus1 == "open" && buttonStatus2 == "open" && buttonStatus3 == "open",
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
                                ]
                            )
                        ],
                        ImageSize->{200,70},
                        BaseStyle->{FontFamily->"Source Sans Pro", FontSize->36},
                        Background->Red
                    ],
                    TextCell[" .", textCellStyle]
                }],
                Row[{}]
            }],
            Column[{
                Dynamic[plot]
            }]
        }]
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

(* Call with pm = 1 to display a version of the graph with a popup-menu with a list of functions to choose from,
call with it = 1 to display graph with multiple iterations *)
BisectionInteractive[pm_,it_] :=
    DynamicModule[
        {listFunctions,listIntervals,passf,ff},

        listIntervals = {
          {0.5,2},
          {(1/2*3.14), (1.5*3.14)},
          {0,2},
          {0,2},
          {0,3.14}
        };

        listFunctions = {
          TraditionalForm[x^2 - 2],
          TraditionalForm[Sin[x]],
          TraditionalForm[Cos[2x]Sin[x]],
          TraditionalForm[x*Log[x]+x^3],
          TraditionalForm[Cos[x]]
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

SecantInteractive[pm_,it_] :=
    DynamicModule[
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
                                If[it==1,Slider[Dynamic[iteration],{0,6,1}]],
                                If[it==1,TextCell[" ",FontSize->25]],
                                If[it==1,TextCell[Dynamic[iteration],FontSize->23],iteration=0;]
                            }]
                        }],

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
                                            Thickness[0.002],
                                            MapIndexed[
                                                {
                                                    Red,
                                                    Line[{
                                                        {#1[[1]], finalFunction/.x->#[[1]]},
                                                        {#1[[2]], finalFunction/.x->#[[2]]}
                                                    }]
                                                } &,
                                                xValues
                                            ]
                                        },
                                        {
                                            Thickness[0.002], Black,
                                            Line[({{#1, 0}, {#1, finalFunction/.x->#1}} &) /@ Flatten[xValues]]
                                        },
                                        {
                                            PointSize[0.01],
                                            Green,
                                            (Point[{#1, 0}] &) /@ Flatten[xValues]
                                        },
                                        {(* x0 and x1 points are black *)
                                            PointSize[0.01],
                                            Black,
                                            Point[{x0,0}],
                                            Point[{x1,0}]
                                        },
                                        {
                                        (* draw point labels only for the starting x0 and x1 *)
                                            Text[
                                                {x0, N[Rationalize[finalFunction /. x -> x0], 2]},
                                                Offset[{0, 70}, {x0, finalFunction /. x -> x0}]
                                            ],
                                            Text[
                                                {x1, N[Rationalize[finalFunction /. x -> x1], 2]},
                                                Offset[{0, 70}, {x1, finalFunction /. x -> x1}]
                                            ]
                                            (*(Text[{#1,N[Rationalize[finalFunction/.x->#1],2]}, Offset[{40,10}, {#1,finalFunction/.x->#1}]])&  /@ Flatten[xValues]*)
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
    DynamicModule[
        {list,ff,x00,draw},
        ff = x * Log[x]-1;
        x00 = 0.2;

        Manipulate[
            draw[ff,x00],

            Column[{
                Row[{
                    TextCell[" f(x) = ",FontSize->25],
                    TextCell[TraditionalForm[x*Log[x]-1],FontSize->25]
                }],
                Row[{
                    TextCell[" x ",FontSize->25],
                    Slider[Dynamic[x00],{0.01,2.99,0.01}],
                    TextCell[Dynamic[x00],FontSize->25]
                }]
            }],

            Initialization:> {
                draw[f_,x0_]:= DynamicModule[
                    {f1,f1x0,fx0,fx0t},
                    f1 = D[f,x];
                    f1x0 = f1 /. x -> x0;
                    fx0 = f /. x-> x0;
                    fx0t = (f1x0*x0/fx0);
                    Plot[
                        f,{x,-3,3},
                        PlotStyle->{
                            Thickness[0.004]
                        },
                        Prolog->{
                            RGBColor[210/255, 223/255, 242/255],
                            Rectangle[{0,-1.5},{3.1,2.4}]
                        },
                        Epilog -> {
                            Red,
                            {
                                Thickness[0.001],
                                Dashed,
                                Line[{
                                    {x0, 0},
                                    {x0, f  /. x-> x0}
                                }]
                            },
                            {
                                Thickness[0.004],
                                Arrow[{
                                    {x0, f  /. x-> x0},
                                    {(x0 - ((f /. x -> x0)/(D[f,x] /. x -> x0))),0}
                                }]
                            },
                            PointSize[0.015],
                            Point[{x0,0}],
                            Green,
                            Point[{E^ProductLog[1],0}],
                            Black,
                            Text[TextCell[Subscript[x,0],FontSize->21],{x0,0.3}],
                            Text[TextCell[Subscript[x,1],FontSize->21],{(x0 - ((f /. x -> x0)/(D[f,x] /. x -> x0))),0.3}],
                            Text[TextCell["Dominio",FontSize->25],{1.5,1.5}],
                            Text[TextCell["Funzione NON definita",FontSize->25],{-1.5,1.5}]
                        },
                        ImageSize -> {800,500}
                    ]
                ]
            },
            Paneled->False
        ]
    ];
    
(* cos(x) *)
SecondExample[] :=
    DynamicModule[
        {ff,x00,draw},

        ff=Cos[x];
        x00=0;

        Manipulate[
            draw[ff,x00],

            Column[{
                Row[{
                    TextCell["f(x) = ",FontSize->25],
                    TextCell[TraditionalForm[Cos[x]],FontSize->25]
                }],
                Row[{
                    TextCell["x ", FontSize -> 25],
                    Slider[Dynamic[x00], {-3.1, 3.1, 0.1}],
                    TextCell[Dynamic[x00], FontSize -> 25]
                }]
            }],

            Initialization:>{
                draw[f_,x0_] := DynamicModule[
                    {},
                    Plot[
                        f,{x,-Pi,Pi},
                        PlotRange->{{-Pi,Pi},{-1,1.3}},
                        Epilog->{
                            Red,
                            {
                                Thickness[0.001],
                                Dashed,
                                Line[{
                                    {x0, 0},
                                    {x0, f  /. x-> x0}
                                }]
                            },
                            Black,
                            Text[TextCell[Subscript[x,0],FontSize->21],{x0,0.3}],
                            If[x0!=0,
                                {
                                    Red,
                                    Thickness[0.004],
                                    Arrow[{
                                        {x0, f  /. x-> x0},
                                        {(x0 - ((f /. x -> x0)/(D[f,x] /. x -> x0))),0}
                                    }],
                                    Black,
                                    Text[TextCell[Subscript[x,1],FontSize->21],{(x0 - ((f /. x -> x0)/(D[f,x] /. x -> x0))),0.3}]
                                },
                                {
                                    Red,
                                    Thickness[0.004],
                                    Line[{
                                        {-2.5,1},
                                        {2.5,1}
                                    }],
                                    Dashed,
                                    Arrow[{
                                        {-2.5,1},
                                        {-3.1,1}
                                    }],
                                    Arrow[{
                                        {2.5,1},
                                        {3.1,1}
                                    }],
                                    Black,
                                    Text[TextCell["Perfettamente Tangente",FontSize->25],{0,1.1}]
                                }
                            ],
                            Red,
                            PointSize[0.015],
                            Point[{x0,0}],
                            Green,
                            Point[{-Pi/2,0}],
                            Point[{Pi/2,0}]


                        },
                        ImageSize -> {800,500}
                    ]
                ]
            },
            Paneled->False
        ]
    ];

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
    DynamicModule[
        {
            textBisection,
            textSecant,
            textNewton,
            x,
            i = 1,
            tau = 0.1
        },

        Manipulate[
            (* function that does calculations needed for plotting *)
            Comparison[i,tau],
            (* manipulate controls *)
            Column[{
                Row[{
                (* control iteration *)
                    Slider[Dynamic[i], {1, 15, 1}, Appearance -> {"Labeled"}]
                }],
                Row[{
                    (* control tolerance *)
                    SetterBar[
                        Dynamic[tau],
                        {0.1, 0.01, 0.001, 0.0001, 0.00001, 0.00001},
                        ImageSize->Full
                    ]
                }]
            }],

            Initialization:> {
                Comparison[it_,t_] := Module[
                    {
                        f,
                        a,
                        b,
                        n,
                        bisectionRoots,
                        secantRoots,
                        newtonRoots
                    },

                    Clear[f];
                    f[x_] := Sin[x];
                    (* initial interval *)
                    a = 2.5;
                    b = 1.4Pi;
                    (* maximum number of iterations *)
                    n = 15;
                    textBisection = "";
                    textSecant = "";
                    textNewton = "";

                    bisectionRoots = (* initial root of the bisection method *)
                        N[NestList[
                            If[ f[#[[1]]]*f[(#[[1]]+#[[2]])/2]<0,
                                {#[[1]],(#[[1]]+#[[2]])/2},
                                {(#[[1]]+#[[2]])/2,#[[2]]}
                            ] &,
                            {a,b},
                            n
                        ]];
                    (* build a nested list of pairs {xi-1, xi} used as bounds by the method *)
                    (* a, b - f(b)(b-a)/(f(b)-f(a)) *)
                    (* initial root of the secant method *)
                    secantRoots =
                        N[NestList[
                            {#[[1]],#[[2]]-f[#[[2]]](#[[2]]-#[[1]])/(f[#[[2]]]-f[#[[1]]])} &,
                            {a,b},
                            n
                        ]];
                    (* initial root of the Newton's method *)
                    newtonRoots = NestList[ #1 - f[#1]/Derivative[1][f][#1] &, a, n];

                    (* check if (b-a<t) i.e. the bisection method has reached the desired tolerance *)
                    If[Abs[bisectionRoots[[it]][[1]]-bisectionRoots[[it]][[2]]]<=t,
                        textBisection = "Bisezione ha raggiunto la tolleranza desiderata!",
                        textBisection = ""
                    ];

                    If[Abs[secantRoots[[it+1]][[2]]-secantRoots[[it]][[2]]]<=t,
                        textSecant = "Secanti ha raggiunto la tolleranza desiderata!",
                        textSecant = ""
                    ];

                    If[Abs[newtonRoots[[it]]-newtonRoots[[it+1]]]<=t,
                        textNewton = "Newton ha raggiunto la tolleranza desiderata!",
                        textNewton = ""
                    ];

                    Row[{
                        (* bisection *)
                        Column[{
                            Plot[f[x], {x, 1.5, 4.5},
                                Epilog -> {
                                    Directive[{Thick, Green, Dashed}],
                                    (* calculation and plot of the subsequent roots for the bisection method *)

                                    InfiniteLine[
                                        {(bisectionRoots[[i]][[1]] + bisectionRoots[[i]][[2]]) / 2, 0},
                                        {0, 1}
                                    ],
                                    {
                                        Green,
                                        PointSize[.01],
                                        Point[{(bisectionRoots[[i]][[1]] + bisectionRoots[[i]][[2]]) / 2, 0}]
                                    },
                                    {
                                        Red,
                                        PointSize[.008],
                                        Point[{Pi,0}],
                                        Text[{Pi,0}, Offset[{0,20},{Pi,0}]]
                                    }
                                },
                                ImageSize -> 500,
                                PlotLabel -> "Bisezione"
                            ]
                            , " ",
                            (* display the current root value for the bisection method *)
                            TextCell[
                                Row[{Subscript["x", i], " = ", (bisectionRoots[[i]][[1]] + bisectionRoots[[i]][[2]]) / 2}, Alignment -> Center],
                                "Text",
                                Darker[Green],
                                TextAlignment -> Center,
                                CellBaseline -> Center,
                                CellSize -> {500, 50}
                            ],

                            TextCell[
                                Row[{Pi, " = ", N[Pi,10]}, Alignment->Center],
                                "Text",
                                Red,
                                TextAlignment -> Center,
                                CellBaseline -> Bottom,
                                CellSize -> {500, 50}
                            ],
                            TextCell[Dynamic[textBisection], "Text"]
                        }],
                        (* secant *)
                        Column[{
                            Plot[f[x], {x, 1.5, 4.5},
                                Epilog -> {
                                    Directive[{Thick, Green, Dashed}],
                                    (* calculation and plot of the subsequent roots for the secant method *)
                                    InfiniteLine[{secantRoots[[i]][[2]], 0}, {0, 1}],
                                    {
                                        Green,
                                        PointSize[.01],
                                        Point[{secantRoots[[i]][[2]], 0}]
                                    },
                                    {
                                        Red,
                                        PointSize[.008],
                                        Point[{Pi,0}],
                                        Text[{Pi,0}, Offset[{0,20},{Pi,0}]]
                                    }
                                },
                                ImageSize -> 500,
                                PlotLabel -> "Secanti"
                            ],
                            " ",
                            (* display the current root value for the secant method *)
                            TextCell[
                                Row[{Subscript["x", i], " = ", secantRoots[[i]][[2]]}, Alignment -> Center],
                                "Text",
                                Darker[Green],
                                TextAlignment -> Center,
                                CellBaseline -> Bottom,
                                CellSize -> {500, 50}
                            ],

                            TextCell[
                                Row[{Pi, " = ", N[Pi,10]}, Alignment->Center],
                                "Text",
                                Red,
                                TextAlignment -> Center,
                                CellBaseline -> Bottom,
                                CellSize -> {500, 50}
                            ],

                            TextCell[Dynamic[textSecant], "Text"]
                        }],
                        (* Newton *)
                        Column[{
                            Plot[f[x], {x, 1.5, 4.5},
                                Epilog -> {
                                    Directive[{Thick, Green, Dashed}],
                                    (* calculation and plot of the subsequent roots for the bNewton'a method *)
                                    InfiniteLine[{newtonRoots[[i]], 0}, {0, 1}],
                                    {
                                        Green,
                                        PointSize[.01],
                                        Point[{newtonRoots[[i]], 0}]
                                    },
                                    {
                                        Red,
                                        PointSize[.008],
                                        Point[{Pi,0}],
                                        Text[{Pi,0}, Offset[{0,20},{Pi,0}]]
                                    }
                                },
                                ImageSize -> 500,
                                PlotLabel -> "Newton"
                            ],
                            (* display the current root value for the Newton's method *)
                            TextCell[
                                Row[{Subscript["x", i], " = ", newtonRoots[[i]]}, Alignment -> Center],
                                "Text",
                                Darker[Green],
                                TextAlignment -> Center,
                                CellBaseline -> Bottom,
                                CellSize -> {500, 50}
                            ],
                            TextCell[
                                Row[{Pi, " = ", N[Pi,10]}, Alignment->Center],
                                "Text",
                                Red,
                                TextAlignment -> Center,
                                CellBaseline -> Bottom,
                                CellSize -> {500, 50}
                            ],

                            TextCell[Dynamic[textNewton], "Text"]
                        }]
                    }]
                ] (* end module *)
            }, (* end initialization *)
            Paneled -> False
        (*
          (* slider that shows the iteration steps *)
          {i,1,10,1,Appearance->{"Open","Labeled"}},
          (* slider that lets the user set the tolerance value *)
          {tau, {0.1,0.01,0.001,0.0001,0.00001,0.000001}, Appearance->{"Open","Labeled"}},
        *)
        ] (* end manipulate *)
    ]; (* end dynamicmodule *)
         
         
AlgoBisez[] :=
    DynamicModule[
        {ww, zz, tt1, ff},
        ww = ToExpression["a"];
        zz = ToExpression["b"];
        tt1 = ToExpression["\[Tau]"];
        ff = x^2 - 2;

        Manipulate[
            BisectionAlgorithm[ww, zz, tt1],

            Column[{
                Row[{
                    TextCell["Sia f = ", FontSize -> 25],
                    TextCell[TraditionalForm[ff], FontSize -> 25]
                }],
                Row[{
                    TextCell["Inserisci punto iniziale: ", FontSize -> 25],
                    InputField[Dynamic[ww], ImageSize -> 150,Alignment->Center, BaseStyle -> FontSize -> 25],
                    TextCell[" e punto finale: ", FontSize -> 25],
                    InputField[Dynamic[zz], ImageSize -> 150,Alignment->Center, BaseStyle -> FontSize -> 25]
                }] ,
                Row[{
                    TextCell["Tolleranza \[Tau] ", FontSize -> 25],
                    InputField[Dynamic[tt1], ImageSize -> 100, Alignment->Center, BaseStyle -> FontSize -> 25]
                }]
            }],

            Initialization :> {
                BisectionAlgorithm[w_, z_, t1_] := DynamicModule[
                    {cValB, fcValB},

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
                                        ] // DisplayForm
                                    }],
                                    Row[{
                                        TextCell["  2a. Se f(c) = 0 ho la soluzione ", FontSize -> 25]
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
                                        TextCell["    c = ", FontSize -> 25, FontColor -> Gray],
                                        Column[{
                                            TextCell[
                                                FractionBox[
                                                    RowBox[{
                                                        TextCell[w + z, FontSize -> 25]
                                                    }],
                                                    RowBox[{
                                                        TextCell["2", FontSize -> 25]
                                                    }]
                                                ] // DisplayForm,
                                                FontSize -> 25,
                                                FontColor -> Gray
                                            ]
                                        }],
                                        Column[{
                                            If[ToString[w] != ToString["a"] && ToString[z] != ToString["b"] && ToString[t1] != ToString["\[Tau]"],
                                                If[Abs[w - z] > ToExpression[ToString[t1]],
                                                    cVal1B = ((w + z)/2) // N,
                                                    cValB = "";

                                                    CreateDialog[
                                                        Column[{
                                                            Row[{
                                                                TextCell["Approssimazione ", FontSize -> 25]
                                                            }],
                                                            Row[{
                                                                TextCell["con tolleranza ", FontSize -> 25],
                                                                TextCell[t1, FontSize -> 25]
                                                            }],
                                                            Row[{
                                                                TextCell["Raggiunta", FontSize -> 25]
                                                            }]
                                                        }, Alignment -> Center]
                                                    ]
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
                                        If[ToString[w] != ToString["a"] && ToString[z] != ToString["b"] && ToString[t1] != ToString["\[Tau]"],
                                            If[Abs[w - z] > ToExpression[ToString[t1]],
                                                Row[{
                                                    TextCell[" in questo caso f(c) = ", FontSize -> 25, FontColor -> Gray],
                                                    fcValB = ff /. x -> cValB;

                                                    TextCell[ToExpression[ToString[fcValB]], FontSize -> 25, FontColor -> Gray]
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
                                                TextCell["Controllo se segno(", FontSize -> 25, FontColor -> Gray],
                                                TextCell[cValB, FontSize -> 25, FontColor -> Gray],
                                                TextCell[") = segno(", FontSize -> 25, FontColor -> Gray],
                                                TextCell[Dynamic[w], FontSize -> 25, FontColor -> Gray],
                                                TextCell[")", FontSize -> 25, FontColor -> Gray]
                                            }],
                                            ""
                                        ]
                                    }],
                                    Spacer[50]
                                }]
                            }],

                            Row[{
                                "                       ",
                                Button[
                                    TextCell["Reitera", FontSize -> 25],
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
                                            Column[{
                                                Row[{TextCell["Approssimazione ", FontSize -> 25]}],
                                                Row[{TextCell["con tolleranza ", FontSize -> 25],
                                                TextCell[t1, FontSize -> 25]}],
                                                Row[{TextCell["Raggiunta", FontSize -> 25]}]}, Alignment -> Center]]
                                    ],
                                    ImageSize -> 200

                                ]
                            }]
                        }]
                ]
            }, Paneled -> False
        ]
    ];

AlgoSec[] :=
    DynamicModule[
        {aa, bb, cc, \[Tau]\[Tau], ff, faVal, fbVal, cVal1},
        faVal = ""; fbVal = ""; cVal1 = "";
        aa = ToExpression["a"];
        bb = ToExpression["b"];
        \[Tau]\[Tau] = ToExpression["\[Tau]"];
        ff = x^2 - 2;

        Manipulate[
            SecAlgorithm[aa, bb, \[Tau]\[Tau]],

            Column[{
                Row[{
                    TextCell["Sia f = ", FontSize -> 25],
                    TextCell[TraditionalForm[ff], FontSize -> 25]
                }],
                Row[{
                    TextCell["Inserisci punto iniziale: ", FontSize -> 25],
                    InputField[Dynamic[aa], ImageSize -> 150, Alignment -> Center, BaseStyle -> FontSize -> 25],
                    TextCell[" e punto finale: ", FontSize -> 25],
                    InputField[Dynamic[bb], ImageSize -> 150, Alignment -> Center, BaseStyle -> FontSize -> 25]
                }],
                Row[{
                    TextCell["Tolleranza \[Tau] ", FontSize -> 25],
                    InputField[Dynamic[\[Tau]\[Tau]], ImageSize -> 100, Alignment -> Center, BaseStyle -> FontSize -> 25]
                }]
            }],

            Initialization :> {
                SecAlgorithm[a_, b_, t_] := DynamicModule[
                    { cVal, fcVal, r, rr},
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
                                             If[ToString[cVal] != ToString[""],
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
                                         Column[{
                                             Row[{TextCell["Approssimazione3 ", FontSize -> 25]}],
                                             Row[{
                                                 TextCell["con tolleranza ", FontSize -> 25],
                                                 TextCell[t, FontSize -> 25]
                                             }],
                                             Row[{TextCell["Raggiunta", FontSize -> 25]}]
                                         }, Alignment -> Center]
                                     ]
                                 ], ImageSize -> 200
                             ]
                         }]
                    }]
                ]
            },
            Paneled -> False
        ]
    ];
         

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
                                }]
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
                                        RowBox[{
                                            TextCell[Dynamic[TraditionalForm[(f /. x -> xi)]], FontSize -> 25, FontColor -> Gray]
                                        }],
                                        RowBox[{
                                            TextCell[Dynamic[TraditionalForm[(D[f, x] /. x -> xi)]], FontSize -> 25, FontColor -> Gray]
                                        }]
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
                            Button[TextCell["Reitera", FontSize -> 25],
                                {
                                    xi1 = N[(xi - ((f /. x -> xi)/(D[f, x] /. x -> xi)))];
                                    If[(ff /. x -> xi1) != 0,
                                        If[Abs[xi - xi1] > \[Tau],
                                            {
                                                xia := ToExpression[xi1];
                                                xii = xia;
                                                ii = i + 1;
                                            },
                                            CreateDialog[
                                                Column[{
                                                    Row[{
                                                        TextCell["Approssimazione ", FontSize -> 25]
                                                    }],
                                                    Row[{
                                                        TextCell["con tolleranza ", FontSize -> 25],
                                                        TextCell[\[Tau], FontSize -> 25]
                                                    }],
                                                    Row[{
                                                        TextCell["Raggiunta", FontSize -> 25]
                                                    }]
                                                }, Alignment -> Center]
                                            ]
                                        ],
                                        CreateDialog[TextCell["Soluzione Trovata", FontSize -> 25]]
                                    ]
                                },
                                ImageSize -> 120
                            ]
                        }]
                    }]
								]
						},
						Paneled -> False
				]
    ];
                   
End[];

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
        testoRow2 = "a partire dalla prima approssimazione data";
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