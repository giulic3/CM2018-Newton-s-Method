(* ::Package:: *)

(* Wolfram Language Package *)
(* Created by the Wolfram Workbench 07-May-2018 *)
(* Copyright *)
(* Matematica Computazionale - Calcolo Numerico e Software Didattico 2017/2018  *)
(* Made by:
    Anna Avena (INF),
    Giulia Cantini (INF),
    Roberto Ferraro (MAT),
    Nicola Mainetti (MAT),
    Matteo Sanfelici (INF) *)

BeginPackage["LearningNewtonsMethod`"];

Unprotect["LearningNewtonsMethod`*"];
ClearAll["LearningNewtonsMethod`*"];

(* Exported symbols added here with SymbolName::usage *)
GoForward::usage = "Allows user to go to the next slide";
GoBack::usage = "Allows user to go back to the previous slide ";
GoHomepage::usage = "Return to the homepage";
ReadInputFile::usage = "Reads expressions from 'inputExp' file";
NormalizeRangeValues::usage = "Checks if values inserted belong or not to the specified interval, and if not, it normalizes them";
NewtonInteractive::usage = "Animated Newton's Method";
ConvertImageToFullyScaledNinePatch::usage = "Set notebook background image";
BisectionInteractive::usage = "Interactive Bisection Method BisectionMethod[pm=0/1,it=0/1] pm->PopupMenu, it->Interactive";
FirstExample::usage = "Shows the cases in which the Newton's method fails ";
SecondExample::usage = "Shows the cases in which the Newton's method fails";
MethodsComparison::usage = "Show convergence to solution using three different methods";
AlgoNewton::usage="Interactive Newton's method for finding roots";
SecantInteractive::usage = "Interactively show step of iteration in the secant method";
Bolzano::usage = "Show root graph example step by step: press a button to advance";
AlgoBisez::usage = "Interactive Bisection method for finding roots";
AlgoSec::usage = "Interactive Secant method for finding roots";
Esercizio::usage="";
HappySmiley::usage="";
WarningMessage::usage="";
x::usage="Unknown variable";

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

(* Function that progressively introduces and explains Bolzano's Theorem,
slide "Problema: ricerca di uno zero di una funzione *)
Bolzano[] :=
    DynamicModule[
        {   (* variables and elements declaration *)
            buttonStatus1 = "closed",
            buttonStatus2 = "closed",
            buttonStatus3 = "closed",
            buttonStatus4 = "closed",
            plot = "",
            f,
            (*x,*)
            a,
            b,
            textCellStyle = {}
        },

        f[x_] := x^2-2;
        a = 1;
        b = 2;
        textCellStyle = {FontSize->36, FontFamily->"Source Sans Pro"};
					(* layout elements disposition *)
        plot =
            Plot[x^2 - 2, {x, -2.5, 2.5},
              ImageSize -> {800, 500},
              Background->White,
              BaseStyle->{FontSize->30}
            ];

        Row[{
            Column[{
                Row[{
                    TextCell["Notiamo che la ", textCellStyle],
                    TextCell["funzione f", textCellStyle, Blue],
                    TextCell["(x) = ", textCellStyle],
                    TextCell[TraditionalForm[ToExpression["x^2 - 2"]], textCellStyle],
                    TextCell[" \[EGrave] ", textCellStyle],
                    (* button that shows that the function is continuous *)
                    Button["continua", 
                        If[buttonStatus1 == "closed" ,
                            (buttonStatus1 = "open";
                                plot =
                                    Plot[x^2 - 2, {x, -2.5, 2.5},
                                        ImageSize -> {800, 500},
                                        Background->White,
                                        BaseStyle->{FontSize->30},
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
                    (* button that shows thaht the function is negative somewhere *)
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
                                                Text[{a, f[a]}, Offset[{60, 10}, {a, f[a]}]]
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
                    (* button that shows where the function is positive *)
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
                                                Text[{a, f[a]}, Offset[{60, 10}, {a, f[a]}]],
                                                Point[{b, f[b]}],
                                                Text[{b, f[b]}, Offset[{20, 20}, {b, f[b]}]]
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
                    (* button that shows where is the solution, the root of the function *)
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
                                            Text[{a, f[a]}, Offset[{60, 10}, {a, f[a]}]],
                                            Point[{b, f[b]}],
                                            Text[{b, f[b]}, Offset[{20, 20}, {b, f[b]}]]
                                        },
                                        {
                                            Red,
                                            Point[{Sqrt[2], 0}],
                                            Text[{Sqrt[2], 0}, Offset[{80, 10}, {Sqrt[2], 0}]]
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
                Dynamic[plot] (* shows the initial function *)
            }]
        }]
    ];

(* Function that reads expressions from 'inputExp' file *)
(* The file contains N rows composed in this way:
function, initial interval point, final interval point, point of the first iteration,
called in the slide "Esercizio" *)
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
        x0 = N[esp[[i++]][[4]],2]; (* gets the third parameter of exp that corresponds to the point of the first iteration *)
        d = Esercizio[func,a,b,x0]; (* Shows the first exercise *)
        Column[{
            Row[{
                Button[ (* Button that allows user to change exercise *)
                    Style["Nuovo Esercizio", FontSize -> 25],
                    {d = Esercizio[ (* sets input parameters that are passed to Esercizio function *)
                        ToString[esp[[1 + Mod[i, Length[esp]]]][[1]]], 
                        esp[[1 + Mod[i, Length[esp]]]][[2]],
                        esp[[1 + Mod[i, Length[esp]]]][[3]],
                        N[esp[[1 + Mod[i++, Length[esp]]]][[4]],2]
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

(* this is needed to treat the function parameters as values passed by reference *)
SetAttributes[NormalizeRangeValues, HoldFirst];
(* TODO add comment, problem with parameter passing *)
NormalizeRangeValues[av_, bv_, strav_, strbv_] :=
    Module[ {},

      a = N[ToExpression[av]];
      b = N[ToExpression[bv]];
      stra = ToString[ToExpression[strav]];
      strb = ToString[ToExpression[strbv]];

      If[
        a < 0.01,
        a = 0.01; stra = "Hai scelto per a un valore che va fuori dal range!",
        stra = ""
      ];
      If[
        b < 0.01,
        b = 0.01; strb = "Hai scelto per b un valore che va fuori dal range!",
        strb = ""
      ];
      If[
        a > 3.99,
        a = 3.99; stra = "Hai scelto per a un valore che va fuori dal range!"
      ];
      If[
        b > 3.99,
        b = 3.99; strb = "Hai scelto per b un valore che va fuori dal range!"
      ];

    ];


(* Function that shows an interactive manipulate,
 slide "Metodo di Newton (2/3 *)
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
            {0, 2*3.14}
            (*{-3, 6}*)
        };

        (* list of functions that can be selected from the popup menu *)
        listaFunzioni={
            TraditionalForm[x^2-2],
            TraditionalForm[Cos[x]],
            TraditionalForm[Sin[x]],
            TraditionalForm[-9 + (x-2)^2],
            TraditionalForm[-4 + x - 3*(x)^2 + (x)^3],
            TraditionalForm[Sin[x]*Cos[x]]
            (*TraditionalForm[-1 + x^2*Log2[x]]*)
        };

        ff=TraditionalForm[x^2-2];

        Manipulate[
            passInput[ff],
            Row[{
                TextCell["  Funzione: ", FontSize->25],
                If[pm==1,
                    PopupMenu[Dynamic[ff], listaFunzioni, MenuStyle->{FontSize->23}],
                    TextCell[TraditionalForm[x^2-2], FontSize->23]
                ]
            }],
            Initialization:>{
                passInput[input_]:= DynamicModule[
                    {nn,x00,warning},
                    (* select the position (index) of the first occurence of input inside listaFunzioni *)
                    selectedInput = Position[listaFunzioni,input][[1]];
                    (* select the corresponding plotting interval *)
                    interv = listaIntervalli[[ selectedInput ]];
                    (* aa and bb store bounds of the selected interval *)
                    aa = interv[[1]][[1]];
                    bb = interv[[1]][[2]];
					warning="";
                    Manipulate[ (* options *)
                        newton[input, N[x00], aa, bb, nn],
                        Column[{
                            Row[{
                                TextCell[" ",FontSize->25],
                                TextCell[Subscript[x,0], FontSize->23],
                                TextCell["   ", FontSize -> 25],
                                TextCell[aa+0.01,FontSize->23],
                                TextCell["   ", FontSize -> 25],
                                Slider[Dynamic[x00],{aa+0.01,bb-0.01,0.01}],
                                TextCell[" ",FontSize->25],
                                TextCell[bb-0.01,FontSize->23],
                                TextCell["   ", FontSize -> 25],
                                InputField[Dynamic[x00], ImageSize -> 150, Alignment -> Center, BaseStyle -> FontSize -> 25],
                                TextCell["   ", FontSize -> 25],
                                TextCell[Dynamic[warning], "Text"]
                            }],
                            Row[{
                                If[it==1,TextCell[" Iterazioni ", FontSize->23]],
                                If[it==1,Slider[Dynamic[nn],{1,12,1}]],
                                If[it==1,TextCell[" ",FontSize->25]],
                                If[it==1,TextCell[Dynamic[nn],FontSize->23],nn=1;]
                            }]
                        }],

                        Initialization:>{
                            newton[inputFun_,x0value_,a_,b_,n_] := Module[
                                {list,funzioneFinale,listArrow,x0},
                                x0 = N[ToExpression[x0value]];
								If[
                                  x0 < a,
                                  x0 = a+0.01; warning = "Hai scelto un valore che va fuori dal range!"
                                ];
                                If[
                                  x0 > b,
                                  x0 = b-0.01; warning = "Hai scelto per a un valore che va fuori dal range!"
                                ];
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
                                        "                         ",
                                        TextCell[Subscript["x", n],FontSize->25,FontFamily->"Source Sans Pro"],
                                        TextCell[ " = ",FontSize->25,FontFamily->"Source Sans Pro"],
                                        TextCell[list[[nn]],FontSize->25,FontFamily->"Source Sans Pro"]
                                    }],
                                    Plot[
                                        funzioneFinale, {x, aa, bb},
                                        PlotRange -> {{aa,bb},Full},
                                        AxesLabel -> {Style["x", 30], Style["y", 30]},
                                        PlotStyle -> Thickness[0.006],
                                        BaseStyle-> {FontSize->30},
                                        Background->White,
                                        Epilog -> {
                                            {
                                                Darker[Green],
                                                Thickness[0.002],
                                                If[n>0,Line[{listArrow}]]
                                            },
                                            {
                                                Darker[Green],
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
call with it = 1 to display graph with multiple iterations
slide "Metodo di bisezione (1/5),(3/5),(4/5)" *)
BisectionInteractive[pm_,it_] :=
    DynamicModule[
        {listFunctions,listIntervals,passf,ff,warninga,warningb},

        listIntervals = {
          {0,4,5},
          {(1/2*3.14), (1.5*3.14),30},
          {0,2,40},
          {0,2,10},
          {0,3.14,30}
        };

        listFunctions = {
          TraditionalForm[x^2 - 2],
          TraditionalForm[Sin[x]],
          TraditionalForm[Cos[2x]Sin[x]],
          TraditionalForm[x*Log[x]+x^3],
          TraditionalForm[Cos[x]]
        };

        ff = TraditionalForm[x^2-2];

        Manipulate[
            passf[ff],
            Row[{
                TextCell["Funzione: ", FontSize->25],
                If[pm == 1,
                    PopupMenu[Dynamic[ff], listFunctions, MenuStyle->{FontSize->23}],
                    TextCell[TraditionalForm[x^2-2], FontSize->25]
                ]
            }],

            Initialization :> {

                passf[fun_]:=	DynamicModule[
                    {
                      ax,
                      bx,
                      stepx,
                      BisezMethod,
                      selectedInput,
                      interv,
                      interva,
                      intervb,
                      yline
                    },
                    selectedInput = Position[listFunctions,ff][[1]];
                    interv = listIntervals[[ selectedInput ]];
                    interva = interv[[1]][[1]];
                    intervb = interv[[1]][[2]];
                    yline = interv[[1]][[3]];
                    ax=interva+0.01;
                    bx=intervb-0.01;
                    If[fun == TraditionalForm[x^2-2],
                        {
                        ax = 1.,
                        bx = 2.
                        }
                    ];
                    warninga = "";
                    warningb = "";


                    Manipulate[
                        BisezMethod[fun, ax, bx, stepx],

                        Column[{
                          Row[{
                              TextCell["a   ", FontSize -> 23],
                              TextCell[interva + 0.01, FontSize -> 23],
                              Slider[Dynamic[ax], {(interva + 0.01), (intervb - 0.01), 0.01}],
                              TextCell[intervb - 0.01, FontSize -> 23],
                              TextCell["   ", FontSize -> 25],
                              InputField[Dynamic[ax], ImageSize -> 150, Alignment -> Center, BaseStyle -> FontSize -> 25],
                              TextCell["   ", FontSize -> 25]
                          (* text filled when input for a is a value outside range *)
                                TextCell[Dynamic[warninga], "Text"]

                            }],
                            Row[{
                                TextCell["b   ", FontSize->23],
                                TextCell[interva+0.01, FontSize->23],
                                Slider[Dynamic[bx],{(interva+0.01), (intervb-0.01),0.01}],
                                TextCell[intervb-0.01, FontSize->23],
                                TextCell["   ",FontSize->25],
                                InputField[Dynamic[bx], ImageSize -> 150,Alignment->Center, BaseStyle -> FontSize -> 25],
                                TextCell["   ",FontSize->25],
                                (* text filled when input for b is a value outside range *)
                                TextCell[Dynamic[warningb], "Text"]

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
                                {line, bisec, intervals, vertline, fx, avalue, bvalue},

                                fx = ToExpression[ToString[ffx]];
                                avalue = N[ToExpression[aa]]; (* used to treat "inputfield"*)
                                bvalue = N[ToExpression[bb]];

                                (* TODO this MUST work in NormalizeRangeValues*)
                                (* if user inputs values outside the slider range, values are "normalized" *)

                                If[
                                  avalue < interva+0.01,
                                  avalue = interva+0.01; warninga = "Hai scelto per a un valore che va fuori dal range!",
                                  warninga = ""
                                ];
                                If[
                                  bvalue < intervb-0.01,
                                  bvalue = intervb-0.01; warningb = "Hai scelto per b un valore che va fuori dal range!",
                                  warningb = ""
                                ];
                                If[
                                  avalue > interva+0.01,
                                  avalue = interva+0.01; warninga = "Hai scelto per a un valore che va fuori dal range!"
                                ];
                                If[
                                  bvalue > intervb-0.01,
                                  bvalue = intervb-0.01; warningb = "Hai scelto per b un valore che va fuori dal range!"
                                ];
                                

                                (*
                                NormalizeRangeValues[avalue,bvalue,warninga,warningb];
                                *)

                                (* helper function that draws horizontal red line that displays the bisection range at the first iteration *)
                                line[f_,{a_,b_},1] := {Min[avalue, bvalue], Max[avalue, bvalue]};


                                (* helper function that draws horizontal red line that displays the bisection range at the nth iteration *)
                                line[f_,{a_,b_},n_] :=
                                    If[ (f/.x->a) * (f/.x->b) <= 0,
                                        bisec[f, line[f,{a,b},n -1]],
                                        {a, b}
                                    ];

                                (* main function that calculate the next iteration's range after checking sign of a and b*)
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

                                (* *)
                                intervals[f_,{a_,b_},step_] :=
                                    Table[
                                        {
                                          {line[f,{a,b},i][[1]], -(i/yline)},
                                          {line[f,{a,b},i][[2]], -(i/yline)}
                                        },
                                        {i, 1, step}
                                    ];

                                vertline[a_] := InfiniteLine[{{a, -1}, {a, 1}}];

                                Column[{
                                    If[(fx /. x -> avalue)*(fx /. x -> bvalue) <= 0,
                                        Column[{
                                            Row[{
                                                "                     ",
                                                TextCell["Intervallo: [ ",FontSize->25,FontFamily->"Source Sans Pro"],
                                                TextCell[line[fx, {avalue, bvalue}, nn][[1]],FontSize->25,FontFamily->"Source Sans Pro"],
                                                TextCell[", ",FontSize->25,FontFamily->"Source Sans Pro"],
                                                TextCell[line[fx, {avalue, bvalue}, nn][[2]],FontSize->25,FontFamily->"Source Sans Pro"],
                                                TextCell[" ]",FontSize->25,FontFamily->"Source Sans Pro"]
                                            }],
                                            Row[{
                                                "                     ",
                                                TextCell["Ampiezza: ",FontSize->25,FontFamily->"Source Sans Pro"],
                                                TextCell[Abs[line[fx, {avalue, bvalue}, nn][[1]]-line[fx, {avalue, bvalue}, nn][[2]]],FontSize->25,FontFamily->"Source Sans Pro"]
                                            }]
                                        }],
                                        Column[{
                                            Row[{
                                                "                     ",
                                                TextCell["Intervallo: [ ",FontSize->25,FontFamily->"Source Sans Pro"],
                                                TextCell[line[fx, {avalue, bvalue}, nn][[1]],FontSize->25,FontFamily->"Source Sans Pro"],
                                                TextCell[", ",FontSize->25,FontFamily->"Source Sans Pro"],
                                                TextCell[line[fx, {avalue, bvalue}, nn][[2]],FontSize->25,FontFamily->"Source Sans Pro"],
                                                TextCell[" ]",FontSize->25,FontFamily->"Source Sans Pro"]
                                            }],
                                            Row[{
                                                "            ",
                                                TextCell["L'intervallo scelto non contiene una soluzione",FontSize->25,FontFamily->"Source Sans Pro"]
                                            }]
                                        }]
                                    ],

                                    Plot[
                                        fx, {x, interva, intervb},
                                        PlotRange -> All,
                                        PlotStyle->Thickness->0.006,
                                        ImageSize -> {800, 500} ,
                                        BaseStyle-> {FontSize->30},
                                        Background->White,
                                        Epilog -> {
                                            {Red, Thickness[0.002], Line /@ intervals[fx, {avalue, bvalue}, nn]},
                                            {Dashed, vertline /@ line[fx, {avalue, bvalue}, nn]},
                                            If[pm == 0 && it == 0,
                                              {Darker[Green],PointSize[0.015],Point[{((avalue+bvalue)/2),-(nn/yline)}]}
                                            ]
                                        }
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

(* slide "Metodo delle secanti (\*/3)" *)
SecantInteractive[pm_,it_] :=
    DynamicModule[
        {Secant, passInput, intervalsList, functionsList, selectedInput, aa, bb, interval,ff, warninga, warningb},

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

        ff = TraditionalForm[x^2-2];

        Manipulate[
            passInput[ff],

            Row[{
                TextCell["Funzione: ", FontSize->25],
                If[pm == 1,
                    PopupMenu[Dynamic[ff], functionsList, MenuStyle->{FontSize->23}],
                    TextCell[TraditionalForm[x^2-2], FontSize->25]
                ]
            }],

            Initialization:> {

                passInput[input_]:= DynamicModule[
                    {
                      ax,
                      bx,
                      iteration
                    },
                    selectedInput = Position[functionsList,input][[1]];
                    interval = intervalsList[[selectedInput]];
                    aa = interval[[1]][[1]];
                    bb = interval[[1]][[2]];
                    ax = aa + 0.01;
                    bx = bb - 0.01;

                    If[input == TraditionalForm[x^2-2],
                      {
                        ax = 1.,
                        bx = 2.
                      }
                    ];

                    warninga = "";
                    warningb = "";

                    Manipulate[
                        Secant[input,N[ax],N[bx],aa,bb,iteration],

                        Column[{
                            Row[{
                              TextCell["a   ", FontSize->23],
                              TextCell[aa + 0.01, FontSize -> 23],
                              Slider[Dynamic[ax],{(aa+0.01), (bb-0.01),0.01}],
                              TextCell[bb - 0.01, FontSize -> 23],
                              TextCell["   ",FontSize->23],
                              InputField[Dynamic[ax], ImageSize -> 150, Alignment -> Center, BaseStyle -> FontSize -> 25],
                              TextCell["   ", FontSize -> 23],
                              TextCell[Dynamic[warninga],"Text"]
                            }],
                            Row[{
                              TextCell["b   ", FontSize->23],
                              TextCell[aa + 0.01, FontSize -> 23],
                              Slider[Dynamic[bx],{(aa+0.01), (bb-0.01),0.01}],
                              TextCell[bb - 0.01, FontSize -> 23],
                              InputField[Dynamic[bx], ImageSize -> 150,Alignment->Center, BaseStyle -> FontSize -> 25],
                              TextCell["   ",FontSize->23],
                              TextCell[Dynamic[warningb],"Text"]

                            }],
                            Row[{
                                If[it == 1,TextCell["Iterazioni ", FontSize->23]],
                                If[it == 1,Slider[Dynamic[iteration],{0,6,1}]],
                                If[it == 1,TextCell[" ",FontSize->23]],
                                If[it == 1,TextCell[Dynamic[iteration],FontSize->23],iteration=0;]
                            }]
                        }],

                        Initialization:>{

                            Secant[inputFun_,x0_,x1_,a_,b_,i_] := Module[
                                {xValues,finalFunction, x0value, x1value},

                                finalFunction = ToExpression[ToString[inputFun]];
                                x0value = N[ToExpression[x0]];
                                x1value = N[ToExpression[x1]];

                                (* if user inputs values outside the slider range, values are "normalized" *)

                                If[
                                  x0value < a+0.01,
                                  x0value = a+0.01; warninga = "Hai scelto per a un valore che va fuori dal range!",
                                  warninga = ""
                                ];
                                If[
                                  x1value < a+0.01,
                                  x1value = a+0.01; warningb = "Hai scelto per b un valore che va fuori dal range!",
                                  warningb = ""
                                ];
                                If[
                                  x0value > b-0.01,
                                  x0value = b-0.01; warninga = "Hai scelto per a un valore che va fuori dal range!"
                                ];
                                If[
                                  x1value > b,
                                  x1value = b-0.01; warningb = "Hai scelto per b un valore che va fuori dal range!"
                                ];

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
                                    BaseStyle-> {FontSize->30},
                                    Background->White,
                                    Epilog -> {
                                        {
                                            Thickness[0.002],
                                            MapIndexed[
                                                {
                                                    Darker[Green],
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
                                            Darker[Green],
                                            (Point[{#1, 0}] &) /@ Flatten[xValues]
                                        },
                                        {(* x0 and x1 points are black *)
                                            PointSize[0.01],
                                            Black,
                                            Point[{x0value,0}],
                                            Point[{x1value,0}]
                                        },
                                      {
                                        If[x0value == 1 && x1value == 2,
                                          (* draw point labels only for the starting x0=1 and x1=2 (aka a and b)*)
                                          {
                                            Text[
                                              {1, -1},
                                              Offset[{0, 70}, {1, -1}]
                                            ],
                                            Text[
                                              {2, 2},
                                              Offset[{0, 70}, {2, 2}]
                                            ]
                                      }
                                        ]
                                      }
                                    },
                                    Axes -> True,
                                    ImageSize -> {750,450},
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
(* slide "Limiti del metodo di Newton (1/2) *)
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
                    TextCell[Subscript["x","0"],FontSize->25],
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
                        BaseStyle-> {FontSize->30},
                        Background->White,
                        Prolog->{
                            RGBColor[210/255, 223/255, 242/255],
                            Rectangle[{0,-1.5},{3.1,2.4}]
                        },
                        Epilog -> {
                            Darker[Green],
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
                            Red,
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
(* slide "Limiti del metodo di Newton (2/2) *)
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
                    TextCell[Subscript["x","0"], FontSize -> 25],
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
                        BaseStyle-> {FontSize->30},
                        Background->White,
                        Epilog->{
                            Darker[Green],
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
                                    Darker[Green],
                                    Thickness[0.004],
                                    Arrow[{
                                        {x0, f  /. x-> x0},
                                        {(x0 - ((f /. x -> x0)/(D[f,x] /. x -> x0))),0}
                                    }],
                                    Black,
                                    Text[TextCell[Subscript[x,1],FontSize->21],{(x0 - ((f /. x -> x0)/(D[f,x] /. x -> x0))),0.3}]
                                },
                                {
                                    Darker[Green],
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
                                    Text[TextCell["Perfettamente Parallela",FontSize->25],{0,1.1}]
                                }
                            ],
                            Darker[Green],
                            PointSize[0.015],
                            Point[{x0,0}],
                            Red,
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
(* cover slide, initialization cells *)
ConvertImageToFullyScaledNinePatch[img_] :=
    Module[ {paddedImage = ImagePad[img,1,Black] },
        ReplaceImageValue[
            paddedImage,
            Flatten[Outer[List,{0,#1},{0,#2}]&@@ImageDimensions[paddedImage],1] -> White]
    ];

(* SetBackground[img_] :=
        SetOptions[SelectedNotebook[],
         System`BackgroundAppearance -> ConvertImageToFullyScaledNinePatch[img_]];*)
         
(* Helper function that calculate the value of f(x) for a specific given x
and display it on screen, called in Esercizio[] *)
AddIteration[i_,fun_,x0_] :=
Module[{xn=Null},
        (*NewtonList = NestList[N[Rationalize[(Rationalize[#1] - ((fun /. x->Rationalize[#1])/(D[fun, x]/.x->Rationalize[#1])))],3] &, Rationalize[x0], 10]*)
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
                    xn = Subscript[x,i-1];
                ];
                Dynamic[N[(Rationalize[xn] - ((fun /. x->Rationalize[xn])/(D[fun, x]/.x->Rationalize[xn])))]]
                
        }]
    ];

(* Function that manage the exercise area,
gets in input the function, the initial interval point, the final interval point and
the first point from which start the iteration *)
(* called in ReadInputFile[] *)
Esercizio[funzione_, a_, b_,x0_] :=
    DynamicModule[ {plot,testoRow1,testoRow2,buttonNew,fun,i,IterationList,Iter2Result,Risultato,xn},
        fun = ToExpression[funzione]; (* the current function *)
        (* plot the current function *)
        plot =
            Plot[
                fun, {x, a, b},
                PlotStyle -> Thickness[0.006],
                BaseStyle-> {FontSize->30},
                Background->White,
                Epilog->{
                    Darker[Green],
                    PointSize[0.015], (* plots the point of the first iteration *)
                    Point[{x0,0}]
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
                                TextCell[x0,FontSize->30,FontColor->Blue]
                                (*InputField[Dynamic[x0],BaseStyle->{FontSize->30,FontColor->Blue},ImageSize->100]*) (* first iteration point *)

                            }],
                            Dynamic[Column@IterationList], (* add new iteration *)
                            Button[
                                Style["Aggiungi Iterazione", FontSize -> 20],
                                {
                                    AppendTo[IterationList, AddIteration[++i,fun,x0]],
                                    Iter2Result =
                                        NestList[N[(Rationalize[#1] - ((fun /. x->Rationalize[#1])/(D[fun, x]/.x->Rationalize[#1])))] &, x0, i+1]
                                }, ImageSize->200],
                            Row[{ (* in this section is verified the result inserted by the user *)
                                TextCell["Inserisci il risultato: ", "Text", FontSize -> 30],
                                InputField[Dynamic[Risultato], String, BaseStyle->FontSize->25, ImageSize->150],
                                "  ",
                                Button[Style["Verifica", FontSize -> 20],
                                    {
                                        If[ ToString[Risultato] == ToString[Iter2Result[[i+1]]], (* verification of the entered value *)
                                            CreateDialog[{ (* if correct *)
                                                Column[{
                                                    TextCell["Complimenti!", FontSize -> 25],
                                                    TextCell["\[HappySmiley]", FontSize -> 200, FontColor -> Green],
                                                    TextCell["Hai risolto l'esercizio correttamente!", FontSize -> 25],
                                                    DefaultButton[]
                                                },Alignment->Center]
                                            },WindowTitle->"Corretto"],
                                            CreateDialog[{ (* if wrong *)
                                                Column[{
                                                    TextCell["Errore!", FontSize -> 25],
                                                    TextCell["\[WarningSign]", FontSize -> 200, FontColor -> Red],
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
(* slide "Confronto tra i tre metodi" *)
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
                    TextCell["Iterazione ",FontSize->25],
                    Slider[Dynamic[i], {1, 15, 1}],
                    TextCell[Dynamic[i],FontSize->25]
                }],
                Row[{
                    (* control tolerance *)
                    TextCell["Tolleranza ",FontSize->25],
                    SetterBar[
                        Dynamic[tau],
                        {0.1, 0.01, 0.001, 0.0001, 0.00001},
                        (*ImageSize->Full,*)
                        BaseStyle->{FontSize->25}
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
                        (* current a and b values *)
                          TextCell[
                            Row[{"a = ", N[Rationalize[bisectionRoots[[i]][[1]]],2],
                              " b = ", N[Rationalize[bisectionRoots[[i]][[2]]],2]
                            },
                              Alignment->Center],
                            "Text",
                            TextAlignment -> Center,
                            CellBaseline -> Bottom,
                            CellSize -> {500, 50},
                            FontSize->25
                          ],

                          Plot[f[x], {x, 1.5, 4.5},
                              BaseStyle-> {FontSize->30},
                              Background->White,
                                Epilog -> {
                                  (*
                                    Directive[{Thick, Green, Dashed}],
                                    *)
                                    (* calculation and plot of the subsequent roots for the bisection method *)

                                    (* draw line passing through approximation *)
                                  {
                                    Green, Thick,
                                    InfiniteLine[
                                      {(bisectionRoots[[i]][[1]] + bisectionRoots[[i]][[2]]) / 2, 0},
                                      {0, 1}
                                    ]
                                  },
                                    (* draw lines passing through interval bounds *)
                                  {
                                    InfiniteLine[
                                      {bisectionRoots[[i]][[1]], 0},
                                      {0, 1}
                                    ],
                                    InfiniteLine[
                                      {bisectionRoots[[i]][[2]], 0},
                                      {0, 1}
                                    ]
                                  },
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
                                ImageSize -> 700,
                                PlotLabel -> "Bisezione",
                                LabelStyle->Directive[FontSize->15],
                                PlotStyle -> Thickness[0.006]

                            ]
                            , " ",
                            (* display the current root value for the bisection method *)
                            TextCell[
                                Row[{Subscript["x", i], " = ", (bisectionRoots[[i]][[1]] + bisectionRoots[[i]][[2]]) / 2}, Alignment -> Center],
                                "Text",
                                Darker[Green],
                                TextAlignment -> Center,
                                CellBaseline -> Center,
                                CellSize -> {500, 50},
                                FontSize->20

                            ],

                            TextCell[
                                Row[{Pi, " = ", N[Pi,10]}, Alignment->Center],
                                "Text",
                                Red,
                                TextAlignment -> Center,
                                CellBaseline -> Bottom,
                                CellSize -> {500, 50},
                                FontSize->20

                            ],
                            TextCell[Dynamic[textBisection], "Text"]
                        }],
                        (* secant *)
                        Column[{
                        (* current a and b values *)
                          (*
                          TextCell[
                            Row[{"a = ", N[Rationalize[secantRoots[[i-1]][[2]]],2],
                              " b = ", N[Rationalize[secantRoots[[i]][[2]]],2]
                            },
                              Alignment->Center],
                            "Text",
                            TextAlignment -> Center,
                            CellBaseline -> Bottom,
                            CellSize -> {500, 50},
                            FontSize->20
                          ],
                          *)
                            TextCell[" ",FontSize->25],
                            TextCell[" "],
                          Plot[f[x], {x, 1.5, 4.5},

                              BaseStyle-> {FontSize->30},
                              Background->White,
                                Epilog -> {
                                  (*
                                    Directive[{Thick, Green, Dashed}],
                                    *)
                                    (* calculation and plot of the subsequent roots for the secant method *)
                                  {
                                    Green, Thick,
                                  InfiniteLine[{secantRoots[[i]][[2]], 0}, {0, 1}]
                                  },
                                  (*,
                                  {
                                    InfiniteLine[
                                      {secantRoots[[i-1]][[2]], 0},
                                      {0, 1}
                                    ],
                                    InfiniteLine[
                                      {secantRoots[[i]][[2]], 0},
                                      {0, 1}
                                    ]
                                  },
                                  *)
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
                                ImageSize -> 700,
                                PlotLabel -> "Secanti",
                                LabelStyle->Directive[FontSize->15],
                                PlotStyle -> Thickness[0.006]
                            ],
                            " ",
                            (* display the current root value for the secant method *)
                            TextCell[
                                Row[{Subscript["x", i], " = ", secantRoots[[i]][[2]]}, Alignment -> Center],
                                "Text",
                                Darker[Green],
                                TextAlignment -> Center,
                                CellBaseline -> Bottom,
                                CellSize -> {500, 50},
                                FontSize->20
                            ],

                            TextCell[
                                Row[{Pi, " = ", N[Pi,10]}, Alignment->Center],
                                "Text",
                                Red,
                                TextAlignment -> Center,
                                CellBaseline -> Bottom,
                                CellSize -> {500, 50},
                                FontSize->20

                            ],
                            TextCell[Dynamic[textSecant], "Text"]
                        }],
                        (* Newton *)
                        Column[{
                            TextCell[" ",FontSize->25],
                            Plot[f[x], {x, 1.5, 4.5},

                                BaseStyle-> {FontSize->30},
                                Background->White,
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
                                ImageSize -> 700,
                                PlotLabel -> "Newton",
                                LabelStyle -> Directive[FontSize->15],
                                PlotStyle -> Thickness[0.006]
                            ],
                            (* display the current root value for the Newton's method *)
                            TextCell[
                                Row[{Subscript["x", i], " = ", newtonRoots[[i]]}, Alignment -> Center],
                                "Text",
                                Darker[Green],
                                TextAlignment -> Center,
                                CellBaseline -> Bottom,
                                CellSize -> {500, 50},
                                FontSize->20

                            ],
                            TextCell[
                                Row[{Pi, " = ", N[Pi,10]}, Alignment->Center],
                                "Text",
                                Red,
                                TextAlignment -> Center,
                                CellBaseline -> Bottom,
                                CellSize -> {500, 50},
                                FontSize->20

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
         
 (* Interactive algorithm that shows step by step the application of Bisection's method,
  slide "Algoritmo" under the section "Metodo di bisezione" *)
AlgoBisez[] :=
    DynamicModule[ (* variables declaration *)
        {ww, zz, tt1, ff,soluzione},
        ww = ToExpression["a"];
        zz = ToExpression["b"];
        tt1 = ToExpression["\[Tau]"];
        ff = x^2 - 2;
	soluzione = x/. FindRoot[ff,{x,1}];
        Manipulate[
            BisectionAlgorithm[ww, zz, tt1],

            Column[{ (* input fields that allows to insert a,b and the tollerance value  *)
                Row[{
                    TextCell["Sia f = ", FontSize -> 25],
                    TextCell[TraditionalForm[ff], FontSize -> 25]
                }],
                Row[{
                    TextCell["Dati due valori ", FontSize -> 25],
                    InputField[Dynamic[ww], ImageSize -> 150,Alignment->Center, BaseStyle -> FontSize -> 25],
                    TextCell[" e ", FontSize -> 25],
                    InputField[Dynamic[zz], ImageSize -> 150,Alignment->Center, BaseStyle -> FontSize -> 25]
                }] ,
                Row[{
                    TextCell["Posta una tolleranza \[Tau] ", FontSize -> 25],
                    InputField[Dynamic[tt1], ImageSize -> 100, Alignment->Center, BaseStyle -> FontSize -> 25]
                }]
            }],

            Initialization :> {
                BisectionAlgorithm[w_, z_, t1_] := DynamicModule[
                    {cValB, fcValB},
					(* variables initialization *)
					
                    a1 = N[ToExpression[w]];
                    If[w != ToExpression["a"], w= a1];
                    b1 = N[ToExpression[z]];
                    If[z != ToExpression["b"], z = b1];
                    If[t1 == ToString["\[Tau]"], "", ""];
                                                                                                 
                    Column[{
                        Row[{ (* theorical area *)
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
                                    Row[{ (* interactive area that shows step by step calculations *)
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
                                            (* check if the tolerance has been reached *)
                                            If[ToString[w] != ToString["a"] && ToString[z] != ToString["b"] && ToString[t1] != ToString["\[Tau]"],
                                                If[Abs[w - z] > ToExpression[ToString[t1]],
                                                    cVal1B = ((w + z)/2) // N, (* if it's not reached, calculate f(c)  *)
                                                    cValB = ""; (* if it's reached, shows a dialog box *)

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
                                                            }],
                                                          Row[{
                                                            TextCell["! Smettere di reiterare !", FontSize -> 20]
                                                          }],
                                                          Row[{
                                                            TextCell["Cliccare sulla x per chiudere", FontSize -> 20]
                                                          }]
                                                        }, Alignment -> Center]
                                                    ]
                                                ],
                                                cVal1B = "" (* // if input values are not inserted yet *)
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
                                        (* check if the tolerance is reached, if  not shows the result of f(c) *) 
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
                                        (* after f(c) is calculated, we check if the sign of f(a) is equal to the sign of f(c) *)
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
                                Button[ (* button that allows to calculate the next Bisection's iteration*)
                                    TextCell["Reitera", FontSize -> 25],
                                    If[a1 < soluzione && b1 > soluzione,
{
                                    (* check if the tolerance is reached *)
                                    If[Abs[w - z] >= ToExpression[ToString[t1]],
                                        If[(ff /. x -> w)*(ff /. x -> cValB) >= 0, (* check if sign f(a) = sign f(c) *)
                                            { (* if true, a = c *)
                                                If[ToString[cValB] != ToString[""],
                                                    ww = cValB,
                                                    Break[]
                                                ];
                                                zz = z;
                                                tt1 = t1
                                            },
                                            { (* if false, b = c *)
                                                If[ToString[cValB] != ToString[""], zz = cValB, Break[]];
                                                ww = w;
                                                tt1 = t1
                                            }
                                        ],

										                    (* if tolerance is reached shows a dialog box *)
                                        CreateDialog[
                                            Column[{
                                                Row[{TextCell["Approssimazione ", FontSize -> 25]}],
                                                Row[{
                                                  TextCell["con tolleranza ", FontSize -> 25],
                                                  TextCell[t1, FontSize -> 25]
                                                }],
                                                Row[{TextCell["Raggiunta", FontSize -> 25]},
                                                  Row[{
                                                    TextCell["! Smettere di reiterare !", FontSize -> 20]
                                                  }],
                                                  Row[{
                                                    TextCell["Cliccare sulla x per chiudere", FontSize -> 20]
                                                  }]
                                            ]}, Alignment -> Center]
                                        ]
                                    ]},{ CreateDialog[
                                            Column[{
                                                Row[{TextCell["Attenzione!", FontSize -> 25]}],
                                                Row[{
                                                  TextCell["Nessuna soluzione per i valori a e b scelti.", FontSize -> 25]
                                                }],
                                               
                                                  Row[{
                                                    TextCell["! Scegliere altri valori !", FontSize -> 20]
                                                  }],
                                                  Row[{
                                                    TextCell["Cliccare sulla x per chiudere", FontSize -> 20]
                                                  }]
                                            }, Alignment -> Center]
                                        ]
                                        }],
                                    ImageSize -> 200
                                ]
                            }]
                        }]
                ]
            }, Paneled -> False
        ]
    ];
    
(* Interactive algotithm that shows step by step the application of Secant's method *)
(* slide "Algoritmo" under the section "Metodo delle secanti" *)
AlgoSec[] :=
    DynamicModule[ (* variables declaration *)
        {aa, bb, cc, \[Tau]\[Tau], ff, faVal, fbVal, cVal1},
        faVal = ""; fbVal = ""; cVal1 = "";
        aa = ToExpression["a"];
        bb = ToExpression["b"];
        \[Tau]\[Tau] = ToExpression["\[Tau]"];
        ff = x^2 - 2;

        Manipulate[
            SecAlgorithm[aa, bb, \[Tau]\[Tau]],

            Column[{
                Row[{ (* input fields that allows to insert a,b and the tolerance value  *)
                    TextCell["Sia f = ", FontSize -> 25],
                    TextCell[TraditionalForm[ff], FontSize -> 25]
                }],
                Row[{
                    TextCell["Dati due valori ", FontSize -> 25],
                    InputField[Dynamic[aa], ImageSize -> 150, Alignment -> Center, BaseStyle -> FontSize -> 25],
                    TextCell[" e ", FontSize -> 25],
                    InputField[Dynamic[bb], ImageSize -> 150, Alignment -> Center, BaseStyle -> FontSize -> 25]
                }],
                Row[{
                    TextCell["Posta una tolleranza \[Tau] ", FontSize -> 25],
                    InputField[Dynamic[\[Tau]\[Tau]], ImageSize -> 100, Alignment -> Center, BaseStyle -> FontSize -> 25]
                }]
            }],

            Initialization :> {
                SecAlgorithm[a_, b_, t_] := DynamicModule[
                    { cVal, fcVal, r, rr},
                    (* variables initialization *)
                    a1 = N[ToExpression[a]];
                    If[a != ToExpression["a"], a = a1];
                    b1 = N[ToExpression[b]];
                    If[b != ToExpression["b"], b = b1];
                    If[t == ToString["\[Tau]"], "", ""];

                    Column[{
                         Row[{ (* theorical area *)
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
                                 Row[{ (* interactive area that shows step by step calculations *)
                                     Column[{
                                         Row[{
                                             Column[{
                                                 Row[{

                                                     If[ToString[a] != ToString["a"] &&
                                                        ToString[b] != ToString["b"] &&
                                                        ToString[t] != ToString["\[Tau]"],
                                                        (* check if the tolerance has been reached *)
                                                        If[Abs[a - b] >= ToExpression[ToString[t]],
                                                           { (* if not, make the calculations *)
                                                               faVal = ff /. x -> a;
                                                               fbVal = ff /. x -> b;
                                                               cVal1 = a - ((faVal*(b - a))/(fbVal - faVal));
                                                               r = x /. Solve[ff == 0, x];
                                                               rr = N[r[[2]]];

                                                               If[ToString[cVal1] ==
                                                                  ToString[rr],
                                                                 CreateDialog[
                                                                   (*Column[{
                                                                    Row[{TextCell["Trovata soluzione: ",
                                                                                        FontSize -> 25]}],
                                                                     Row[{TextCell["x = ", FontSize -> 25],
                                                                         TextCell[cVal1, FontSize -> 25]}]
                                                                      Row[{TextCell["Cliccare sulla x per chiudere", FontSize -> 25]}]
                                                                  },*)
                                                                     Column[{
                                                                         Row[{TextCell["Approssimazione ",FontSize -> 25]}],
                                                                         Row[{TextCell["con tolleranza ", FontSize -> 25],TextCell[t, FontSize -> 25]}],
                                                                         Row[{TextCell["Raggiunta", FontSize -> 25]}],
                                                                         Row[{
                                                                           TextCell["! Smettere di reiterare !", FontSize -> 20]
                                                                         }],
                                                                       Row[{
                                                                         TextCell["Cliccare sulla x per chiudere", FontSize -> 20]
                                                                       }]
                                                                     },Alignment -> Center]]
                                                               ]
                                                           },
                                                           {
                                                              (* if it's reached, shows a dialog box *)
                                                               CreateDialog[
                                                                            Column[{
                                                                              Row[{TextCell["Approssimazione ",FontSize -> 25]}],
                                                                              Row[{TextCell["con tolleranza ", FontSize -> 25],TextCell[t, FontSize -> 25]}],
                                                                              Row[{TextCell["Raggiunta", FontSize -> 25]},
                                                                                Row[{
                                                                                  TextCell["! Smettere di reiterare !", FontSize -> 20]
                                                                                }],
                                                                                Row[{
                                                                                  TextCell["Cliccare sulla x per chiudere", FontSize -> 20]
                                                                                }]

                                                                     ]},
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
                                             Row[{TextCell["Raggiunta", FontSize -> 25]}],
                                           Row[{
                                             TextCell["! Smettere di reiterare !", FontSize -> 20]
                                           }],
                                           Row[{
                                             TextCell["Cliccare sulla x per chiudere", FontSize -> 20]
                                           }]
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
         
(* Interactive algotithm that shows step by step the application of Newton's method *)
(* slide "Algoritmo" under the section "Metodo di Newton" *)
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
                                                    }],
                                                    Row[{
                                                      TextCell["! Smettere di reiterare !", FontSize -> 20]
                                                    }],
                                                    Row[{
                                                        TextCell["Cliccare sulla x per chiudere", FontSize -> 20]
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

Protect["LearningNewtonsMethod`*"];
    
EndPackage[]
