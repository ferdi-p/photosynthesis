(* ::Package:: *)

dashing=AbsoluteDashing[1];
labelstyle=13;
thickness=AbsoluteThickness[3];
plotMarkers=({Graphics`PlotMarkers[][[1,1]],12});

colorCurves=ColorData[116][1](*Darker@Gray*);
colorPoints=ColorData[116][2](*Darker@Gray*);

ImagePaddingLeftTop={{50,5},{40,40}};
ImagePaddingUnits={{50,5},{45,10}};
ImagePaddingYUnits={{55,5},{20,10}};
(*ImagePaddingUnitsNarrow={{30,5},{20,5}};*)
ImagePaddingExtra={{80,5},{45,10}};
ImagePaddingExtra2={{65,0},{45,10}};
ImagePaddingExtra3={{70,0},{45,10}};
ImagePaddingXunits={{40,5},{45,10}};
ImagePaddingXunitsOnly={{30,5},{35,10}};
ImagePaddingNoXunits={{30,5},{0,10}};
ImagePaddingLeftBottom2Axis={{50,0},{45,0}};
ImagePaddingLeftBottom2AxisAlternative={{50,5},{45,10}};

ImagePaddingLeft2Axis={{25,0},{30,0}};
(*ImagePaddingNoUnits={{25,5},{20,10}};*)
rawImageSize=(110{GoldenRatio,1});
imageSizePadding[padding_]:={ImageSize->rawImageSize+(Total/@padding),ImagePadding->padding};
imageSizePadding2Axis[padding_]:={ImageSize->rawImageSize+(padding[[1,1]]+Total/@padding),ImagePadding->padding};


(*imageSizePadding[padding_]:={ImageSize->rawImageSize[[1]]+(Total/@padding[[1]])(*{rawImageSize[[1]]+(Total/@padding[[1]]),rawImageSize[[2]]+(Total/@padding[[2]])}*),ImagePadding->padding};
imageSizePadding2Axis[padding_]:={ImageSize->rawImageSize[[1]]+2 Total/@padding[[1]],ImagePadding->padding};*)
gridRounding=1;

style={
imageSizePadding[ImagePaddingUnits],
LabelStyle->labelstyle,
Frame->{{True,None},{True,None}},
AxesOrigin->{0,0},
FrameTicks->{{autoTicks,Automatic},
{autoTicks,Automatic}},
FormatType -> (Style[TraditionalForm[##],SingleLetterItalics -> True] &),
Axes->None,
ImagePadding->All};

hexRGBA[s_String]:=Module[
{c},
c=FromDigits[#,16]/255.&/@StringPartition[StringTrim[s,"#"],2];
RGBColor[c[[1]],c[[2]],c[[3]],If[Length[c]>=4,c[[4]],1]]];

colorsBase={
"P"->hexRGBA["6c8ebfff"],
"N"->hexRGBA["9a78a9ff"],
"U"->hexRGBA["990000ff"],
"FvFm"->colorCurves,
"L"->hexRGBA["d79b00ff"],
"w"->hexRGBA["990000ff"]
};

colorsFinal={
ROSDamage->"L",
heatDamage->"w",
health->"P",
RpX->"P",
RT->"w",
RD->"L",
YP->"P",
YN->"N",
YU->"U",
gP->"P",
gN->"N",
gU->"U",
FvFm->"FvFm",
w->"w",
L->"L"

};
colors=colorsFinal/.colorsBase;

autoTicks=Charting`ScaledTicks[{Identity,Identity}][##,{4,2}]&;






(*simulateMemory[pars_]:=simulateMemory[pars]=simulate[pars];*)
plotSimulation[Quantity_,pars_,timesToShow_,options_]:=
Module[
{sol=simulate[pars]},
Plot[Quantity/.addTime/.sol/.pars,{t,timesToShow[[1]],timesToShow[[2]]},Evaluate@options,PlotStyle->{(*(Quantity/.colors),*)thickness},Exclusions->None,Evaluate@style]
];


(*padding1=ImagePadding\[Rule](ImagePadding/.style)(*{{50,10},{40,5}}*);*)
maxLforPlots=640(*422*);


marker=({Graphics`PlotMarkers[][[1,1]],7});


units0={
L->"\[Mu]mol quanta \!\(\*SuperscriptBox[\(m\), \(-\(2\)\(\\\ \)\)]\)\!\(\*SuperscriptBox[\(s\), \(-1\)]\)",
w->"\[Degree]C",
W->"\[Degree]C",
energy->"\[Mu]mol quanta \!\(\*SuperscriptBox[\(m\), \(-2\)]\)"(*,
,_->""*)
};
units=
Join[
#->""&/@{YP,YN,YU},
#->"\!\(\*SuperscriptBox[\(s\), \(-1\)]\)"&/@{RpX,RT,RD},
{
w->(w/.units0)
},
#->(L/.units0)&/@{L,hP,gN,gP,gU},
#->(energy/.units0)&/@{c,cX,n0,n,nX,p,pX,pT,pD,pTD},
#->("\!\(\*SuperscriptBox[\(s\), \(-1\)]\)")&/@{xD,xT,rD,rT},
{_->""}];


names={
ROSDamage->"ROS-damage",
heatDamage->"heat-damage",
health->"functional",(*
damage->"photo-damage",
inhibited->"heat inhibited",*)
RpX->"\!\(\*SubscriptBox[\(R\), SuperscriptBox[\(P\), \(*\)]]\)",
RD->"\!\(\*SubscriptBox[\(R\), SubscriptBox[\(P\), \(D\)]]\)",
RT->"\!\(\*SubscriptBox[\(R\), SubscriptBox[\(P\), \(T\)]]\)",
YP->"Y(II)",
YN->"Y(NPQ)",
YU->"Y(NO)",
w->"temperature",
L->"light",
c->"\!\(\*
StyleBox[\"c\",\nFontSlant->\"Italic\"]\)",
cX->"\!\(\*SuperscriptBox[\(c\), \(*\)]\)",
p->"\!\(\*
StyleBox[\"p\",\nFontSlant->\"Italic\"]\)",
pX->"\!\(\*SuperscriptBox[\(p\), \(*\)]\)",
n->"\!\(\*
StyleBox[\"n\",\nFontSlant->\"Italic\"]\)",
nX->"\!\(\*SuperscriptBox[\(n\), \(*\)]\)",
n0->"\!\(\*SubscriptBox[\(n\), \(0\)]\)",
\[Phi]PSII->"\!\(\*SubscriptBox[\(\[Phi]\), \(PSII\)]\)",
FvFm->"\!\(\*SubscriptBox[\(F\), \(V\)]\)/\!\(\*SubscriptBox[\(F\), \(M\)]\)",
hP->"photosynthesis",
gP->"\!\(\*SubscriptBox[\(g\), \(P\)]\)",
gN->"\!\(\*SubscriptBox[\(g\), \(N\)]\)",
gU->"\!\(\*SubscriptBox[\(g\), \(U\)]\)",
pD->"\!\(\*SubscriptBox[\(p\), \(D\)]\)",
pT->"\!\(\*SubscriptBox[\(p\), \(T\)]\)",
pTD->"\!\(\*SubscriptBox[\(p\), \(TD\)]\)",
xD->"ROS damage rate"
};

namesVerbal={
gP->"photochemistry",
gN->"NPQ",
gU->"other loss",
xD->"ROS damage rate",
RpX->"overexcitation",
RT->"heatdamage malfunction",
RD->"photodamage malfunction"};


dayTicks=MapAt[
Style[#,10]&,
Flatten[Table[{24 3600(#[[1]]+i),#[[2]]}&/@{{0,"12am"},
{0.25,"6am"},
{0.5,"12pm"},
{0.75,"6pm"},
{1,"12am"}
},{i,0,100}],1],
{All,2}];
hourTicks=Table[{ 3600 i ,i},{i,24}];

halfDayTicks=MapAt[
Style[#,10]&,
Flatten[Table[{24 3600(#[[1]]+i),#[[2]]}&/@{{0,"12am"},
{0.25,"6am"},
{0.375,"9am"},
{0.5,"12pm"},
{0.675,"3pm"},
{0.75,"6pm"},
{1,"12am"}
},{i,0,100}],1],
{All,2}];

hourTicks=Table[{ 3600 i ,i},{i,24}];


(*defaultRanges=Join[{w->{24,37}},Join[#[[1]]->{-.05,1.05}(#[[2]]/.pars[0])&/@{FvFm->1,FvPrimeFmPrime->1,d->1,L->2100,\[Phi]->1,P->350,NPQout->620,fROS->10^-5,damage->1,
c->ini[c]+ini[cX],cX->ini[c]+ini[cX],p->ini[p]+ini[pX],pX->ini[p]+ini[pX],n0->ini[n0]+ini[n]+ini[nX],n->ini[n0]+ini[n]+ini[nX],nX->ini[n0]+ini[n]+ini[nX]},{_->Full}]];*)


logTicks=Table[{10^i,("10")^ToString@i},{i,-10,10,2}];


plotSimulationStitch[Quantity_,parsTimesToShowList_,options_]:=Show[Table[plotSimulation[Quantity,parsTimesToShow[[1]],parsTimesToShow[[2]],Join[options,{PlotRange->Full,ScalingFunctions->{"Log",Automatic}}]],{parsTimesToShow,parsTimesToShowList}],PlotRange->{0,Full},Exclusions->None,FrameTicks->{{Log@#[[1]],#[[2]]}&/@logTicks[[1;;-1;;2]],Automatic}];
plotSimulationStitch[FvFm,Table[{Join[{L->1500,tmin->0,tmax-> "tmax"/.timeRange[i]},pars[i]],{"tmin"/.timeRange[i],"tmax"/.timeRange[i]}},{i,2,8,1}],{}]



makeOneSimulationSteadyStatePlot[pars_,focalpar_,focalparrange_,singlePlotQuants_,tend_,options_]:=ListPlot[
Table[
Table[
Module[
{parsIterated=Join[{focalpar->x,tmax->tend},pars]},
{x,quant/.finalState[parsIterated]/.parsIterated}],
{x,focalparrange}],{quant,singlePlotQuants}],
Evaluate@options,
AxesOrigin->({0,0}/.pars),
FrameLabel->{{None,None},
{focalpar/.units,None}},
Joined->True,
PlotStyle->thickness,
Evaluate@style];


GridMap[f_,nestedList_,optionsRows_:{},optionsColumns_:{}]:=Style[Column[Row[#,optionsRows]&/@Map[f,nestedList,{2}],Evaluate@optionsColumns,Alignment->Center],LineBreakWithin->False];


font="Arial";


tempMin=22;
tempMax=36;
FvFmMaxPlot=.72;
FvFmMinPlot=-0.05 1/1.05 FvFmMaxPlot;


quants={{c,cX},{p,pX},{pT,pD,pTD},{n0,n,nX},{RpX,RT,RD},{gP,gN,gU},{\[Phi]PSII,FvFm}};


legendVertical[values_,unit_,colors_,legend_]:=Column[{
Style[legend,FontFamily->"Arial"],
Grid[{Plot[.6,{x,0,1},ImageSize->{20,10},PlotStyle->{AbsoluteThickness[2.5],#/.colors},Axes->None,PlotRange->{0,1}],Style[Row[{#,unit}," "],FontFamily->"Arial"]}&/@values,Spacings->{.5,.5}]},Alignment->Center];



legendVerticalVariables[quants_]:=Column[{
Grid[{Plot[.6,{x,0,1},ImageSize->{20,10},PlotStyle->{AbsoluteThickness[2.5],#/.colors},Axes->None,PlotRange->{0,1}],Style[#/.names,FontFamily->"Arial",12]}&/@quants,Spacings->{.5,.5}]},Alignment->Center];


fullSimulationPanel[parameters_,timesToPlot_,options_,optionsByQuantity_]:=GridMap[
plotSimulation[
#,
parameters,
timesToPlot,
{
Evaluate@optionsByQuantity[#],
Evaluate@options,
PlotRange->Full,
PlotLabel->#/.names,
imageSizePadding[ImagePaddingExtra],
Evaluate@style
}
]&,Join[{{L,w}},quants]];


widgetHeat=Rasterize[Magnify[Import[NotebookDirectory[]<>"widgets/thermometer.png"],.7]];
widgetModerate=Rasterize[Magnify[Import[NotebookDirectory[]<>"widgets/leaf.png"],.1]];
widgetSun=Import[NotebookDirectory[]<>"widgets/simple_sun.png"];
widgetMoon=Import[NotebookDirectory[]<>"widgets/simple_crescent.png"];
