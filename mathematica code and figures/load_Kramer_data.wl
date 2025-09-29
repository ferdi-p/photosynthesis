(* ::Package:: *)

fileKramer=Quiet[ToExpression@Import[NotebookDirectory[]<>"data/Kramer_et_al_2004_Fig2B_phis.csv"]];


dataKramer=fileKramer[[3;;-1,#]]&/@{{3,4},{1,2},{5,6}};


(*ListPlot[dataKramer,PlotLegends->{"P","N","U"}]*)


gKramer={#[[1]],0.9 #[[1]]#[[2]]}&/@#&/@dataKramer;


ListPlot[gKramer,Joined->True,PlotMarkers->Automatic]
