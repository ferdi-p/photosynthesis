(* ::Package:: *)

dataDucret=Import[NotebookDirectory[]<>"data/Ducret_et_al_2025_Fig3.xlsx","Dataset"][[1]];


dataDucret[Select[#["Treatment"]=="Control"\[And]#["temperature"]==27.0&]]


dataDucretFig3BControlMean=Transpose[{Keys[#],Flatten@Values[#]}&@({Mean[Values[#][[All,2]]]}&/@Normal[dataDucret[Select[#["Treatment"]=="Control"\[And]#["temperature"]==27.0&]][All,{"light","FvFm"}][GroupBy[#light &]]])];
ListPlot[dataDucretFig3BControlMean]
