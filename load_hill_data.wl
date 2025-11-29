(* ::Package:: *)

fileHill2012=Import[NotebookDirectory[]<>"data/Hill 2012 Fig 1.csv"];
dataHill[]={};
{
dataHill["Light"],
dataHill["FvFm","Control"],
dataHill["FvFm","Bleaching"],
dataHill["YII","Control"],
dataHill["YII","Bleaching"],
dataHill["YNPQ","Control"],
dataHill["YNPQ","Bleaching"],
dataHill["YNO","Control"],
dataHill["YNO","Bleaching"]
}=
Table[
Quiet@Select[ToExpression@(fileHill2012[[3;;-1,{1,2}+2 i]]),NumberQ[#[[1]]]\[And]NumberQ[#[[2]]]&],
{i,0,8}];
dataHill["FvFm","Control"]
