(* ::Package:: *)

Clear@finalState;
finalState[pars_]:=finalState[pars]=Module[{sol=simulate[pars]},#->(#[0.99tmax]/.pars/.sol)&/@vars];


Lrange=Range[0,2200,10];
wRange=Range[28,40,.2];
{wmin,wmax}=wRange[[{1,-1}]];
wRange1D=Range[25,36,.2];
