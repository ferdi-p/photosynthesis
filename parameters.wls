#!/usr/bin/env wolframscript
(* ::Package:: *)

(*primary data we use for our estimations*)

LKalaji=15000.;
CExcitationKalaji=1. / (50. 10^-6);

tUncontrolledLossMuller=1.6 10^-9;

yEfficiencyEarl=0.9;

FvFmMaxTakahashi = 0.65;

LsaturationChalker = 250;

LequilibrateStrasser = 280;
tEquilibrateStrasser = 0.1;

energyInStrasser = tEquilibrateStrasser \[Alpha]C totalc LequilibrateStrasser;
energyOutStrasser = tEquilibrateStrasser 0.5 \[Beta]PtotalP;

XantophyllToChlorophyllRubio = 0.1;

LNPQmax = 2000;

tUncontrolledPlusNPQMuller=0.5 10^-9;

\[Phi]NtotalP=0.1;

LrepairHalfSaturation = 5;

LrepairMin = 5;

(*put together model parameters*)

parsPre=
{
(*base model parameters (before damage)*)
\[Alpha]C->CExcitationKalaji/LKalaji,
\[Alpha]U->1/tUncontrolledLossMuller,

totalc->yEfficiencyEarl/\[Alpha]C,
\[Alpha]PtotalP -> \[Alpha]U FvFmMaxTakahashi/(1-FvFmMaxTakahashi),
\[Beta]PtotalP -> \[Alpha]C totalc LsaturationChalker,
totalp -> energyInStrasser-energyOutStrasser,

\[Alpha]P -> \[Alpha]PtotalP/totalp,
\[Beta]P -> \[Beta]PtotalP/totalp,

totaln ->  XantophyllToChlorophyllRubio totalc,
\[Beta]Ntotaln -> \[Alpha]C LNPQmax totalc,
\[Beta]N -> \[Beta]Ntotaln/totaln,

\[Alpha]N -> (1/tUncontrolledPlusNPQMuller-\[Alpha]U)/totaln,

(*NPQ activation and deactivation*)
\[Phi]N-> \[Phi]NtotalP/totalp,
\[Delta]N-> 10^-2,

(*ROS-damage repair*)
\[Kappa]E -> \[Alpha]C totalc LrepairHalfSaturation,
\[Epsilon]E -> \[Alpha]C totalc LrepairMin,

(*temperature parameters from Arrhenius fit*)
\[Sigma]T1->75880.48690911727`,
\[Sigma]T2->1.6217151701578067`*^6,
\[Rho]T->0.001`,
\[Gamma]T1->0.0017539602638653462`,
\[Gamma]T2->11270.382309814662`,
wRef->35.`,

(*ROS creation*)
\[Mu]pX->70,
\[Mu]pD->70,
\[Mu]pT->600,
\[Mu]cX->0,


\[Phi]R-> 3600^-1 totalp,
\[Kappa]D->.1 totalp,
\[Rho]D->(2 3600.)^-1,

(*initial values*)

ini[c]->totalc,
ini[cX]->0,

ini[p]->totalp,
ini[pX]->0,
ini[pT]->0,
ini[pD]->0,
ini[pTD]->0,

ini[n0]->totaln,
ini[nX]->0,
ini[n]->0,

(*default time span*)

tmin->0,
tmax->200,

(*default environment*)



L->200,
w->28


};

(*this function applies the parameter list to itself to calculate the numeric values of the derived parameters*)
updateReplaceListWithItself[parsPre_]:=MapAt[#//.parsPre&,parsPre,{All,2}];
parsPre2=Nest[updateReplaceListWithItself,parsPre,3];

(*output values*)
parsPre2//Column

(*calculate total pools*)
total[c]=ini[c]+ini[cX];
total[p]=ini[p]+ini[pX];
total[n]=ini[n0]+ini[n]+ini[nX];

(*the \[Gamma]'s for time scaling, together with time ranges for which they are good*)
timeScaleList=Table[
Module[{x},
x = {
"tmax"->10.^i,
"tmin"->10.^(i-2),
\[Gamma]C->Max[10^-7,Min[1,10.^(-i-5)]],
\[Gamma]N->Max[10^-4,Min[1,10.^(-i+1)]],
\[Gamma]P->Max[10^-3,Min[1,10.^(-i+2)]],
\[Gamma]N0->Max[10^-2,Min[1,10.^(-i+5)]]
};
timeRange[i]=x[[1;;2]];
pars[i]=Join[x[[3;;-1]],parsPre2];
x
],
{i,Range[-10,8,2]~Join~{\[Infinity]}}
];

(*parameters that turn off damage dynamics*)
parsNoDamage={\[Mu]pX->0,\[Mu]pD->0,\[Mu]pT->0,\[Mu]cX->0};
