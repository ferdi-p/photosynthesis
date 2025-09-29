#!/usr/bin/env wolframscript
(* ::Package:: *)

total[c]=ini[c]+ini[cX];
total[p]=ini[p]+ini[pX];
total[n]=ini[n0]+ini[n]+ini[nX];
parsPre=
{
\[Alpha]C->1.3,(*
\[Alpha]U->.5 10^9,*)
yEfficiency->0.9,
totalc->yEfficiency/\[Alpha]C,

totalp->26,
Lsaturate->250,
\[Beta]P->Lsaturate/totalp,

totaln->.07 totalp, (*multiplied with some factor to try*)
LNPQmax->2000,
\[Beta]N-> \[Alpha]C LNPQmax totalc/totaln(*2.6 10^4*) ,
\[Alpha]N->5 10^9,

(*the \[Phi] are the \[Gamma] in the paper - still?*)

\[Phi]N-> 2 10^-2/totalp,
\[Delta]N-> 10^-1,

\[Phi]DpX->28,
\[Phi]DpD->28,
\[Phi]DpT->180,(*
\[Phi]DpTD->50,*)

\[Phi]R-> 3600^-1(*(2 3600)^-1*) totalp,
\[Kappa]D->.1 totalp,
\[Kappa]E->5,
\[Epsilon]E->5(*9.1*),
\[Rho]D->(2 3600)^-1,
\[Alpha]PtotalP->9.285714285714285`*^8,\[Alpha]U->5.`*^8,\[Sigma]T1->75880.48690575601`,\[Sigma]T2->1.6217247268996052`*^6,\[Rho]T->0.001`,xTref1->0.0017539602637067046`,xTref2->11271.523556046794`,wRef->35.`,
(*\[Sigma]T->505834.6529812634`,xTref->20.`,\[Rho]T->0.001`,wRef->35.`,\[Alpha]PtotalP->9.705882352941177`*^8,\[Alpha]U->5.`*^8,*)
\[Alpha]P->\[Alpha]PtotalP/totalp,

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

ini[d]->0,

L->200,
w->28,

tmin->0,
tmax->200


};
updateReplaceListWithItself[parsPre_]:=MapAt[#//.parsPre&,parsPre,{All,2}];
parsPre2=Nest[updateReplaceListWithItself,parsPre,3];

timeScaleList=Table[
Module[{x},
x = {
"tmax"->10.^i,
"tmin"->10.^(i-1),
\[Gamma]C->Min[1,10.^(-i-3)],
\[Gamma]N->Min[1,10.^(-i+1)],
\[Gamma]P->Min[1,10.^(-i+3)],
\[Gamma]N0->Min[1,10.^(-i+5)],
\[Gamma]T->Min[1,10.^(-i+6)],
\[Gamma]D->Min[1,10.^(-i+6)]
};
timeRange[i]=x[[1;;2]];
pars[i]=Join[x[[3;;-1]],parsPre2];
x
],
{i,Range[-10,8,1]~Join~{\[Infinity]}}
];


parsNoDamage={\[Phi]DpX->0,\[Phi]DpD->0,\[Phi]DpT->0,\[Phi]DpTD->0};
