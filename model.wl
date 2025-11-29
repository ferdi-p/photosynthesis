(* ::Package:: *)

(*quenching and uncontrolled excitation loss*)
gP = \[Alpha]P p cX;
gN = \[Alpha]N n cX;
gU = \[Alpha]U cX;
gF = gU;
gR = gU;

(*processing by photochemistry and NPQ*)
hP = \[Beta]P pX;
hN = \[Beta]N nX;

(*NPQ activation and deactivatino dynamics*)
fN = \[Phi]N pX n0;
fN0 = \[Delta]N n;

(*ROS-damage creation*)
(*with additional mechanism not used in paper: ROS from excited chlorophyll directly, paramter set to 0*)
xD =Min[10^3,RpX+RT+RD+RcX];
{RpX,RT,RD,RcX}=cX {\[Mu]pX pX, \[Mu]pT (pT+pTD), \[Mu]pD (pD+pTD),\[Mu]cX};

(*ROS-damage repair*)
e = Max[0,hP-\[Epsilon]E];
rD = \[Phi]R e/(e+\[Kappa]E) 1/(pTD+pD+\[Kappa]D);


(*heat-damage creation and repair*)
xT = Min[10^3,\[Gamma]T1  E^(-\[Sigma]T1/(w+273))/E^(-\[Sigma]T1/(wRef+273))+\[Gamma]T2  E^(-\[Sigma]T2/(w+273))/E^(-\[Sigma]T2/(wRef+273))];
rT = \[Rho]T;

(*right hand sides of diferential equations*)
ODEs=
{
cX -> \[Gamma]C (\[Alpha]C L c -(gP+gN+gU)),
c->-\[Gamma]C (\[Alpha]C L c -(gP+gN+gU)),

pX-> \[Gamma]P(gP - hP)  - (xD + xT) pX,
p-> -\[Gamma]P(gP - hP) - (xD + xT)p + rD pD + rT pT,
pT->(xT(p+pX) -  rT pT) - xD pT + rD pTD,
pD->(xD(p+pX) -  rD pD) - xT pD + rT pTD,
pTD->(xD pT - rD pTD)+(xT pD - rT pTD),

n0->\[Gamma]N0(fN0 - fN),
n->-\[Gamma]N(gN-hN)-\[Gamma]N0(fN0 - fN),
nX->\[Gamma]N(gN-hN)
};


(*state variables*)
vars=ODEs[[All,1]];
(*replacement rule to add time argument*)
addTime=#->#[t]&/@vars;

(*put together ODEs with parameter pars, with time argument and initial conditions*)
equations[pars_]:=Flatten[{
#'[t]==(#/.ODEs/.addTime),
#[tmin]==ini[#]
}&/@vars/.pars];

(*simulate ODEs with parameter pars*)
simulate[pars_]:=NDSolve[
equations[pars],
vars,
{t,tmin,tmax}/.pars, Method->{"EquationSimplification"->"Residual"}
][[1]];

(*derived model metrics*)
\[Phi]PSII=(\[Alpha]P (p))/(\[Alpha]P  (p) + \[Alpha]U+\[Alpha]N (n));
FvFm=(\[Alpha]P (p+pX))/(\[Alpha]P  (p+pX) + \[Alpha]U);
(*health=(p+pX)/ totalp;
ROSDamage = (pD+pTD)/ totalp;
heatDamage = (pT+pTD)/ totalp;
allp = p+pX+pT+pD+pTD;*)
YP=gP/(gP+gN+gU);
YN=gN/(gP+gN+gU);
YU=gU/(gP+gN+gU);
(*tempEffect= rT/(xT+rT);*)
