(* ::Package:: *)

gP = \[Alpha]P p cX;
gN = \[Alpha]N n cX;
gU = \[Alpha]U cX;
gF = gU;
gR = gU;
hP = \[Beta]P pX;
hN = \[Beta]N nX;

fN = \[Phi]N pX n0;
fN0 = \[Delta]N n;

(*xT = Min[10^-2,xTref E^(-\[Sigma]T/(w+273))/E^(-\[Sigma]T/(wRef+273))];*)
xT = Min[10^3,xTref1  E^(-\[Sigma]T1/(w+273))/E^(-\[Sigma]T1/(wRef+273))+xTref2  E^(-\[Sigma]T2/(w+273))/E^(-\[Sigma]T2/(wRef+273))];


rT = \[Rho]T;

{RpX,RT,RD,RcX}=cX {\[Phi]DpX pX, \[Phi]DpT (pT+pTD), \[Phi]DpD (pD+pTD) (*, \[Phi]DpTD pTD*),\[Phi]DcX};
xD =Min[10^3,RpX+RT+RD+RcX];
e = Max[0,hP-\[Epsilon]E];
rD = \[Phi]R e/(e+\[Kappa]E) 1/(pTD+pD+\[Kappa]D);
(*rD = \[Rho]D;*)
(*rD = \[Phi]R /(pTD+pD+\[Kappa]D);*)

ODEsPre=
{
cX -> \[Gamma]C (\[Alpha]C L c -(gP+gN+gU)),
c->-\[CapitalDelta][cX],

pX-> \[Gamma]P(gP - hP)  - (\[Gamma]D xD + \[Gamma]T xT) pX,
p->-\[Gamma]P(gP - hP) - (\[Gamma]D xD + \[Gamma]T xT)p + \[Gamma]D rD pD + \[Gamma]T rT pT,
pT->\[Gamma]T (xT(p+pX) -  rT pT) - \[Gamma]D xD pT + \[Gamma]D rD pTD,
pD->\[Gamma]D (xD(p+pX) -  rD pD) - \[Gamma]T xT pD + \[Gamma]T rT pTD,
pTD->\[Gamma]D (xD pT - rD pTD)+\[Gamma]T (xT pD - rT pTD),

n0->\[Gamma]N0(fN0 - fN),
n->-\[CapitalDelta][nX]-\[CapitalDelta][n0],
nX->\[Gamma]N(gN-hN)
};


ODEs=ODEsPre/.((\[CapitalDelta][x_]:>(x/.ODEsPre)));
vars=ODEs[[All,1]];
addTime=#->#[t]&/@vars;

\[Phi]PSII=(\[Alpha]P (p+pX))/(\[Alpha]P  (p+pX) + \[Alpha]U+\[Alpha]N (n+nX));
FvFm=(\[Alpha]P (p+pX))/(\[Alpha]P  (p+pX) + \[Alpha]U);
health=(p+pX)/ totalp;
ROSDamage = (pD+pTD)/ totalp;
heatDamage = (pT+pTD)/ totalp;
allp = p+pX+pT+pD+pTD;
YP=gP/(gP+gN+gU);
YN=gN/(gP+gN+gU);
YU=gU/(gP+gN+gU);
tempEffect= rT/(xT+rT);
equations[pars_]:=Flatten[{
#'[t]==(#/.ODEs/.addTime),
#[tmin]==ini[#]
}&/@vars/.pars];
simulate[pars_]:=NDSolve[
equations[pars],
vars,
{t,tmin,tmax}/.pars, Method->{"EquationSimplification"->"Residual"}
][[1]];
