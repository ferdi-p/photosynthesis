(* ::Package:: *)

eqs[rhs_,vars_]:=NSolve[{0==rhs,vars>=0},vars];
(*eqs[rhs_,vars_]:=Union[Quiet@NSolve[{0==rhs,vars>=0,\[Phi]<=1},vars,Method->"Symbolic"],Quiet@NSolve[{0==rhs,\[Phi]==1},vars](*,Quiet@NSolve[{0==rhs,\[Phi]==0},vars],Quiet@NSolve[{0==rhs,\[Phi]==1},vars]*)];*)
(*eqs[rhs1,Join[pars,{L->1.9}],{\[Phi]}]*)

jacobi[rhs_,vars_]:=Table[D[rhs,var],{var,vars}];
(*jacobi[rhs1,Join[pars,{L->1}],{\[Phi]}]//MatrixForm*)
stability[jaci_,eqi_]:=With[{eigenval=Max@Re@Quiet@Eigenvalues[jaci/.eqi]},Which[TrueQ[0>eigenval],"stable",TrueQ[0<eigenval],"unstable",True,"inconclusive"]];(*stability[jacobi[rhs1,Join[pars,{L->2}],{\[Phi]}],eqs[rhs1,Join[pars,{L->2}],{\[Phi]}][[1]]]*)
(*findEqStab[rhs_,pars_,vars_]:=Normal@GroupBy[eqs[rhs,pars,vars],stability[jacobi[rhs,pars,vars],#]&];
findEqStab[rhs1,Join[pars,{L->1.9}],{\[Phi]}]*)
findEqStab[rhs_,vars_]:={stability[jacobi[rhs,vars],#],#}&/@eqs[rhs,vars];
(*findEqStab[rhs1,Join[pars,{L->1.1}],{\[Phi]}]*)
makeEqStabList[rhs_,vars_,focalpar_,range_]:=Module[{dataset=Dataset[Flatten[Table[<|"stability"->#[[1]],"values"->Join[{focalpar->x},#[[2]]]|>&/@findEqStab[rhs/.focalpar->x,vars],{x,range}],1]][GroupBy["stability"]]},
dataset[All,All,"values"]
];
(*makeEqStabList[rhs1,pars,{\[Phi]},L,Join[Range[0,1.8,.01],Range[1.8,2,.002],Range[2,3,.01]]];*)

