(* ::Package:: *)

BeginPackage["utils`"]


intervalComplement::usage="f[x_Interval] gives the complement of an interval with respect to the real number line.
f[x_Interval,y_Interval] gives the complement of y as a subset of x."


removePoints::usage="removePoints[x_Interval] removes isolated points from an interval.  This is useful if you want an interval to represent a set of integers, with the convention that Interval[{a,b}] is the integers n with a<=n<b."


Begin["`Private`"]


intervalComplement[Interval[{a_,b_}]]:=If[a==-\[Infinity],Interval[{b,\[Infinity]}],If[b==\[Infinity],Interval[{-\[Infinity],a}],Interval[{-\[Infinity],a},{b,\[Infinity]}]]];
intervalComplement[x_Interval]:=IntervalIntersection[intervalComplement[Interval[First[x]]],intervalComplement[Rest[x]]];
intervalComplement[x_Interval,y_Interval]:=IntervalIntersection[x,intervalComplement[y]];


removePoints[x_Interval]:=DeleteCases[x,{z_,z_}];


End[]


EndPackage[]
