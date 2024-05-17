(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["Flip`SphereMesh`"];


SphereMesh::usage = "SphereMesh[recs,radius,base] create a recursively defined, triangulated sphere mesh from an icosahedron"


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*Base shape. *)


icosV=Normalize/@{{0, 0, Sqrt[5/2 - Sqrt[5]/2]}, {Sqrt[(2*(5 - Sqrt[5]))/5], 
  0, Sqrt[1/2 - 1/(2*Sqrt[5])]}, {Sqrt[(5 - 2*Sqrt[5])/5], 
  1, Sqrt[1/2 - 1/(2*Sqrt[5])]}, 
 {-Sqrt[1/2 + 1/(2*Sqrt[5])], (-1 + Sqrt[5])/2, 
  Sqrt[1/2 - 1/(2*Sqrt[5])]}, {-Sqrt[1/2 + 1/(2*Sqrt[5])], 
  (1 - Sqrt[5])/2, Sqrt[1/2 - 1/(2*Sqrt[5])]}, 
 {Sqrt[(5 - 2*Sqrt[5])/5], -1, Sqrt[1/2 - 1/(2*Sqrt[5])]}, 
 {Sqrt[1/2 + 1/(2*Sqrt[5])], (-1 + Sqrt[5])/2, 
  -Sqrt[1/2 - 1/(2*Sqrt[5])]}, {-Sqrt[(5 - 2*Sqrt[5])/5], 1, 
  -Sqrt[1/2 - 1/(2*Sqrt[5])]}, {-Sqrt[(2*(5 - Sqrt[5]))/5], 
  0, -Sqrt[1/2 - 1/(2*Sqrt[5])]}, {-Sqrt[(5 - 2*Sqrt[5])/5], 
  -1, -Sqrt[1/2 - 1/(2*Sqrt[5])]}, 
 {Sqrt[1/2 + 1/(2*Sqrt[5])], (1 - Sqrt[5])/2, 
  -Sqrt[1/2 - 1/(2*Sqrt[5])]}, 
 {0, 0, -Sqrt[5/2 - Sqrt[5]/2]}};


icosF={{1, 2, 3}, {1, 3, 4}, {1, 4, 5}, {1, 5, 6}, {1, 6, 2}, {2, 7, 3},
   {3, 8, 4}, {4, 9, 5}, {5, 10, 6}, {6, 11, 2}, {7, 8, 3}, {8, 9, 4},
   {9, 10, 5}, {10, 11, 6}, {11, 7, 2}, {7, 12, 8}, {8, 12, 9},
   {9, 12, 10}, {10, 12, 11}, {11, 12, 7}};


(* ::Subsection:: *)
(*Error messages for the exported objects*)


SphereMesh::huuge="It's unwise to select more than about 8 recursions (`1`).";


SphereMesh::neg="Recursions must be non-negative: `1`";


(* ::Subsection:: *)
(*Definition of the exported functions*)


SphereMesh[numSubdivisions_Integer /; Negative[numSubdivisions]] :=
	Message[SphereMesh::neg,numSubdivisions]


SphereMesh[numSubdivisions_Integer /; NonNegative[numSubdivisions]] := 
	Module[{v=icosV,f=icosF},
	
		If[numSubdivisions > 8, Message[SphereMesh::huuge,numSubdivisions]];

		indexr[vx_]:=Module[{p},
			If[Length[p=Position[v,vx]]!=0,
				p[[1,1]],
				AppendTo[v,vx];Length[v]]];

		subdivider[{i1_,i2_,i3_}]:=Module[{v1,v2,v3,i12,i23,i31},
			{v1,v2,v3}=v[[{i1,i2,i3}]];
			i12=indexr[Normalize[v1+v2]];
			i23=indexr[Normalize[v2+v3]];
			i31=indexr[Normalize[v3+v1]];
			{{i1,i12,i31},{i12,i2,i23},{i23,i3,i31},{i12,i23,i31}}];

		Do[f=Flatten[subdivider/@f,1],{numSubdivisions}];

	{v,f}]


(* ::Section:: *)
(*Package Footer*)


End[];
EndPackage[];
