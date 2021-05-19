(* ::Package:: *)

BeginPackage["Resonators`"]
importS2p::usage = "Import S21 part of S2P data in RI format in form {FREQ,10Log10[S21],\[Phi]}";
importS2pFull::usage = "Import all of S2P data in RI format  in form {FREQ,S11,S12,S21,S22}";
importS2pCSVlog::usage = "Import CSV in RI format as {FREQ,10Log10[S21],\[Phi]}";
NLMfitS21scLogmag::usage = "Fit side-coupled resonance transmission with fano asymmetry to data in tuples of {freq, \!\(\*SubscriptBox[\(S\), \(21\)]\)(dB)}. Returns plot and fit parameter table (slow)";
NLMfitS21scLogmagQuiet::usage = "Fit side-coupled resonance transmission with fano asymmetry to data in tuples of {freq, \!\(\*SubscriptBox[\(S\), \(21\)]\)(dB)}. Returns parameters only (fast)";

importS2p[dir_]:=Module[{allData,data},
(*return {FREQ,10Log10[S21],\[Phi]}*)
data = Select[Import[dir,"Table"],NumberQ[#[[1]]]&];
allData = Drop[data,1]\[Transpose];
{allData[[1]]/1.*^9,allData[[4]],allData[[5]]}
]

importS2pFull[dir_]:=Module[{allData,data,freqFactor},
(*return {FREQ,S11,S12,S21,S22}*)
data = Select[Import[dir,"Table"],NumberQ[#[[1]]]&];
allData = Drop[data,1]\[Transpose];
freqFactor = If[allData[[1,5]]>1.*^8,1.*^-9,1.];
{allData[[1]]*freqFactor,
allData[[2]] + I allData[[3]],
allData[[6]] + I allData[[7]],
allData[[4]] + I allData[[5]],
allData[[8]] + I allData[[9]]
}
]

importS2pCSVlog[dir_,index_:0]:=Module[{allData,data,fullData,freqFactor},
(*always return {FREQ,10Log10[S21],\[Phi]}*)
fullData =Import[dir,"Table","FieldSeparators"-> {","," ","\t"}];
data = Select[fullData,NumberQ[#[[1]]]&];
allData = Drop[data,1]\[Transpose];
freqFactor = If[allData[[1,5]]>1.*^8,1.*^-9,1.];
{allData[[1]]*freqFactor,10Log10[Abs[allData[[2+2*index]]+I allData[[3+2*index]]]^2],ArcTan[allData[[2+2*index]], allData[[3+2*index]]]}
]

importS2pCSVri[dir_,index_:0]:=Module[{allData,data,fullData,freqFactor},
(*always return {FREQ,I,Q}*)
fullData =Import[dir,"Table","FieldSeparators"-> {","," ","\t"}];
data = Select[fullData,NumberQ[#[[1]]]&];
allData = Drop[data,1]\[Transpose];
freqFactor = If[allData[[1,5]]>1.*^8,1.*^-9,1.];
{allData[[1]]*freqFactor,allData[[2+2*index]],allData[[3+2*index]]}
]

importS2pCSVcmplx[dir_,index_:0]:=Module[{allData,data,fullData,freqFactor},
(*always return {FREQ,I+iQ}*)
fullData =Import[dir,"Table","FieldSeparators"-> {","," ","\t"}];
data = Select[fullData,NumberQ[#[[1]]]&];
allData = Drop[data,1]\[Transpose];
freqFactor = If[allData[[1,5]]>1.*^8,1.*^-9,1.];
{allData[[1]]*freqFactor,allData[[2+2*index]]+I allData[[3+2*index]]}
]

NLMfitS21scLogmag[localdata_] := Module[{A0est,minPt,\[Sigma],Qest,Qist,weights,model},
(*Fit side-coupled resonance measurement to data in tuples of {freq, Subscript[S, 21](dB)}*)
A0est = Mean[{First[localdata] [[2]],Last[localdata][[2]]}];
minPt = First @ SortBy[ localdata,#[[2]]&];
\[Sigma] = 2(Select[localdata,#[[2]]<Max[A0est - 3,Mean[{A0est,minPt[[2]]}]]&]\[Transpose][[1]]//InterquartileRange);
Qest =minPt[[1]]/\[Sigma];
Qist = 10^(1/20 (A0est-minPt[[2]])) (Qest-10^((-A0est+minPt[[2]])/20) Qest);
model = NonlinearModelFit[localdata,10Log10[Abs[(Qe + 2 I Qi Qe ((\[Nu] - \[Nu]0-\[Delta]\[Nu])/(\[Nu]0+\[Delta]\[Nu])+\[Delta]\[Nu]/\[Nu]0))/(Qi + Qe + 2 I Qi Qe (\[Nu] - \[Nu]0-\[Delta]\[Nu])/(\[Nu]0+\[Delta]\[Nu]))]^2]+A0,{{\[Nu]0,minPt[[1]]},{Qe,Qest},{Qi,Qist},{\[Delta]\[Nu],0},{A0,A0est}},\[Nu],Weights->(1+E^(-(#-minPt[[1]])^2/(2 \[Sigma]^2))&)];
Row[{Show[ListPlot[localdata[[;;;;10]],PlotRange->All,Joined-> True,FrameLabel->{"\[Nu]","\!\(\*SubscriptBox[\(S\), \(21\)]\)(dB)"}],Plot[Normal[model],{\[Nu],localdata\[Transpose][[1]]//Min,localdata\[Transpose][[1]]//Max},PlotStyle->{Red},PlotRange->All]],
model["ParameterTable"],TableForm[{{minPt[[1]]},{Qest},{Qist},{0.0},{A0est}},TableHeadings->{{"\[Nu]0","\!\(\*SubscriptBox[\(Q\), \(e\)]\)","\!\(\*SubscriptBox[\(Q\), \(i\)]\)","\[Delta]\[Nu]","A0"},{"Crude Estimates"}}]},"       "]
]

NLMfitS21scLogmagQuiet[localdata_] := Module[{A0est,minPt,\[Sigma],Qest,Qist,weights,model},
(*Fit side-coupled resonance measurement to data in tuples of {freq, Subscript[S, 21](dB)}*)
A0est = Mean[{First[localdata] [[2]],Last[localdata][[2]]}];
minPt = First @ SortBy[ localdata,#[[2]]&];
\[Sigma] = 2(Select[localdata,#[[2]]<Max[A0est - 3,Mean[{A0est,minPt[[2]]}]]&]\[Transpose][[1]]//InterquartileRange);
Qest =minPt[[1]]/\[Sigma];
Qist = 10^(1/20 (A0est-minPt[[2]])) (Qest-10^((-A0est+minPt[[2]])/20) Qest);
model = NonlinearModelFit[localdata,10Log10[Abs[(Qe + 2 I Qi Qe ((\[Nu] - \[Nu]0-\[Delta]\[Nu])/(\[Nu]0+\[Delta]\[Nu])+\[Delta]\[Nu]/\[Nu]0))/(Qi + Qe + 2 I Qi Qe (\[Nu] - \[Nu]0-\[Delta]\[Nu])/(\[Nu]0+\[Delta]\[Nu]))]^2]+A0,{{\[Nu]0,minPt[[1]]},{Qe,Qest},{Qi,Qist},{\[Delta]\[Nu],0},{A0,A0est}},\[Nu],Weights->(1+E^(-(#-minPt[[1]])^2/(2 \[Sigma]^2))&)];
model["BestFitParameters"]
]

NLMfitS11dcLogmag[localdata_] := Module[{A0est,maxPt,\[Sigma],Qest,Qist,weights,model},
(*Fit side-coupled resonance measurement to data in tuples of {freq, Subscript[S, 21](dB)}*)
A0est = Mean[{First[localdata] [[2]],Last[localdata][[2]]}];
maxPt = Last @ SortBy[ localdata,#[[2]]&];
\[Sigma] = 2(Select[localdata,#[[2]]>Min[A0est + 3,Mean[{A0est,maxPt[[2]]}]]&]\[Transpose][[1]]//InterquartileRange);
Qest =maxPt[[1]]/\[Sigma];
Qist = Qest;
model = NonlinearModelFit[localdata,10Log10[Abs[B0 *Exp[-I \[Phi]]+A0*(1- (Qi - 2 I Qi Qe \[Delta]\[Nu]/\[Nu]0)/((Qi+Qe) + 2 I Qi Qe (\[Nu]-\[Nu]0)/\[Nu]0))]^2],{{\[Nu]0,maxPt[[1]]},{Qe,Qest},{Qi,Qist},{\[Delta]\[Nu],0},{A0,0.5},{B0,1.5},{\[Phi],\[Pi]}},\[Nu],Weights->(1+E^(-(#-maxPt[[1]])^2/(2 \[Sigma]^2))&)];
Row[{Show[ListPlot[localdata[[;;;;10]],PlotRange->All,Joined-> True,FrameLabel->{"\[Nu]","\!\(\*SubscriptBox[\(S\), \(21\)]\)(dB)"}],Plot[Normal[model],{\[Nu],localdata\[Transpose][[1]]//Min,localdata\[Transpose][[1]]//Max},PlotStyle->{Red},PlotRange->All]],
model["ParameterTable"],TableForm[{{maxPt[[1]]},{Qest},{Qist},{0.0},{A0est}},TableHeadings->{{"\[Nu]0","\!\(\*SubscriptBox[\(Q\), \(e\)]\)","\!\(\*SubscriptBox[\(Q\), \(i\)]\)","\[Delta]\[Nu]","A0"},{"Crude Estimates"}}]},"       "]
]

NLMfitS11dcLogmagQuiet[localdata_] := Module[{A0est,maxPt,\[Sigma],Qest,Qist,weights,model},
(*Fit side-coupled resonance measurement to data in tuples of {freq, Subscript[S, 21](dB)}*)
A0est = Mean[{First[localdata] [[2]],Last[localdata][[2]]}];
maxPt = Last @ SortBy[ localdata,#[[2]]&];
\[Sigma] = 2(Select[localdata,#[[2]]>Min[A0est + 3,Mean[{A0est,maxPt[[2]]}]]&]\[Transpose][[1]]//InterquartileRange);
Qest =maxPt[[1]]/\[Sigma];
Qist = Qest;
model = NonlinearModelFit[localdata,10Log10[Abs[B0 *Exp[-I \[Phi]]+A0(1- (Qi - 2 I Qi Qe \[Delta]\[Nu]/\[Nu]0)/((Qi+Qe) + 2 I Qi Qe (\[Nu]-\[Nu]0)/\[Nu]0))]^2],{{\[Nu]0,maxPt[[1]]},{Qe,Qest},{Qi,Qist},{\[Delta]\[Nu],0},{A0,0.5},{B0,1.5},{\[Phi],\[Pi]}},\[Nu],Weights->(1+E^(-(#-maxPt[[1]])^2/(2 \[Sigma]^2))&)];
model["BestFitParameters"]
]
EndPackage[]






