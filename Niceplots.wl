(* ::Package:: *)

BeginPackage["Niceplots`"]
colorGradToIndexed::usage = "Convert gradient from COLORDATA to indexed color list for plotting purposes";
blueredcolorPMfunc::usage = "Color function Blue > White > Red corresponding to positive / negative";
jet::usage = "Matlab style color function";

SetOptions[Plot,BaseStyle->{FontFamily->"Helvetica",FontSize->14},PlotStyle->{AbsolutePointSize[3],AbsoluteThickness[1.0]},Frame->True,FrameStyle->Directive[14,Black],ImageSize->300,AspectRatio->1,Axes->False];
SetOptions[ParametricPlot,BaseStyle->{FontFamily->"Helvetica",FontSize->14},PlotStyle->{AbsolutePointSize[3],AbsoluteThickness[1.0]},Frame->True,FrameStyle->Directive[14,Black],ImageSize->300,AspectRatio->1,Axes->False];
SetOptions[LogPlot,BaseStyle->{FontFamily->"Helvetica",FontSize->14},PlotStyle->{AbsolutePointSize[3],AbsoluteThickness[1.0]},Frame->True,FrameStyle->Directive[14,Black],ImageSize->300,AspectRatio->1,Axes->False];
SetOptions[LogLogPlot,BaseStyle->{FontFamily->"Helvetica",FontSize->14},PlotStyle->{AbsolutePointSize[3],AbsoluteThickness[1.0]},Frame->True,FrameStyle->Directive[14,Black],ImageSize->300,AspectRatio->1,Axes->False];
SetOptions[ListPlot,BaseStyle->{FontFamily->"Helvetica",FontSize->14},PlotStyle->{AbsolutePointSize[3],AbsoluteThickness[1.0]},Frame->True,FrameStyle->Directive[14,Black],ImageSize->300,AspectRatio->1,Axes->False];
SetOptions[ListLogPlot,BaseStyle->{FontFamily->"Helvetica",FontSize->14},PlotStyle->{AbsolutePointSize[3],AbsoluteThickness[1.0]},Frame->True,FrameStyle->Directive[14,Black],ImageSize->300,AspectRatio->1,Axes->False];
SetOptions[ListLogLinearPlot,BaseStyle->{FontFamily->"Helvetica",FontSize->14},PlotStyle->{AbsolutePointSize[3],AbsoluteThickness[1.0]},Frame->True,FrameStyle->Directive[14,Black],ImageSize->300,AspectRatio->1,Axes->False];
SetOptions[ArrayPlot,BaseStyle->{FontFamily->"Helvetica",FontSize->14},ImageSize->300,AspectRatio->1,Frame->True,FrameStyle->Directive[14,Black],Axes->False];
SetOptions[DensityPlot,BaseStyle->{FontFamily->"Helvetica",FontSize->14},ImageSize->300,AspectRatio->1,Frame->True,FrameStyle->Directive[14,Black],Axes->False];



blueredcolorPMfunc[zval_,max_] := Module[
{},
Which[0<(zval/max)<1,
Hue[1.0,Abs[(zval/max)],0.9],
(zval/max)>=1,
Hue[1.0,1,0.9],
-1<(zval/max)<0,
Hue[0.63,Abs[(zval/max)],0.9],
(zval/max)<=-1,
Hue[0.63,1,0.9]
]
]

jet[u_?NumericQ]:=Blend[{{0,RGBColor[0,0,9/16]},{1/9,Blue},{23/63,Cyan},{13/21,Yellow},{47/63,Orange},{55/63,Red},{1,RGBColor[1/2,0,0]}},u]/;0<=u<=1

colorGradToIndexed[name_,pts_]:=Table[ColorData[name][i/pts],{i,1,pts}]


EndPackage[]
