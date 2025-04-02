(* ::Package:: *)

BeginPackage["Niceplots`"]
colorGradToIndexed::usage = "Convert gradient from COLORDATA to indexed color list for plotting purposes";
blueredcolorPMfunc::usage = "Color function Blue > White > Red corresponding to positive / negative";
jet::usage = "Matlab style color function";
(*
SetOptions[Plot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[LogPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[LogLogPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[LogLinearPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[ListPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[ListLogPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[ListLogLinearPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[ArrayPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},Frame-> True,FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],ImageSize->360,AspectRatio->1,Frame->True,Axes->False];
SetOptions[DensityPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},Frame-> True,FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],ImageSize->360,AspectRatio->1,Frame->True,Axes->False];
*)

blueredcolorPMfunc[zval_,max_] := Module[{},
Which[0<(zval/max)<1,
Hue[1.0,Abs[(zval/max)],0.9],
(zval/max)>=1,
Hue[1.0,1,0.9],
-1<(zval/max)<0,
Hue[0.63,Abs[(zval/max)],0.9],
(zval/max)<=-1,
Hue[0.63,1,0.9]]]

jet[u_?NumericQ]:=Blend[{{0,RGBColor[0,0,9/16]},{1/9,Blue},{23/63,Cyan},{13/21,Yellow},{47/63,Orange},{55/63,Red},{1,RGBColor[1/2,0,0]}},u]/;0<=u<=1

colorGradToIndexed[name_,pts_]:=Table[ColorData[name][i/pts],{i,1,pts}]/;1<=pts

colorGradient[colorList_,pts_?NumericQ]:=Table[Blend[colorList,u/(pts-1)],{u,0,pts-1}]/;1<=pts

Crayola[name_]:=ColorData["Crayola"][name]

EndPackage[]



