(* ::Package:: *)

BeginPackage["Niceplots`"]
colorGradToIndexed::usage = "Convert gradient from COLORDATA to indexed color list for plotting purposes";
blueredcolorPMfunc::usage = "Color function Blue > White > Red corresponding to positive / negative";
jet::usage = "Matlab style color function";

SetOptions[Plot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[LogPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[LogLogPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[LogLinearPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[ListPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[ListLogPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[ListLogLinearPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},PlotStyle->{{AbsolutePointSize[7],AbsoluteThickness[1.5]}},FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],Frame->True,ImageSize->360,AspectRatio->1,Axes->False];
SetOptions[ArrayPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},Frame-> True,FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],ImageSize->360,AspectRatio->1,Frame->True,Axes->False];
SetOptions[DensityPlot,BaseStyle->{FontFamily->"Arial",FontSize->12},Frame-> True,FrameStyle-> Directive[14,Black,AbsoluteThickness[1.5]],ImageSize->360,AspectRatio->1,Frame->True,Axes->False];


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
cray[name_]:=ColorData["Crayola"][name]

Options[plotGrid]={ImagePadding->40};
plotGrid[l_List,w_,h_,opts:OptionsPattern[]]:=Module[{nx,ny,sidePadding=OptionValue[plotGrid,ImagePadding],topPadding=0,widths,heights,dimensions,positions,frameOptions=FilterRules[{opts},FilterRules[Options[Graphics],Except[{ImagePadding,Frame,FrameTicks}]]]},{ny,nx}=Dimensions[l];
widths=(w-2 sidePadding)/nx Table[1,{nx}];
widths[[1]]=widths[[1]]+sidePadding;
widths[[-1]]=widths[[-1]]+sidePadding;
heights=(h-2 sidePadding)/ny Table[1,{ny}];
heights[[1]]=heights[[1]]+sidePadding;
heights[[-1]]=heights[[-1]]+sidePadding;
positions=Transpose@Partition[Tuples[Prepend[Accumulate[Most[#]],0]&/@{widths,heights}],ny];
Graphics[Table[Inset[Show[l[[ny-j+1,i]],ImagePadding->{{If[i==1,sidePadding,0],If[i==nx,sidePadding,0]},{If[j==1,sidePadding,0],If[j==ny,sidePadding,topPadding]}},AspectRatio->Full],positions[[j,i]],{Left,Bottom},{widths[[i]],heights[[j]]}],{i,1,nx},{j,1,ny}],PlotRange->{{0,w},{0,h}},ImageSize->{w,h},Evaluate@Apply[Sequence,frameOptions]]]

ClearAll[rasterizeBackground]
Options[rasterizeBackground]={"TransparentBackground"->False,Antialiasing->False};
rasterizeBackground[g_Graphics,rs_Integer:3000,OptionsPattern[]]:=Module[{raster,plotrange,rect},{raster,plotrange}=Reap[First@Rasterize[Show[g,Epilog->{},Prolog->{},PlotRangePadding->0,ImagePadding->0,ImageMargins->0,PlotLabel->None,FrameTicks->None,Frame->None,Axes->None,Ticks->None,PlotRangeClipping->False,Antialiasing->OptionValue[Antialiasing],GridLines->{Function[Sow[{##},"X"];None],Function[Sow[{##},"Y"];None]}],"Graphics",ImageSize->rs,Background->Replace[OptionValue["TransparentBackground"],{True->None,False->Automatic}]],_,#1->#2[[1]]&];
rect=Transpose[{"X","Y"}/. plotrange];
Graphics[raster/. Raster[data_,_,rest__]:>Raster[data,rect,rest],Options[g]]]

rasterizeBackground[g_,rs_Integer:3000,OptionsPattern[]]:=g/. gr_Graphics:>rasterizeBackground[gr,rs]

EndPackage[]

