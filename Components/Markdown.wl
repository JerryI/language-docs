BeginPackage["Markdown`Generator`", {
    "Parallel`Developer`",
    "JerryI`Misc`Events`Promise`",
    "JerryI`Misc`Events`",
    "JerryI`Misc`Async`"
}]

Begin["`Internal`"]

Begin["MDGeneratorStuff`"]

baseUrl = "https://reference.wolfram.com/language/ref/"; 

$TimeoutExamples = 1;

getTitle[xml_] := 
StringJoin["# " , ImportString[
	ExportString[
		FirstCase[xml, 
			XMLElement["h1", {"class" -> "main-title"}, _], 
			"", Infinity
		], 
		"HTMLFragment"
	], 
	"HTML"
]]


getDescription[xml_] := StringJoin["> ", ImportString[
	ExportString[FirstCase[xml, XMLElement["p", {"class" -> "code-description"}, _], "", Infinity], "HTMLFragment"], 
	"HTML"
]]; 

splitExpression[astr_] := With[{str = astr},
  Select[Select[(StringTake[str, Partition[Join[{1}, #, {StringLength[str]}], 2]] &@
   Flatten[{#1 - 1, #2 + 1} & @@@ 
     Sort@
      Cases[
       CodeParser`CodeConcreteParse[str, 
         CodeParser`SourceConvention -> "SourceCharacterIndex"][[2]], 
       LeafNode[Token`Newline, _, a_] :> Lookup[a, Source, Nothing]]]), StringQ], (StringLength[#]>0) &]
];

(*`*)

getDetails[xml_] := 
StringRiffle[#, "\n"]& @ 
Map[StringJoin["- ", ImportString[ExportString[#, "HTMLFragment"], "HTML"]]&] @ 
Cases[XMLElement["li", __]] @ 
Last @ 
FirstCase[xml, XMLElement["ul", {"class" -> "functionList"}, _], "", Infinity]; 


$PreviousOut;

evaluateResult[data_String] := With[{
  splitted = StringTrim /@ splitExpression[data]
},
 
  TimeConstrained[ 
  	With[{results = Map[Function[exp, With[{

  	},
  	  Print[exp];
  	  $PreviousOut[] = If[StringTake[exp, -1] === ";",
  	     ToExpression[StringDrop[exp, -1], InputForm, Hold] /. {Out -> $PreviousOut} // ReleaseHold;
  	    Missing[]
  	  ,
        ToExpression[exp, InputForm, Hold] /. {Out -> $PreviousOut} // ReleaseHold
  	  ]
	
  	] ], splitted] // DeleteMissing},

  	  results
	
  	], 

   $TimeoutExamples] // evaluateResultPost
  
  
]

evaluateResult[data_String] := "* Result is suppressed due to safety reasons * " // evaluateResultPost

evaluateResultPost[any_String] := StringJoin["```mathematica title=\"Result\"\n", any, "\n```"];
evaluateResultPost[$Failed] := "(* failed! *)" //evaluateResultPost;
evaluateResultPost[$Aborted] := "(* timeout! *)" //evaluateResultPost;



evaluateResultPost[g_Graphics]   := WLJS[g]
evaluateResultPost[g_Graphics3D] := WLJS[g]
evaluateResultPost[g_Image]      := WLJS[g]

evaluateResultPost[any_] := ToString[any, InputForm ] //evaluateResultPost;
evaluateResultPost[any_List] := StringRiffle[evaluateResultPost /@ any, "\n"] 

getExamples[xml_] := 
Module[{section, blocks}, 
	section = FirstCase[xml, XMLElement["section", {"id" -> "Examples"}, data_] :> data, "", Infinity]; 
	
	blocks = Cases[section, 
		XMLElement["h2", _, _] | 
		XMLElement["div", {___, "class" -> "clipboard-input", ___}, _] |  
		XMLElement["p", __], 
		
		Infinity
	]; 
	
	StringRiffle[blocks /. {
		XMLElement["h2", _, {h2_String, ___}] :> StringJoin["### ", h2], 
		XMLElement["div", at: {___, "class" -> "clipboard-input", ___}, _] :> With[{
          data = Import[StringJoin[baseUrl, Association[at]["data-code"]]]
        },
			StringJoin["```mathematica\n", data, "\n```", "\n ", evaluateResult[data], "\n"]
         ], 
		p: XMLElement["p", ___] :> ImportString[ExportString[p, "HTMLFragment"], "HTML"]
	}, "\n\n"]
]

generateFrontMatter[url_String] := With[{},
	StringJoin["---\ncategoryName: Core\norigin: ", url, "\n---"]
]

MDGeneratorStuff`generateAll[name_String] := 
Module[{ 
	html, xml
}, 
	html = URLRead[StringJoin[baseUrl, name]]["Body"]; 
	xml = ImportString[html, "XMLObject"]; 
	
	StringRiffle[{
        StringJoin["# ", name],
		getDescription[xml], 
		getDetails[xml], 
		"## Examples", 
		getExamples[xml]
	}, "\n\n"]
]

End[]

LaunchKernels[]; 

que = {};

remotePromise[p_, res_] := (
    que = que /. {p -> Nothing};
    Print["Resolve remote..."];
    EventFire[p, Resolve, res];
);
SetSharedFunction[remotePromise];

Echo["Total Kernels"];
Echo[$KernelCount]; 

DistributeDefinitions["MDGeneratorStuff`"];



checkQ := (
    If[QueueRun[] || Length[que] > 0, SetTimeout[checkQ, 100], Echo["All tasks has been finished"]; ];
);

refToMd[sym_String] := With[{p = Promise[]},
    With[{uid = p // First}, 
        AppendTo[que, uid];
        ParallelSubmit[remotePromise[uid, MDGeneratorStuff`generateAll[sym] ] ] 
    ];
    (*QueueRun[] // Echo;*)
    checkQ;
    p
]

End[]
EndPackage[]

Markdown`Generator`Internal`refToMd