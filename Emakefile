doc() ->
    doc("doc").
doc(DocPath) ->
    {ok, Makefiles} = file:consult("Emakefile"),
    Files = lists:map(fun({F, _Opts}) -> F end, Makefiles),
    edoc:files(Files, [{dir,DocPath}]).