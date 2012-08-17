-module(rec2json).

-export([parse_file/1,parse_file/2]).

parse_file(FileName) ->
    parse_file(FileName, []).

parse_file(FileName, Options) ->
    {error, nyi}.

parse(String) ->
    parse(String, []).

parse(String, Opts) ->
    {error, nyi}.
