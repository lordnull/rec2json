%% Copyright 2012 Micah Warren
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%   http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

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
