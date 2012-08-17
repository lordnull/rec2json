-module(rec2json_SUITE).

-compile(export_all).
-include_lib("triq/include/triq.hrl").
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap, {seconds, 60}}].

init_per_suite(Conf) -> [{num_tests, 1000} | Conf].

end_per_suite(_Conf) -> ok.

init_per_group(_GroupName, Conf) -> Conf.

end_per_group(_GroupName, _Conf) -> ok.

init_per_testcase(_Testcase, Conf) -> Conf.

end_per_testcase(_Testcase, _Conf) -> ok.

groups() -> [].

all() -> [test_hrl_files].

test_hrl_files() -> [].

test_hrl_files(Conf) ->
    DataDir = ?config(data_dir, Conf),
    NumTests = ?config(num_tests, Conf),
    test_server:format("The config:  ~p", [Conf]),
    HrlFiles = filelib:wildcard(filename:join([DataDir, "hrl", "*.hrl"])),
    test_hrl_files(HrlFiles, Conf).

test_hrl_files(HrlFiles, Conf) ->
    test_hrl_files(HrlFiles, Conf, true).

test_hrl_files([], _Conf, Acc) ->
    Acc;

test_hrl_files([Filename | Tail], Conf, Acc) ->
    DataDir = ?config(data_dir, Conf),
    NumTests = ?config(num_tests, Conf),
    Path = filename:absname(Filename),
    RecTag = filename:basename(Path, ".hrl"),
    test_server:format("~n===== Test case ~p : File ~p =====~n", [self(), RecTag]),
    rec2json_compile:scan_file(Path, [{imports_dir, [filename:join([DataDir, "hrl"])]}]),
    Test = list_to_atom("rec2json_hrls_" ++ RecTag),
    OnOutput = fun
        (".", _) -> ok;
        (S,F) -> test_server:format(S,F)
    end,
    Result = triq:check(rec2json_triq:Test(), NumTests),
    case Result of
        true ->
            test_server:format("Test ~p:  ok~n~n~n", [RecTag]),
            Acc andalso true;
        F ->
            test_server:format("Test ~p:  Failed with ~p~n~n~n", [RecTag, Result]),
            false
    end.

