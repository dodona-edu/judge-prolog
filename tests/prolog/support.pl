:- module(support, [with_temp_dir/1, read_result/1]).
:- use_module(library(filesex), [delete_directory_and_contents/1]).
:- use_module(library(http/json), [atom_json_dict/3]).

:- meta_predicate with_temp_dir(0).
with_temp_dir(Goal) :-
    tmp_file(judge_test, Dir),
    make_directory(Dir),
    setup_call_cleanup(
        working_directory(Old, Dir),
        once(Goal),
        ( working_directory(_, Old),
          delete_directory_and_contents(Dir)
        )).

read_result(Json) :-
    read_file_to_string('result.json', Text, []),
    atom_json_dict(Text, Json, []).
