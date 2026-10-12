:- module(support, [with_temp_dir/1, read_result/1, write_file/2, run_swipl/4]).
:- use_module(library(filesex), [delete_directory_and_contents/1]).
:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(library(process)).
:- use_module(library(readutil), [read_stream_to_codes/2]).

:- dynamic init_file/1.

:- prolog_load_context(directory, Dir),
   absolute_file_name('../../init.pl', Path, [relative_to(Dir)]),
   assertz(init_file(Path)).

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

write_file(File, Text) :-
    setup_call_cleanup(
        open(File, write, S),
        write(S, Text),
        close(S)).

% Runs swipl with init.pl and returns its exit status and what it printed on
% stderr.
% Flags from swipl() in prologGeneral.py, keep in sync.
run_swipl(Goal, Script, Status, Stderr) :-
    current_prolog_flag(executable, Swipl),
    init_file(Init),
    setup_call_cleanup(
        process_create(Swipl,
                       [ '-t', Goal, '--quiet=yes', '--tty=yes',
                         '--signals=no', '-f', Init, Script
                       ],
                       [stdout(null), stderr(pipe(Err)), process(Pid)]),
        ( read_stream_to_codes(Err, Codes),
          string_codes(Stderr, Codes)
        ),
        close(Err)),
    process_wait(Pid, exit(Status)).
