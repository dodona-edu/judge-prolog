:- use_module(library(plunit)).
:- use_module(support).

:- begin_tests(init).

test(file_in_the_working_directory_is_consulted_silently) :-
    with_temp_dir(( write_file('database.pl', "fact(1).\n"),
                    make_directory(resources),
                    write_file('resources/test.pl', ":- consult(database).\n"),
                    run_swipl('fact(1)','resources/test.pl', Status, Stderr)
                  )),
    Status == 0,
    Stderr == "".

test(messages_have_no_thread_context) :-
    with_temp_dir(( write_file('test.pl', "p :- print_message(error, format(\"boom\", [])).\n"),
                    run_swipl(p,'test.pl', _, Stderr)
                  )),
    Stderr == "ERROR: boom\n".

:- end_tests(init).
