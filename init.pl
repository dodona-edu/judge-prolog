% Loaded with -f by swipl() in prologGeneral.py, before the script file.

% SWI-Prolog 10 prefixes messages outside the interactive toplevel with "[Thread main] ".
:- set_prolog_flag(message_context, []).

% Exercises consult files in the working directory, which SWI-Prolog 10 still finds but warns about.
:- multifile user:message_hook/3.
user:message_hook(deprecated(source_search_working_directory(_, _)), warning, _).
