:-use_module((themes/dork)).

:-prolog_load_context(directory, Dir)
,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(src, project_root(src)).
user:file_search_path(lib, project_root(lib)).
user:file_search_path(data, project_root(data)).
user:file_search_path(output, project_root(input)).
user:file_search_path(output, project_root(output)).

:-doc_browser.

:-use_module(configuration).
:-use_module(src(nni)).

edit_files:-
	edit(project_root(load_project))
	,edit(project_root(configuration))
	,edit(src(nni))
	,edit(data(scorpion))
	,edit(data(character))
	,edit(data(class))
	,edit(src(chargen))
	,edit(src(display))
%	,edit(src(charsheet))
	,edit(src(layout))
	.
:-edit_files.

%:-load_test_files([]).
%:-run_tests.

% Large data may require a larger stack.
%:-set_prolog_flag(stack_limit, 2_147_483_648).
%:- set_prolog_flag(stack_limit, 4_294_967_296).
%:-set_prolog_flag(stack_limit, 8_589_934_592).
%:-set_prolog_flag(stack_limit, 17_179_869_184).
:-current_prolog_flag(stack_limit, X)
 ,format('Global stack limit ~D~n',[X]).
