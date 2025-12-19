%% =============================================================================
%% Gold Standard Loader
%% =============================================================================
%% Loads all disorder definitions from the gold_standard directory.
%%
%% Usage:
%%   ?- [schema, 'gold_standard/loader'].
%%   ?- disorder(X, Name, Category).
%%
%% To add a new disorder:
%%   1. Create a new file: gold_standard/<disorder_id>.pl
%%   2. Add the filename to the disorders list below
%% =============================================================================

%% Note: schema.pl must be loaded before this file
%% We don't use use_module here to avoid predicate ownership conflicts

%% Get the directory where this loader file is located
:- prolog_load_context(directory, Dir),
   asserta(gold_standard_dir(Dir)).

%% List of disorder files to load (add new disorders here)
%% These are just the base names - the loader will resolve full paths
gold_standard_disorder_files([
    'mdd',
    'gad',
    'adhd',
    'ptsd'
    % 'asd' - not yet created
]).

%% Load a single disorder file using absolute path
load_disorder(BaseName) :-
    gold_standard_dir(Dir),
    atomic_list_concat([Dir, '/', BaseName, '.pl'], FullPath),
    (   load_files(FullPath, [if(changed)])
    ->  format('Loaded: ~w~n', [BaseName])
    ;   format('Warning: Failed to load ~w~n', [FullPath])
    ).

%% Load all disorder files on startup
:- gold_standard_disorder_files(Files),
   maplist(load_disorder, Files).

%% Utility: List all loaded disorders
list_gold_standard_disorders :-
    writeln('Gold Standard Disorders:'),
    forall(disorder(ID, Name, Category),
           format('  - ~w: ~w (~w)~n', [ID, Name, Category])).
