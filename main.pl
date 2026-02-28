% ============================================================================
% TechPath Advisor — Main Entry Point
% ============================================================================
% Loads all modules, seeds the knowledge base, and starts the HTTP server.
%
% Usage:  swipl main.pl
% ============================================================================

:- use_module(knowledge_base).
:- use_module(inference).
:- use_module(server).
:- use_module(templates).

% --- Server port ---
server_port(5000).

% --- Initialization: runs automatically when the file is loaded ---
:- initialization(main, main).

main :-
    format('~n========================================~n'),
    format('   TechPath Advisor — Expert System~n'),
    format('========================================~n~n'),

    % Seed the knowledge base
    format('[*] Seeding knowledge base...~n'),
    seed_knowledge_base,

    % Print stats
    aggregate_all(count, career(_, _, _, _), CareerCount),
    aggregate_all(count, attribute(_, _, _, _), AttrCount),
    aggregate_all(count, career_attribute(_, _, _), MappingCount),
    format('[+] Loaded ~w careers, ~w attributes, ~w mappings~n',
           [CareerCount, AttrCount, MappingCount]),

    % Start the HTTP server
    server_port(Port),
    format('[*] Starting HTTP server on port ~w...~n', [Port]),
    start_server(Port),
    format('[+] TechPath Advisor running at http://localhost:~w~n', [Port]),
    format('[+] Press Ctrl+C to stop~n~n'),

    % Keep the process alive
    thread_get_message(_).
