% ============================================================================
% TechPath Advisor — HTTP Server & API Route Handlers
% ============================================================================
% This module sets up the SWI-Prolog HTTP server and defines all JSON API
% endpoints for public and admin operations.
% ============================================================================

:- module(server, [
    start_server/1,
    stop_server/1
]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_cors)).

:- use_module(knowledge_base).
:- use_module(inference).
:- use_module(templates).

% Allow CORS from any origin
:- set_setting(http:cors, [*]).

% ============================================================================
% Route Registration
% ============================================================================

% --- Static files ---
% Use a custom handler to bypass sandbox restrictions on file serving.
:- http_handler(root(static), serve_static_files, [prefix]).

serve_static_files(Request) :-
    memberchk(path(Path), Request),
    atom_concat('/static/', RelPath, Path),
    atom_concat('static/', RelPath, FilePath),
    ( exists_file(FilePath)
    -> guess_content_type(FilePath, ContentType),
       http_reply_file(FilePath, [mime_type(ContentType), unsafe(true)], Request)
    ;  throw(http_reply(not_found(Path)))
    ).

guess_content_type(File, Type) :-
    file_name_extension(_, Ext, File),
    ext_to_mime(Ext, Type), !.
guess_content_type(_, 'application/octet-stream').

ext_to_mime(css,  'text/css').
ext_to_mime(js,   'application/javascript').
ext_to_mime(html, 'text/html').
ext_to_mime(json, 'application/json').
ext_to_mime(png,  'image/png').
ext_to_mime(jpg,  'image/jpeg').
ext_to_mime(jpeg, 'image/jpeg').
ext_to_mime(svg,  'image/svg+xml').
ext_to_mime(ico,  'image/x-icon').
ext_to_mime(woff, 'font/woff').
ext_to_mime(woff2,'font/woff2').
ext_to_mime(ttf,  'font/ttf').

% --- Landing page ---
:- http_handler(root(.), landing_page_handler, []).

% --- Wizard page ---
:- http_handler(root(wizard), wizard_page_handler, []).

% --- Admin page ---
:- http_handler(root(admin), admin_page_handler, []).

% --- Public API ---
:- http_handler(root('api/attributes'), api_get_attributes, []).
:- http_handler(root('api/attributes/skill'), api_get_attributes_by_type(skill), []).
:- http_handler(root('api/attributes/interest'), api_get_attributes_by_type(interest), []).
:- http_handler(root('api/attributes/personality'), api_get_attributes_by_type(personality), []).
:- http_handler(root('api/careers'), api_get_careers, []).
:- http_handler(root('api/careers/') , api_get_career, [prefix]).
:- http_handler(root('api/analyze'), api_analyze, [methods([post, options])]).

% --- Admin API ---
:- http_handler(root('api/admin/careers'), api_admin_careers, [methods([get, post, options])]).
:- http_handler(root('api/admin/careers/'), api_admin_career_by_id, [prefix, methods([delete, put, options])]).
:- http_handler(root('api/admin/attributes'), api_admin_attributes, [methods([get, post, options])]).
:- http_handler(root('api/admin/attributes/'), api_admin_attribute_by_id, [prefix, methods([delete, options])]).
:- http_handler(root('api/admin/mappings'), api_admin_mappings, [methods([post, delete, options])]).

% ============================================================================
% Server Start / Stop
% ============================================================================
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

stop_server(Port) :-
    http_stop_server(Port, []).

% ============================================================================
% Landing Page Handler
% ============================================================================
landing_page_handler(Request) :-
    memberchk(method(get), Request),
    render_landing_page(Request).

% ============================================================================
% Wizard Page Handler
% ============================================================================
wizard_page_handler(Request) :-
    memberchk(method(get), Request),
    render_wizard_page(Request).

% ============================================================================
% Admin Page Handler
% ============================================================================
admin_page_handler(Request) :-
    memberchk(method(get), Request),
    render_admin_page(Request).

% ============================================================================
% PUBLIC API HANDLERS
% ============================================================================

% --- GET /api/attributes — all attributes grouped by type ---
api_get_attributes(Request) :-
    cors_enable(Request, [methods([get])]),
    memberchk(method(get), Request),
    get_attributes_by_type(skill, Skills),
    get_attributes_by_type(interest, Interests),
    get_attributes_by_type(personality, Personalities),
    maplist(attr_to_json, Skills, SkillsJson),
    maplist(attr_to_json, Interests, InterestsJson),
    maplist(attr_to_json, Personalities, PersonalitiesJson),
    reply_json_dict(_{
        skills: SkillsJson,
        interests: InterestsJson,
        personality: PersonalitiesJson
    }).

% Handle OPTIONS preflight
api_get_attributes(Request) :-
    memberchk(method(options), Request),
    cors_enable(Request, [methods([get])]),
    format('Content-type: text/plain~n~n').

% --- GET /api/attributes/:type ---
api_get_attributes_by_type(Type, Request) :-
    cors_enable(Request, [methods([get])]),
    memberchk(method(get), Request),
    get_attributes_by_type(Type, Attrs),
    maplist(attr_to_json, Attrs, AttrsJson),
    reply_json_dict(_{attributes: AttrsJson}).

% --- GET /api/careers — all careers ---
api_get_careers(Request) :-
    cors_enable(Request, [methods([get])]),
    memberchk(method(get), Request),
    findall(
        _{id: Id, title: Title, description: Desc},
        career(Id, Title, Desc, _),
        Careers
    ),
    reply_json_dict(_{careers: Careers}).

api_get_careers(Request) :-
    memberchk(method(options), Request),
    cors_enable(Request, [methods([get])]),
    format('Content-type: text/plain~n~n').

% --- GET /api/careers/:id ---
api_get_career(Request) :-
    cors_enable(Request, [methods([get])]),
    memberchk(method(get), Request),
    memberchk(path(Path), Request),
    atom_string(Path, PathStr),
    split_string(PathStr, "/", "", Parts),
    last(Parts, IdStr),
    number_string(Id, IdStr),
    (   career(Id, Title, Desc, Meta)
    ->  metadata_to_json(Meta, MetaJson),
        get_career_attributes(Id, Attrs),
        maplist(career_attr_to_json, Attrs, AttrsJson),
        reply_json_dict(_{
            id: Id,
            title: Title,
            description: Desc,
            metadata: MetaJson,
            attributes: AttrsJson
        })
    ;   reply_json_dict(_{error: 'Career not found'}, [status(404)])
    ).

% --- POST /api/analyze ---
api_analyze(Request) :-
    cors_enable(Request, [methods([post])]),
    memberchk(method(post), Request),
    http_read_json_dict(Request, Body, []),
    (   get_dict(attributes, Body, AttrIds)
    ->  (   is_list(AttrIds), AttrIds \= []
        ->  maplist(ensure_integer, AttrIds, IntIds),
            analyze_profile(IntIds, 10, Results),
            maplist(result_to_json, Results, ResultsJson),
            reply_json_dict(_{results: ResultsJson})
        ;   reply_json_dict(_{error: 'attributes must be a non-empty array of IDs'}, [status(400)])
        )
    ;   reply_json_dict(_{error: 'Missing required field: attributes'}, [status(400)])
    ).

api_analyze(Request) :-
    memberchk(method(options), Request),
    cors_enable(Request, [methods([post])]),
    format('Content-type: text/plain~n~n').

% ============================================================================
% ADMIN API HANDLERS
% ============================================================================

% --- POST /api/admin/careers — Create career ---
% --- GET /api/admin/careers — List all with mappings ---
api_admin_careers(Request) :-
    cors_enable(Request, [methods([get, post])]),
    memberchk(method(Method), Request),
    api_admin_careers_dispatch(Method, Request).

api_admin_careers(Request) :-
    memberchk(method(options), Request),
    cors_enable(Request, [methods([get, post])]),
    format('Content-type: text/plain~n~n').

api_admin_careers_dispatch(get, _Request) :-
    findall(
        Career,
        (
            career(Id, Title, Desc, Meta),
            metadata_to_json(Meta, MetaJson),
            get_career_attributes(Id, Attrs),
            maplist(career_attr_to_json, Attrs, AttrsJson),
            Career = _{id: Id, title: Title, description: Desc, metadata: MetaJson, attributes: AttrsJson}
        ),
        Careers
    ),
    reply_json_dict(_{careers: Careers}).

api_admin_careers_dispatch(post, Request) :-
    http_read_json_dict(Request, Body, []),
    get_dict(title, Body, Title),
    get_dict(description, Body, Desc),
    (get_dict(salary, Body, Salary) -> true ; Salary = 'N/A'),
    (get_dict(demand, Body, Demand) -> true ; Demand = medium),
    (get_dict(education, Body, Edu) -> true ; Edu = 'Not specified'),
    next_career_id(NewId),
    Meta = [salary-Salary, demand-Demand, education-Edu],
    assert(career(NewId, Title, Desc, Meta)),
    reply_json_dict(_{success: true, id: NewId, message: 'Career created'}).

% --- DELETE /api/admin/careers/:id ---
api_admin_career_by_id(Request) :-
    cors_enable(Request, [methods([delete, put])]),
    memberchk(method(Method), Request),
    memberchk(path(Path), Request),
    atom_string(Path, PathStr),
    split_string(PathStr, "/", "", Parts),
    last(Parts, IdStr),
    number_string(Id, IdStr),
    api_admin_career_by_id_dispatch(Method, Id).

api_admin_career_by_id(Request) :-
    memberchk(method(options), Request),
    cors_enable(Request, [methods([delete, put])]),
    format('Content-type: text/plain~n~n').

api_admin_career_by_id_dispatch(delete, Id) :-
    (   career(Id, _, _, _)
    ->  retractall(career(Id, _, _, _)),
        retractall(career_attribute(Id, _, _)),
        reply_json_dict(_{success: true, message: 'Career deleted'})
    ;   reply_json_dict(_{error: 'Career not found'}, [status(404)])
    ).

api_admin_career_by_id_dispatch(put, _Id) :-
    reply_json_dict(_{error: 'PUT not yet implemented - use Sprint 3'}, [status(501)]).

% --- POST /api/admin/attributes — Create attribute ---
% --- GET (falls through to api_get_attributes) ---
api_admin_attributes(Request) :-
    cors_enable(Request, [methods([get, post])]),
    memberchk(method(Method), Request),
    api_admin_attributes_dispatch(Method, Request).

api_admin_attributes(Request) :-
    memberchk(method(options), Request),
    cors_enable(Request, [methods([get, post])]),
    format('Content-type: text/plain~n~n').

api_admin_attributes_dispatch(post, Request) :-
    http_read_json_dict(Request, Body, []),
    get_dict(name, Body, Name),
    get_dict(type, Body, TypeStr),
    atom_string(Type, TypeStr),
    (get_dict(description, Body, Desc) -> true ; Desc = ''),
    next_attribute_id(NewId),
    assert(attribute(NewId, Type, Name, Desc)),
    reply_json_dict(_{success: true, id: NewId, message: 'Attribute created'}).

api_admin_attributes_dispatch(get, _Request) :-
    findall(
        _{id: Id, name: Name, type: Type, description: Desc},
        attribute(Id, Type, Name, Desc),
        Attrs
    ),
    reply_json_dict(_{attributes: Attrs}).

% --- DELETE /api/admin/attributes/:id ---
api_admin_attribute_by_id(Request) :-
    cors_enable(Request, [methods([delete])]),
    memberchk(method(Method), Request),
    memberchk(path(Path), Request),
    atom_string(Path, PathStr),
    split_string(PathStr, "/", "", Parts),
    last(Parts, IdStr),
    number_string(Id, IdStr),
    api_admin_attribute_by_id_dispatch(Method, Id).

api_admin_attribute_by_id(Request) :-
    memberchk(method(options), Request),
    cors_enable(Request, [methods([delete])]),
    format('Content-type: text/plain~n~n').

api_admin_attribute_by_id_dispatch(delete, Id) :-
    (   attribute(Id, _, _, _)
    ->  retractall(attribute(Id, _, _, _)),
        retractall(career_attribute(_, Id, _)),
        reply_json_dict(_{success: true, message: 'Attribute deleted'})
    ;   reply_json_dict(_{error: 'Attribute not found'}, [status(404)])
    ).

% --- POST/DELETE /api/admin/mappings ---
api_admin_mappings(Request) :-
    memberchk(method(post), Request),
    cors_enable(Request, [methods([post, delete])]),
    api_admin_mappings_dispatch(post, Request).

api_admin_mappings(Request) :-
    memberchk(method(delete), Request),
    cors_enable(Request, [methods([post, delete])]),
    api_admin_mappings_dispatch(delete, Request).

api_admin_mappings(Request) :-
    memberchk(method(options), Request),
    cors_enable(Request, [methods([post, delete])]),
    format('Content-type: text/plain~n~n').

api_admin_mappings_dispatch(post, Request) :-
    http_read_json_dict(Request, Body, []),
    get_dict(career_id, Body, CIdRaw),
    get_dict(attribute_id, Body, AIdRaw),
    get_dict(weight, Body, WeightRaw),
    ensure_integer(CIdRaw, CId),
    ensure_integer(AIdRaw, AId),
    ensure_integer(WeightRaw, Weight),
    (   career(CId, _, _, _)
    ->  (   attribute(AId, _, _, _)
        ->  (   career_attribute(CId, AId, _)
            ->  reply_json_dict(_{error: 'Mapping already exists'}, [status(409)])
            ;   assert(career_attribute(CId, AId, Weight)),
                reply_json_dict(_{success: true, message: 'Mapping created'})
            )
        ;   reply_json_dict(_{error: 'Attribute not found'}, [status(404)])
        )
    ;   reply_json_dict(_{error: 'Career not found'}, [status(404)])
    ).

api_admin_mappings_dispatch(delete, Request) :-
    http_parameters(Request, [
        career_id(CId, [integer]),
        attribute_id(AId, [integer])
    ]),
    (   career_attribute(CId, AId, _)
    ->  retractall(career_attribute(CId, AId, _)),
        reply_json_dict(_{success: true, message: 'Mapping deleted'})
    ;   reply_json_dict(_{error: 'Mapping not found'}, [status(404)])
    ).

% ============================================================================
% JSON Conversion Helpers
% ============================================================================

% Convert attribute dict to JSON-friendly dict
attr_to_json(Attr, Json) :-
    get_dict(id, Attr, Id),
    get_dict(name, Attr, Name),
    get_dict(type, Attr, Type),
    get_dict(description, Attr, Desc),
    Json = _{id: Id, name: Name, type: Type, description: Desc}.

% Convert career attribute dict to JSON-friendly dict
career_attr_to_json(Attr, Json) :-
    get_dict(id, Attr, Id),
    get_dict(name, Attr, Name),
    get_dict(type, Attr, Type),
    get_dict(weight, Attr, Weight),
    Json = _{id: Id, name: Name, type: Type, weight: Weight}.

% Convert metadata key-value list to JSON dict
metadata_to_json([], _{}).
metadata_to_json(Meta, Json) :-
    meta_pairs_to_dict(Meta, Json).

meta_pairs_to_dict(Pairs, Dict) :-
    maplist(meta_pair_to_kv, Pairs, KVPairs),
    dict_pairs(Dict, _, KVPairs).

meta_pair_to_kv(Key-Value, Key-Value).

% Convert inference result to JSON
result_to_json(Result, Json) :-
    get_dict(metadata, Result, Meta),
    get_dict(matched_attributes, Result, MatchedAttrs),
    get_dict(missing_attributes, Result, MissingAttrs),
    metadata_to_json(Meta, MetaJson),
    maplist(career_attr_to_json, MatchedAttrs, MatchedJson),
    maplist(career_attr_to_json, MissingAttrs, MissingJson),
    get_dict(career_id, Result, CId),
    get_dict(title, Result, Title),
    get_dict(description, Result, Desc),
    get_dict(confidence, Result, Conf),
    get_dict(explanation, Result, Expl),
    Json = _{
        career_id: CId,
        title: Title,
        description: Desc,
        metadata: MetaJson,
        confidence: Conf,
        matched_attributes: MatchedJson,
        missing_attributes: MissingJson,
        explanation: Expl
    }.

% Ensure a value is an integer (handles JSON numbers that may be floats)
ensure_integer(X, X) :- integer(X), !.
ensure_integer(X, I) :- float(X), !, I is round(X).
ensure_integer(X, I) :- atom(X), !, atom_number(X, I).
ensure_integer(X, I) :- string(X), !, number_string(I, X).
