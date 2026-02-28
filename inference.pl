% ============================================================================
% TechPath Advisor — Inference Engine
% ============================================================================
% This module implements the core reasoning logic: profile analysis,
% confidence scoring, and explanation generation.
% ============================================================================

:- module(inference, [
    analyze_profile/3,
    calculate_confidence/5,
    generate_explanation/4,
    get_attributes_by_type/2,
    get_career_attributes/2
]).

:- use_module(knowledge_base).

% ============================================================================
% analyze_profile(+UserAttrIds, +TopN, -Results)
%
% Main entry point. Given a list of user-selected attribute IDs and a
% desired number of results N, returns the top N career matches sorted
% by confidence score (descending).
%
% Each result is a dict with keys:
%   career_id, title, description, metadata, confidence,
%   matched_attributes, missing_attributes, explanation
% ============================================================================
analyze_profile(UserAttrIds, TopN, Results) :-
    findall(
        Confidence-Result,
        (
            career(CId, Title, Desc, Meta),
            calculate_confidence(CId, UserAttrIds, Confidence, Matched, Missing),
            Confidence > 0,
            generate_explanation(Title, Matched, Confidence, Explanation),
            Result = result{
                career_id: CId,
                title: Title,
                description: Desc,
                metadata: Meta,
                confidence: Confidence,
                matched_attributes: Matched,
                missing_attributes: Missing,
                explanation: Explanation
            }
        ),
        Pairs
    ),
    sort(1, @>=, Pairs, SortedPairs),
    pairs_values(SortedPairs, AllResults),
    take_top_n(AllResults, TopN, Results).

% ============================================================================
% calculate_confidence(+CareerID, +UserAttrs, -Confidence, -Matched, -Missing)
%
% For a given career, computes:
%   - TotalPossibleWeight: sum of all mapped attribute weights
%   - MatchedWeight: sum of weights for attributes the user possesses
%   - Confidence: (MatchedWeight / TotalPossibleWeight) * 100, rounded to 1 decimal
%   - Matched: list of matched attribute info
%   - Missing: list of missing attribute info
% ============================================================================
calculate_confidence(CareerID, UserAttrs, Confidence, Matched, Missing) :-
    findall(
        Weight,
        career_attribute(CareerID, _, Weight),
        AllWeights
    ),
    AllWeights \= [],
    sum_list(AllWeights, TotalPossibleWeight),
    TotalPossibleWeight > 0,
    findall(
        attr{id: AttrID, name: Name, type: Type, weight: W},
        (
            career_attribute(CareerID, AttrID, W),
            member(AttrID, UserAttrs),
            attribute(AttrID, Type, Name, _)
        ),
        Matched
    ),
    findall(
        attr{id: AttrID, name: Name, type: Type, weight: W},
        (
            career_attribute(CareerID, AttrID, W),
            \+ member(AttrID, UserAttrs),
            attribute(AttrID, Type, Name, _)
        ),
        Missing
    ),
    findall(W, (member(A, Matched), get_dict(weight, A, W)), MatchedWeights),
    sum_list(MatchedWeights, MatchedWeight),
    RawConfidence is (MatchedWeight / TotalPossibleWeight) * 100,
    Confidence is round(RawConfidence * 10) / 10.

% ============================================================================
% generate_explanation(+Title, +MatchedAttrs, +Confidence, -Explanation)
%
% Builds a human-readable explanation string from the matched attributes,
% grouped by type (skills, interests, personality).
% ============================================================================
generate_explanation(Title, MatchedAttrs, Confidence, Explanation) :-
    filter_by_type(MatchedAttrs, skill, Skills),
    filter_by_type(MatchedAttrs, interest, Interests),
    filter_by_type(MatchedAttrs, personality, Personalities),
    extract_names(Skills, SkillNames),
    extract_names(Interests, InterestNames),
    extract_names(Personalities, PersonalityNames),
    build_explanation_parts(SkillNames, InterestNames, PersonalityNames, Parts),
    (   Parts = []
    ->  format(atom(Explanation),
            'Partial match for ~w (confidence: ~1f%).',
            [Title, Confidence])
    ;   atomic_list_concat(Parts, ', ', PartsStr),
        format(atom(Explanation),
            'Recommended because ~w align with the core requirements of a ~w (confidence: ~1f%).',
            [PartsStr, Title, Confidence])
    ).

% --- Filter attributes by type ---
filter_by_type([], _, []).
filter_by_type([A|Rest], Type, [A|Filtered]) :-
    get_dict(type, A, Type), !,
    filter_by_type(Rest, Type, Filtered).
filter_by_type([_|Rest], Type, Filtered) :-
    filter_by_type(Rest, Type, Filtered).

% --- Extract names from attribute dicts ---
extract_names([], []).
extract_names([A|Rest], [Name|Names]) :-
    get_dict(name, A, Name),
    extract_names(Rest, Names).

% --- Build explanation parts for each attribute type ---
build_explanation_parts(Skills, Interests, Personalities, Parts) :-
    (   Skills \= []
    ->  join_names(Skills, SkillStr),
        format(atom(SP), 'your skills in ~w', [SkillStr]),
        SP1 = [SP]
    ;   SP1 = []
    ),
    (   Interests \= []
    ->  join_names(Interests, IntStr),
        format(atom(IP), 'your interest in ~w', [IntStr]),
        IP1 = [IP]
    ;   IP1 = []
    ),
    (   Personalities \= []
    ->  join_names(Personalities, PerStr),
        format(atom(PP), 'your ~w personality', [PerStr]),
        PP1 = [PP]
    ;   PP1 = []
    ),
    append([SP1, IP1, PP1], Parts).

% --- Join a list of names with "and" for the last element ---
join_names([], '').
join_names([X], X).
join_names([X, Y], Result) :-
    format(atom(Result), '~w and ~w', [X, Y]).
join_names([X | Rest], Result) :-
    Rest = [_ | _],
    Rest \= [_],
    join_names(Rest, RestStr),
    format(atom(Result), '~w, ~w', [X, RestStr]).

% ============================================================================
% Helper predicates
% ============================================================================

% take_top_n(+List, +N, -TopN) — Take first N elements from a list
take_top_n(_, 0, []) :- !.
take_top_n([], _, []) :- !.
take_top_n([H|T], N, [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take_top_n(T, N1, Rest).

% get_attributes_by_type(+Type, -Attributes) — Get all attributes of a type
get_attributes_by_type(Type, Attributes) :-
    findall(
        attr{id: Id, type: Type, name: Name, description: Desc},
        attribute(Id, Type, Name, Desc),
        Attributes
    ).

% get_career_attributes(+CareerID, -Attributes) — Get mapped attributes for a career
get_career_attributes(CareerID, Attributes) :-
    findall(
        attr{id: AttrID, name: Name, type: Type, weight: Weight},
        (
            career_attribute(CareerID, AttrID, Weight),
            attribute(AttrID, Type, Name, _)
        ),
        Attributes
    ).
