% ============================================================================
% TechPath Advisor — HTML Templates (Material Design)
% ============================================================================
:- module(templates, [
    render_landing_page/1,
    render_wizard_page/1,
    render_admin_page/1
]).

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

% ============================================================================
% Shared head tags — DaisyUI 4 + Tailwind + Material Icons + Custom CSS
% ============================================================================
head_tags(PageTitle) -->
    html([
        title(PageTitle),
        meta([name(viewport), content('width=device-width, initial-scale=1.0')]),
        meta([name(description), content('Expert system for tech career guidance')]),
        link([rel(icon), type('image/svg+xml'), href('/static/favicon.svg')]),
        link([rel(stylesheet), href('https://cdn.jsdelivr.net/npm/daisyui@4/dist/full.min.css')]),
        link([rel(stylesheet), href('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap')]),
        link([rel(stylesheet), href('https://fonts.googleapis.com/icon?family=Material+Icons+Outlined')]),
        script([src('https://cdn.tailwindcss.com')], []),
        link([rel(stylesheet), href('/static/css/custom.css')]),
        style([], ['
            [data-theme=\"techpath\"] {
                --p: 39.5% 0.04 325;
                --pf: 33% 0.04 325;
                --pc: 100% 0 0;
                --s: 76% 0.06 180;
                --sf: 68% 0.06 180;
                --sc: 20% 0.02 180;
                --a: 82% 0.12 75;
                --af: 75% 0.12 75;
                --ac: 30% 0.04 75;
                --n: 30% 0.02 325;
                --nf: 25% 0.02 325;
                --nc: 95% 0.005 325;
                --b1: 98.5% 0.003 80;
                --b2: 96.5% 0.005 80;
                --b3: 94% 0.006 80;
                --bc: 30% 0.02 325;
                --in: 76% 0.06 180;
                --su: 76% 0.06 180;
                --wa: 82% 0.12 75;
                --er: 65% 0.18 30;
                --rounded-box: 0.75rem;
                --rounded-btn: 0.5rem;
                --rounded-badge: 1rem;
                --btn-text-case: none;
            }
            .material-icons-outlined { font-size: 20px; vertical-align: middle; }
        ']),
        script([], ['document.documentElement.setAttribute(\"data-theme\",\"techpath\");'])
    ]).

% ============================================================================
% Shared navbar — Material App Bar
% ============================================================================
navbar(ActivePage) -->
    html(
        div([class('navbar bg-white border-b border-gray-200 sticky top-0 z-40 px-4 lg:px-8')], [
            div([class('navbar-start')], [
                a([class('flex items-center gap-2.5 text-lg font-semibold no-underline hover:opacity-80 transition-opacity'), href('/')], [
                    img([src('/static/favicon.svg'), class('w-8 h-8'), alt('TechPath')]),
                    span([class('text-gray-800 hidden sm:inline')], ['TechPath'])
                ])
            ]),
            div([class('navbar-end gap-1')], [
                \nav_link('/', 'Home', 'home', ActivePage),
                \nav_link('/wizard', 'Assessment', 'assignment', ActivePage),
                \nav_link('/admin', 'Admin', 'settings', ActivePage)
            ])
        ])
    ).

nav_link(Href, Label, Icon, ActivePage) -->
    {(Href = ActivePage ->
        BtnClass = 'inline-flex items-center gap-1.5 px-4 py-2 rounded-full text-sm font-medium text-white transition-all',
        ExtraStyle = 'background:#584053;'
    ;
        BtnClass = 'inline-flex items-center gap-1.5 px-4 py-2 rounded-full text-sm font-medium text-gray-600 hover:bg-gray-100 transition-all',
        ExtraStyle = ''
    )},
    html(a([class(BtnClass), style(ExtraStyle), href(Href)], [
        span([class('material-icons-outlined'), style('font-size:18px')], [Icon]),
        Label
    ])).

% ============================================================================
% Shared footer — Material Design
% ============================================================================
page_footer -->
    html(
        footer([class('md-footer')], [
            div([class('max-w-6xl mx-auto px-6 py-12')], [
                div([class('grid grid-cols-1 md:grid-cols-3 gap-10')], [
                    div([], [
                        div([class('flex items-center gap-2 mb-3')], [
                            img([src('/static/favicon.svg'), class('w-6 h-6'), alt('TechPath')]),
                            span([class('text-lg font-semibold text-white')], ['TechPath Advisor'])
                        ]),
                        p([class('text-sm opacity-60 leading-relaxed')],
                          ['An expert system powered by SWI-Prolog that analyzes your profile to recommend ideal tech career paths.'])
                    ]),
                    div([], [
                        h3([class('text-sm font-semibold uppercase tracking-wider text-white/50 mb-4')], ['Navigation']),
                        ul([class('space-y-2.5 text-sm')], [
                            li([], [a([href('/'), class('opacity-60 hover:opacity-100 no-underline transition-opacity')], ['Home'])]),
                            li([], [a([href('/wizard'), class('opacity-60 hover:opacity-100 no-underline transition-opacity')], ['Career Assessment'])]),
                            li([], [a([href('/api/careers'), class('opacity-60 hover:opacity-100 no-underline transition-opacity')], ['API Explorer'])])
                        ])
                    ]),
                    div([], [
                        h3([class('text-sm font-semibold uppercase tracking-wider text-white/50 mb-4')], ['Technology']),
                        div([class('flex flex-wrap gap-2')], [
                            span([class('inline-block px-3 py-1 text-xs rounded-md bg-white/10 text-white/70')], ['SWI-Prolog']),
                            span([class('inline-block px-3 py-1 text-xs rounded-md bg-white/10 text-white/70')], ['Expert System']),
                            span([class('inline-block px-3 py-1 text-xs rounded-md bg-white/10 text-white/70')], ['Tailwind CSS']),
                            span([class('inline-block px-3 py-1 text-xs rounded-md bg-white/10 text-white/70')], ['Material Design'])
                        ])
                    ])
                ]),
                div([class('border-t border-white/10 mt-10 pt-6')], [
                    p([class('text-center text-sm text-white/40')],
                      ['Built with SWI-Prolog | 2026 TechPath Advisor'])
                ])
            ])
        ])
    ).

% ============================================================================
% LANDING PAGE — Material Design
% ============================================================================
render_landing_page(_Request) :-
    reply_html_page(
        [\head_tags('TechPath Advisor - Discover Your Ideal Tech Career')],
        [\landing_body]
    ).

landing_body -->
    html([
        \navbar('/'),

        % ── Hero Section ───────────────────────────────────────────────────
        div([class('relative overflow-hidden'), style('background: linear-gradient(135deg, #584053 0%, #3d2c3a 100%);')], [
            div([class('max-w-5xl mx-auto px-6 py-24 md:py-32 text-center relative z-10')], [
                div([class('inline-flex items-center gap-2 px-4 py-2 rounded-full bg-white/10 text-white/80 text-sm font-medium mb-8 backdrop-blur-sm')], [
                    span([class('material-icons-outlined'), style('font-size:16px')], ['psychology']),
                    'Powered by SWI-Prolog Expert System'
                ]),
                h1([class('text-4xl md:text-6xl font-bold text-white leading-tight tracking-tight mb-6')], [
                    'Find Your ', br([]),
                    span([style('color: rgba(255,255,255,0.85)')], ['Perfect Tech Career'])
                ]),
                p([class('text-lg md:text-xl text-white/70 max-w-2xl mx-auto leading-relaxed mb-10')],
                  ['Answer a few questions about your skills, interests, and personality. Our Prolog inference engine will match you with the ideal career path.']),
                div([class('flex flex-col sm:flex-row gap-4 justify-center')], [
                    a([class('inline-flex items-center justify-center gap-2 px-8 py-3.5 rounded-full bg-white text-gray-800 font-semibold text-base hover:shadow-lg transition-all'), href('/wizard')], [
                        'Start Free Assessment',
                        span([class('material-icons-outlined'), style('font-size:20px')], ['arrow_forward'])
                    ]),
                    a([class('inline-flex items-center justify-center gap-2 px-8 py-3.5 rounded-full border-2 border-white/30 text-white font-medium text-base hover:bg-white/10 transition-all'), href('#how-it-works')], [
                        'Learn More'
                    ])
                ])
            ]),
            % decorative circles
            div([class('absolute top-0 right-0 w-96 h-96 rounded-full opacity-5'), style('background: #8DC6BF; filter: blur(80px); transform: translate(30%, -30%);')], []),
            div([class('absolute bottom-0 left-0 w-72 h-72 rounded-full opacity-5'), style('background: #FCBC66; filter: blur(60px); transform: translate(-30%, 30%);')], [])
        ]),

        % ── Stats Bar ─────────────────────────────────────────────────────
        div([class('bg-white border-b border-gray-100')], [
            div([class('max-w-5xl mx-auto px-6 py-8')], [
                div([class('grid grid-cols-2 md:grid-cols-4 gap-8')], [
                    \stat_item('15+', 'Career Paths', 'trending_up'),
                    \stat_item('38+', 'Attributes', 'category'),
                    \stat_item('100+', 'Expert Rules', 'rule'),
                    \stat_item('Prolog', 'Inference Engine', 'memory')
                ])
            ])
        ]),

        % ── How It Works ──────────────────────────────────────────────────
        div([class('py-20 bg-gray-50'), id('how-it-works')], [
            div([class('max-w-5xl mx-auto px-6')], [
                div([class('text-center mb-14')], [
                    span([class('inline-flex items-center gap-1.5 px-3 py-1 rounded-full text-xs font-semibold uppercase tracking-wider'), style('background: rgba(141,198,191,0.15); color: #5fa8a0;')], ['How It Works']),
                    h2([class('text-3xl font-bold text-gray-900 mt-4')], ['Four Simple Steps']),
                    p([class('mt-3 text-gray-500 max-w-xl mx-auto')],
                      ['Our expert system uses forward-chaining inference to match your profile against a curated knowledge base.'])
                ]),
                div([class('grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6')], [
                    \step_card('1', 'Interests', 'Select technology domains that fascinate you.', 'favorite'),
                    \step_card('2', 'Skills', 'Choose tools, languages and frameworks you know.', 'code'),
                    \step_card('3', 'Personality', 'Identify work-style traits that describe you.', 'psychology'),
                    \step_card('4', 'Results', 'Get ranked career matches with confidence scores.', 'insights')
                ])
            ])
        ]),

        % ── Features Grid ─────────────────────────────────────────────────
        div([class('py-20 bg-white')], [
            div([class('max-w-5xl mx-auto px-6')], [
                div([class('text-center mb-14')], [
                    span([class('inline-flex items-center gap-1.5 px-3 py-1 rounded-full text-xs font-semibold uppercase tracking-wider'), style('background: rgba(252,188,102,0.15); color: #c48830;')], ['Features']),
                    h2([class('text-3xl font-bold text-gray-900 mt-4')], ['Why TechPath?'])
                ]),
                div([class('grid grid-cols-1 md:grid-cols-3 gap-6')], [
                    \feature_card('Target', 'Personalized Matching',
                        'Weighted attribute scoring produces tailored recommendations unique to your profile.', 'track_changes'),
                    \feature_card('Brain', 'Expert Knowledge Base',
                        '15 career paths with 100+ rules curated from industry research and expert knowledge.', 'school'),
                    \feature_card('Fast', 'Instant Analysis',
                        'Prolog inference engine processes your profile in milliseconds with detailed explanations.', 'bolt'),
                    \feature_card('Chart', 'Confidence Scores',
                        'Each recommendation comes with a percentage confidence and explanation of why it matches.', 'analytics'),
                    \feature_card('Lock', 'Privacy First',
                        'No data stored. Your profile is analyzed in real-time and never saved to any database.', 'lock'),
                    \feature_card('API', 'Open Architecture',
                        'RESTful API allows integration with other tools. Full JSON responses for all endpoints.', 'api')
                ])
            ])
        ]),

        % ── CTA Section ───────────────────────────────────────────────────
        div([class('py-20'), style('background: linear-gradient(135deg, #584053 0%, #3d2c3a 100%);')], [
            div([class('max-w-3xl mx-auto px-6 text-center')], [
                h2([class('text-3xl md:text-4xl font-bold text-white mb-4')], ['Ready to Discover Your Path?']),
                p([class('text-lg text-white/60 mb-8 max-w-2xl mx-auto')],
                  ['It takes less than 2 minutes. No signup required. Get personalized career recommendations powered by expert-system AI.']),
                a([class('inline-flex items-center gap-2 px-8 py-3.5 rounded-full bg-white text-gray-800 font-semibold text-base hover:shadow-lg transition-all'), href('/wizard')],
                  ['Start Assessment', span([class('material-icons-outlined'), style('font-size:20px')], ['arrow_forward'])])
            ])
        ]),

        \page_footer
    ]).

% ── Landing page components ────────────────────────────────────────────
stat_item(Value, Label, Icon) -->
    html(
        div([class('text-center')], [
            div([class('flex justify-center mb-2')], [
                span([class('material-icons-outlined'), style('font-size:24px; color: #584053; opacity:0.6;')], [Icon])
            ]),
            div([class('text-2xl font-bold text-gray-900')], [Value]),
            div([class('text-sm text-gray-500 mt-0.5')], [Label])
        ])
    ).

step_card(Number, Title, Description, Icon) -->
    html(
        div([class('md-card p-6')], [
            div([class('flex items-center gap-3 mb-4')], [
                div([class('w-10 h-10 rounded-xl flex items-center justify-center'), style('background: rgba(88,64,83,0.08);')], [
                    span([class('material-icons-outlined'), style('font-size:22px; color: #584053;')], [Icon])
                ]),
                span([class('text-xs font-bold text-gray-300 uppercase tracking-wider')], ['Step ', Number])
            ]),
            h3([class('text-base font-semibold text-gray-900 mb-2')], [Title]),
            p([class('text-sm text-gray-500 leading-relaxed')], [Description])
        ])
    ).

feature_card(_IconLabel, Title, Description, Icon) -->
    html(
        div([class('md-card-flat p-6 rounded-xl')], [
            div([class('w-10 h-10 rounded-xl flex items-center justify-center mb-4'), style('background: rgba(141,198,191,0.15);')], [
                span([class('material-icons-outlined'), style('font-size:22px; color: #5fa8a0;')], [Icon])
            ]),
            h3([class('text-base font-semibold text-gray-900 mb-2')], [Title]),
            p([class('text-sm text-gray-500 leading-relaxed')], [Description])
        ])
    ).

% ============================================================================
% WIZARD PAGE — Material Design
% ============================================================================
render_wizard_page(_Request) :-
    reply_html_page(
        [\head_tags('TechPath Advisor - Career Assessment Wizard')],
        [\wizard_body]
    ).

wizard_body -->
    html([
        \navbar('/wizard'),

        % ── Progress bar ───────────────────────────────────────────────────
        div([class('bg-white border-b border-gray-100')], [
            div([class('max-w-3xl mx-auto px-6 py-5')], [
                div([class('flex items-center justify-between mb-3')], [
                    span([class('text-sm font-semibold text-gray-800'), id('step-label')], ['Step 1 of 4']),
                    span([class('text-sm text-gray-400'), id('step-name')], ['Select Interests'])
                ]),
                div([class('md-progress-track')], [
                    div([class('md-progress-bar'), id('progress-bar'), style('width: 25%')], [])
                ]),
                div([class('md-stepper'), id('steps-indicator')], [
                    div([class('md-step active'), id('step-ind-1')], [
                        span([class('md-step-num')], ['1']),
                        span([class('hidden sm:inline text-xs')], ['Interests'])
                    ]),
                    div([class('md-step'), id('step-ind-2')], [
                        span([class('md-step-num')], ['2']),
                        span([class('hidden sm:inline text-xs')], ['Skills'])
                    ]),
                    div([class('md-step'), id('step-ind-3')], [
                        span([class('md-step-num')], ['3']),
                        span([class('hidden sm:inline text-xs')], ['Personality'])
                    ]),
                    div([class('md-step'), id('step-ind-4')], [
                        span([class('md-step-num')], ['4']),
                        span([class('hidden sm:inline text-xs')], ['Review'])
                    ])
                ])
            ])
        ]),

        % ── Main wizard area ──────────────────────────────────────────────
        div([class('min-h-[70vh] bg-gray-50')], [
            div([class('max-w-3xl mx-auto px-6 py-10')], [

                % --- Step 1: Interests ---
                div([id('step-1'), class('wizard-step animate-fadeIn')], [
                    div([class('mb-8')], [
                        div([class('flex items-center gap-3 mb-2')], [
                            span([class('material-icons-outlined'), style('font-size:28px; color: #584053;')], ['favorite']),
                            h2([class('text-2xl font-bold text-gray-900')], ['What excites you?'])
                        ]),
                        p([class('text-gray-500 ml-10')],
                          ['Select the technology domains you find most interesting.'])
                    ]),
                    div([id('interests-grid'), class('grid grid-cols-1 sm:grid-cols-2 gap-3')], [
                        \loading_placeholder
                    ]),
                    div([id('step1-error'), class('mt-4 p-3 rounded-lg bg-red-50 text-red-700 text-sm hidden')], [
                        'Please select at least one interest to continue.'
                    ]),
                    div([class('flex justify-end mt-8')], [
                        button([class('btn btn-primary gap-2'), onclick('nextStep(1)')], [
                            'Continue',
                            span([class('material-icons-outlined'), style('font-size:18px')], ['arrow_forward'])
                        ])
                    ])
                ]),

                % --- Step 2: Skills ---
                div([id('step-2'), class('wizard-step hidden')], [
                    div([class('mb-8')], [
                        div([class('flex items-center gap-3 mb-2')], [
                            span([class('material-icons-outlined'), style('font-size:28px; color: #584053;')], ['code']),
                            h2([class('text-2xl font-bold text-gray-900')], ['What can you do?'])
                        ]),
                        p([class('text-gray-500 ml-10')],
                          ['Select the programming languages, tools, and frameworks you have experience with.'])
                    ]),
                    div([class('relative mb-6')], [
                        span([class('absolute left-3 top-1/2 -translate-y-1/2 material-icons-outlined text-gray-400'), style('font-size:20px')], ['search']),
                        input([type(text), id('skill-search'),
                               class('input w-full pl-10'),
                               placeholder('Search skills...'), oninput('filterSkills()')], [])
                    ]),
                    div([id('skills-grid'), class('flex flex-wrap gap-2.5')], [
                        \loading_placeholder
                    ]),
                    div([id('step2-error'), class('mt-4 p-3 rounded-lg bg-red-50 text-red-700 text-sm hidden')], [
                        'Please select at least one skill to continue.'
                    ]),
                    div([class('flex justify-between mt-8')], [
                        button([class('btn btn-ghost gap-2'), onclick('prevStep(2)')], [
                            span([class('material-icons-outlined'), style('font-size:18px')], ['arrow_back']),
                            'Back'
                        ]),
                        button([class('btn btn-primary gap-2'), onclick('nextStep(2)')], [
                            'Continue',
                            span([class('material-icons-outlined'), style('font-size:18px')], ['arrow_forward'])
                        ])
                    ])
                ]),

                % --- Step 3: Personality ---
                div([id('step-3'), class('wizard-step hidden')], [
                    div([class('mb-8')], [
                        div([class('flex items-center gap-3 mb-2')], [
                            span([class('material-icons-outlined'), style('font-size:28px; color: #584053;')], ['psychology']),
                            h2([class('text-2xl font-bold text-gray-900')], ['How do you work?'])
                        ]),
                        p([class('text-gray-500 ml-10')],
                          ['Select the personality traits and work-style preferences that best describe you.'])
                    ]),
                    div([id('personality-grid'), class('grid grid-cols-1 sm:grid-cols-2 gap-3')], [
                        \loading_placeholder
                    ]),
                    div([id('step3-error'), class('mt-4 p-3 rounded-lg bg-red-50 text-red-700 text-sm hidden')], [
                        'Please select at least one personality trait to continue.'
                    ]),
                    div([class('flex justify-between mt-8')], [
                        button([class('btn btn-ghost gap-2'), onclick('prevStep(3)')], [
                            span([class('material-icons-outlined'), style('font-size:18px')], ['arrow_back']),
                            'Back'
                        ]),
                        button([class('btn btn-primary gap-2'), onclick('nextStep(3)')], [
                            'Continue',
                            span([class('material-icons-outlined'), style('font-size:18px')], ['arrow_forward'])
                        ])
                    ])
                ]),

                % --- Step 4: Review & Submit ---
                div([id('step-4'), class('wizard-step hidden')], [
                    div([class('mb-8')], [
                        div([class('flex items-center gap-3 mb-2')], [
                            span([class('material-icons-outlined'), style('font-size:28px; color: #584053;')], ['checklist']),
                            h2([class('text-2xl font-bold text-gray-900')], ['Review Your Profile'])
                        ]),
                        p([class('text-gray-500 ml-10')],
                          ['Review your selections below, then submit for analysis.'])
                    ]),

                    \review_section('favorite', 'Interests', 'review-interests'),
                    \review_section('code', 'Skills', 'review-skills'),
                    \review_section('psychology', 'Personality', 'review-personality'),

                    div([class('flex justify-between mt-10')], [
                        button([class('btn btn-ghost gap-2'), onclick('prevStep(4)')], [
                            span([class('material-icons-outlined'), style('font-size:18px')], ['arrow_back']),
                            'Back'
                        ]),
                        button([class('btn btn-primary btn-lg gap-2'),
                                id('submit-btn'), onclick('submitProfile()')], [
                            'Get Recommendations',
                            span([class('material-icons-outlined'), style('font-size:20px')], ['arrow_forward'])
                        ])
                    ])
                ]),

                % --- Loading overlay ---
                div([id('loading-overlay'), class('hidden fixed inset-0 bg-white/80 backdrop-blur-sm flex items-center justify-center z-50')], [
                    div([class('text-center space-y-4')], [
                        div([class('md-spinner mx-auto')], []),
                        p([class('text-base font-semibold text-gray-800')], ['Analyzing your profile...']),
                        p([class('text-sm text-gray-400')], ['Running Prolog inference engine'])
                    ])
                ]),

                % --- Results container ---
                div([id('results-container'), class('hidden')], [])
            ])
        ]),

        \page_footer,

        script([src('/static/js/wizard.js')], [])
    ]).

% ── Wizard sub-components ──────────────────────────────────────────────
loading_placeholder -->
    html(
        div([class('col-span-full flex justify-center py-12')], [
            div([class('md-spinner')], [])
        ])
    ).

review_section(Icon, Label, DivId) -->
    html(
        div([class('mb-5')], [
            div([class('flex items-center gap-2 mb-3')], [
                span([class('material-icons-outlined'), style('font-size:18px; color: #584053; opacity:0.5;')], [Icon]),
                h3([class('text-xs font-semibold uppercase tracking-wider text-gray-400')], [Label])
            ]),
            div([id(DivId), class('flex flex-wrap gap-2 min-h-[2.5rem] p-4 bg-gray-50 rounded-xl border border-gray-100')], [])
        ])
    ).

% ============================================================================
% ADMIN PAGE — Material Design
% ============================================================================
render_admin_page(_Request) :-
    reply_html_page(
        [\head_tags('TechPath Advisor - Admin Panel')],
        [\admin_body]
    ).

admin_body -->
    html([
        \navbar('/admin'),

        % ── Header ─────────────────────────────────────────────────────────
        div([class('bg-white border-b border-gray-100')], [
            div([class('max-w-6xl mx-auto px-6 py-8')], [
                div([class('flex items-center gap-3')], [
                    span([class('material-icons-outlined'), style('font-size:28px; color: #584053;')], ['admin_panel_settings']),
                    div([], [
                        h1([class('text-2xl font-bold text-gray-900')], ['Admin Panel']),
                        p([class('text-sm text-gray-400 mt-0.5')], ['Manage careers, attributes, and knowledge base mappings.'])
                    ])
                ])
            ])
        ]),

        % ── Main content ──────────────────────────────────────────────────
        div([class('max-w-6xl mx-auto px-6 py-8 bg-gray-50 min-h-screen')], [

            % Tabs (Material segmented buttons)
            div([class('md-tabs mb-8'), id('admin-tabs')], [
                button([class('md-tab active'), onclick('showTab(\"careers\")')], ['Careers']),
                button([class('md-tab'), onclick('showTab(\"attributes\")')], ['Attributes']),
                button([class('md-tab'), onclick('showTab(\"mappings\")')], ['Mappings'])
            ]),

            % --- Careers Tab ---
            div([id('tab-careers'), class('admin-tab')], [
                div([class('md-card')], [
                    div([class('flex items-center justify-between p-5 border-b border-gray-100')], [
                        h2([class('text-base font-semibold text-gray-800')], ['Careers']),
                        button([class('btn btn-primary btn-sm gap-1.5'), onclick('openCareerModal()')], [
                            span([class('material-icons-outlined'), style('font-size:18px')], ['add']),
                            'Add Career'
                        ])
                    ]),
                    div([class('overflow-x-auto')], [
                        table([class('table w-full'), id('careers-table')], [
                            thead([], [
                                tr([], [
                                    th([], ['ID']),
                                    th([], ['Title']),
                                    th([], ['Description']),
                                    th([class('text-right')], ['Actions'])
                                ])
                            ]),
                            tbody([id('careers-tbody')], [
                                tr([], [td([colspan('4'), class('text-center py-10 text-gray-400')], ['Loading...'])])
                            ])
                        ])
                    ])
                ])
            ]),

            % --- Attributes Tab ---
            div([id('tab-attributes'), class('admin-tab hidden')], [
                div([class('md-card')], [
                    div([class('flex items-center justify-between p-5 border-b border-gray-100')], [
                        h2([class('text-base font-semibold text-gray-800')], ['Attributes']),
                        button([class('btn btn-primary btn-sm gap-1.5'), onclick('openAttrModal()')], [
                            span([class('material-icons-outlined'), style('font-size:18px')], ['add']),
                            'Add Attribute'
                        ])
                    ]),
                    div([class('flex gap-2 p-4 border-b border-gray-50')], [
                        button([class('md-chip active'), onclick('filterAttrs(\"all\")')], ['All']),
                        button([class('md-chip'), onclick('filterAttrs(\"skill\")')], ['Skills']),
                        button([class('md-chip'), onclick('filterAttrs(\"interest\")')], ['Interests']),
                        button([class('md-chip'), onclick('filterAttrs(\"personality\")')], ['Personality'])
                    ]),
                    div([class('overflow-x-auto')], [
                        table([class('table w-full'), id('attrs-table')], [
                            thead([], [
                                tr([], [
                                    th([], ['ID']),
                                    th([], ['Name']),
                                    th([], ['Type']),
                                    th([], ['Description']),
                                    th([class('text-right')], ['Actions'])
                                ])
                            ]),
                            tbody([id('attrs-tbody')], [
                                tr([], [td([colspan('5'), class('text-center py-10 text-gray-400')], ['Loading...'])])
                            ])
                        ])
                    ])
                ])
            ]),

            % --- Mappings Tab ---
            div([id('tab-mappings'), class('admin-tab hidden')], [
                div([class('md-card')], [
                    div([class('flex items-center justify-between p-5 border-b border-gray-100')], [
                        h2([class('text-base font-semibold text-gray-800')], ['Career-Attribute Mappings']),
                        button([class('btn btn-primary btn-sm gap-1.5'), onclick('openMappingModal()')], [
                            span([class('material-icons-outlined'), style('font-size:18px')], ['add']),
                            'Add Mapping'
                        ])
                    ]),
                    div([class('overflow-x-auto')], [
                        table([class('table w-full'), id('mappings-table')], [
                            thead([], [
                                tr([], [
                                    th([], ['Career']),
                                    th([], ['Attribute']),
                                    th([], ['Weight']),
                                    th([class('text-right')], ['Actions'])
                                ])
                            ]),
                            tbody([id('mappings-tbody')], [
                                tr([], [td([colspan('4'), class('text-center py-10 text-gray-400')], ['Loading...'])])
                            ])
                        ])
                    ])
                ])
            ]),

            % --- Add Career Modal ---
            dialog([id('career-modal'), class('modal')], [
                div([class('modal-box max-w-md')], [
                    h3([class('text-lg font-semibold text-gray-900 mb-6')], ['Add Career']),
                    div([class('space-y-4')], [
                        div([], [
                            label([class('text-sm font-medium text-gray-600 mb-1.5 block')], ['Title']),
                            input([type(text), id('career-title'), class('input w-full'), placeholder('e.g. Data Scientist')], [])
                        ]),
                        div([], [
                            label([class('text-sm font-medium text-gray-600 mb-1.5 block')], ['Description']),
                            textarea([id('career-desc'), class('textarea w-full'), rows('3'), placeholder('Career description...')], [])
                        ])
                    ]),
                    div([class('flex justify-end gap-3 mt-8')], [
                        button([class('btn btn-ghost'), onclick('closeModal(\"career-modal\")')], ['Cancel']),
                        button([class('btn btn-primary'), onclick('addCareer()')], ['Save'])
                    ])
                ]),
                form([method('dialog'), class('modal-backdrop')], [
                    button([], ['close'])
                ])
            ]),

            % --- Add Attribute Modal ---
            dialog([id('attr-modal'), class('modal')], [
                div([class('modal-box max-w-md')], [
                    h3([class('text-lg font-semibold text-gray-900 mb-6')], ['Add Attribute']),
                    div([class('space-y-4')], [
                        div([], [
                            label([class('text-sm font-medium text-gray-600 mb-1.5 block')], ['Name']),
                            input([type(text), id('attr-name'), class('input w-full'), placeholder('e.g. Python')], [])
                        ]),
                        div([], [
                            label([class('text-sm font-medium text-gray-600 mb-1.5 block')], ['Type']),
                            select([id('attr-type'), class('select w-full')], [
                                option([value('skill')], ['Skill']),
                                option([value('interest')], ['Interest']),
                                option([value('personality')], ['Personality'])
                            ])
                        ]),
                        div([], [
                            label([class('text-sm font-medium text-gray-600 mb-1.5 block')], ['Description']),
                            input([type(text), id('attr-desc'), class('input w-full'), placeholder('Brief description...')], [])
                        ])
                    ]),
                    div([class('flex justify-end gap-3 mt-8')], [
                        button([class('btn btn-ghost'), onclick('closeModal(\"attr-modal\")')], ['Cancel']),
                        button([class('btn btn-primary'), onclick('addAttr()')], ['Save'])
                    ])
                ]),
                form([method('dialog'), class('modal-backdrop')], [
                    button([], ['close'])
                ])
            ]),

            % --- Add Mapping Modal ---
            dialog([id('mapping-modal'), class('modal')], [
                div([class('modal-box max-w-md')], [
                    h3([class('text-lg font-semibold text-gray-900 mb-6')], ['Add Mapping']),
                    div([class('space-y-4')], [
                        div([], [
                            label([class('text-sm font-medium text-gray-600 mb-1.5 block')], ['Career']),
                            select([id('mapping-career'), class('select w-full')], [])
                        ]),
                        div([], [
                            label([class('text-sm font-medium text-gray-600 mb-1.5 block')], ['Attribute']),
                            select([id('mapping-attr'), class('select w-full')], [])
                        ]),
                        div([], [
                            label([class('text-sm font-medium text-gray-600 mb-1.5 block')], ['Weight (1-10)']),
                            input([type(number), id('mapping-weight'), class('input w-full'), min('1'), max('10'), value('5')], [])
                        ])
                    ]),
                    div([class('flex justify-end gap-3 mt-8')], [
                        button([class('btn btn-ghost'), onclick('closeModal(\"mapping-modal\")')], ['Cancel']),
                        button([class('btn btn-primary'), onclick('addMapping()')], ['Save'])
                    ])
                ]),
                form([method('dialog'), class('modal-backdrop')], [
                    button([], ['close'])
                ])
            ])
        ]),

        \page_footer,

        script([src('/static/js/admin.js')], [])
    ]).
