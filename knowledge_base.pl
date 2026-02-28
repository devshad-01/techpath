% ============================================================================
% TechPath Advisor — Knowledge Base
% ============================================================================
% This module defines the dynamic facts representing careers, attributes,
% career-attribute mappings, and user accounts. The seed_knowledge_base/0
% predicate populates the system with initial data.
% ============================================================================

:- module(knowledge_base, [
    seed_knowledge_base/0,
    career/4,
    attribute/4,
    career_attribute/3,
    user_account/4,
    next_career_id/1,
    next_attribute_id/1
]).

% --- Dynamic fact declarations ---
:- dynamic career/4.            % career(Id, Title, Description, Metadata)
:- dynamic attribute/4.         % attribute(Id, Type, Name, Description)
:- dynamic career_attribute/3.  % career_attribute(CareerID, AttributeID, Weight)
:- dynamic user_account/4.      % user_account(Id, Username, PasswordHash, Role)

% ============================================================================
% Helper: generate next available ID
% ============================================================================
next_career_id(NextId) :-
    findall(Id, career(Id, _, _, _), Ids),
    (   Ids = [] -> NextId = 1
    ;   max_list(Ids, Max), NextId is Max + 1
    ).

next_attribute_id(NextId) :-
    findall(Id, attribute(Id, _, _, _), Ids),
    (   Ids = [] -> NextId = 1
    ;   max_list(Ids, Max), NextId is Max + 1
    ).

% ============================================================================
% seed_knowledge_base/0 — Clears all facts and asserts the initial data set.
% ============================================================================
seed_knowledge_base :-
    retractall(career(_, _, _, _)),
    retractall(attribute(_, _, _, _)),
    retractall(career_attribute(_, _, _)),
    retractall(user_account(_, _, _, _)),
    seed_careers,
    seed_attributes,
    seed_mappings,
    seed_users.

% ============================================================================
% CAREERS (15 entries)
% career(Id, Title, Description, Metadata)
% Metadata = [salary-Range, demand-Level, education-Requirement]
% ============================================================================
seed_careers :-
    assert(career(1,
        'Data Scientist',
        'Analyzes complex datasets to extract actionable insights using statistical methods, machine learning, and data visualization. Works closely with business stakeholders to drive data-informed decision making.',
        [salary-'$95K-$140K', demand-high, education-'Bachelor\'s / Master\'s in CS, Statistics, or related field'])),

    assert(career(2,
        'Machine Learning Engineer',
        'Designs, builds, and deploys machine learning models into production systems. Bridges the gap between data science research and scalable software engineering.',
        [salary-'$110K-$160K', demand-high, education-'Bachelor\'s / Master\'s in CS or AI'])),

    assert(career(3,
        'Frontend Developer',
        'Creates responsive, interactive user interfaces for web applications using modern JavaScript frameworks. Focuses on user experience, accessibility, and performance optimization.',
        [salary-'$75K-$120K', demand-high, education-'Bachelor\'s in CS or self-taught with portfolio'])),

    assert(career(4,
        'Backend Developer',
        'Designs and implements server-side logic, APIs, and database architectures that power web and mobile applications. Ensures scalability, security, and data integrity.',
        [salary-'$80K-$130K', demand-high, education-'Bachelor\'s in CS or Software Engineering'])),

    assert(career(5,
        'Full-Stack Developer',
        'Works across the entire web application stack, handling both client-side interfaces and server-side logic. Valued for versatility and the ability to deliver complete features independently.',
        [salary-'$85K-$135K', demand-high, education-'Bachelor\'s in CS or equivalent experience'])),

    assert(career(6,
        'DevOps Engineer',
        'Automates and streamlines software development, testing, and deployment pipelines. Manages cloud infrastructure, containerization, and monitoring to ensure reliable, fast releases.',
        [salary-'$90K-$140K', demand-high, education-'Bachelor\'s in CS or IT'])),

    assert(career(7,
        'Cloud Architect',
        'Designs and oversees an organization\'s cloud computing strategy, including adoption plans, cloud application design, and cloud management. Ensures security, scalability, and cost efficiency.',
        [salary-'$120K-$170K', demand-high, education-'Bachelor\'s in CS with cloud certifications'])),

    assert(career(8,
        'Cybersecurity Analyst',
        'Protects organizational systems and networks from cyber threats by monitoring security infrastructure, conducting vulnerability assessments, and responding to security incidents.',
        [salary-'$85K-$130K', demand-high, education-'Bachelor\'s in CS, Cybersecurity, or IT'])),

    assert(career(9,
        'Mobile App Developer',
        'Designs and builds applications for mobile platforms (iOS and Android). Focuses on performance, usability, and platform-specific design guidelines to create engaging mobile experiences.',
        [salary-'$80K-$130K', demand-high, education-'Bachelor\'s in CS or Software Engineering'])),

    assert(career(10,
        'UX/UI Designer',
        'Researches user needs and designs intuitive, visually appealing interfaces for digital products. Combines user psychology, visual design principles, and prototyping to create seamless experiences.',
        [salary-'$70K-$115K', demand-medium, education-'Bachelor\'s in Design, HCI, or related field'])),

    assert(career(11,
        'Site Reliability Engineer',
        'Ensures the reliability, availability, and performance of large-scale production systems. Applies software engineering principles to infrastructure and operations problems.',
        [salary-'$100K-$155K', demand-high, education-'Bachelor\'s in CS or Software Engineering'])),

    assert(career(12,
        'Data Engineer',
        'Builds and maintains the data infrastructure, pipelines, and warehouses that enable data scientists and analysts to work effectively. Focuses on data quality, reliability, and scalability.',
        [salary-'$90K-$140K', demand-high, education-'Bachelor\'s in CS or Data Engineering'])),

    assert(career(13,
        'Game Developer',
        'Creates interactive entertainment experiences across platforms. Combines programming, creative design, and physics simulation to build engaging gameplay mechanics and immersive worlds.',
        [salary-'$65K-$115K', demand-medium, education-'Bachelor\'s in CS or Game Development'])),

    assert(career(14,
        'Blockchain Developer',
        'Develops decentralized applications (dApps) and smart contracts on blockchain platforms. Works with cryptographic protocols and distributed systems to build trustless, transparent solutions.',
        [salary-'$95K-$150K', demand-medium, education-'Bachelor\'s in CS with blockchain knowledge'])),

    assert(career(15,
        'AI Research Scientist',
        'Conducts original research to advance the state of the art in artificial intelligence. Publishes papers, develops novel algorithms, and pushes the boundaries of what intelligent systems can achieve.',
        [salary-'$120K-$180K', demand-high, education-'Master\'s / PhD in AI, ML, or related field'])).

% ============================================================================
% ATTRIBUTES
% attribute(Id, Type, Name, Description)
% Type: skill | interest | personality
% ============================================================================
seed_attributes :-
    % --- Skills (IDs 1–15) ---
    assert(attribute(1,  skill, 'Python',      'General-purpose programming language widely used in data science, AI, and backend development')),
    assert(attribute(2,  skill, 'JavaScript',   'The language of the web, essential for frontend and increasingly used in backend development')),
    assert(attribute(3,  skill, 'Java',         'Enterprise-grade object-oriented language used in backend systems, Android, and large-scale applications')),
    assert(attribute(4,  skill, 'C++',          'High-performance systems programming language used in game engines, OS development, and embedded systems')),
    assert(attribute(5,  skill, 'SQL',          'Standard language for querying and managing relational databases')),
    assert(attribute(6,  skill, 'React',        'Popular JavaScript library for building component-based user interfaces')),
    assert(attribute(7,  skill, 'Node.js',      'JavaScript runtime for building scalable server-side applications')),
    assert(attribute(8,  skill, 'Docker',       'Containerization platform for packaging and deploying applications consistently')),
    assert(attribute(9,  skill, 'Kubernetes',   'Container orchestration system for automating deployment, scaling, and management')),
    assert(attribute(10, skill, 'TensorFlow',   'Open-source machine learning framework for building and training neural networks')),
    assert(attribute(11, skill, 'AWS',          'Amazon Web Services cloud computing platform with extensive service offerings')),
    assert(attribute(12, skill, 'Git',          'Distributed version control system essential for collaborative software development')),
    assert(attribute(13, skill, 'Figma',        'Collaborative interface design tool for creating UI/UX prototypes and design systems')),
    assert(attribute(14, skill, 'Solidity',     'Programming language for writing smart contracts on Ethereum and EVM-compatible blockchains')),
    assert(attribute(15, skill, 'Unity',        'Cross-platform game engine for creating 2D and 3D interactive experiences')),

    % --- Interests (IDs 16–26) ---
    assert(attribute(16, interest, 'Data Science',          'Working with data to discover patterns, build models, and drive decisions')),
    assert(attribute(17, interest, 'Web Development',       'Building websites and web applications for the modern internet')),
    assert(attribute(18, interest, 'Cloud Computing',       'Designing and managing scalable infrastructure in the cloud')),
    assert(attribute(19, interest, 'Cybersecurity',         'Protecting systems, networks, and data from digital threats')),
    assert(attribute(20, interest, 'Mobile Development',    'Creating applications for smartphones and tablets')),
    assert(attribute(21, interest, 'Artificial Intelligence','Building intelligent systems that can learn, reason, and act')),
    assert(attribute(22, interest, 'Game Development',      'Creating interactive entertainment and simulation experiences')),
    assert(attribute(23, interest, 'Blockchain',            'Building decentralized applications and trustless systems')),
    assert(attribute(24, interest, 'User Experience',       'Designing intuitive, accessible, and delightful digital experiences')),
    assert(attribute(25, interest, 'DevOps',                'Automating and improving the software development and delivery lifecycle')),
    assert(attribute(26, interest, 'Networking',            'Understanding and managing computer networks and communication protocols')),

    % --- Personality Traits (IDs 27–38) ---
    assert(attribute(27, personality, 'Analytical',      'Enjoys breaking down complex problems into logical, manageable components')),
    assert(attribute(28, personality, 'Creative',        'Thrives on generating original ideas and finding novel solutions')),
    assert(attribute(29, personality, 'Detail-Oriented', 'Pays meticulous attention to accuracy, consistency, and precision')),
    assert(attribute(30, personality, 'Collaborative',   'Works effectively in teams and values shared problem-solving')),
    assert(attribute(31, personality, 'Independent',     'Prefers autonomous work and self-directed learning')),
    assert(attribute(32, personality, 'Problem-Solver',  'Energized by tackling difficult challenges and finding solutions')),
    assert(attribute(33, personality, 'Communicative',   'Strong at explaining complex ideas clearly to diverse audiences')),
    assert(attribute(34, personality, 'Patient',         'Comfortable with iterative processes and long debugging sessions')),
    assert(attribute(35, personality, 'Adaptable',       'Quickly adjusts to new technologies, tools, and changing requirements')),
    assert(attribute(36, personality, 'Leadership',      'Naturally takes initiative and guides teams toward shared goals')),
    assert(attribute(37, personality, 'Curious',         'Driven by a desire to understand how things work and explore new domains')),
    assert(attribute(38, personality, 'Methodical',      'Approaches tasks systematically with structured plans and processes')).

% ============================================================================
% CAREER-ATTRIBUTE MAPPINGS
% career_attribute(CareerID, AttributeID, Weight)
% Weight: 1 (minor relevance) to 10 (critical requirement)
% ============================================================================
seed_mappings :-
    % --- 1. Data Scientist ---
    assert(career_attribute(1, 1, 10)),   % Python (critical)
    assert(career_attribute(1, 5, 7)),    % SQL
    assert(career_attribute(1, 10, 6)),   % TensorFlow
    assert(career_attribute(1, 12, 4)),   % Git
    assert(career_attribute(1, 16, 10)),  % Interest: Data Science
    assert(career_attribute(1, 21, 8)),   % Interest: AI
    assert(career_attribute(1, 27, 9)),   % Personality: Analytical
    assert(career_attribute(1, 37, 6)),   % Personality: Curious
    assert(career_attribute(1, 32, 7)),   % Personality: Problem-Solver

    % --- 2. Machine Learning Engineer ---
    assert(career_attribute(2, 1, 10)),   % Python
    assert(career_attribute(2, 4, 5)),    % C++
    assert(career_attribute(2, 10, 9)),   % TensorFlow
    assert(career_attribute(2, 8, 6)),    % Docker
    assert(career_attribute(2, 12, 5)),   % Git
    assert(career_attribute(2, 21, 10)),  % Interest: AI
    assert(career_attribute(2, 16, 7)),   % Interest: Data Science
    assert(career_attribute(2, 27, 8)),   % Personality: Analytical
    assert(career_attribute(2, 32, 8)),   % Personality: Problem-Solver
    assert(career_attribute(2, 34, 5)),   % Personality: Patient

    % --- 3. Frontend Developer ---
    assert(career_attribute(3, 2, 10)),   % JavaScript
    assert(career_attribute(3, 6, 9)),    % React
    assert(career_attribute(3, 13, 5)),   % Figma
    assert(career_attribute(3, 12, 5)),   % Git
    assert(career_attribute(3, 17, 10)),  % Interest: Web Development
    assert(career_attribute(3, 24, 7)),   % Interest: User Experience
    assert(career_attribute(3, 28, 7)),   % Personality: Creative
    assert(career_attribute(3, 29, 8)),   % Personality: Detail-Oriented
    assert(career_attribute(3, 30, 5)),   % Personality: Collaborative

    % --- 4. Backend Developer ---
    assert(career_attribute(4, 1, 7)),    % Python
    assert(career_attribute(4, 3, 8)),    % Java
    assert(career_attribute(4, 7, 8)),    % Node.js
    assert(career_attribute(4, 5, 9)),    % SQL
    assert(career_attribute(4, 8, 6)),    % Docker
    assert(career_attribute(4, 12, 6)),   % Git
    assert(career_attribute(4, 17, 9)),   % Interest: Web Development
    assert(career_attribute(4, 27, 7)),   % Personality: Analytical
    assert(career_attribute(4, 32, 8)),   % Personality: Problem-Solver
    assert(career_attribute(4, 38, 6)),   % Personality: Methodical

    % --- 5. Full-Stack Developer ---
    assert(career_attribute(5, 2, 9)),    % JavaScript
    assert(career_attribute(5, 6, 8)),    % React
    assert(career_attribute(5, 7, 8)),    % Node.js
    assert(career_attribute(5, 5, 6)),    % SQL
    assert(career_attribute(5, 12, 5)),   % Git
    assert(career_attribute(5, 8, 5)),    % Docker
    assert(career_attribute(5, 17, 10)),  % Interest: Web Development
    assert(career_attribute(5, 35, 7)),   % Personality: Adaptable
    assert(career_attribute(5, 32, 6)),   % Personality: Problem-Solver
    assert(career_attribute(5, 31, 5)),   % Personality: Independent

    % --- 6. DevOps Engineer ---
    assert(career_attribute(6, 1, 6)),    % Python
    assert(career_attribute(6, 8, 10)),   % Docker
    assert(career_attribute(6, 9, 9)),    % Kubernetes
    assert(career_attribute(6, 11, 8)),   % AWS
    assert(career_attribute(6, 12, 7)),   % Git
    assert(career_attribute(6, 25, 10)),  % Interest: DevOps
    assert(career_attribute(6, 18, 7)),   % Interest: Cloud Computing
    assert(career_attribute(6, 38, 8)),   % Personality: Methodical
    assert(career_attribute(6, 32, 7)),   % Personality: Problem-Solver
    assert(career_attribute(6, 35, 6)),   % Personality: Adaptable

    % --- 7. Cloud Architect ---
    assert(career_attribute(7, 11, 10)),  % AWS
    assert(career_attribute(7, 8, 7)),    % Docker
    assert(career_attribute(7, 9, 8)),    % Kubernetes
    assert(career_attribute(7, 1, 5)),    % Python
    assert(career_attribute(7, 18, 10)),  % Interest: Cloud Computing
    assert(career_attribute(7, 25, 6)),   % Interest: DevOps
    assert(career_attribute(7, 36, 8)),   % Personality: Leadership
    assert(career_attribute(7, 27, 7)),   % Personality: Analytical
    assert(career_attribute(7, 38, 7)),   % Personality: Methodical
    assert(career_attribute(7, 33, 6)),   % Personality: Communicative

    % --- 8. Cybersecurity Analyst ---
    assert(career_attribute(8, 1, 6)),    % Python
    assert(career_attribute(8, 3, 4)),    % Java
    assert(career_attribute(8, 5, 5)),    % SQL
    assert(career_attribute(8, 19, 10)),  % Interest: Cybersecurity
    assert(career_attribute(8, 26, 8)),   % Interest: Networking
    assert(career_attribute(8, 27, 9)),   % Personality: Analytical
    assert(career_attribute(8, 29, 9)),   % Personality: Detail-Oriented
    assert(career_attribute(8, 32, 8)),   % Personality: Problem-Solver
    assert(career_attribute(8, 38, 7)),   % Personality: Methodical

    % --- 9. Mobile App Developer ---
    assert(career_attribute(9, 2, 7)),    % JavaScript
    assert(career_attribute(9, 3, 8)),    % Java
    assert(career_attribute(9, 6, 6)),    % React (React Native)
    assert(career_attribute(9, 12, 5)),   % Git
    assert(career_attribute(9, 20, 10)),  % Interest: Mobile Development
    assert(career_attribute(9, 24, 6)),   % Interest: User Experience
    assert(career_attribute(9, 28, 6)),   % Personality: Creative
    assert(career_attribute(9, 29, 7)),   % Personality: Detail-Oriented
    assert(career_attribute(9, 34, 5)),   % Personality: Patient

    % --- 10. UX/UI Designer ---
    assert(career_attribute(10, 13, 10)), % Figma
    assert(career_attribute(10, 2, 5)),   % JavaScript
    assert(career_attribute(10, 6, 4)),   % React
    assert(career_attribute(10, 24, 10)), % Interest: User Experience
    assert(career_attribute(10, 17, 6)),  % Interest: Web Development
    assert(career_attribute(10, 28, 10)), % Personality: Creative
    assert(career_attribute(10, 33, 8)),  % Personality: Communicative
    assert(career_attribute(10, 34, 6)),  % Personality: Patient
    assert(career_attribute(10, 30, 7)),  % Personality: Collaborative

    % --- 11. Site Reliability Engineer ---
    assert(career_attribute(11, 1, 7)),   % Python
    assert(career_attribute(11, 8, 9)),   % Docker
    assert(career_attribute(11, 9, 8)),   % Kubernetes
    assert(career_attribute(11, 11, 8)),  % AWS
    assert(career_attribute(11, 12, 6)),  % Git
    assert(career_attribute(11, 25, 9)),  % Interest: DevOps
    assert(career_attribute(11, 18, 7)),  % Interest: Cloud Computing
    assert(career_attribute(11, 32, 9)),  % Personality: Problem-Solver
    assert(career_attribute(11, 38, 7)),  % Personality: Methodical
    assert(career_attribute(11, 34, 6)),  % Personality: Patient

    % --- 12. Data Engineer ---
    assert(career_attribute(12, 1, 9)),   % Python
    assert(career_attribute(12, 5, 10)),  % SQL
    assert(career_attribute(12, 3, 5)),   % Java
    assert(career_attribute(12, 8, 7)),   % Docker
    assert(career_attribute(12, 11, 6)),  % AWS
    assert(career_attribute(12, 12, 5)),  % Git
    assert(career_attribute(12, 16, 9)),  % Interest: Data Science
    assert(career_attribute(12, 18, 6)),  % Interest: Cloud Computing
    assert(career_attribute(12, 29, 8)),  % Personality: Detail-Oriented
    assert(career_attribute(12, 38, 8)),  % Personality: Methodical
    assert(career_attribute(12, 32, 6)),  % Personality: Problem-Solver

    % --- 13. Game Developer ---
    assert(career_attribute(13, 4, 10)),  % C++
    assert(career_attribute(13, 15, 9)),  % Unity
    assert(career_attribute(13, 2, 5)),   % JavaScript
    assert(career_attribute(13, 12, 4)),  % Git
    assert(career_attribute(13, 22, 10)), % Interest: Game Development
    assert(career_attribute(13, 28, 9)),  % Personality: Creative
    assert(career_attribute(13, 34, 7)),  % Personality: Patient
    assert(career_attribute(13, 32, 7)),  % Personality: Problem-Solver
    assert(career_attribute(13, 30, 5)),  % Personality: Collaborative

    % --- 14. Blockchain Developer ---
    assert(career_attribute(14, 14, 10)), % Solidity
    assert(career_attribute(14, 2, 7)),   % JavaScript
    assert(career_attribute(14, 1, 5)),   % Python
    assert(career_attribute(14, 12, 5)),  % Git
    assert(career_attribute(14, 23, 10)), % Interest: Blockchain
    assert(career_attribute(14, 19, 5)),  % Interest: Cybersecurity
    assert(career_attribute(14, 27, 8)),  % Personality: Analytical
    assert(career_attribute(14, 29, 7)),  % Personality: Detail-Oriented
    assert(career_attribute(14, 31, 6)),  % Personality: Independent
    assert(career_attribute(14, 37, 6)),  % Personality: Curious

    % --- 15. AI Research Scientist ---
    assert(career_attribute(15, 1, 10)),  % Python
    assert(career_attribute(15, 4, 7)),   % C++
    assert(career_attribute(15, 10, 9)),  % TensorFlow
    assert(career_attribute(15, 21, 10)), % Interest: AI
    assert(career_attribute(15, 16, 7)),  % Interest: Data Science
    assert(career_attribute(15, 27, 10)), % Personality: Analytical
    assert(career_attribute(15, 37, 9)),  % Personality: Curious
    assert(career_attribute(15, 34, 7)),  % Personality: Patient
    assert(career_attribute(15, 31, 6)),  % Personality: Independent
    assert(career_attribute(15, 32, 8)).  % Personality: Problem-Solver

% ============================================================================
% SEED USERS (placeholder for Sprint 3 auth)
% user_account(Id, Username, PasswordHash, Role)
% ============================================================================
seed_users :-
    assert(user_account(1, admin, '$2a$10$placeholder_hash', admin)).
