# Sprint 1 — Foundation: Knowledge Base, Inference Engine & HTTP Server

## Goal

Build the entire SWI-Prolog backend of TechPath Advisor: a knowledge base of Prolog facts representing tech careers, attributes, and weighted mappings; a rule-based inference engine that scores and ranks careers against a user profile; and an HTTP server exposing JSON API endpoints.

## Context

TechPath Advisor is a web-based expert system for tech career guidance, built entirely in SWI-Prolog. The system matches users to careers based on three attribute types: **Skills** (e.g., Python, React), **Interests** (e.g., Data Science, Cybersecurity), and **Personality Traits** (e.g., Analytical, Creative). Each career is linked to attributes via weighted mappings expressed as Prolog facts. The inference engine uses Prolog's native backtracking and unification to compute confidence scores and generate explanations.

---

## Step-by-step Requirements

### 1. Project Structure

Create the following file structure inside `/home/shad/Projects/TechPath`:

```
main.pl               — entry point: loads all modules, starts HTTP server
knowledge_base.pl     — dynamic fact declarations and seed data
inference.pl          — inference engine rules (scoring, ranking, explanation)
server.pl             — HTTP server setup and JSON API route handlers
templates.pl          — HTML generation helpers (shared layout, components)
admin.pl              — admin route handlers (placeholder for Sprint 3)
persistence.pl        — save/load knowledge base to .pl file (Sprint 3)
static/
  css/custom.css      — custom CSS overrides
  js/wizard.js        — client-side JS for wizard interactivity
  js/admin.js         — client-side JS for admin panel (Sprint 3)
```

### 2. Knowledge Base (`knowledge_base.pl`)

Define the following dynamic predicates:

```prolog
:- dynamic career/4.
%% career(Id, Title, Description, Metadata)
%% Id: integer, Title: atom/string, Description: atom/string
%% Metadata: list of key-value pairs e.g. [salary-'$90K-$130K', demand-high, education-'Bachelor\'s']

:- dynamic attribute/4.
%% attribute(Id, Type, Name, Description)
%% Type: one of skill, interest, personality

:- dynamic career_attribute/3.
%% career_attribute(CareerID, AttributeID, Weight)
%% Weight: integer 1-10

:- dynamic user_account/4.
%% user_account(Id, Username, PasswordHash, Role)
%% Role: admin or user (placeholder for Sprint 3)
```

### 3. Seed Data

Populate the knowledge base with **at least 15 careers**, **at least 40 attributes**, and **comprehensive mappings**. All seed data should be asserted via a `seed_knowledge_base/0` predicate that first retracts all existing facts, then asserts all seed data.

**Careers (15+):**

| ID  | Title                     |
| --- | ------------------------- |
| 1   | Data Scientist            |
| 2   | Machine Learning Engineer |
| 3   | Frontend Developer        |
| 4   | Backend Developer         |
| 5   | Full-Stack Developer      |
| 6   | DevOps Engineer           |
| 7   | Cloud Architect           |
| 8   | Cybersecurity Analyst     |
| 9   | Mobile App Developer      |
| 10  | UX/UI Designer            |
| 11  | Site Reliability Engineer |
| 12  | Data Engineer             |
| 13  | Game Developer            |
| 14  | Blockchain Developer      |
| 15  | AI Research Scientist     |

Each career must have a detailed description (2-3 sentences) and meaningful metadata (salary range, demand level, education requirement).

**Attributes — Skills (IDs 1–15):**
Python, JavaScript, Java, C++, SQL, React, Node.js, Docker, Kubernetes, TensorFlow, AWS, Git, Figma, Solidity, Unity

**Attributes — Interests (IDs 16–26):**
Data Science, Web Development, Cloud Computing, Cybersecurity, Mobile Development, Artificial Intelligence, Game Development, Blockchain, User Experience, DevOps, Networking

**Attributes — Personality Traits (IDs 27–38):**
Analytical, Creative, Detail-Oriented, Collaborative, Independent, Problem-Solver, Communicative, Patient, Adaptable, Leadership, Curious, Methodical

Each attribute must have a brief description.

**Mappings (career_attribute/3):**
Every career must have **at least 6 mappings** spanning all three attribute types (at least 2 skills, 1 interest, 2 personality traits). Use realistic weights (1–10). Examples:

- Data Scientist: Python(10), SQL(7), TensorFlow(6), Data Science(10), AI(8), Analytical(9), Curious(6), Problem-Solver(7)
- Frontend Developer: JavaScript(10), React(9), Figma(5), Web Development(10), Creative(7), Detail-Oriented(8), Collaborative(5)
- Cybersecurity Analyst: Python(6), Java(4), Cybersecurity(10), Networking(8), Analytical(9), Detail-Oriented(9), Problem-Solver(8), Methodical(7)

### 4. Inference Engine (`inference.pl`)

Implement the following Prolog rules:

#### `analyze_profile(+UserAttributeIds, +TopN, -Results)`

Main entry point. Takes a list of attribute IDs and a count N, returns the top N career matches sorted by confidence descending.

#### `calculate_confidence(+CareerID, +UserAttrs, -Confidence, -Matched, -Missing)`

For a given career:

1. Retrieve all `career_attribute(CareerID, AttrID, Weight)` facts.
2. Sum all weights → `TotalPossibleWeight`.
3. For each mapping, check if `AttrID` is in `UserAttrs`. If yes, add to matched list and accumulate weight. If no, add to missing list.
4. `Confidence = (MatchedWeight / TotalPossibleWeight) * 100`, rounded to 1 decimal.

#### `generate_explanation(+Title, +MatchedAttrs, +Confidence, -Explanation)`

Produce a human-readable string. Group matched attributes by type and format, e.g.:

> "Recommended because your skills in Python and SQL, your interest in Data Science, and your Analytical personality align with the core requirements of a Data Scientist (confidence: 82.3%)."

#### Helper rules:

- `sort_results_desc/2` — sorts result list by confidence descending.
- `take_top_n/3` — takes first N elements from a list.
- `get_attributes_by_type/2` — retrieves all attributes of a given type.
- `get_career_attributes/2` — retrieves all mapped attributes for a career.

### 5. HTTP Server (`server.pl`)

Use SWI-Prolog's built-in HTTP libraries:

```prolog
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_cors)).
```

#### JSON API Endpoints:

| Method | Path                   | Description                                                        |
| ------ | ---------------------- | ------------------------------------------------------------------ |
| GET    | `/api/attributes`      | Returns all attributes grouped by type as JSON                     |
| GET    | `/api/attributes/Type` | Returns attributes of a specific type                              |
| GET    | `/api/careers`         | Returns all careers (id, title, description)                       |
| GET    | `/api/careers/Id`      | Returns single career with all mapped attributes                   |
| POST   | `/api/analyze`         | Accepts JSON `{"attributes": [1,3,7,...]}`, returns ranked results |

#### Admin API Endpoints (no auth required yet):

| Method | Path                       | Description                          |
| ------ | -------------------------- | ------------------------------------ |
| POST   | `/api/admin/careers`       | Create a new career (assert facts)   |
| DELETE | `/api/admin/careers/Id`    | Delete career and its mappings       |
| POST   | `/api/admin/attributes`    | Create a new attribute               |
| DELETE | `/api/admin/attributes/Id` | Delete an attribute and its mappings |
| POST   | `/api/admin/mappings`      | Create a career-attribute mapping    |
| DELETE | `/api/admin/mappings`      | Delete a mapping                     |

### 6. Entry Point (`main.pl`)

- Load all modules.
- Call `seed_knowledge_base/0` to populate facts.
- Start the HTTP server on port 5000.
- Print: `"TechPath Advisor running at http://localhost:5000"`.

Usage: `swipl main.pl`

### 7. Basic Landing Page

Serve a simple HTML page at `/` confirming the server is running, with links to test API endpoints.

---

## Verification Checklist

- [ ] `swipl main.pl` starts the server on port 5000 with no errors.
- [ ] `GET http://localhost:5000/` returns an HTML page confirming the server is running.
- [ ] `GET http://localhost:5000/api/attributes` returns 40+ attributes grouped by type as JSON.
- [ ] `GET http://localhost:5000/api/attributes/skill` returns only skill attributes.
- [ ] `GET http://localhost:5000/api/careers` returns 15+ careers as JSON.
- [ ] `GET http://localhost:5000/api/careers/1` returns a single career with its mapped attributes.
- [ ] `POST http://localhost:5000/api/analyze` with body `{"attributes": [1, 5, 16, 21, 27, 32]}` returns ranked careers with confidence scores, matched/missing attributes, and explanation strings.
- [ ] Admin CRUD endpoints work correctly.
- [ ] The inference engine correctly computes weighted confidence scores.
- [ ] Explanation strings are human-readable and reference specific user attributes.
