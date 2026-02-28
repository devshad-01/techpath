# TechPath Advisor

An expert system for personalized tech career guidance, built entirely in **SWI-Prolog**. It uses a rule-based inference engine with weighted attribute scoring to match user profiles against a curated knowledge base of 15 tech career paths.

![SWI-Prolog](https://img.shields.io/badge/SWI--Prolog-9.0+-blue?style=flat-square)
![License](https://img.shields.io/badge/license-MIT-green?style=flat-square)
![Status](https://img.shields.io/badge/status-active-brightgreen?style=flat-square)

---

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Tech Stack](#tech-stack)
- [Project Structure](#project-structure)
- [Getting Started](#getting-started)
- [Usage](#usage)
- [API Reference](#api-reference)
- [Knowledge Base](#knowledge-base)
- [Inference Engine](#inference-engine)
- [Color Palette](#color-palette)
- [Sprint Plan](#sprint-plan)
- [License](#license)

---

## Overview

TechPath Advisor analyzes a user's skills, interests, and personality traits to recommend the most suitable tech career paths. The system is powered by a Prolog inference engine that performs weighted confidence scoring across 38 attributes and 145 expert-curated mappings.

Users interact with a 4-step wizard:

1. **Interests** — Select technology domains that fascinate you
2. **Skills** — Choose programming languages, tools, and frameworks you know
3. **Personality** — Identify work-style traits that describe you
4. **Results** — Get ranked career recommendations with confidence scores and explanations

---

## Features

- **Rule-based expert system** — Prolog backtracking and unification for career matching
- **Weighted confidence scoring** — Attributes weighted 1–10 across skills, interests, and personality
- **Human-readable explanations** — Auto-generated text explaining why each career matched
- **4-step profile wizard** — Guided assessment with progress tracking
- **Admin CRUD panel** — Manage careers, attributes, and mappings via web UI
- **Material Design UI** — Clean, professional interface with custom theme
- **RESTful JSON API** — CORS-enabled endpoints for programmatic access
- **No external database** — All data stored as dynamic Prolog facts
- **Self-contained** — Single command starts the entire application

---

## Tech Stack

| Layer         | Technology                                             |
| ------------- | ------------------------------------------------------ |
| Backend       | SWI-Prolog (`thread_httpd`, `http_json`, `html_write`) |
| Frontend CSS  | Tailwind CSS + DaisyUI (CDN)                           |
| Frontend JS   | Vanilla JavaScript                                     |
| Design System | Material Design 3 inspired custom CSS                  |
| Icons         | Google Material Icons Outlined                         |
| Fonts         | Inter (Google Fonts)                                   |

---

## Project Structure

```
TechPath/
├── main.pl                 # Entry point — loads modules, seeds KB, starts server
├── knowledge_base.pl       # Dynamic facts: careers, attributes, mappings
├── inference.pl            # Inference engine: analyze_profile/3, confidence scoring
├── server.pl               # HTTP server, all API routes, JSON handlers
├── templates.pl            # Server-side HTML generation (landing, wizard, admin)
├── admin.pl                # Admin module (placeholder for future auth)
├── persistence.pl          # Persistence module (placeholder for snapshot/restore)
├── .gitignore
├── data/                   # Reserved for knowledge base snapshots
├── PRD/
│   ├── README.md           # Project requirements overview
│   ├── SPRINT_1.md         # Sprint 1: Foundation
│   ├── SPRINT_2.md         # Sprint 2: UX & Wizard
│   └── SPRINT_3.md         # Sprint 3: Admin & Polish
└── static/
    ├── favicon.svg         # Custom SVG favicon
    ├── css/
    │   └── custom.css      # Material Design custom styles
    └── js/
        ├── wizard.js       # Multi-step wizard logic & results rendering
        └── admin.js        # Admin panel CRUD operations
```

---

## Getting Started

### Prerequisites

- **SWI-Prolog 9.0+** — [Download](https://www.swi-prolog.org/download/stable)

### Installation

```bash
# Clone the repository
git clone https://github.com/devshad-01/techpath.git
cd techpath

# Start the server
swipl main.pl
```

The application will be available at **http://localhost:5000**.

### Verify

```bash
curl http://localhost:5000/api/careers
```

---

## Usage

### Career Assessment

1. Navigate to **http://localhost:5000**
2. Click **Start Free Assessment**
3. Complete the 4-step wizard (Interests → Skills → Personality → Review)
4. View your ranked career recommendations with confidence percentages

### Admin Panel

Navigate to **http://localhost:5000/admin** to:

- **Careers tab** — Add or delete career paths
- **Attributes tab** — Add or delete skills, interests, and personality traits (with type filters)
- **Mappings tab** — Add or delete career-attribute mappings with weights (1–10)

---

## API Reference

### Public Endpoints

| Method | Endpoint                | Description                                     |
| ------ | ----------------------- | ----------------------------------------------- |
| GET    | `/api/careers`          | List all careers                                |
| GET    | `/api/careers/:id`      | Get career details with attributes              |
| GET    | `/api/attributes`       | List all attributes grouped by type             |
| GET    | `/api/attributes/:type` | Filter by `skill`, `interest`, or `personality` |
| POST   | `/api/analyze`          | Analyze profile and get recommendations         |

### Analyze Request

```bash
curl -X POST http://localhost:5000/api/analyze \
  -H "Content-Type: application/json" \
  -d '{"attributes": [1, 3, 16, 27], "top_n": 5}'
```

**Response:**

```json
{
  "results": [
    {
      "id": 1,
      "title": "Data Scientist",
      "confidence": 72.5,
      "explanation": "Strong match based on skills: Python, Statistics; interests: Data Analysis; personality: Analytical Thinking",
      "metadata": {
        "salary_range": "$95,000 - $150,000",
        "demand_level": "Very High",
        "education": "Bachelor's/Master's"
      }
    }
  ]
}
```

### Admin Endpoints

| Method | Endpoint                    | Description                                                  |
| ------ | --------------------------- | ------------------------------------------------------------ |
| POST   | `/api/admin/careers`        | Create a career                                              |
| DELETE | `/api/admin/careers/:id`    | Delete a career                                              |
| POST   | `/api/admin/attributes`     | Create an attribute                                          |
| DELETE | `/api/admin/attributes/:id` | Delete an attribute                                          |
| POST   | `/api/admin/mappings`       | Create a mapping                                             |
| DELETE | `/api/admin/mappings`       | Delete a mapping (query params: `career_id`, `attribute_id`) |

---

## Knowledge Base

The system ships with a pre-seeded knowledge base:

| Fact Type  | Count | Examples                                                    |
| ---------- | ----- | ----------------------------------------------------------- |
| Careers    | 15    | Data Scientist, DevOps Engineer, Cybersecurity Analyst, ... |
| Attributes | 38    | 15 skills, 11 interests, 12 personality traits              |
| Mappings   | 145   | Weighted connections between careers and attributes         |

### Career Paths

Data Scientist, Machine Learning Engineer, Frontend Developer, Backend Developer, Full-Stack Developer, DevOps Engineer, Cloud Architect, Cybersecurity Analyst, Mobile App Developer, UX/UI Designer, Site Reliability Engineer, Data Engineer, Game Developer, Blockchain Developer, AI Research Scientist

### Attribute Types

- **Skills** (15) — Python, JavaScript, SQL, Java, Docker, AWS, React, etc.
- **Interests** (11) — Data Analysis, Web Development, Cloud Computing, Cybersecurity, etc.
- **Personality** (12) — Analytical Thinking, Creative Problem Solving, Detail-Oriented, etc.

---

## Inference Engine

The inference engine (`inference.pl`) uses a weighted scoring approach:

```
confidence = (matched_weight / total_possible_weight) × 100
```

### Key Predicates

| Predicate                | Description                                                            |
| ------------------------ | ---------------------------------------------------------------------- |
| `analyze_profile/3`      | Main entry point — takes attribute IDs + top_n, returns ranked matches |
| `calculate_confidence/5` | Computes weighted confidence score for a career                        |
| `generate_explanation/4` | Produces human-readable match explanation                              |

### How It Works

1. User submits a list of attribute IDs (skills + interests + personality)
2. For each career, the engine finds overlapping attributes
3. Matched attribute weights are summed and divided by total possible weight
4. Careers are ranked by confidence percentage (descending)
5. An explanation is generated grouping matched attributes by type

---

## Color Palette

| Color | Hex       | Role                     |
| ----- | --------- | ------------------------ |
| Plum  | `#584053` | Primary — headings, CTAs |
| Teal  | `#8DC6BF` | Secondary — accents      |
| Gold  | `#FCBC66` | Accent — highlights      |
| Coral | `#F97B4F` | Error — warnings         |

---

## Sprint Plan

| Sprint | Focus                                  | Status      |
| ------ | -------------------------------------- | ----------- |
| 1      | Foundation — KB, inference engine, API | ✅ Complete |
| 2      | UX — Landing page, wizard, results     | ✅ Complete |
| 3      | Admin panel, persistence, polish       | ✅ Complete |

Detailed sprint documents available in the [`PRD/`](PRD/) directory.

---

## License

This project is developed as an academic exercise for expert systems coursework.
