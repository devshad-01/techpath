# TechPath Advisor — Product Requirements Document

## Overview

**TechPath Advisor** is a web-based expert system that emulates a human career counselor to recommend technology career paths. It uses a rule-based inference engine built entirely in **SWI-Prolog**, operating on a knowledge base of Prolog facts and rules. It matches users based on their technical skills, domain interests, and personality traits.

## Architecture Summary

| Layer                 | Role                                                 | Technology                                          |
| --------------------- | ---------------------------------------------------- | --------------------------------------------------- |
| Presentation (Client) | User & Admin web UI                                  | HTML + DaisyUI (CDN) + vanilla JS, served by Prolog |
| Application (Server)  | HTTP server, inference engine, route handlers, auth  | SWI-Prolog (http, json, html_write libraries)       |
| Data (Knowledge Base) | Careers, attributes, mappings stored as Prolog facts | Dynamic Prolog facts, persisted to .pl files        |

## Sprint Plan

The project is divided into **3 sprints**, each building on the previous:

| Sprint                    | Focus                                                          | Deliverable                                                                                                         |
| ------------------------- | -------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| [Sprint 1](./SPRINT_1.md) | Foundation — Knowledge base, inference engine, HTTP server/API | Working Prolog server with seeded knowledge base, inference engine returning JSON career recommendations            |
| [Sprint 2](./SPRINT_2.md) | User Experience — Web profile wizard & results dashboard       | Full user-facing flow: interest/skill/personality selection → personalized career recommendations with explanations |
| [Sprint 3](./SPRINT_3.md) | Admin Panel, Persistence & Polish                              | Web-based admin CRUD for knowledge base, auth, persistent storage, error handling                                   |

## How to Use

1. Open the sprint file (e.g., `SPRINT_1.md`)
2. Copy the entire content as a prompt
3. Paste it into the AI assistant to execute that sprint
4. Verify the deliverables listed at the bottom of each sprint
5. Move to the next sprint
