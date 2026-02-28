# Sprint 3 — Admin Panel, Authentication, Persistence & Polish

## Goal

Complete TechPath Advisor by building a web-based admin panel for managing the knowledge base, implementing basic authentication, adding persistent storage of the knowledge base to .pl files, and polishing the UI.

## Prerequisites

Sprints 1 and 2 must be complete: HTTP server running, all API endpoints functional, user-facing wizard and results pages working.

---

## Step-by-step Requirements

### 1. Authentication

#### 1.1 Login System

- Implement `POST /api/auth/login` accepting `{"username": "...", "password": "..."}`.
- Validate against `user_account/4` facts. Use `crypto_password_hash/2` from SWI-Prolog's `library(crypto)` for password hashing.
- On success, return a session token (simple random token stored as a dynamic fact `session/3` — token, username, timestamp).
- Seed a default admin: username `admin`, password `admin123`.

#### 1.2 Auth Middleware

- Create a `require_admin/1` predicate that checks the `Authorization` header for a valid session token.
- Apply to all `/api/admin/*` endpoints.
- Return 401 JSON error if missing/invalid.

### 2. Admin Panel Web Pages

#### 2.1 Login Page (`/login`)

- DaisyUI form with username/password fields and a `btn btn-primary` "Login" button.
- Client-side JS handles the POST and stores the token in localStorage.
- On success, redirect to `/admin`.

#### 2.2 Admin Dashboard (`/admin`)

- Summary cards showing: total careers, total attributes (by type), total mappings.
- Quick action DaisyUI buttons: "Add Career", "Add Attribute".
- DaisyUI `navbar` with sidebar links: Dashboard, Careers, Attributes, Mappings, Logout.

#### 2.3 Careers Management (`/admin/careers`)

- DaisyUI `table` listing all careers with ID, Title, Mapping Count, Actions (Edit, Delete).
- "Add Career" button opens a DaisyUI `modal` with form fields: Title, Description, Salary, Demand, Education.
- Edit opens the same modal pre-filled.
- Delete shows a DaisyUI `modal` confirmation dialog.
- All CRUD via the admin JSON API with token in Authorization header.

#### 2.4 Attributes Management (`/admin/attributes`)

- DaisyUI `tabs` to filter by Skill / Interest / Personality.
- DaisyUI `table` with ID, Name, Type, Description, Actions.
- Add/Edit via `modal`.
- Delete with confirmation.

#### 2.5 Mappings Management (`/admin/mappings`)

- Dropdown to select a career, then shows its mappings in a DaisyUI `table`.
- Add mapping: select attribute dropdown + weight `range` slider (1-10).
- Edit weight inline or via modal.
- Remove with confirmation.

### 3. Persistence (`persistence.pl`)

#### 3.1 Save Knowledge Base

- `save_knowledge_base/0` writes all current `career/4`, `attribute/4`, and `career_attribute/3` facts to `data/kb_snapshot.pl` using `tell/1` and `listing/1` (or formatted writes).
- Called automatically after every admin CRUD operation.

#### 3.2 Load Knowledge Base

- `load_knowledge_base/0` checks if `data/kb_snapshot.pl` exists. If yes, consult it instead of running the seeder.
- On first run (no snapshot), run the seeder and save a snapshot.
- Modify `main.pl` to use this logic.

### 4. UI Polish

#### 4.1 Toast Notifications

- Client-side JS toast system using DaisyUI `alert` components.
- Success/error/info toasts for all CRUD operations.

#### 4.2 Animations

- CSS transitions on cards and page elements (hover effects, fade-ins).
- Wizard step transitions.

#### 4.3 DaisyUI Theme Toggle

- Sun/moon toggle in navbar switching DaisyUI's `data-theme` between `light` and `dark` (or other DaisyUI themes like `cupcake`/`dracula`).
- Persist choice in localStorage.

#### 4.4 Loading States

- DaisyUI `skeleton` classes for table rows and cards while data loads.

### 5. Error Handling

#### Backend

- Consistent JSON error responses: `{"error": "message"}` with appropriate HTTP status codes.
- 404 handler for undefined routes.
- Input validation on all POST endpoints.

#### Frontend (JS)

- Fetch error handling: show DaisyUI `alert alert-error` toast on network failures.
- 401 responses: clear token, redirect to login.
- 403 responses: show "Permission denied" toast.

### 6. Final Verification

- Review seed data: every career has 6+ diverse mappings with varied weights.
- Test inference with multiple profile combinations: pure data profile, pure web profile, mixed profile — all should produce differentiated, sensible results.
- Confirm admin changes are reflected in subsequent user assessments.

---

## Verification Checklist

### Authentication

- [ ] `POST /api/auth/login` with `{"username": "admin", "password": "admin123"}` returns a token.
- [ ] Admin API routes return 401 without token.
- [ ] Login page works end-to-end.
- [ ] Logout clears session.

### Admin Panel

- [ ] Dashboard shows correct counts.
- [ ] Can create, edit, and delete careers.
- [ ] Can create, edit, and delete attributes.
- [ ] Can add, edit weight, and remove mappings.
- [ ] **After adding a new career with mappings via admin, a new profile analysis includes it.**

### Persistence

- [ ] Knowledge base is saved to `data/kb_snapshot.pl` after admin changes.
- [ ] Restarting the server loads the snapshot instead of re-seeding.
- [ ] Admin changes survive server restarts.

### Polish

- [ ] Toast notifications for all CRUD operations.
- [ ] Theme toggle switches between light and dark.
- [ ] Loading states use skeleton loaders.
- [ ] All error states handled gracefully.

### End-to-End

- [ ] User: Home → Wizard → Steps 1-4 → Results with scores & explanations → Career detail.
- [ ] Admin: Login → Dashboard → Manage careers/attributes/mappings → Changes reflected in user flow.
- [ ] Server starts cleanly with `swipl main.pl`, serves both API and web pages on port 5000.
