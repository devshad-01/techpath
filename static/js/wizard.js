// ============================================================================
// TechPath Advisor — Wizard JavaScript
// Multi-step profile wizard with API integration and results rendering.
// ============================================================================

console.log('TechPath Advisor — Wizard JS loaded');

// ── State ──────────────────────────────────────────────────────────────────
let currentStep = 1;
const totalSteps = 4;
const selected = { interest: new Set(), skill: new Set(), personality: new Set() };
let attributeCache = {};

const stepNames = {
    1: 'Select Interests',
    2: 'Select Skills',
    3: 'Describe Personality',
    4: 'Review & Submit'
};

// ── Init ───────────────────────────────────────────────────────────────────
document.addEventListener('DOMContentLoaded', () => {
    loadAttributes('interest',    'interests-grid',   renderInterestCard);
    loadAttributes('skill',       'skills-grid',      renderSkillBadge);
    loadAttributes('personality', 'personality-grid',  renderPersonalityCard);
});

// ── Fetch & render attributes ──────────────────────────────────────────────
async function loadAttributes(type, containerId, renderer) {
    const container = document.getElementById(containerId);
    try {
        const res  = await fetch(`/api/attributes/${type}`);
        const json = await res.json();
        // API returns { attributes: [...] }
        const data = json.attributes || json;
        container.innerHTML = '';
        if (!data || data.length === 0) {
            container.innerHTML = '<p class="col-span-full text-center opacity-50 py-8">No attributes found.</p>';
            return;
        }
        data.forEach(attr => {
            attributeCache[attr.id] = attr;
            container.insertAdjacentHTML('beforeend', renderer(attr));
        });
    } catch (e) {
        console.error(`Failed to load ${type}:`, e);
        container.innerHTML = `
            <div class="col-span-full alert alert-error">
                <span>Failed to load ${type}s. Please refresh the page.</span>
            </div>`;
    }
}

// ── Card / Badge renderers (Material Design) ──────────────────────────────
function renderInterestCard(attr) {
    return `
    <div class="selection-card" id="attr-${attr.id}" onclick="toggleAttr(${attr.id}, 'interest')">
        <div class="flex items-start gap-3">
            <div class="check-circle" id="check-${attr.id}"></div>
            <div>
                <div class="font-medium text-sm text-gray-800">${attr.name}</div>
                ${attr.description ? `<div class="text-xs text-gray-400 mt-1 leading-relaxed">${attr.description}</div>` : ''}
            </div>
        </div>
    </div>`;
}

function renderSkillBadge(attr) {
    return `
    <button class="skill-chip" id="attr-${attr.id}" onclick="toggleAttr(${attr.id}, 'skill')">
        ${attr.name}
    </button>`;
}

function renderPersonalityCard(attr) {
    return `
    <div class="selection-card" id="attr-${attr.id}" onclick="toggleAttr(${attr.id}, 'personality')">
        <div class="flex items-start gap-3">
            <div class="check-circle" id="check-${attr.id}"></div>
            <div>
                <div class="font-medium text-sm text-gray-800">${attr.name}</div>
                ${attr.description ? `<div class="text-xs text-gray-400 mt-1 leading-relaxed">${attr.description}</div>` : ''}
            </div>
        </div>
    </div>`;
}

// ── Toggle selection (Material Design) ─────────────────────────────────────
function toggleAttr(id, type) {
    const el    = document.getElementById(`attr-${id}`);
    const check = document.getElementById(`check-${id}`);
    if (!el) return;

    if (selected[type].has(id)) {
        selected[type].delete(id);
        el.classList.remove('selected', 'selected-secondary');
        if (check) check.innerHTML = '';
    } else {
        selected[type].add(id);
        const cls = type === 'personality' ? 'selected-secondary' : 'selected';
        el.classList.add(cls);
        if (check) check.innerHTML = '<svg class="w-3 h-3 text-white" fill="currentColor" viewBox="0 0 20 20"><path fill-rule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clip-rule="evenodd"/></svg>';
    }
    updateSelectionCount(type);
}

function updateSelectionCount(type) {
    const stepMap = { interest: 1, skill: 2, personality: 3 };
    const step = stepMap[type];
    const count = selected[type].size;
    const label = document.querySelector(`#step-${step} .selection-count`);
    if (label) {
        label.textContent = count > 0 ? `${count} selected` : '';
        label.classList.toggle('hidden', count === 0);
    }
}

// ── Step navigation ────────────────────────────────────────────────────────
function nextStep(from) {
    const types = { 1: 'interest', 2: 'skill', 3: 'personality' };
    const errId = `step${from}-error`;
    if (selected[types[from]] && selected[types[from]].size === 0) {
        document.getElementById(errId)?.classList.remove('hidden');
        document.getElementById(errId)?.scrollIntoView({ behavior: 'smooth', block: 'center' });
        return;
    }
    document.getElementById(errId)?.classList.add('hidden');
    if (from === 3) populateReview();
    showStep(from + 1);
}

function prevStep(from) {
    showStep(from - 1);
}

function showStep(step) {
    // Fade out current, show next
    document.querySelectorAll('.wizard-step').forEach(el => {
        el.classList.add('hidden');
        el.classList.remove('animate-fadeIn');
    });
    const next = document.getElementById(`step-${step}`);
    if (next) {
        next.classList.remove('hidden');
        next.classList.add('animate-fadeIn');
    }

    // Update progress bar
    const pct = (step / totalSteps) * 100;
    const bar = document.getElementById('progress-bar');
    if (bar) bar.style.width = `${pct}%`;

    // Update step label
    const label = document.getElementById('step-label');
    if (label) label.textContent = `Step ${step} of ${totalSteps}`;
    const name = document.getElementById('step-name');
    if (name) name.textContent = stepNames[step] || '';

    // Update step indicators (Material stepper)
    for (let i = 1; i <= totalSteps; i++) {
        const ind = document.getElementById(`step-ind-${i}`);
        if (!ind) continue;
        ind.classList.remove('active', 'completed');
        if (i < step)  ind.classList.add('completed');
        if (i === step) ind.classList.add('active');
    }

    currentStep = step;
    window.scrollTo({ top: 0, behavior: 'smooth' });
}

// ── Skill search ───────────────────────────────────────────────────────────
function filterSkills() {
    const q = document.getElementById('skill-search').value.toLowerCase();
    document.querySelectorAll('#skills-grid > button').forEach(el => {
        el.style.display = el.textContent.toLowerCase().includes(q) ? '' : 'none';
    });
}

// ── Review step ────────────────────────────────────────────────────────────
function populateReview() {
    renderReviewBadges('review-interests',   'interest');
    renderReviewBadges('review-skills',      'skill');
    renderReviewBadges('review-personality', 'personality');
}

function renderReviewBadges(containerId, type) {
    const c = document.getElementById(containerId);
    if (!c) return;
    c.innerHTML = '';
    const colorMap = {
        interest:    'background: rgba(88,64,83,0.1); color: #584053; border: 1px solid rgba(88,64,83,0.2);',
        skill:       'background: rgba(141,198,191,0.15); color: #4a8f87; border: 1px solid rgba(141,198,191,0.3);',
        personality: 'background: rgba(252,188,102,0.15); color: #a07830; border: 1px solid rgba(252,188,102,0.3);'
    };
    selected[type].forEach(id => {
        const attr = attributeCache[id];
        if (!attr) return;
        c.insertAdjacentHTML('beforeend', `
        <span class="inline-flex items-center gap-2 px-3 py-1.5 rounded-lg text-sm font-medium" style="${colorMap[type]}">
            ${attr.name}
            <button class="hover:opacity-60 transition-opacity text-xs" onclick="removeFromReview(${id},'${type}')" style="line-height:1;">&#10005;</button>
        </span>`);
    });
    if (selected[type].size === 0) {
        c.innerHTML = '<span class="text-sm text-gray-300 italic">None selected</span>';
    }
}

function removeFromReview(id, type) {
    toggleAttr(id, type);
    populateReview();
}

// ── Submit ─────────────────────────────────────────────────────────────────
async function submitProfile() {
    const allIds = [...selected.interest, ...selected.skill, ...selected.personality];
    if (allIds.length === 0) {
        alert('Please select at least one attribute.');
        return;
    }

    document.getElementById('loading-overlay')?.classList.remove('hidden');
    const btn = document.getElementById('submit-btn');
    if (btn) btn.disabled = true;

    try {
        const res = await fetch('/api/analyze', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ attributes: allIds, top_n: 5 })
        });
        const json = await res.json();
        // API returns { results: [...] }
        const data = json.results || json;
        renderResults(data);
    } catch (e) {
        console.error('Analysis failed:', e);
        alert('Something went wrong. Please try again.');
    } finally {
        document.getElementById('loading-overlay')?.classList.add('hidden');
        if (btn) btn.disabled = false;
    }
}

// ── Results rendering (Material Design) ────────────────────────────────────
function renderResults(results) {
    // Hide wizard UI
    document.querySelectorAll('.wizard-step').forEach(el => el.classList.add('hidden'));
    const progressArea = document.querySelector('#steps-indicator')?.closest('.bg-white');
    if (progressArea) progressArea.classList.add('hidden');

    const container = document.getElementById('results-container');
    container.classList.remove('hidden');

    if (!results || results.length === 0) {
        container.innerHTML = `
        <div class="text-center py-16">
            <span class="material-icons-outlined" style="font-size:64px; color: #ccc;">search_off</span>
            <h2 class="text-2xl font-bold text-gray-800 mt-4 mb-2">No Matches Found</h2>
            <p class="text-gray-400 mb-6">Try selecting more attributes for better results.</p>
            <button class="btn btn-primary" onclick="location.reload()">Try Again</button>
        </div>`;
        return;
    }

    const topMatch = results[0];
    const totalAttrs = [...selected.interest].length + [...selected.skill].length + [...selected.personality].length;

    let html = `
    <!-- Results header -->
    <div class="text-center mb-10 pt-4">
        <span class="inline-flex items-center gap-1.5 px-3 py-1 rounded-full text-xs font-semibold uppercase tracking-wider" style="background: rgba(141,198,191,0.15); color: #5fa8a0;">
            <span class="material-icons-outlined" style="font-size:14px">insights</span>
            Your Results
        </span>
        <h2 class="text-3xl font-bold text-gray-900 mt-3 mb-1">Career Recommendations</h2>
        <p class="text-gray-400">Based on ${totalAttrs} profile attributes</p>
    </div>

    <!-- Top match card -->
    <div class="md-card p-0 mb-8">
        <div style="background: linear-gradient(135deg, #584053 0%, #3d2c3a 100%);" class="p-8 text-center rounded-t-xl">
            <span class="material-icons-outlined text-white/60" style="font-size:40px">emoji_events</span>
            <div class="text-xs uppercase tracking-widest text-white/50 font-semibold mt-2">Best Match</div>
            <h3 class="text-2xl font-bold text-white mt-1">${topMatch.title || topMatch.career || 'Unknown'}</h3>
        </div>
        <div class="p-8 text-center">
            <div class="flex justify-center mb-4">
                <div class="radial-progress text-primary" style="--value:${topMatch.confidence.toFixed(0)};--size:5rem;--thickness:4px;" role="progressbar">
                    ${topMatch.confidence.toFixed(1)}%
                </div>
            </div>
            <p class="text-gray-500 max-w-xl mx-auto text-sm leading-relaxed">${topMatch.explanation || ''}</p>
            ${topMatch.metadata ? renderMetadataChips(topMatch.metadata) : ''}
        </div>
    </div>`;

    // Other matches
    if (results.length > 1) {
        html += `<h3 class="text-sm font-semibold uppercase tracking-wider text-gray-400 mb-4">Other Matches</h3>`;
        html += '<div class="space-y-3">';
        results.slice(1).forEach((r, i) => {
            const pct = r.confidence.toFixed(1);
            const rank = i + 2;
            const barColor = pct >= 60 ? '#584053' : pct >= 35 ? '#FCBC66' : '#F97B4F';
            html += `
            <div class="collapse collapse-arrow bg-white border border-gray-100 rounded-xl result-item">
                <input type="radio" name="result-accordion" />
                <div class="collapse-title">
                    <div class="flex items-center justify-between pr-4">
                        <div class="flex items-center gap-3">
                            <span class="w-7 h-7 rounded-full flex items-center justify-center text-xs font-bold text-white" style="background: ${barColor};">${rank}</span>
                            <span class="font-semibold text-gray-800">${r.title || r.career || 'Unknown'}</span>
                        </div>
                        <div class="flex items-center gap-3">
                            <div class="w-24 h-1.5 bg-gray-100 rounded-full hidden sm:block overflow-hidden">
                                <div class="h-full rounded-full" style="width:${pct}%; background:${barColor};"></div>
                            </div>
                            <span class="text-sm font-semibold" style="color:${barColor};">${pct}%</span>
                        </div>
                    </div>
                </div>
                <div class="collapse-content">
                    <p class="text-gray-500 text-sm leading-relaxed mb-3">${r.explanation || 'No detailed explanation available.'}</p>
                    ${r.metadata ? renderMetadataChips(r.metadata) : ''}
                </div>
            </div>`;
        });
        html += '</div>';
    }

    html += `
    <div class="flex flex-col sm:flex-row gap-3 justify-center mt-10 mb-6">
        <button class="btn btn-primary gap-2" onclick="location.reload()">
            <span class="material-icons-outlined" style="font-size:18px">refresh</span>
            New Assessment
        </button>
        <button class="btn btn-ghost gap-2" onclick="window.print()">
            <span class="material-icons-outlined" style="font-size:18px">print</span>
            Print Results
        </button>
    </div>`;

    container.innerHTML = html;
    container.scrollIntoView({ behavior: 'smooth' });
}

function renderMetadataChips(meta) {
    if (!meta) return '';
    let chips = '';
    if (meta.salary_range) chips += `<span class="inline-flex items-center gap-1 px-3 py-1 rounded-lg text-xs font-medium bg-gray-50 text-gray-500 border border-gray-100"><span class="material-icons-outlined" style="font-size:14px">payments</span> ${meta.salary_range}</span>`;
    if (meta.demand_level) chips += `<span class="inline-flex items-center gap-1 px-3 py-1 rounded-lg text-xs font-medium bg-gray-50 text-gray-500 border border-gray-100"><span class="material-icons-outlined" style="font-size:14px">trending_up</span> ${meta.demand_level}</span>`;
    if (meta.education)    chips += `<span class="inline-flex items-center gap-1 px-3 py-1 rounded-lg text-xs font-medium bg-gray-50 text-gray-500 border border-gray-100"><span class="material-icons-outlined" style="font-size:14px">school</span> ${meta.education}</span>`;
    return `<div class="flex flex-wrap gap-2 justify-center mt-4">${chips}</div>`;
}
