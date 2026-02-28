// ============================================================================
// TechPath Advisor — Admin Panel JavaScript
// CRUD operations for careers, attributes, and mappings.
// ============================================================================

console.log('TechPath Advisor — Admin JS loaded');

let allCareers = [];
let allAttrs = [];

// ── Init ───────────────────────────────────────────────────────────────────
document.addEventListener('DOMContentLoaded', () => {
    loadCareers();
    loadAttributes();
    loadMappings();
});

// ── Tab switching (Material tabs) ──────────────────────────────────────────
function showTab(name) {
    document.querySelectorAll('.admin-tab').forEach(el => el.classList.add('hidden'));
    document.getElementById(`tab-${name}`).classList.remove('hidden');
    document.querySelectorAll('#admin-tabs .md-tab').forEach(el => el.classList.remove('active'));
    event.target.classList.add('active');
}

// ── Modal helpers ──────────────────────────────────────────────────────────
function openCareerModal() { document.getElementById('career-modal').showModal(); }
function openAttrModal()   { document.getElementById('attr-modal').showModal(); }
function openMappingModal() {
    populateSelects();
    document.getElementById('mapping-modal').showModal();
}
function closeModal(id) { document.getElementById(id).close(); }

// ── CAREERS ────────────────────────────────────────────────────────────────
async function loadCareers() {
    try {
        const res = await fetch('/api/careers');
        const json = await res.json();
        allCareers = json.careers || json;
        renderCareers();
    } catch (e) { console.error('Failed to load careers:', e); }
}

function renderCareers() {
    const tbody = document.getElementById('careers-tbody');
    if (!allCareers.length) {
        tbody.innerHTML = '<tr><td colspan="4" class="text-center py-10 text-gray-400">No careers found.</td></tr>';
        return;
    }
    tbody.innerHTML = allCareers.map(c => `
        <tr>
            <td class="font-mono text-xs text-gray-400">${c.id}</td>
            <td class="font-medium text-gray-800">${c.title}</td>
            <td class="text-sm text-gray-500 max-w-xs truncate">${c.description || ''}</td>
            <td class="text-right">
                <button class="inline-flex items-center gap-1 px-3 py-1.5 rounded-lg text-xs font-medium text-red-500 hover:bg-red-50 transition-colors" onclick="deleteCareer(${c.id})">
                    <span class="material-icons-outlined" style="font-size:16px">delete</span> Delete
                </button>
            </td>
        </tr>`).join('');
}

async function addCareer() {
    const title = document.getElementById('career-title').value.trim();
    const desc  = document.getElementById('career-desc').value.trim();
    if (!title) { alert('Title is required'); return; }
    try {
        await fetch('/api/admin/careers', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ title, description: desc })
        });
        closeModal('career-modal');
        document.getElementById('career-title').value = '';
        document.getElementById('career-desc').value = '';
        loadCareers();
    } catch (e) { alert('Failed to add career'); }
}

async function deleteCareer(id) {
    if (!confirm('Delete this career?')) return;
    try {
        await fetch(`/api/admin/careers/${id}`, { method: 'DELETE' });
        loadCareers();
        loadMappings();
    } catch (e) { alert('Failed to delete'); }
}

// ── ATTRIBUTES ─────────────────────────────────────────────────────────────
async function loadAttributes() {
    try {
        const res = await fetch('/api/attributes');
        const json = await res.json();
        allAttrs = json.attributes || json;
        renderAttrs(allAttrs);
    } catch (e) { console.error('Failed to load attributes:', e); }
}

function renderAttrs(attrs) {
    const tbody = document.getElementById('attrs-tbody');
    if (!attrs.length) {
        tbody.innerHTML = '<tr><td colspan="5" class="text-center py-10 text-gray-400">No attributes found.</td></tr>';
        return;
    }
    const typeColors = {
        skill:       'background: rgba(88,64,83,0.1); color: #584053;',
        interest:    'background: rgba(141,198,191,0.15); color: #4a8f87;',
        personality: 'background: rgba(252,188,102,0.15); color: #a07830;'
    };
    tbody.innerHTML = attrs.map(a => `
        <tr>
            <td class="font-mono text-xs text-gray-400">${a.id}</td>
            <td class="font-medium text-gray-800">${a.name}</td>
            <td><span class="inline-block px-2.5 py-1 rounded-md text-xs font-medium" style="${typeColors[a.type] || ''}">${a.type}</span></td>
            <td class="text-sm text-gray-500 max-w-xs truncate">${a.description || ''}</td>
            <td class="text-right">
                <button class="inline-flex items-center gap-1 px-3 py-1.5 rounded-lg text-xs font-medium text-red-500 hover:bg-red-50 transition-colors" onclick="deleteAttr(${a.id})">
                    <span class="material-icons-outlined" style="font-size:16px">delete</span> Delete
                </button>
            </td>
        </tr>`).join('');
}

function filterAttrs(type) {
    document.querySelectorAll('#tab-attributes .md-chip').forEach(b => b.classList.remove('active'));
    event.target.classList.add('active');
    if (type === 'all') renderAttrs(allAttrs);
    else renderAttrs(allAttrs.filter(a => a.type === type));
}

async function addAttr() {
    const name = document.getElementById('attr-name').value.trim();
    const type = document.getElementById('attr-type').value;
    const desc = document.getElementById('attr-desc').value.trim();
    if (!name) { alert('Name is required'); return; }
    try {
        await fetch('/api/admin/attributes', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ name, type, description: desc })
        });
        closeModal('attr-modal');
        document.getElementById('attr-name').value = '';
        document.getElementById('attr-desc').value = '';
        loadAttributes();
    } catch (e) { alert('Failed to add attribute'); }
}

async function deleteAttr(id) {
    if (!confirm('Delete this attribute?')) return;
    try {
        await fetch(`/api/admin/attributes/${id}`, { method: 'DELETE' });
        loadAttributes();
        loadMappings();
    } catch (e) { alert('Failed to delete'); }
}

// ── MAPPINGS ───────────────────────────────────────────────────────────────
async function loadMappings() {
    try {
        // Build mapping table from career data
        const careersRes = await fetch('/api/careers');
        const careersJson = await careersRes.json();
        const careers = careersJson.careers || careersJson;

        const attrsRes = await fetch('/api/attributes');
        const attrsJson = await attrsRes.json();
        const attrs = attrsJson.attributes || attrsJson;

        const careerMap = {};
        careers.forEach(c => careerMap[c.id] = c.title);
        const attrMap = {};
        attrs.forEach(a => attrMap[a.id] = a.name);

        // Fetch mappings for each career
        const allMappings = [];
        for (const c of careers) {
            try {
                const res = await fetch(`/api/careers/${c.id}`);
                const data = await res.json();
                if (data.attributes) {
                    data.attributes.forEach(a => {
                        allMappings.push({
                            career_id: c.id,
                            career_title: c.title,
                            attribute_id: a.id,
                            attribute_name: a.name || attrMap[a.id] || `#${a.id}`,
                            weight: a.weight || 5
                        });
                    });
                }
            } catch (e) { /* skip */ }
        }

        renderMappings(allMappings);
    } catch (e) { console.error('Failed to load mappings:', e); }
}

function renderMappings(mappings) {
    const tbody = document.getElementById('mappings-tbody');
    if (!mappings.length) {
        tbody.innerHTML = '<tr><td colspan="4" class="text-center py-10 text-gray-400">No mappings found.</td></tr>';
        return;
    }
    tbody.innerHTML = mappings.map(m => `
        <tr>
            <td class="font-medium text-gray-800">${m.career_title}</td>
            <td class="text-gray-600">${m.attribute_name}</td>
            <td><span class="inline-block px-2.5 py-1 rounded-md text-xs font-medium bg-gray-100 text-gray-600">${m.weight}</span></td>
            <td class="text-right">
                <button class="inline-flex items-center gap-1 px-3 py-1.5 rounded-lg text-xs font-medium text-red-500 hover:bg-red-50 transition-colors"
                    onclick="deleteMapping(${m.career_id}, ${m.attribute_id})">
                    <span class="material-icons-outlined" style="font-size:16px">delete</span> Delete
                </button>
            </td>
        </tr>`).join('');
}

function populateSelects() {
    const cs = document.getElementById('mapping-career');
    cs.innerHTML = allCareers.map(c => `<option value="${c.id}">${c.title}</option>`).join('');
    const as = document.getElementById('mapping-attr');
    as.innerHTML = allAttrs.map(a => `<option value="${a.id}">${a.name} (${a.type})</option>`).join('');
}

async function addMapping() {
    const career_id    = parseInt(document.getElementById('mapping-career').value);
    const attribute_id = parseInt(document.getElementById('mapping-attr').value);
    const weight       = parseInt(document.getElementById('mapping-weight').value) || 5;
    try {
        await fetch('/api/admin/mappings', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ career_id, attribute_id, weight })
        });
        closeModal('mapping-modal');
        loadMappings();
    } catch (e) { alert('Failed to add mapping'); }
}

async function deleteMapping(careerId, attrId) {
    if (!confirm('Delete this mapping?')) return;
    try {
        await fetch(`/api/admin/mappings?career_id=${careerId}&attribute_id=${attrId}`, {
            method: 'DELETE'
        });
        loadMappings();
    } catch (e) { alert('Failed to delete mapping'); }
}
