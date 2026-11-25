---
title: r-sitrep-pptx skill — Handoff State
date: 2026-05-22
author: Claude (Cowork session)
status: in-progress
---

# Handoff: r-sitrep-pptx skill — filename convention update

This document is a machine-readable handoff for resuming work in a fresh
conversation. All paths are absolute. All code snippets are copy-pasteable.

---

## 1. What the project is

The user (Aaron, abochner@rtsl.org) maintains an R-based weekly **Cholera
Situation Report**. The R pipeline renders a Word `.docx` sitrep. In prior
sessions we extended that pipeline so it also produces a **linked PowerPoint
deck**: every ggplot/flextable becomes its own slide via "Insert and Link"
(cached preview + external r:link), so re-running the R pipeline overwrites
the PNGs and PowerPoint's "Update Links" refreshes the deck.

That capability was packaged into a reusable Cowork skill called
**`r-sitrep-pptx`**, tested with evals (**3/3 evals passed, 26/26 assertions
passed**), packaged as a `.skill` file, and shipped.

---

## 2. What was accomplished (prior to this handoff)

- Built end-to-end R → PNG → linked PowerPoint workflow for the user's
  cholera sitrep.
- Solved 8 documented technical gotchas (see
  `outputs/r-sitrep-pptx/references/gotchas.md`):
  1. Windows `png()` black-bottom-half bug → use `ragg::agg_png`.
  2. `BiocGenerics::align` S4 masking → prefix all flextable calls with
     `flextable::`.
  3. Highlights flextable variable dimensions → render via a fixed
     12 × 5.2 in @ 150 dpi ggplot canvas instead.
  4. Map whitespace gap → two-pass `trim_white()` (edge + interior gap
     collapse).
  5. PowerPoint relative paths fail for external image links → use
     absolute Windows paths.
  6. `<a:blip>` needs **both** `r:embed` (cached preview) and `r:link`
     (external link relationship) for "Insert and Link".
  7. `.pptx` is a zip and locks while open in PowerPoint → close before
     rebuild.
  8. OneDrive sparse-placeholder files → ensure files are materialized.
- Created the `r-sitrep-pptx` skill scaffolding under
  `outputs/r-sitrep-pptx/` (SKILL.md, scripts, references, evals).
- Evals: 3 specs (basic-cholera-from-scratch, explicit-ncdc-linked-only,
  custom-template-different-disease). All passed (26/26 assertions).
- Packaged as `outputs/r-sitrep-pptx.skill` and shipped.

---

## 3. Outstanding request (verbatim)

User's most recent instruction:

> "Yeah, please have the powerpoint names end in a similar '2026_W07'
> format - the only difference from what you suggested is two digit week
> numbers"

Original triggering message:

> "Can you have the final name for the powerpoint be saved with the year
> and week in the file name, using a structure just like the SitReps?"

User also said:

> "What is this using so many tokens, I provided a small piece of
> information?"

**Implication:** Stop asking clarifying questions. Implement the filename
change directly with **minimal tool calls**.

---

## 4. Decision (already made, do not re-ask)

**Final PowerPoint filename pattern:**

```
<Disease>_Sitrep_<YYYY>_W<##>_linked.pptx
```

- `<Disease>` — capitalized disease name (e.g. `Cholera`).
- `<YYYY>` — four-digit year from R `report_date`.
- `<##>` — **two-digit ISO week** from R `report_date` (e.g. `07`, not `7`).
- `_linked` — preserved marker (was previously requested).
- Example for the current cholera deck: `Cholera_Sitrep_2026_W07_linked.pptx`
  (replace `07` with whatever week the active `report_date` falls in).

Mirrors the existing Word sitrep convention `Cholera_Sitrep_2026_W7.docx`
but uses zero-padded week as the user explicitly requested.

---

## 5. Current state of files

### 5.1 The user's cholera project (live, on OneDrive)

Root: `C:\Users\AaronBochner\OneDrive - Resolve to Save Lives\Documents\Claude\Generate Powerpoint from R outputs\cholera Sitrep`

Key files:

- `00_Run the report.R` — defines `report_date` (the single source of truth
  for the current sitrep week).
- `powerpoint/IMS_presentation_cholera_linked.pptx` — **needs renaming** to
  the new convention `Cholera_Sitrep_<YYYY>_W<##>_linked.pptx` for the
  current `report_date`.
- `powerpoint/` folder — contains the generated PNGs the deck links to via
  absolute paths.

### 5.2 The skill source (working copy, not yet repackaged)

Root: `C:\Users\AaronBochner\AppData\Roaming\Claude\local-agent-mode-sessions\b695e840-1705-4e0c-8ece-a59d05e4264e\b4c21e49-d63a-47e6-9976-c48dc5a21c12\local_1aca7031-9049-4229-b432-b2a6ffb2ac76\outputs\r-sitrep-pptx\`

In bash: `/sessions/focused-loving-clarke/mnt/outputs/r-sitrep-pptx/`

Files that **need edits**:

| File | Change |
|---|---|
| `scripts/build_pptx.py` | Accept `year` + `week` (or `report_date`) from config; build filename via `f"{base}_{year}_W{week:02d}_linked.pptx"`. |
| `references/r_export_template.R` | Derive `year <- format(report_date, "%Y")`; `week <- sprintf("%02d", lubridate::isoweek(report_date))`; pass both into the config consumed by `build_pptx.py`. |
| `SKILL.md` | Document the new filename convention `<Disease>_Sitrep_YYYY_W##_linked.pptx` (two-digit week). |
| `references/readme_template.md` | Replace `{DISEASE}_linked.pptx` examples with the new `{DISEASE}_Sitrep_{YYYY}_W{##}_linked.pptx` pattern. |

Files that do **not** need edits:

- `references/gotchas.md`
- `scripts/inspect_template.py`
- `evals/evals.json` (assertions don't pin filename — re-run to confirm)

### 5.3 The packaged skill

`outputs/r-sitrep-pptx.skill` — **stale**. Must be re-zipped after the
edits above.

---

## 6. Exact next steps

Do these in order, with the smallest number of tool calls possible.

### Step 1 — Edit `scripts/build_pptx.py`

Find the line that constructs `pptx_path` from `config["pptx_path"]`. Replace
with logic that:

1. Reads `year` and `week` from `config` (fail loudly if missing).
2. Reads `filename_base` from `config` (default: `f"{disease.capitalize()}_Sitrep"`).
3. Builds `pptx_filename = f"{base}_{year}_W{int(week):02d}_linked.pptx"`.
4. Joins it onto `config["output_dir"]` to produce `pptx_path`.

Pseudo-diff:

```python
# OLD
pptx_path = config["pptx_path"]

# NEW
output_dir = config["output_dir"]                       # absolute Windows path
disease    = config["disease"]                          # e.g. "cholera"
year       = str(config["year"])                        # "2026"
week       = int(config["week"])                        # 7
base       = config.get("filename_base", f"{disease.capitalize()}_Sitrep")
pptx_filename = f"{base}_{year}_W{week:02d}_linked.pptx"
pptx_path  = str(Path(output_dir) / pptx_filename)
```

### Step 2 — Edit `references/r_export_template.R`

Add near the top of the export block (after `report_date` is in scope):

```r
year <- format(report_date, "%Y")
week <- sprintf("%02d", lubridate::isoweek(report_date))
```

And in the JSON/config blob written for `build_pptx.py`, include:

```r
list(
  # ...existing keys...
  output_dir    = normalizePath(powerpoint_dir, winslash = "/"),
  disease       = "cholera",
  year          = year,
  week          = week,
  filename_base = "Cholera_Sitrep"   # optional; matches Word sitrep pattern
)
```

Remove any prior `pptx_path` key (it's now derived).

### Step 3 — Edit `SKILL.md`

Add a section under output naming (or update the existing one):

```markdown
### Output filename convention

The generated deck is named:

    <FilenameBase>_<YYYY>_W<##>_linked.pptx

where `<##>` is the **two-digit ISO week** (e.g. `07`, not `7`) derived
from the R `report_date`, and `<FilenameBase>` defaults to
`<Disease>_Sitrep` (e.g. `Cholera_Sitrep`). Example:

    Cholera_Sitrep_2026_W07_linked.pptx

This mirrors the Word sitrep naming convention so weekly outputs sort
together.
```

### Step 4 — Edit `references/readme_template.md`

Find any example mentioning `{DISEASE}_linked.pptx` and replace with
`{DISEASE}_Sitrep_{YYYY}_W{##}_linked.pptx`. Add a one-line note that
`{##}` is the two-digit ISO week.

### Step 5 — Rename the user's live deck

In the user's project folder:

- Determine the current week:
  - Read `00_Run the report.R` for the `report_date` value.
  - Compute `sprintf("%02d", lubridate::isoweek(report_date))` → e.g. `W21`
    for the current date (2026-05-22 is in ISO week 21).
- Rename `powerpoint/IMS_presentation_cholera_linked.pptx` →
  `powerpoint/Cholera_Sitrep_2026_W21_linked.pptx` (substitute the actual
  week the user's `report_date` resolves to — do **not** assume W21 without
  reading `00_Run the report.R`).
- The PNGs the deck links to do not change names; only the deck itself is
  renamed. If the user's R pipeline writes a `pptx_path` to disk anywhere,
  update that too.

### Step 6 — Re-run the skill's evals (sanity)

From `outputs/r-sitrep-pptx/`:

```bash
cd /sessions/focused-loving-clarke/mnt/outputs/r-sitrep-pptx
python evals/run_evals.py
```

Expect 3/3 evals, 26/26 assertions. If any assertion now pins the old
filename, update the assertion to the new convention.

### Step 7 — Repackage the skill

```bash
cd /sessions/focused-loving-clarke/mnt/outputs
python /sessions/focused-loving-clarke/mnt/.claude/skills/skill-creator/scripts/package_skill.py r-sitrep-pptx
```

This produces `outputs/r-sitrep-pptx.skill`. Present it to the user with
`mcp__cowork__present_files` so they can re-install.

### Step 8 — Confirm to the user

Brief message: which filename was used for their live deck, that the skill
was repackaged, and link both files. **No long postamble.** Aaron has
already flagged token usage as a concern.

---

## 7. Operating constraints (carry into the new session)

1. **Minimal tool calls.** Aaron has explicitly complained about token
   usage. Do not re-read files already covered in this handoff. Do not ask
   clarifying questions — the decision in §4 is final.
2. **No `AskUserQuestion` for this task.** The previous session tried and
   the permission stream errored; Aaron then answered in text and was
   frustrated. Just implement.
3. **Two-digit week is non-negotiable.** Use `:02d` / `sprintf("%02d", ...)`.
4. **Preserve `_linked` suffix** in the deck filename.
5. **Absolute Windows paths** for any image links the deck depends on
   (existing pipeline already does this — don't break it).
6. **Don't touch unrelated parts of the skill.** The 8 gotchas in
   `references/gotchas.md` are load-bearing.

---

## 8. Reference paths (quick index)

```
USER PROJECT (live, OneDrive):
  C:\Users\AaronBochner\OneDrive - Resolve to Save Lives\Documents\Claude\Generate Powerpoint from R outputs\cholera Sitrep\
    00_Run the report.R                 # report_date lives here
    powerpoint\IMS_presentation_cholera_linked.pptx   # RENAME

SKILL SOURCE (working copy):
  Windows:
    C:\Users\AaronBochner\AppData\Roaming\Claude\local-agent-mode-sessions\b695e840-1705-4e0c-8ece-a59d05e4264e\b4c21e49-d63a-47e6-9976-c48dc5a21c12\local_1aca7031-9049-4229-b432-b2a6ffb2ac76\outputs\r-sitrep-pptx\
  Bash:
    /sessions/focused-loving-clarke/mnt/outputs/r-sitrep-pptx/

    SKILL.md                              # EDIT — document filename convention
    scripts/build_pptx.py                 # EDIT — derive filename from year+week
    scripts/inspect_template.py           # no change
    references/r_export_template.R        # EDIT — derive year + 2-digit week from report_date
    references/readme_template.md         # EDIT — update example filename
    references/gotchas.md                 # no change
    evals/evals.json                      # re-run; update if any assertion pins old filename

SKILL PACKAGE (stale):
  /sessions/focused-loving-clarke/mnt/outputs/r-sitrep-pptx.skill       # REPACKAGE

TRANSCRIPT OF PRIOR SESSION (if needed):
  C:\Users\AaronBochner\AppData\Roaming\Claude\local-agent-mode-sessions\b695e840-1705-4e0c-8ece-a59d05e4264e\b4c21e49-d63a-47e6-9976-c48dc5a21c12\local_1aca7031-9049-4229-b432-b2a6ffb2ac76\.claude\projects\C--Users-AaronBochner-AppData-Roaming-Claude-local-agent-mode-sessions-b695e840-1705-4e0c-8ece-a59d05e4264e-b4c21e49-d63a-47e6-9976-c48dc5a21c12-local-1aca7031-9049-4229-b432-b2a6ffb2ac76-outputs\e1852700-0773-44ba-af07-36948b7b6620.jsonl
```

---

## 9. First message to send to the new session

> "Here is the handoff summary from our previous session: open
> `C:\Users\AaronBochner\OneDrive - Resolve to Save Lives\Documents\Claude\Generate Powerpoint from R outputs\cholera Sitrep\handoff_state.md`
> and resume at Step 1. Use minimal tool calls; do not ask clarifying
> questions — all decisions are in §4."
