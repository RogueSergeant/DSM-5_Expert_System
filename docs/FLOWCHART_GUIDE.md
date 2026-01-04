# System Architecture Flowchart Guide

## Overview

The `system-architecture-flowchart.drawio` file provides a comprehensive visual representation of the entire DSM-5 Diagnostic Decision Support System, from LLM knowledge extraction through to final diagnosis generation.

## Opening the Flowchart

### Option 1: Online (Recommended)
1. Go to [app.diagrams.net](https://app.diagrams.net) (formerly draw.io)
2. Click **File → Open From → Device**
3. Select `docs/system-architecture-flowchart.drawio`
4. The flowchart will render in your browser

### Option 2: Desktop App
1. Download draw.io desktop from [github.com/jgraph/drawio-desktop](https://github.com/jgraph/drawio-desktop/releases)
2. Open the `.drawio` file directly

### Option 3: VS Code Extension
1. Install the "Draw.io Integration" extension by Henning Dieterichs
2. Open the `.drawio` file in VS Code
3. Edit and view inline

## Flowchart Structure

The diagram is organized into **5 sequential phases**, each represented by a color-coded swimlane:

### **Phase 1: Knowledge Extraction** (Orange/Yellow) - Offline
- **Purpose**: Convert DSM-5-TR text to structured Prolog knowledge base
- **Key Components**:
  - DSM text input (`data/dsm5_text/`)
  - Provider selection (Anthropic/OpenAI/Ollama)
  - LLM API call with extended thinking
  - Validation pipeline (syntax + schema)
  - Output to production or timestamped files
- **Frequency**: Once per disorder

### **Phase 2: System Initialization** (Blue) - Once per Runtime
- **Purpose**: Load Prolog engine and knowledge base
- **Key Components**:
  - PrologEngine initialization
  - Load `schema.pl` (inference rules)
  - Load disorder files (`gold_standard/loader.pl`)
  - Knowledge base ready for queries
- **Frequency**: Once at application startup

### **Phase 3: Diagnostic Session** (Purple) - Per Patient
- **Purpose**: Initialize session state for new patient
- **Key Components**:
  - SessionManager initialization
  - DiagnosticState creation
  - Clear previous patient facts
  - Set patient context (age, etc.)
- **Frequency**: Once per patient

### **Phase 4: Iterative Questioning** (Pink/Red) - Loop
- **Purpose**: A* search-driven question selection and answer processing
- **Key Components**:
  - **LOOP START**: Get next best question using A* heuristic
  - **Search**: Expand search space, score questions
  - **User Interaction**: Present question, receive answer
  - **Processing**: Determine question type (symptom, exclusion, duration, subjective)
  - **Assertion**: Assert appropriate Prolog fact
  - **Alias Handling**: Auto-answer related symptoms
  - **Pruning**: Update diagnostic status, remove ruled-out disorders
  - **Loop Decision**: Continue or proceed to diagnosis
- **Frequency**: Iterative until sufficient evidence

### **Phase 5: Diagnosis Generation** (Green) - Final
- **Purpose**: Generate comprehensive diagnosis with explanation
- **Key Components**:
  - `full_diagnosis/3` Prolog predicate
  - Parallel criterion checks (symptoms, duration, onset, exclusions, subjective)
  - Overall status determination
  - Confidence calculation
  - Result dictionary construction
  - Optional explanation and visualisation
- **Frequency**: Once when ready

## Color Coding Legend

The flowchart uses consistent color coding for different component types:

| Color | Component Type | Example |
|-------|---------------|---------|
| **Blue** (#dae8fc) | Data/Files | DSM text, Prolog files, State objects |
| **Green** (#d5e8d4) | Processes/Functions | `run_extraction.py`, `answer_question()` |
| **Orange** (#ffe6cc) | LLM/External APIs | Anthropic, OpenAI, Ollama providers |
| **Red** (#f8cecc) | Decision Points | "Valid?", "Question Type?", "Continue?" |
| **Purple** (#e1d5e7) | Prolog Engine Operations | `assertz()`, `query()`, `retractall()` |
| **Gray** (#f5f5f5) | User Interface | User prompts, answer inputs |
| **Yellow** (#fff2cc) | Notes/Annotations | Optional paths, explanations |

## Key Architectural Patterns Shown

### 1. **Three-Tier Hybrid Architecture**
- **Tier A (Prolog)**: Objective criteria - shown in green process boxes
- **Tier B (LLM)**: Subjective criteria - shown in orange LLM boxes
- **Tier C (Integration)**: Confidence propagation - shown in calculation steps

### 2. **Python ↔ Prolog Interaction**
- Python processes (green) → Prolog operations (purple)
- Data flows through `assertz()`, `query()`, `retractall()`
- Bidirectional arrows show query-response pattern

### 3. **A* Search Optimization**
- Heuristic scoring clearly labeled with point values
- Search space expansion shown as subprocess
- Loop structure with pruning feedback

### 4. **Validation Pipeline**
- Sequential validation steps (syntax → schema)
- Decision diamond for pass/fail
- Branching to success or error paths

## Reading the Flowchart

### Flow Direction
- **Top to Bottom**: Main progression through phases
- **Left to Right**: Process flow within each phase
- **Loops**: Curved arrows returning to earlier states

### Arrow Types
- **Solid arrows**: Primary data/control flow
- **Dashed arrows**: Optional or informational connections
- **Thick arrows**: Cross-phase transitions (major flow)

### Shape Meanings
- **Rectangles (rounded)**: Processes, functions
- **Parallelograms**: Data, files, objects
- **Diamonds**: Decision points
- **Hexagons**: User interface elements
- **Ellipses**: Start/end points, state transitions

## Navigation Tips

1. **Zoom Controls**: Use mouse wheel or toolbar zoom (Ctrl/Cmd + Mouse Wheel)
2. **Panning**: Click and drag on empty space to pan around
3. **Select Elements**: Click on shapes to see details
4. **Layers**: If available, toggle layers for different views
5. **Export**: File → Export As → PNG/PDF/SVG for presentations

## Important Data Transformations Highlighted

### 1. **DSM Text → Prolog** (Phase 1)
- Shows prompt engineering step
- LLM thinking parameters clearly labeled
- Validation ensuring correctness

### 2. **User Answer → Prolog Fact** (Phase 4)
- Normalization step shown
- Type determination branching
- All assertion types shown (symptom, exclusion, duration, subjective)

### 3. **Prolog Inference → Diagnosis Result** (Phase 5)
- Parallel criterion checks visualised
- Aggregation into overall status
- Confidence calculation formula

## Use Cases for This Flowchart

### For Developers
- **Understanding**: See how all components interact
- **Debugging**: Trace execution path for bugs
- **Extension**: Identify where to add new features
- **Onboarding**: Quick visual introduction to system

### For Academic Presentation
- **Architecture Explanation**: Show hybrid AI design
- **Methodology**: Demonstrate systematic approach
- **Validation**: Highlight quality assurance steps
- **Complexity**: Justify engineering decisions

### For Documentation
- **Reference**: Visual complement to code comments
- **Design Decisions**: Show why architecture is structured this way
- **Integration Points**: Identify API boundaries

## Technical Details Shown

### Prolog Predicates
- Knowledge Base: `disorder/3`, `symptom/4`, `symptom_category/5`
- Patient Facts: `patient_symptom/4`, `patient_duration/3`, `patient_exclusion_status/3`
- Inference: `meets_symptom_criteria/2`, `criterion_check/5`, `full_diagnosis/3`

### Python Classes/Functions
- `PrologEngine`: Wrapper for pyswip
- `SessionManager`: State management and pruning
- `DiagnosticSearch`: A* heuristic search
- `run_extraction.py`: CLI entry point

### LLM Parameters
- **Anthropic**: `thinking_budget` (tokens)
- **OpenAI**: `reasoning_effort` (levels)
- **Ollama**: `think` (low/medium/high)

## Editing the Flowchart

If you need to modify or extend the flowchart:

1. **Open in draw.io** (app.diagrams.net or desktop)
2. **Add new shapes**: Drag from left panel
3. **Match styling**: Use eyedropper tool or copy format
4. **Maintain consistency**: Follow color coding and shape conventions
5. **Update arrows**: Right-click → Edit Style for arrow types
6. **Save**: File → Save (overwrites `.drawio` file)
7. **Export**: File → Export As → PNG for embedding in docs

## Complementary Documentation

This flowchart complements:
- `docs/ARCHITECTURE.md` - Detailed textual description
- `docs/API_REFERENCE.md` - Prolog predicate reference
- `docs/GETTING_STARTED.md` - Practical usage guide
- `CLAUDE.md` - Developer quick reference

## Common Questions

**Q: Why are some arrows dashed?**
A: Dashed arrows indicate optional flows, informational connections, or subprocess details that are not part of the main execution path.

**Q: What do the thick cross-phase arrows represent?**
A: These show the major transitions between lifecycle phases (e.g., extracted knowledge → initialized system → diagnostic session).

**Q: Can I export this to PDF?**
A: Yes! Open in draw.io and use File → Export As → PDF. Recommended settings: Fit to 1 page wide, landscape orientation.

**Q: How do I update this if I add a new disorder?**
A: The Phase 1 flow remains the same (just change the disorder name in data boxes). Phase 2 would list the new disorder in the "Load Disorders" step.

**Q: Is this flowchart auto-generated from code?**
A: No, it's manually created based on comprehensive code analysis. This ensures clarity and educational value over mechanical accuracy.

## Troubleshooting

**Flowchart won't open**:
- Ensure you're using a compatible viewer (draw.io app/web, VS Code extension)
- Check file isn't corrupted (should be valid XML)

**Shapes are overlapping**:
- Reset zoom to 100%
- Try different page sizes: File → Page Setup → Fit

**Can't find specific component**:
- Use Ctrl/Cmd+F to search for text in shapes
- Check layer visibility if layers are used

**Export quality is poor**:
- Use higher DPI setting (300+ for print)
- Export as SVG for infinite scalability
- Increase zoom level before export

---

**Created**: January 2026
**Format**: draw.io XML (mxGraph)
**Dimensions**: 1600×2400px (optimised for A4 portrait)
**Complexity**: 100+ shapes, 5 major phases, full system lifecycle
