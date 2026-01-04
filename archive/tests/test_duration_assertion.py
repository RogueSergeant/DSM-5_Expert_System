"""
Test whether duration and subjective assertions are working correctly.
"""

import sys
from pathlib import Path

# Add project root to path for imports
project_root = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(project_root))

from src.search.manager import SessionManager

# Create manager and start session
manager = SessionManager()
manager.start_new_session()

print("Testing duration and subjective assertions...\n")

# Test 1: Duration check
print("=" * 60)
print("Test 1: Duration Assertion (GAD)")
print("=" * 60)

# Answer duration question
manager.answer_question("gad_duration_check", "YES")

# Check if duration was asserted
query = f"patient_duration('{manager.state.patient_id}', gad, Duration)"
result = list(manager.prolog.query(query))

if result:
    print(f"✓ Duration asserted successfully: {result[0]['Duration']} days")
else:
    print(f"✗ Duration NOT asserted!")

# Check if meets_duration_criteria
query2 = f"meets_duration_criteria('{manager.state.patient_id}', gad)"
result2 = list(manager.prolog.query(query2))

if result2:
    print(f"✓ meets_duration_criteria: SUCCESS")
else:
    print(f"✗ meets_duration_criteria: FAILED")

# Test 2: Subjective check
print("\n" + "=" * 60)
print("Test 2: Subjective Assertion (GAD clinical significance)")
print("=" * 60)

# Answer subjective question
manager.answer_question("gad_subj_clinical_significance", "YES")

# Check if subjective was asserted
query3 = f"subjective_assessment('{manager.state.patient_id}', gad_subj_clinical_significance, Status, Conf)"
result3 = list(manager.prolog.query(query3))

if result3:
    print(f"✓ Subjective asserted successfully: Status={result3[0]['Status']}, Conf={result3[0]['Conf']}")
else:
    print(f"✗ Subjective NOT asserted!")

# Check if meets_subjective_criteria
query4 = f"meets_subjective_criteria('{manager.state.patient_id}', gad)"
result4 = list(manager.prolog.query(query4))

if result4:
    print(f"✓ meets_subjective_criteria: SUCCESS")
else:
    print(f"✗ meets_subjective_criteria: FAILED")

    # Debug: Check what subjective criteria are defined for GAD
    query5 = "subjective_criterion(gad, CritID, Category, Question)"
    result5 = list(manager.prolog.query(query5))
    print(f"\nDefined subjective criteria for GAD:")
    for r in result5:
        print(f"  - {r['CritID']}: {r['Question'][:60]}...")

    # Check which ones are assessed
    query6 = f"subjective_assessment('{manager.state.patient_id}', CritID, Status, Conf)"
    result6 = list(manager.prolog.query(query6))
    print(f"\nAssessed subjective criteria:")
    for r in result6:
        print(f"  - {r['CritID']}: {r['Status']} (conf: {r['Conf']})")
